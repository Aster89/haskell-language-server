{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OrPatterns #-}

module Ide.Plugin.CaseSplit
  ( descriptor
  , Log
  ) where

import Control.Arrow ((&&&), second)
import Control.Lens (Fold, prism', (^?), (<&>), (^.))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (runReader, MonadReader (ask))
import Control.Monad.State.Strict (MonadState (get, put), evalStateT)
import Control.Monad.Trans (lift)
import Data.Bifunctor (bimap)
import Data.Data (Data())
import Data.Function (on, (&))
import Data.Generics.Schemes (everywhereM)
import Data.List (groupBy, minimumBy)
import Data.List.Extra (chunksOf)
import Data.Maybe (fromJust, isJust)
import Development.IDE (Pretty (pretty), Recorder, WithPriority, IdeState (shakeExtras), FileDiagnostic (fdStructuredMessage), runAction, GetParsedModule (GetParsedModule), srcSpanToRange, HscEnvEq (hscEnv), GhcSessionDeps (GhcSessionDeps))
import Development.IDE.Core.FileStore (getVersionedTextDoc)
import Development.IDE.Core.PluginUtils (runActionE, useE, activeDiagnosticsInRange)
import Development.IDE.GHC.Compat (GhcMessage (GhcDsMessage), HsMatchContext (CaseAlt), ConLike (RealDataCon), NamedThing (getName), HoleKind (HoleVar), HscEnv (hsc_dflags), Id)
import Development.IDE.GHC.Compat (getLoc)
import Development.IDE.GHC.Compat.Error (DsMessage(DsNonExhaustivePatterns), msgEnvelopeErrorL)
import Development.IDE.GHC.Compat.ExactPrint (exactPrint, d0, d1, noAnnSrcSpanDP1, setEntryDP)
import Development.IDE.Types.Diagnostics (_SomeStructuredMessage, FileDiagnostic (fdLspDiagnostic))
import GHC (DynFlags(extensions), ParsedModule (pm_parsed_source), HasLoc (getHasLoc), realSrcSpan, EpToken (EpTok), EpaLocation' (EpaSpan), AnnList (AnnList), AnnListBrackets (ListBraces), LMatch)
import GHC (EpAnn(EpAnn))
import GHC.Driver.DynFlags (OnOff(On))
import GHC.Hs (GhcPs, deltaPos, unnamedHoleRdrName)
import GHC.HsToCore.Pmc.Solver.Types (PmAltConApp(..), PmAltCon(..), TmState (ts_facts), Nabla (nabla_tm_st), VarInfo (vi_pos))
import GHC.Parser.Annotation (noSrcSpanA, EpUniToken (EpUniTok), IsUnicodeSyntax (NormalSyntax, UnicodeSyntax), emptyComments, TrailingAnn (AddSemiAnn), addTrailingAnnToA, AnnList (al_brackets, al_anchor))
import GHC.Types.Name.Reader (nameRdrName)
import GHC.Types.SrcLoc (SrcSpan(RealSrcSpan), GenLocated (L), combineSrcSpans)
import GHC.Types.Unique.SDFM (lookupUSDFM)
import Ide.Logger ((<+>))
import Ide.Plugin.Error (getNormalizedFilePathE, PluginError (PluginInternalError))
import Ide.PluginUtils (diffText, WithDeletions (IncludeDeletions))
import Ide.Types (defaultPluginDescriptor, mkPluginHandler, pluginGetClientCapabilities, PluginDescriptor(pluginHandlers, pluginPriority), PluginId, PluginMethodHandler)
import Language.Haskell.Syntax (MatchGroup (MG, mg_alts), LHsExpr, NoExtField (NoExtField), Pat (..), HsConDetails (PrefixCon), HsLocalBindsLR (EmptyLocalBinds))
import Language.Haskell.Syntax.Expr (HsExpr (HsCase, HsHole), Match (..), GRHSs (GRHSs), GRHS (GRHS))
import Type.Reflection (eqTypeRep, type (:~~:) (HRefl), typeRep, typeOf)
import qualified Data.List.NonEmpty.Extra as NE (singleton, fromList, NonEmpty, zipWith, toList, map, splitAt)
import           Data.Text (Text)
import qualified Data.Text                                as T
import qualified Development.IDE.Core.Shake               as Shake
import qualified Language.LSP.Protocol.Lens                   as L
import           Language.LSP.Protocol.Message (Method(Method_TextDocumentCodeAction))
import qualified Language.LSP.Protocol.Message            as LSP
import           Language.LSP.Protocol.Types (Range, CodeActionParams(CodeActionParams, _range, _textDocument), CodeActionKind(CodeActionKind_QuickFix), type (|?)(InR, InL), CodeAction(..), isSubrangeOf)
import qualified Language.LSP.Protocol.Types as Diag (Diagnostic(_range))
import           Development.IDE.GHC.Compat.Core (GrhsAnn(..), HasSrcSpan, srcSpanStartLine, LocatedAn, lann_trailing, AnnListItem, srcSpanStartCol, EpAnnHsCase (EpAnnHsCase), HsMatchContext (LamAlt), HsLamVariant (LamCase))
import qualified Development.IDE.GHC.Compat.Core as Ext (Extension (UnicodeSyntax))
import Control.Monad.Trans.Except (throwE)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty.Extra ((|:))
import Data.Semigroup (sconcat)

data Log where
  LogShake :: Shake.Log -> Log
  LogWAEResponseError :: LSP.TResponseError LSP.Method_WorkspaceApplyEdit -> Log
  LogResolve :: Pretty a => a -> Log

instance Pretty Log where
  pretty = \case
    LogShake logMsg -> "LogShake " <+> pretty logMsg
    LogWAEResponseError rspErr -> "RequestWorkspaceApplyEdit Failed with " <+> pretty rspErr
    LogResolve msg -> "LogResolve " <+> pretty msg

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor _ plId = (defaultPluginDescriptor plId "Provides the split case code action")
  { pluginHandlers = mkPluginHandler LSP.SMethod_TextDocumentCodeAction suggestCaseSplitProvider
  , pluginPriority = 1
  }

suggestCaseSplitProvider :: PluginMethodHandler IdeState 'Method_TextDocumentCodeAction
suggestCaseSplitProvider
  state
  _
  CodeActionParams{ _textDocument
                  , _range = cursor
                  }
  = do
  nfp <- getNormalizedFilePathE $ _textDocument ^. L.uri

  verTxtDocId <- liftIO $ runAction "CaseSplit.GetVersionedTextDoc" state $ getVersionedTextDoc _textDocument

  (hsc_dflags . hscEnv -> dynFlags) <- runActionE "CaseSplit.GhcSessionDeps" state $ useE GhcSessionDeps nfp

  let arrowSyntax = if On Ext.UnicodeSyntax `elem` extensions dynFlags
                      then UnicodeSyntax
                      else NormalSyntax

  pm <- runActionE "CaseSplit.GetParsedModule" state $ useE GetParsedModule nfp

  fileDiags <- activeDiagnosticsInRange (shakeExtras state) nfp cursor

  fileDiagAndDsMsg
    <- if | (Nothing; Just []) <- fileDiags
             -> throwE $ PluginInternalError "Error in retrieving diagnostics at the cursor."
          | Just fileDiags@(_:_) <- fileDiags
             -> fileDiags
                -- pair each file diag with its ds messages, if any
                & fmap (id &&& getMaybeDsMsg)
                -- discard those with `Nothing` ds messages
                & filter (isJust . snd)
                -- unwrap the surviving `Just`s
                & fmap (second fromJust)
                -- wrap back in the monad
                & pure

  (diag, pmAltsConApps) <-
    if | null fileDiagAndDsMsg
          -> throwE $ PluginInternalError "Error in retrieving diagnostics at the cursor."
       | otherwise
          -> fileDiagAndDsMsg
             -- obtain the innermost
             & minimumBy (ordSubrange `on` Diag._range . fdLspDiagnostic . fst)
             -- extract the `Diagnostic` and the pattern-match constructos
             & bimap fdLspDiagnostic dsMsgToPmAlts
             & pure

  if | Nothing <- pmAltsConApps
          -> throwE $ PluginInternalError "Error in retrieving missing patterns."
     | Just [] <- pmAltsConApps
          -> pure $ InL [] -- This happens when the type of the expression is unknown.
     | Just (NE.fromList -> pmAltsConApps) <- pmAltsConApps -> do

        (old, new) <- liftIO $ makeEditText pm pmAltsConApps cursor arrowSyntax

        caps <- lift pluginGetClientCapabilities

        pure $ InL [InR
          $ CodeAction { _title       = "Add placeholders for all missing patterns"
                       , _kind        = Just CodeActionKind_QuickFix
                       , _diagnostics = Just [diag]
                       , _isPreferred = Nothing
                       , _disabled    = Nothing
                       , _edit        = Just $ diffText caps (verTxtDocId, old) new IncludeDeletions
                       , _command     = Nothing
                       , _data_       = Nothing }]
  where

    getMaybeDsMsg :: FileDiagnostic -> Maybe DsMessage
    getMaybeDsMsg d = fdStructuredMessage d ^? _SomeStructuredMessage . msgEnvelopeErrorL . _DsMessage

    dsMsgToPmAlts :: DsMessage -> Maybe [PmAltConApp]
    dsMsgToPmAlts =
      \case DsNonExhaustivePatterns !CaseAlt _ _ ![identifier] !nablas -> nablasToPmAlts identifier nablas
            DsNonExhaustivePatterns (LamAlt LamCase) _ _ _ _ -> Just [] -- TODO: implement this
            _ -> Nothing

-- | Retrieve list of pattern match constructors
-- for the type identified by the given `Id` -- TODO: I have to review if this means anything at all
--
-- Relevant information at https://simon.peytonjones.org/assets/pdfs/lower-your-guards.pdf
nablasToPmAlts :: Id -> [Nabla] -> Maybe [PmAltConApp]
nablasToPmAlts identifier nablas = fmap concat $ traverse go nablas
  where
    go = fmap vi_pos
       . flip lookupUSDFM identifier
       . ts_facts
       . nabla_tm_st

-- | Assign an 'Ordering' to two 'Range's @r1@ and @r2@ of which either is assumed to be subset of the other.
-- Will throw a runtime error if @r1@ is not a subrange of @r2@ or vice versa.
ordSubrange :: Range -> Range -> Ordering
ordSubrange r1 r2
  | r1 == r2 = EQ
  | r1 `isSubrangeOf` r2 = LT
  | r2 `isSubrangeOf` r1 = GT
  | otherwise = error "ordSubrange: ranges are not subranges of each other"

_DsMessage :: Fold GhcMessage DsMessage
_DsMessage = prism' GhcDsMessage $ \case
  GhcDsMessage dsmsg -> Just dsmsg
  _ -> Nothing

-- | Given a `ParsedModule` this function uses `exactPrint` to produce the
-- `Text`s of said module before and after the `MissingPatterns` are appended
-- to the existing ones in the innermost `case` expression enclosing the
-- `Range` of the cursor, using the arrow style passed as the last
-- `IsUnicodeSyntax` argument.
makeEditText :: ParsedModule -> MissingPatterns -> Range -> IsUnicodeSyntax -> IO (Text, Text)
makeEditText pm missingPs cursor arrowSyntax = do

  let ps = pm_parsed_source pm
      old = T.pack $ exactPrint ps
      ps' = everywhereM go ps -- We transform the `ParsedSource` bottom-up
            `evalStateT` False -- and we pass a `Bool` through `State` to update only one node.
            `runReader` arrowSyntax
      new = T.pack $ exactPrint ps'

  pure (old, new)

    where
      go :: forall d m. (MonadState Bool m, MonadReader IsUnicodeSyntax m, Data d) => d -> m d
      go node = do
          found <- get
          if | not found
             -- ^ Proceed only if we haven't found & edited the node yet,
             , Just HRefl <- typeOf node `eqTypeRep` typeRep @(HsExpr GhcPs)
             -- ^ only inspect nodes of the appropriate type,
             , HsCase extCase scrut existingPs <- node
             -- ^ only inspect `case` expressions (and deconstruct the bits),
             , (EpAnnHsCase (EpTok caseTok) (EpTok ofTok)) <- extCase
             -- ^ extract `case` and `of` tokens,
             , (MG _ (L (EpAnn endTok _ _) _)) <- existingPs
             -- ^ extract the end-of-case-expression "token",
             , let caseSSpan = getHasLoc caseTok
                   ofSSpan = getHasLoc ofTok
                   endSSpan = getHasLoc endTok
             -- ^ get the location for the three tokens,
             , cursor `inSpan`  caseExprSpan caseSSpan ofSSpan endSSpan
             -- ^ make sure the cursor is somewhere in this `case` expression,
               -> do put True
                     -- ^ take note we've found the node,
                     missingPs' <- traverse makeMatch missingPs
                     -- ^ make a match out of each missing pattern,
                     pure $ HsCase extCase scrut $ appendMissingPats existingPs missingPs'
                     -- ^ and append missing patterns to existing ones.
             | otherwise -> pure node
             -- ^ Anything else, leave the node unchanged.

-- | Given the `SrcSpan` of the `case` token, the `of` token, and the end of
-- the alternatives, this function combines them to return a `SrcSpan` that goes
-- from the `case` token to the end of the whole `case` expression.
caseExprSpan :: SrcSpan -> SrcSpan -> SrcSpan -> SrcSpan
caseExprSpan caseSSpan _ endSSpan@(RealSrcSpan _ _) = combineSrcSpans caseSSpan endSSpan
caseExprSpan caseSSpan ofSSpan _ = combineSrcSpans caseSSpan ofSSpan

-- | Predicate telling the given `Range` falls within the given `SrcSpan`.
inSpan :: Range -> SrcSpan -> Bool
inSpan range s = maybe False (range `isSubrangeOf`) (srcSpanToRange s)

-- | Predicate telling if two located annotations are (actually, start) on the
-- same line.
isOnelined :: LocatedAn ann e -> LocatedAn ann e -> Bool
isOnelined = (==) `on` getStartLine

-- | Predicate telling whether an
isBraced :: EpAnn (AnnList a) -> Bool
isBraced (EpAnn _ (AnnList _ (ListBraces (EpTok (EpaSpan _)) _) _ _ _) _) = True
isBraced _ = False

-- | Get the starting column of an `HasSrcSpan`.
getStartCol :: HasSrcSpan a => a -> Int
getStartCol = srcSpanStartCol . realSrcSpan . getLoc

-- | Get the starting line of an `HasSrcSpan`.
getStartLine :: HasSrcSpan a => a -> Int
getStartLine = srcSpanStartLine . realSrcSpan . getLoc

-- | Set the DeltaPos for the given annotation.
setDP :: Int -> Int -> LocatedAn t a -> LocatedAn t a
setDP deltaLine deltaColumn lann = setEntryDP lann $ deltaPos deltaLine deltaColumn

-- | Add semicolon, unless one is already present.
addSemiCol :: LocatedAn AnnListItem a -> LocatedAn AnnListItem a
addSemiCol (L l@(EpAnn _ ls _) e)
  | none isSemiCol (lann_trailing ls)
  = L (addTrailingAnnToA (AddSemiAnn (EpTok d0)) emptyComments l) e
addSemiCol l = l

-- | Given a `MatchGroup` and a list of `LMatch`s, this function inserts the
-- latter matches in the former group, trying to honor the existing layout.
--
-- When no existing matches are present yet, we insert the missing ones one per
-- line, adding semicolons if the alternatives are braced.
--
-- When some matches are already present, we write as many missing matches per
-- line as there are in the last line of the pre-existing ones (see
-- `TSomePatternsOnOneLineNoBraces.hs` and
-- `TSomePatternsOnOneLineNoBraces.expected.hs`).
appendMissingPats :: MatchGroup GhcPs (LHsExpr GhcPs) -> NE.NonEmpty (LMatch GhcPs (LHsExpr GhcPs)) -> MatchGroup GhcPs (LHsExpr GhcPs)
-- no matches present yet
appendMissingPats mg@(MG { mg_alts = L l [] }) missing
  = mg { mg_alts = L l (NE.toList $ NE.zipWith ($) (fmt $ isBraced l) missing) }
    where
      fmt True = NE.map (setDP 1 defaultIndent .)
               $ replicate (length missing - 1) addSemiCol |: id
      fmt False = setDP 1 defaultIndent :| repeat (setDP 1 0)

      -- | Default indentation, with respect to the current layout context, to
      -- use when there's no matches present yet.
      defaultIndent :: Int
      defaultIndent = 2


-- there are already existing patterns - add the ones that are missing
appendMissingPats mg@(MG { mg_alts = L altsLoc@(EpAnn _ ann _) existings }) missing
  = if | let brackets = al_brackets ann
       , Just anchor <- getStartCol . getHasLoc <$> al_anchor ann
        -> mg { mg_alts = L altsLoc (NE.toList $ alts anchor brackets) }
       | otherwise -> error "This should not be possible"
  where
    alts anchor brackets
         = let -- groups of patterns on the same line
               ptrnGrps = groupBy isOnelined existings
               nGrps = length ptrnGrps
               -- length of the last group of ≥1 patterns written on one line
               nLastGr = ptrnGrps
                       & last
                       & length
               missingGrps = chunksOf1 nLastGr missing
                           <&> \case (m :| ms) -> NE.zipWith ($)
                                                             (replicate (length ms) addSemiCol |: id)
                                                             (setDP 1 indent m :| map (setDP 0 1) ms)
               (indent, addSemiCols)
                 = if isBraced altsLoc
                       then (anchor - case brackets of
                                     ListBraces (EpTok (getStartCol . getHasLoc -> col)) _ -> col
                                     _ -> error "this is impossible"
                            ,NE.zipWith ($) (replicate (nGrps - 1) id <> replicate (length missingGrps) (mapLast addSemiCol) |: id))
                       else (0, id)

           in sconcat $ addSemiCols $ NE.fromList (map NE.fromList ptrnGrps) <> missingGrps

-- | Version of `Data.List.Extra.chunksOf` (**not** to be confused with
-- `Data.List.Split.chunksOf`) for a `NonEmpty` lists.
chunksOf1 :: Int -> NE.NonEmpty a -> NE.NonEmpty (NE.NonEmpty a)
chunksOf1 n xs
  | n >= 1
  , (b:before, after) <- NE.splitAt n xs
    = (b :| before) :| case after of
                         [] -> []
                         _ -> map NE.fromList $ chunksOf n after
  | otherwise = error "chunksOf1: the `Int` argument should be ≥ 1"

-- | Maps a funciton f over the last element of a `NonEmpty` list.
mapLast :: (a -> a) -> NE.NonEmpty a -> NE.NonEmpty a
mapLast f (a :| []) = f a :| []
mapLast f (a :| as) = a :| mapLast' f as
  where
    mapLast' f as = init as ++ [f $ last as]

isSemiCol :: TrailingAnn -> Bool
isSemiCol (AddSemiAnn _) = True
isSemiCol _ = False

makeMatch :: MonadReader IsUnicodeSyntax m => PmAltConApp -> m (LMatch GhcPs (LHsExpr GhcPs))
makeMatch PACA{ paca_con = PmAltConLike (RealDataCon ctor)
              , paca_ids }
        = do arrow <- ask
             pure $ L noSrcSpanA
              $ Match { m_ext = NoExtField
                      , m_ctxt = CaseAlt
                      , m_pats = L noSrcSpanA
                               $ [L noSrcSpanA ConPat { pat_con_ext = (Nothing, Nothing)
                                                      , pat_con = L noSrcSpanA $ nameRdrName $ getName ctor
                                                      , pat_args = PrefixCon $ map (const $ L noAnnSrcSpanDP1 $ WildPat NoExtField) paca_ids
                                                      }]
                      , m_grhss = GRHSs emptyComments
                                        -- TODO: check whether ga_sep default choice is really not printing anything.
                                        (NE.singleton $ L noSrcSpanA $ GRHS (EpAnn noSrcSpanA
                                                                                   (GrhsAnn{ ga_vbar = Nothing
                                                                                           , ga_sep = Right $ EpUniTok d1 arrow })
                                                                                   emptyComments) []
                                                      $ L noSrcSpanA $ HsHole $ HoleVar $ L noAnnSrcSpanDP1 $ unnamedHoleRdrName)
                                        (EmptyLocalBinds NoExtField)
                      }
makeMatch _ = error "boom"

type MissingPatterns = NE.NonEmpty PmAltConApp

none :: Foldable t => (a -> Bool) -> t a -> Bool
none p xs = not $ any p xs
