{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OrPatterns #-}

module Ide.Plugin.CaseSplit
  ( caseSplitPluginCodeActionTitle
  , descriptor
  , Log
  ) where

import Control.Arrow ((&&&))
import Control.Lens (Fold, prism', (^?), (<&>), (^.))
import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.State.Strict (MonadState (get, put), State, evalState)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT)
import Data.Bifunctor (bimap)
import Data.Data (Data())
import Data.Function (on, (&))
import Data.Generics.Schemes (everywhereM)
import Data.List (minimumBy)
import Data.List.Extra (chunksOf)
import Data.Maybe (mapMaybe)
import Data.Semigroup (sconcat)
import Development.IDE (Pretty (pretty), Recorder, WithPriority, IdeState (shakeExtras), FileDiagnostic (fdStructuredMessage), runAction, GetParsedModule (GetParsedModule), srcSpanToRange, HscEnvEq (hscEnv), GhcSessionDeps (GhcSessionDeps))
import Development.IDE.Core.FileStore (getVersionedTextDoc)
import Development.IDE.Core.PluginUtils (runActionE, useE, activeDiagnosticsInRange)
import Development.IDE.GHC.Compat (GhcMessage (GhcDsMessage), HsMatchContext (CaseAlt), ConLike (RealDataCon), NamedThing (getName), HoleKind (HoleVar), HscEnv (hsc_dflags), Id)
import Development.IDE.GHC.Compat (getLoc)
import Development.IDE.GHC.Compat.Error (DsMessage(DsNonExhaustivePatterns), msgEnvelopeErrorL)
import Development.IDE.GHC.Compat.ExactPrint (exactPrint, d0, d1, noAnnSrcSpanDP1, setEntryDP, getEntryDP)
import Development.IDE.Types.Diagnostics (_SomeStructuredMessage, FileDiagnostic (fdLspDiagnostic))
import GHC (DynFlags(extensions), ParsedModule (pm_parsed_source), HasLoc (getHasLoc), realSrcSpan, EpToken (EpTok), AnnList (AnnList), AnnListBrackets (ListBraces), LMatch)
import GHC (EpAnn(EpAnn))
import GHC.Driver.DynFlags (OnOff(On))
import GHC.Hs (GhcPs, deltaPos, unnamedHoleRdrName, DeltaPos (deltaColumn), getDeltaLine, HsRecFields(HsRecFields))
import GHC.HsToCore.Pmc.Solver.Types (PmAltConApp(..), PmAltCon(..), TmState (ts_facts), Nabla (nabla_tm_st), VarInfo (vi_pos))
import GHC.Parser.Annotation (noSrcSpanA, EpUniToken (EpUniTok), IsUnicodeSyntax (NormalSyntax, UnicodeSyntax), emptyComments, TrailingAnn (AddSemiAnn), addTrailingAnnToA, AnnList (al_anchor))
import GHC.Types.Name.Reader (nameRdrName)
import GHC.Types.SrcLoc (SrcSpan(RealSrcSpan), GenLocated (L), combineSrcSpans)
import GHC.Types.Unique.SDFM (lookupUSDFM)
import Ide.Logger ((<+>))
import Ide.Plugin.Error (getNormalizedFilePathE, PluginError (PluginInternalError, PluginStaleResolve))
import Ide.PluginUtils (diffText, WithDeletions (IncludeDeletions))
import Ide.Types (defaultPluginDescriptor, mkPluginHandler, pluginGetClientCapabilities, PluginDescriptor(pluginHandlers, pluginPriority), PluginId, PluginMethodHandler)
import Language.Haskell.Syntax (MatchGroup (MG, mg_alts), LHsExpr, NoExtField (NoExtField), Pat (..), HsConDetails (PrefixCon, RecCon), HsLocalBindsLR (EmptyLocalBinds))
import Language.Haskell.Syntax.Expr (HsExpr (HsCase, HsHole), Match (..), GRHSs (GRHSs), GRHS (GRHS))
import Type.Reflection (eqTypeRep, type (:~~:) (HRefl), typeRep, typeOf)
import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE (singleton, fromList, zipWith, toList, map, splitAt, groupBy1, length, last)
import           Data.List.NonEmpty.Extra ((|:))
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
suggestCaseSplitProvider state _ CodeActionParams{ _textDocument, _range = cursor }
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
                -- discard those with 'Nothing' ds messages and unwrap the surviving 'Just's
                & (mapMaybe sequence :: [(a, Maybe b)] -> [(a, b)])
                -- wrap back in the monad
                & pure

  (diag, pmAltsConApps) <-
    if | null fileDiagAndDsMsg
          -> throwE $ PluginInternalError "Error in retrieving diagnostics at the cursor."
       | otherwise
          -> fileDiagAndDsMsg
             -- obtain the innermost
             & minimumBy (ordSubrange `on` Diag._range . fdLspDiagnostic . fst)
             -- extract the 'Diagnostic' and the pattern-match constructos
             & bimap fdLspDiagnostic dsMsgToPmAlts
             & pure

  if | Nothing <- pmAltsConApps
          -> throwE PluginStaleResolve
     | Just [] <- pmAltsConApps
          -> pure $ InL [] -- This happens when the type of the expression is unknown.
     | Just (NE.fromList -> pmAltsConApps) <- pmAltsConApps
     , Just (old, new) <- makeEditText pm pmAltsConApps cursor arrowSyntax -> do

        caps <- lift pluginGetClientCapabilities

        pure $ InL [InR
          $ CodeAction { _title       = caseSplitPluginCodeActionTitle
                       , _kind        = Just CodeActionKind_QuickFix
                       , _diagnostics = Just [diag]
                       , _isPreferred = Nothing
                       , _disabled    = Nothing
                       , _edit        = Just $ diffText caps (verTxtDocId, old) new IncludeDeletions
                       , _command     = Nothing
                       , _data_       = Nothing }]
     | otherwise
          -> throwE $ PluginInternalError "Error in updating the AST."
  where

    getMaybeDsMsg :: FileDiagnostic -> Maybe DsMessage
    getMaybeDsMsg d = fdStructuredMessage d ^? _SomeStructuredMessage . msgEnvelopeErrorL . _DsMessage

    dsMsgToPmAlts :: DsMessage -> Maybe [PmAltConApp]
    dsMsgToPmAlts =
      \case DsNonExhaustivePatterns CaseAlt _ _ [identifier] nablas -> nablasToPmAlts identifier nablas
            DsNonExhaustivePatterns (LamAlt LamCase) _ _ _ _ -> Just [] -- TODO: implement this
            _ -> Nothing

caseSplitPluginCodeActionTitle :: Text
caseSplitPluginCodeActionTitle = "Add placeholders for the first `-fmax-uncovered-patterns` missing patterns"

-- | Retrieve list of pattern match constructors
-- for the type identified by the given 'Id'.
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

type MissingPatterns = NonEmpty PmAltConApp

-- | Given a 'ParsedModule' this function uses 'exactPrint' to produce the
-- 'Text's of said module before and after the 'MissingPatterns' are appended
-- to the existing ones in the innermost @case@ expression enclosing the
-- 'Range' of the cursor, using the arrow style passed as the last
-- 'IsUnicodeSyntax' argument.
makeEditText :: ParsedModule -> MissingPatterns -> Range -> IsUnicodeSyntax -> Maybe (Text, Text)
makeEditText pm missingPs cursor arrowSyntax =

  let ps = pm_parsed_source pm
      old = T.pack $ exactPrint ps
      -- We want to update exactly one node of the AST, the one that is
      -- associated to the innermost @case@ expression containing the cursor,
      -- therefore:
      ps' = runMaybeT (everywhereM (go arrowSyntax) ps) -- we transform the 'ParsedSource' bottom-up
                                          -- (allowing failure, incidentally),
            `evalState` False -- and we pass a 'Bool' through 'State' to bail
                               -- out after one update.
      new = fmap (T.pack . exactPrint) ps'

  in sequence (old, new)

    where
      go :: forall a. Data a => IsUnicodeSyntax -> a -> MaybeT (State Bool) a
      go arrow node = do
          found <- get
          if | -- Proceed only if we haven't found & edited the node yet,
               not found
               -- only inspect nodes of the appropriate type,
             , Just HRefl <- typeOf node `eqTypeRep` typeRep @(HsExpr GhcPs)
               -- only inspect @case@ expressions (and deconstruct the bits),
             , HsCase extCase scrut existingPs <- node
               -- extract @case@ and @of@ tokens,
             , EpAnnHsCase (EpTok caseTok) (EpTok ofTok) <- extCase
               -- extract the end-of-case-expression "token",
             , MG _ (L (EpAnn endTok _ _) _) <- existingPs
               -- get the location for the three tokens,
             , let caseSSpan = getHasLoc caseTok
                   ofSSpan = getHasLoc ofTok
                   endSSpan = getHasLoc endTok
               -- make sure the cursor is somewhere in this @case@ expression,
             , cursor `inSpan` caseExprSpan caseSSpan ofSSpan endSSpan
               -> do -- take note we've found the node,
                     put True
                     -- make a match out of each missing pattern,
                     let missingPs' = traverse (makeMatch arrow) missingPs
                     -- and append missing patterns to existing ones.
                     case appendMissingPats existingPs =<< missingPs' of
                        -- If something goes wrong, we communicate abortion,
                        Nothing -> mzero
                        -- otherwise we continue.
                        Just newPats -> pure $ HsCase extCase scrut newPats
             -- Anything else, leave the node unchanged.
             | otherwise -> pure node

-- | Given the 'SrcSpan' of the @case@ token, the @of@ token, and the end of
-- the alternatives, this function combines them to return a 'SrcSpan' that goes
-- from the @case@ token to the end of the whole @case@ expression.
caseExprSpan :: SrcSpan -> SrcSpan -> SrcSpan -> SrcSpan
caseExprSpan caseSSpan _ endSSpan@(RealSrcSpan _ _) = combineSrcSpans caseSSpan endSSpan
caseExprSpan caseSSpan ofSSpan _ = combineSrcSpans caseSSpan ofSSpan

-- | Predicate telling the given 'Range' falls within the given 'SrcSpan'.
inSpan :: Range -> SrcSpan -> Bool
inSpan range s = maybe False (range `isSubrangeOf`) (srcSpanToRange s)

-- | Given a 'MatchGroup' and a list of 'LMatch'es, this function inserts the
-- latter matches in the former group, trying to honor the existing layout,
-- returning the new 'MatchGroup' in the 'Maybe' monad to account for failure.
--
-- Honoring the existing layout means two things:
--
--   1. producing valid code, which means:
--
--      - adding semicolons wherever they are needed, i.e.
--
--        - if patterns are braced, for every patterns,
--
--        - otherwise, for all but the last patterns for groups of patterns
--          that are not aligned vertically, e.g.
--
--            - patterns shown on the same line, which this plugin can produce,
--
--            - patterns shown on different lines but in a "staircase" way,
--              which this plugin never produces).
--
--      - using the correct indentation when patterns are not braced (when
--        patterns are braced, the code will stay valid irrespective of the
--        indentation of the alternatives).
--
--   2. such valid code tries to adhere to the existing layout, which means:
--
--      - don't alter position of existing patterns nor of the opening @{@;
--
--      - when patterns are not braced, we align the first pattern we insert
--        with the pre-existing previous pattern
--
--      - we have to make some arbitrary decision
--
--        - when patterns are not braced and no previous pattern exists,
--          we indent by @indentation def@ with respect to whatever layout
--          context is the current one;
--
--        - as regards the number of patterns to print per line, we inspect the
--          last group of patterns appearing on one line, to determine how many
--          patterns per line we insert.
--
--        - when patterns are braced, we also align them vertically (it would
--          not be necessary, in principle).
--
--
-- Refer to test cases to see practical examples.
appendMissingPats :: MatchGroup GhcPs (LHsExpr GhcPs) -> NonEmpty (LMatch GhcPs (LHsExpr GhcPs)) -> Maybe (MatchGroup GhcPs (LHsExpr GhcPs))
-- No matches present yet,
appendMissingPats mg@(MG { mg_alts = L l [] }) missing
  = Just $ mg { mg_alts = L l (NE.toList $ NE.zipWith ($)
                                                      (fmt $ getOpeningBraceCol l) -- so we format every
                                                      missing                      -- pattern to insert.
                              ) }
    where
      -- | Formatting here means to indent and, if needed, to add semicolons to
      -- the **individual** (hence the return type is a list of endomorphisms)
      -- matches to be inserted.
      --
      -- The @Maybe Int@ argument encodes whether the alternatives are wrapped
      -- in braces and, if so, it provides the column where the @{@ is located.
      fmt :: Maybe Int -> NonEmpty (LMatch GhcPs (LHsExpr GhcPs) -> LMatch GhcPs (LHsExpr GhcPs))
      fmt = -- Each pattern is put on its own line
            NE.map (setDPLine 1 .)
          . \case
                  -- When patterns are not braced, the the first pattern is
                  -- indented by @indentation def@ (with respect to the current
                  -- layout context), and all following patterns are on the
                  -- same column as that.
                  Nothing -> setDPCol (indentation def) :| repeat (setDPCol 0)
                  -- When patterns are braced,
                  Just _ -> -- all patterns are indented by @indentation def@,
                            -- because they are all referred to the opening @{@
                            NE.map (setDPCol (indentation def) .)
                            -- and all but the last pattern need a @;@.
                          $ replicate (length missing - 1) addSemiCol |: id

-- There are already existing patterns, so we can safely turn the input list in
-- a 'NonEmpty'.
appendMissingPats mg@(MG { mg_alts = L altsLoc@(EpAnn _ ann _) (NE.fromList -> existings) }) missing
  = if | -- Retrieve the column of the anchor.
         Just anchor <- getStartCol . getHasLoc <$> al_anchor ann
       , let
             -- Group patterns that are on the same line,
             ptrnGrps = NE.groupBy1 isOnelined existings
             -- and get the length of the last group.
             nLastGr = NE.length $ NE.last ptrnGrps

             missingGrps = -- Group the patterns to be inserted with the same
                           -- stride as the length of the last group.
                           chunksOf1 nLastGr missing
                           -- For each group
                         <&> \ms -> NE.zipWith ($)
                                               (NE.zipWith (.)
                                                           -- only the first pattern goes
                                                           -- on a new line and is indented,
                                                           -- while the others go on the
                                                           -- same line, one space apart;
                                                           (setDP 1 indent :| repeat (setDP 0 1))
                                                           -- all but the last pattern
                                                           -- get a semicolon.
                                                           (replicate (length ms - 1) addSemiCol |: id)) -- TODO: gimme a name
                                               ms

             -- Here we compute the indent (used above) and the function to add
             -- semicolons, to the end of each group:
             (indent, addSemiCols) = case getOpeningBraceCol altsLoc of
                -- if the patters are not braced, then all inserted patterns
                -- need 0 indentation and no semicolon;
                Nothing -> (0, id)
                -- if they are braced, then
                Just openingBrace ->
                                     -- they need to be indented with respect
                                     -- to the opening brace
                                     ( anchor - openingBrace
                                     -- and all but the last item need a
                                     -- semicolon (and we avoid, checking the
                                     -- non-last groups because they're
                                     -- supposed to have the semicolon already
                                     , NE.zipWith ($)
                                                  (replicate (NE.length ptrnGrps - 1) id <>
                                                   replicate (length missingGrps) (mapLast addSemiCol) |: id)
                                     )

        -> Just $ mg { mg_alts = L altsLoc (NE.toList $ sconcat $ addSemiCols $ ptrnGrps <> missingGrps) }

       | otherwise -> Nothing

isSemiCol :: TrailingAnn -> Bool
isSemiCol (AddSemiAnn _) = True
isSemiCol _ = False

-- | Given a 'PmAltConApp', this function produces an 'LMatch' to be inserted
-- in the list of existing 'LMatch'es contained by a 'MatchGroup'.
--
-- The returned 'LMatch' is wrapped in 'Maybe' to account for failure, and it
-- is constructed in its entirety, by passing "default" values wherever
-- possible, except, obviously, for two:
--
--  - the constructor name,
--  - the arguments to the constructor, all rendered as individual underscores
--    when there's less than @maxUnderscores def@, or as a single @{}@ otherwise.
--
-- As regards the monad transformers,
--
-- The first argument of type 'UnicodeSyntax' simply contains the symbol used
-- for the arrow, which can be @->@ or @→@ depending on whether the 'UnicodeSyntax'
-- language extension is being used.
makeMatch :: IsUnicodeSyntax -> PmAltConApp -> Maybe (LMatch GhcPs (LHsExpr GhcPs))
makeMatch arrow PACA{ paca_con = PmAltConLike (RealDataCon dataCon)
                    , paca_ids
                    }
        = let -- Extract the name of the constructor
              ctorName = L noSrcSpanA $ nameRdrName $ getName dataCon
              -- assemble the construtor with the arguments, adding
              -- underscores or empty braces:
              ctor = case length paca_ids of
                              -- for low number of arguments
                              n | n <= maxUnderscores def
                                   -- create as many underscores as needed
                                -> ConPat { pat_con_ext = (Nothing, Nothing)
                                          , pat_con = ctorName
                                          , pat_args = PrefixCon $ map (const $ L noAnnSrcSpanDP1 $ WildPat NoExtField) paca_ids
                                          }
                                   -- otherwise use braces.
                              _ -> ConPat { pat_con_ext = (Just (EpTok d1), Just (EpTok d0))
                                          , pat_con = ctorName
                                          , pat_args = RecCon (HsRecFields NoExtField [] Nothing)
                                          }
          in do Just $ L noSrcSpanA
                 $ Match { m_ext = NoExtField
                         , m_ctxt = CaseAlt
                         , m_pats = L noSrcSpanA [L noSrcSpanA ctor]
                         , m_grhss = GRHSs emptyComments
                                           -- TODO: check whether ga_sep default choice is really not printing anything.
                                           (NE.singleton $ L noSrcSpanA $ GRHS (EpAnn noSrcSpanA
                                                                                      (GrhsAnn{ ga_vbar = Nothing
                                                                                              , ga_sep = Right $ EpUniTok d1 arrow })
                                                                                      emptyComments) []
                                                         $ L noSrcSpanA $ HsHole $ HoleVar $ L noAnnSrcSpanDP1 $ unnamedHoleRdrName)
                                           (EmptyLocalBinds NoExtField)
                         }
makeMatch _ _ = Nothing

data Default = Default {
  -- | Max number of underscores to show for the constructor of an alternative.
  -- Beyond this, the record syntax with empty braces is used.
  maxUnderscores :: Int
  -- | Indentation used when there's no existing alternatives to refer to.
  -- Such indentation is with respect to the current layout context.
, indentation :: Int
  -- TODO other things that we could store here are:
  --
  --    - the maximum number of alternatives on one line
  --    - whether or not to put the @;@ for the last alternative
}

def :: Default
def = Default { maxUnderscores = 3
              , indentation = 2 }

-- | Predicate telling if two located annotations are (actually, start) on the
-- same line.
isOnelined :: LocatedAn ann e -> LocatedAn ann e -> Bool
isOnelined = (==) `on` getStartLine

-- | Given an @EpAnn (AnnList a)@ return the starting column of
-- its opening brace, if any, otherwise 'Nothing'.
getOpeningBraceCol :: EpAnn (AnnList a) -> Maybe Int
getOpeningBraceCol (EpAnn _ (AnnList _ (ListBraces (EpTok col) _) _ _ _) _) = Just $ getStartCol $ getHasLoc col
getOpeningBraceCol _ = Nothing

-- | Get the starting column of an 'HasSrcSpan'.
getStartCol :: HasSrcSpan a => a -> Int
getStartCol = srcSpanStartCol . realSrcSpan . getLoc

-- | Get the starting line of an 'HasSrcSpan'.
getStartLine :: HasSrcSpan a => a -> Int
getStartLine = srcSpanStartLine . realSrcSpan . getLoc

-- | Set the DeltaPos for the given annotation.
setDP :: Int -> Int -> LocatedAn t a -> LocatedAn t a
setDP deltaLine deltaColumn lann = setEntryDP lann $ deltaPos deltaLine deltaColumn

-- | Set the deltaColumn for the given annotation.
setDPCol :: Int -> LocatedAn t a -> LocatedAn t a
setDPCol deltaColumn lann = setEntryDP lann
                          $ (\d -> deltaPos (getDeltaLine d) deltaColumn)
                          $ getEntryDP lann

-- | Set the deltaLine for the given annotation.
setDPLine :: Int -> LocatedAn t a -> LocatedAn t a
setDPLine deltaLine lann = setEntryDP lann
                          $ (\d -> deltaPos deltaLine (deltaColumn d))
                          $ getEntryDP lann

-- | Add semicolon, unless one is already present.
addSemiCol :: LocatedAn AnnListItem a -> LocatedAn AnnListItem a
addSemiCol (L l@(EpAnn _ ls _) e)
  | none isSemiCol (lann_trailing ls)
  = L (addTrailingAnnToA (AddSemiAnn (EpTok d0)) emptyComments l) e
addSemiCol l = l

-- | Version of 'Data.List.Extra.chunksOf' (**not** to be confused with
-- 'Data.List.Split.chunksOf') for a 'NonEmpty' lists.
chunksOf1 :: Int -> NonEmpty a -> NonEmpty (NonEmpty a)
chunksOf1 n xs
  | n >= 1
  , (b:before, after) <- NE.splitAt n xs
    = (b :| before) :| case after of
                         [] -> []
                         _ -> map NE.fromList $ chunksOf n after
  | otherwise = error "chunksOf1: the `Int` argument should be ≥ 1"

-- | Maps a funciton f over the last element of a 'NonEmpty' list.
mapLast :: (a -> a) -> NonEmpty a -> NonEmpty a
mapLast f (a :| []) = f a :| []
mapLast f (a :| as) = a :| mapLast' f as
  where
    mapLast' f as = init as ++ [f $ last as]

-- | Convenient negation of 'any'.
none :: Foldable t => (a -> Bool) -> t a -> Bool
none p xs = not $ any p xs
