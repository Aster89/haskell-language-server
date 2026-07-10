{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Ide.Plugin.CaseSplit
  ( descriptor
  , Log
  ) where

import qualified Data.Map                                 as M
import qualified Data.Text                                as T
import           Development.IDE                          hiding (line)
import           Development.IDE.Core.PluginUtils
import           Ide.Plugin.Error
import           Ide.Types
import qualified Language.LSP.Protocol.Message            as LSP
import           Language.LSP.Protocol.Message (Method(Method_TextDocumentCodeAction))
import qualified Development.IDE.Core.Shake               as Shake
import Language.LSP.Protocol.Types
import Development.IDE.GHC.Compat (GhcMessage (GhcDsMessage), HsMatchContext (CaseAlt), Outputable (ppr), showSDocUnsafe, HscEnv (hsc_dflags), ConLike (RealDataCon), NamedThing (getName), HoleKind (HoleVar), mkVarOcc)
import Development.IDE.GHC.Compat.Error (DsMessage(DsNonExhaustivePatterns), msgEnvelopeErrorL)
import Data.Maybe (mapMaybe, listToMaybe, fromMaybe)
import Control.Lens (Fold, prism', (^?), (<&>), (^.))
import GHC.HsToCore.Pmc.Solver.Types (PmAltConApp(..), PmAltCon(..), TmState (ts_facts), Nabla (nabla_tm_st), VarInfo (vi_pos))
import GHC.Types.Unique.SDFM
import GHC.Types.Name.Reader (nameRdrName, mkRdrUnqual)
import GHC (DynFlags(maxUncoveredPatterns), ParsedModule (pm_parsed_source), HasLoc (getHasLoc), realSrcSpan, srcSpanStartCol, EpAnnHsCase (hsCaseAnnCase), EpToken (EpTok), EpaLocation' (EpaSpan), AnnList (AnnList), AnnListBrackets (ListBraces, ListNone), LMatch)
import Development.IDE.GHC.Compat (getLoc)
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Development.IDE.GHC.Compat.ExactPrint
import Data.Data (Data(), gmapQ)
import Data.Generics.Schemes (everywhere)
import Debug.Trace
import Type.Reflection (eqTypeRep,
                        type (:~~:) (HRefl),
                        typeRep, typeOf)
import GHC.Hs (GhcPs, deltaPos, unnamedHoleRdrName)
import GHC.Types.SrcLoc (SrcSpan, GenLocated (L), EpaLocation' (EpaDelta), srcLocSpan)
import Control.Monad (MonadPlus(mplus, mzero))
import Language.Haskell.Syntax.Expr (HsExpr (HsCase, HsHole), Match (..), GRHSs (GRHSs), GRHS (GRHS))
import Control.Monad.Trans (lift)
import Ide.PluginUtils (diffText, WithDeletions (IncludeDeletions))
import Development.IDE.Core.FileStore (getVersionedTextDoc)
import qualified Language.LSP.Protocol.Lens                   as L
import Language.Haskell.Syntax (MatchGroup (MG, mg_alts), LHsExpr, NoExtField (NoExtField), Pat (..), HsConDetails (PrefixCon), HsLocalBindsLR (EmptyLocalBinds))
import Language.Haskell.GHC.ExactPrint.Utils
import GHC (EpAnn(EpAnn))
import GHC.Parser.Annotation (noSrcSpanA, EpAnnComments (EpaComments), NoAnn (noAnn), EpUniToken (EpUniTok), IsUnicodeSyntax (NormalSyntax), emptyComments, TrailingAnn (AddSemiAnn), SrcSpanAnnA)
import Data.List.NonEmpty.Extra (singleton)
import Development.IDE.GHC.Compat.Core (GrhsAnn(..), AnnListItem (AnnListItem), HasSrcSpan, srcSpanStart, EpAnnHsCase (..))
import Data.List.Extra (allSame)

inRange :: SrcSpan -> Range -> Bool
inRange s range = maybe False (`isSubrangeOf` range) (srcSpanToRange s)

data Log
  = LogShake Shake.Log
  | LogWAEResponseError (LSP.TResponseError LSP.Method_WorkspaceApplyEdit)
  | forall a. (Pretty a) => LogResolve a

instance Pretty Log where
  pretty = \case
    LogShake logMsg -> pretty logMsg
    LogWAEResponseError rspErr -> "RequestWorkspaceApplyEdit Failed with " <+> pretty rspErr
    LogResolve msg -> pretty msg

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor _ plId = (defaultPluginDescriptor plId "Provides a code action to split case")
  { pluginHandlers = mkPluginHandler LSP.SMethod_TextDocumentCodeAction suggestCaseSplitProvider
  , pluginPriority = 1
  , pluginModifyDynflags = mempty { dynFlagsModifyGlobal = \dynFlags -> dynFlags { maxUncoveredPatterns = 30 } }
  }

astTraversalWith :: forall b r m. (MonadPlus m, Data b) => b -> (forall a. Data a => a -> m r) -> m r
astTraversalWith ast f = foldl mplus mzero $ flip gmapQ ast $ \y -> f y `mplus` astTraversalWith y f

suggestCaseSplitProvider :: PluginMethodHandler IdeState 'Method_TextDocumentCodeAction
suggestCaseSplitProvider
  state
  _
  CodeActionParams{ _textDocument
                  , _context = CodeActionContext{
                      _diagnostics = Diagnostic { _range = range }:_
                      }
                  }
  = do
  nfp <- getNormalizedFilePathE $ _textDocument ^. L.uri

  verTxtDocId <- liftIO $ runAction "dfdaias" state $ getVersionedTextDoc _textDocument

  pm <- runActionE "not important, as long as unique" state
      $ useE GetParsedModule nfp

  -- (hsc_dflags . hscEnv -> df)
  --   <- runActionE "classplugin.addMethodPlaceholders.GhcSessionDeps" state
  --     $ useE GhcSessionDeps nfp

  diags :: [FileDiagnostic] <- activeDiagnosticsInRange (shakeExtras state) nfp range
                                <&> \case Nothing -> error "Oops, no diagnostics!"
                                          Just fileDiags -> fileDiags

  case mapMaybe (\d -> fdStructuredMessage d ^? _SomeStructuredMessage
                                               . msgEnvelopeErrorL
                                               . _DsMessage) diags of
    [dsmsg] -- TODO: I need to filter for the diagnostic I want.
            -- It might important to check the case of two nested `case`
            -- expressions, both with non-exhaustive patterns.
            -> do
      let pmAltsConApp = dsMsgToPmAlts dsmsg -- TODO: convert to NonEmpty

      (old, new) <- handleMaybeM (PluginInternalError "Unable to makeEditText")
          $ liftIO $ runMaybeT
          $ makeEditText pm pmAltsConApp range

      caps <- lift pluginGetClientCapabilities

      let edit :: WorkspaceEdit -- TODO this should be passed to the CodeAction ctor below
              = diffText caps (verTxtDocId, old) new IncludeDeletions

      pure $ InL [InR
        $ CodeAction { _title       = "Add placeholders for all missing patterns"
                     , _kind        = Just CodeActionKind_QuickFix
                     , _diagnostics = Nothing -- TODO: I should probably encode here that it addresses the DsNonExhaustivePatterns diag
                     , _isPreferred = Nothing -- TODO: Just True?
                     , _disabled    = Nothing
                     , _edit        = Just edit
                     , _command     = Nothing
                     , _data_       = Nothing }]
    _ -> error "Oops, unexpected number of messages!"

  where
    pmAltsToWorkspaceEdit :: [PmAltConApp] -> WorkspaceEdit
    pmAltsToWorkspaceEdit = stringsToWorkspaceEdit . fmap pmAltToString

    stringsToWorkspaceEdit :: [String] -> WorkspaceEdit
    stringsToWorkspaceEdit = edit . T.pack . unlines

    pmAltToString :: PmAltConApp -> String
    pmAltToString pmalt = unwords
                        $ showPpr (paca_con pmalt):(const "_" <$> paca_ids pmalt)

    dsMsgToPmAlts :: DsMessage -> [PmAltConApp]
    dsMsgToPmAlts =
      \case DsNonExhaustivePatterns !CaseAlt _ _ ![ids] !nablas ->
                nablas <&>
                  (\nabla -> let facts = ts_facts $ nabla_tm_st nabla
                             in case vi_pos <$> lookupUSDFM facts ids of
                                   Just [x] -> x
                                   _ -> error "Oops")
            _ -> error "Oops, DsMessage of unexpected shape!"


    textEdit msg = let pragmaInsertRange = let p = _end range in Range p p
                       extract = T.init
                               . T.unlines
                               . map (("            " `T.append`) . (`T.append` " -> _"))
                               . T.lines
                   in TextEdit pragmaInsertRange $ "\n" `T.append` extract msg
    edit msg =
      WorkspaceEdit {
        _changes = Just $ M.singleton (_textDocument ^. L.uri) $ [textEdit msg]
      , _documentChanges = Nothing
      , _changeAnnotations = Nothing
      }

suggestCaseSplitProvider _ _ _ = pure $ InL []

_DsMessage :: Fold GhcMessage DsMessage
_DsMessage = prism' GhcDsMessage $ \case
  GhcDsMessage dsmsg -> Just dsmsg
  _ -> Nothing

showPpr ::  Outputable a => a -> String
showPpr = showSDocUnsafe . ppr

makeEditText :: Monad m => ParsedModule -> MissingPatterns -> Range -> MaybeT m (T.Text, T.Text)
makeEditText pm missingPs range = do

  -- BEGIN experiment with astTraversalWith
  --let !foo = astTraversalWith (pm_parsed_source pm)
  --                            (\node -> case eqTypeRep (typeRep @(HsExpr GhcPs)) (typeOf node) of
  --              Nothing -> Nothing
  --              Just HRefl -> case node :: HsExpr GhcPs of
  --                HsCase _ e _ | getHasLoc e `inRange` range -> trace "The case I want!" $ Just node
  --                HsCase _ _ _ -> trace "Another case" $ Nothing
  --                _ -> Nothing
  --              )
  -- END experiment with astTraversalWith

  let ps = pm_parsed_source pm
      !ps' = everywhere f ps
      !old = T.pack $ exactPrint ps
      !new = T.pack $ exactPrint ps'

  --let !foo = astTraversalWith ps'
  --                            (\node -> case eqTypeRep (typeRep @(HsExpr GhcPs)) (typeOf node) of
  --              Nothing -> Nothing
  --              Just HRefl -> case node :: HsExpr GhcPs of
  --                HsCase _ e _ | getHasLoc e `inRange` range -> trace "The case I want!" $ Just node
  --                HsCase _ _ _ -> trace "Another `case`" $ Nothing
  --                _ -> Nothing
  --              )

  pure (old, new)

    where
      f :: forall d. Data d => d -> d
      f = \node -> case typeOf node `eqTypeRep` typeRep @(HsExpr GhcPs) of
                Nothing -> node
                Just HRefl -> case node of
                  h@(HsCase x e existingPs) | getHasLoc e `inRange` range
                    -> let (braced, alts) = case mg_alts existingPs of
                                 (L (EpAnn _ (AnnList _ (ListBraces (EpTok (EpaSpan l)) _) _ _ _) _) ls) -> (Just (getStartCol l), ls)
                                 (L (EpAnn _ (AnnList _ ListNone _ _ _) _) ls) -> (Nothing, ls)
                                 _ -> error "Ooops"
                           onelined = case alts of
                                        [_] -> False
                                        _ -> allSame $ map getStartCol alts
                           indent = case alts of
                                 (l:_) -> trace "1-based column of the first alternative = " $
                                          traceShowId $
                                          getStartCol l
                                 _ -> case hsCaseAnnCase x of
                                         EpTok (EpaSpan l) -> trace "1-based column of `case` = " $
                                                              traceShowId
                                                              (getStartCol l)
                                                              + defaultIndent
                                         _ -> error "Missing `case` keyword???"
                       in trace (unlines [ "AST = " ++ showAst h
                                         , "braced = " ++ show braced
                                         , "indent = " ++ show indent
                                         , "onelined = " ++ show onelined]) $
                          HsCase x e $ addMissingPatterns (indent - fromMaybe 0 braced) missingPs existingPs
                  _ -> node

getStartCol :: HasSrcSpan a => a -> Int
getStartCol = srcSpanStartCol . realSrcSpan . getLoc

newIndentedLn :: Int -> GenLocated SrcSpanAnnA e -> GenLocated SrcSpanAnnA e
newIndentedLn indent (L _ e) = L l e
  where l = EpAnn (EpaDelta noSrcSpanA (deltaPos 1 indent) [])
                  (AnnListItem [])
                  emptyComments

addMissingPatterns :: Int -> MissingPatterns -> MatchGroup GhcPs (LHsExpr GhcPs) -> MatchGroup GhcPs (LHsExpr GhcPs)
addMissingPatterns indent missing mg@(MG { mg_alts = L l [] })
  = mg { mg_alts = L l       (zipWith ($)
                                      (newIndentedLn indent:repeat (newIndentedLn 0))
                                      (makeMatch <$> missing)) }
addMissingPatterns _ missing mg@(MG { mg_alts = L l as })
  = let indent = getStartCol (getLoc l) in
    mg { mg_alts = L l (as ++ zipWith ($)
                                      (newIndentedLn 0:repeat (newIndentedLn 0))
                                      (makeMatch <$> missing)) }
    -- TODO: In the braced case, `a` should be added semicolon if it doesn't have it yet

makeMatch :: PmAltConApp -> LMatch GhcPs (LHsExpr GhcPs)
makeMatch PACA{ paca_con = PmAltConLike (RealDataCon ctor), .. }
      = L noSrcSpanA
        $ Match { m_ext = NoExtField
                , m_ctxt = CaseAlt
                , m_pats = L noSrcSpanA
                         $ [L noSrcSpanA ConPat { pat_con_ext = (Nothing, Nothing)
                                                , pat_con = L noSrcSpanA $ nameRdrName $ getName ctor
                                                , pat_args = PrefixCon $ map (const $ L noAnnSrcSpanDP1 $ WildPat NoExtField) paca_ids
                                                }]
                , m_grhss = GRHSs emptyComments
                                  -- TODO: remember to respect unicode arrow of preceeding cases or the -XUnicodeSyntax (https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/unicode_syntax.html#extension-UnicodeSyntax)
                                  -- TODO: check whether ga_sep default choice is really not printing anything.
                                  (singleton $ L noSrcSpanA $ GRHS (EpAnn noSrcSpanA
                                                                          (GrhsAnn{ ga_vbar = Nothing
                                                                                  , ga_sep = Right (EpUniTok d1 NormalSyntax) })
                                                                          emptyComments) []
                                                            $ L noSrcSpanA $ HsHole $ HoleVar $ L noAnnSrcSpanDP1
                                             $ unnamedHoleRdrName)
                                  (EmptyLocalBinds NoExtField)
                }
makeMatch _ = error "boom"

type MissingPatterns = [PmAltConApp]

defaultIndent :: Int
defaultIndent = 2

{-
 -
(SourceLoc, [MissingParam]`

11:38

Magnus says:- seek the source location
- take AST node of the case statement
... list of alternatives
- expand the list of alternatives with the missing cases
- exactprint the entire thing

11:39



Magnus says:show the resulting AST

11:39

Magnus says:- range
- the given node that I'm looking at is a case statement

11:41

Magnus says:
https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Data-Data.html#t:Data


11:43

Magnus says:
https://hackage-content.haskell.org/package/ghc-exactprint-1.14.0.0/docs/Language-Haskell-GHC-ExactPrint-Transform.html


11:43

Magnus says:
https://hackage-content.haskell.org/package/ghc-9.14.1/docs/GHC-Hs-Pat.html#t:Pat


https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Data-Data.html#v:gfoldl


12:27



Magnus says:HRefl <- typeOf thing (typeOf (undefined :: HsExpr Ps))

12:28

Magnus says:
https://github.com/haskell/haskell-language-server/pull/4672/changes#diff-ff019bb7466e44fc95c1d9aab09aab975941acee015706389c8712f53019d6dbR165-R196


 -}
