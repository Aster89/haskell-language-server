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
import qualified Development.IDE.Core.Shake               as Shake
import Language.LSP.Protocol.Types
import Development.IDE.GHC.Compat (GhcMessage (GhcDsMessage), HsMatchContext (CaseAlt), Outputable (ppr), showSDocUnsafe, HscEnv (hsc_dflags))
import Development.IDE.GHC.Compat.Error (DsMessage(DsNonExhaustivePatterns), msgEnvelopeErrorL)
import Data.Maybe (mapMaybe)
import Control.Lens (Fold, prism', (^?), (<&>))
import GHC.HsToCore.Pmc.Solver.Types
import GHC.Types.Unique.SDFM
import GHC (DynFlags(maxUncoveredPatterns), ParsedModule (pm_parsed_source), ParsedSource, HasLoc (getHasLoc), Located)
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Development.IDE.GHC.Compat.ExactPrint
import Data.Data (Data(), gmapQ)
import Debug.Trace
import GHC (EpAnnHsCase(EpAnnHsCase))
import Type.Reflection (eqTypeRep,
                        type (:~~:) (HRefl),
                        typeRep, typeOf)
import GHC.Hs (GhcPs)
import GHC.Types.SrcLoc (SrcSpan)
import Ide.PluginUtils (subRange)
import Control.Monad (MonadPlus(mplus, mzero))
import Language.Haskell.Syntax (HsModule)
import Language.Haskell.Syntax.Decls (HsDecl (ValD))
import Language.Haskell.Syntax.Expr (HsExpr (HsCase))

inRange :: Range -> SrcSpan -> Bool
inRange range s = maybe False (subRange range) (srcSpanToRange s)

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
  , pluginModifyDynflags = mempty { dynFlagsModifyGlobal = \dynFlags -> dynFlags { maxUncoveredPatterns = 20 } }
  }

astTraversalWith :: forall b r m. (MonadPlus m, Data b) => b -> (forall a. Data a => a -> m r) -> m r
astTraversalWith ast f = foldl mplus mzero $ flip gmapQ ast $ \y -> f y `mplus` astTraversalWith y f

suggestCaseSplitProvider :: PluginMethodHandler IdeState 'LSP.Method_TextDocumentCodeAction
suggestCaseSplitProvider
  state
  _
  CodeActionParams{ _textDocument = TextDocumentIdentifier{..}
                  , _context = CodeActionContext{ _diagnostics = Diagnostic { _range = range }:_ }
                  }
  = do
  nfp <- getNormalizedFilePathE _uri

  pm <- runActionE "not important, as long as unique" state
      $ useE GetParsedModule nfp

  (hsc_dflags . hscEnv -> df) <- runActionE "classplugin.addMethodPlaceholders.GhcSessionDeps" state
      $ useE GhcSessionDeps nfp

  let _missing'patterns :: MissingPatterns = _
  let _see'the'class'plugin'for'inspiration :: T.Text -> T.Text -> WorkspaceEdit = _

  (old, new) <- handleMaybeM (PluginInternalError "Unable to makeEditText")
      $ liftIO $ runMaybeT
      $ makeEditText pm _missing'patterns range

  let edit :: WorkspaceEdit -- TODO this should be passed to the CodeAction ctor below
        = _see'the'class'plugin'for'inspiration old new

  diags :: [FileDiagnostic] <- activeDiagnosticsInRange (shakeExtras state) nfp range
                                <&> \case Nothing -> error "Oops, no diagnostics!"
                                          Just fileDiags -> fileDiags

  case mapMaybe (\d -> fdStructuredMessage d ^? _SomeStructuredMessage . msgEnvelopeErrorL . _DsMessage) diags of
    [dsmsg] -> do
      pure $ InL [InR
        $ CodeAction { _title       = "Add placeholders for all missing patterns"
                     , _kind        = Just CodeActionKind_QuickFix
                     , _diagnostics = Nothing -- TODO: I should probably encode here that it addresses the DsNonExhaustivePatterns diag
                     , _isPreferred = Nothing -- TODO: Just True?
                     , _disabled    = Nothing
                     , _edit        = Just $ pmAltsToWorkspaceEdit $ dsMsgToPmAlts dsmsg
                     , _command     = Nothing
                     , _data_       = Nothing }]
    _ -> error "Oops, unexpected number of messages!"

  where
    pmAltsToWorkspaceEdit :: [PmAltConApp] -> WorkspaceEdit
    pmAltsToWorkspaceEdit = stringsToWorkspaceEdit . fmap pmAltToString

    stringsToWorkspaceEdit :: [String] -> WorkspaceEdit
    stringsToWorkspaceEdit = edit . T.pack . unlines

    pmAltToString :: PmAltConApp -> String
    pmAltToString pmalt = unwords $ showPpr (paca_con pmalt):(const "_" <$> paca_ids pmalt)

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
        _changes = Just $ M.singleton _uri $ [textEdit msg]
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

type MissingPatterns = [T.Text] -- TODO: find what this should be

makeEditText :: Monad m => ParsedModule -> MissingPatterns -> Range -> MaybeT m (T.Text, T.Text)
makeEditText pm missingPatterns range = do

  -- BEGIN experiment with astTraversalWith
  let !foo = traceWith (("Hello: " ++) . show . typeOf)
           $ astTraversalWith (pm_parsed_source pm)
                              (\node -> case eqTypeRep (typeRep @(HsExpr GhcPs)) (typeOf node) of
                Nothing -> []
                Just HRefl -> case node :: HsExpr GhcPs of
                  HsCase _ _ _ -> trace "Hello again!" $ [node]
                  _ -> []
                )
  -- END experiment with astTraversalWith
  let ps = pm_parsed_source pm
      old = T.pack $ exactPrint ps
      ps' :: ParsedSource = addMissingPatterns ps missingPatterns range
      new = T.pack $ exactPrint ps'
  pure (old, new)

addMissingPatterns :: ParsedSource -> MissingPatterns -> Range -> ParsedSource
addMissingPatterns ps _ _ = ps -- TODO: implement this
