{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE RecordWildCards #-}

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
import Development.IDE.GHC.Compat (GhcMessage (GhcDsMessage), HsMatchContext (CaseAlt), Outputable (ppr), showSDocUnsafe)
import Development.IDE.GHC.Compat.Error (DsMessage(DsNonExhaustivePatterns), msgEnvelopeErrorL)
import Data.Maybe (mapMaybe)
import Control.Lens (Fold, prism', (^?), (<&>), (&))
import GHC.HsToCore.Pmc.Solver.Types
import GHC.Types.Unique.SDFM
import GHC (DynFlags(maxUncoveredPatterns))

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

suggestCaseSplitProvider :: PluginMethodHandler IdeState 'LSP.Method_TextDocumentCodeAction
suggestCaseSplitProvider
  state
  _
  CodeActionParams{ _textDocument = TextDocumentIdentifier{..}
                  , _context = CodeActionContext{ _diagnostics = Diagnostic { _range = range }:_ }
                  }
  = do
  nfp <- getNormalizedFilePathE _uri

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
    pmAltsToWorkspaceEdit = stringsToWorkspaceEdit . fmap pmAltToString
    stringsToWorkspaceEdit = edit . T.pack . unlines
    pmAltToString pmalt = unwords $ showPpr (paca_con pmalt):(const "_" <$> paca_ids pmalt)
    dsMsgToPmAlts =
      \case DsNonExhaustivePatterns !CaseAlt _ _ ![ids] !nablas ->
                nablas <&>
                  (\nabla -> let facts = ts_facts $ nabla_tm_st nabla
                             in case vi_pos <$> lookupUSDFM facts ids of
                                   Just [x] -> x
                                   _ -> error "Oops")
            _ -> error "Oops, DsMessage of unexpected shape!"


    pragmaInsertRange = let p = _end range in Range p p
    textEdit msg = let extract = T.init
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
