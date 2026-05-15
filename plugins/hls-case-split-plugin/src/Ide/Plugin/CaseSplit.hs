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

import           Control.Lens                             hiding (List)
import           Control.Monad.IO.Class                   (MonadIO (liftIO))
import qualified Data.Map                                 as M
import qualified Data.Text                                as T
import           Development.IDE                          hiding (line)
import           Development.IDE.Core.FileStore           (getVersionedTextDoc)
import           Development.IDE.Core.PluginUtils
import           Ide.Plugin.Error
import           Ide.Types
import qualified Language.LSP.Protocol.Lens               as L
import qualified Language.LSP.Protocol.Message            as LSP
import qualified Development.IDE.Core.Shake               as Shake
import Language.LSP.Protocol.Types

-- ---------------------------------------------------------------------
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
  }

data Pragma = LangExt T.Text | OptGHC T.Text
  deriving (Show, Eq, Ord)

suggestCaseSplitProvider :: PluginMethodHandler IdeState 'LSP.Method_TextDocumentCodeAction
suggestCaseSplitProvider
  state
  _
  CodeActionParams{ _textDocument = docId@TextDocumentIdentifier{..},
                    _context = CodeActionContext{_diagnostics = [Diagnostic{..}]}
                  }
  = do
  verTxtDocId <- liftIO $ runAction "classplugin.codeAction.getVersionedTextDoc" state $ getVersionedTextDoc docId
  normalizedFilePath <- getNormalizedFilePathE (verTxtDocId ^. L.uri)
  activeDiagnosticsInRange (shakeExtras state) normalizedFilePath _range >>= \case
    Just [d] -> let Diagnostic{ _message = msg } = fdLspDiagnostic d
                in pure $ InL [InR (CodeAction "Add placeholders for all missing patterns" (Just CodeActionKind_QuickFix) Nothing Nothing Nothing (Just $ edit msg) Nothing Nothing)]
        where
          pragmaInsertRange = let p = _end _range in Range p p
          textEdits msg = let extract = T.init
                                      . T.unlines
                                      . reverse
                                      . map (("    " `T.append`) . (`T.append` " -> _"))
                                      . take 2
                                      . reverse
                                      . T.lines
                          in [TextEdit pragmaInsertRange $ ("\n" `T.append`) (extract  msg)]
          edit msg =
            WorkspaceEdit
              (Just $ M.singleton _uri (textEdits msg))
              Nothing
              Nothing
    _ -> undefined
suggestCaseSplitProvider _ _ _ = pure $ InL $ []
