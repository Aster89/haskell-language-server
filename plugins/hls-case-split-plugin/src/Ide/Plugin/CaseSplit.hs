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
import Development.IDE.GHC.Compat (GhcMessage (GhcDsMessage))
import Development.IDE.GHC.Compat.Core (SrcSpan)
import Ide.PluginUtils (subRange)
import Control.Monad.RWS
import Development.IDE.GHC.Compat.Error (DsMessage(DsNonExhaustivePatterns))

-- TODO: remove this duplication
-- | Check if some `HasSrcSpan` value isin the given range
inRange :: Range -> SrcSpan -> Bool
inRange range s = maybe False (subRange range) (srcSpanToRange s)


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
  -- XXX I think I might need pluginCommands; at least, that's what in the class plugin
  -- allows having access to the parsed module and, from there, to the AST.
  , pluginPriority = 1
  }

data Pragma = LangExt T.Text | OptGHC T.Text
  deriving (Show, Eq, Ord)

suggestCaseSplitProvider :: PluginMethodHandler IdeState 'LSP.Method_TextDocumentCodeAction
suggestCaseSplitProvider
  state
  _
  CodeActionParams{ _textDocument = TextDocumentIdentifier{..}
                  , _range = range
                  }
  = do
  nfp <- getNormalizedFilePathE _uri

  diags :: [FileDiagnostic] <- activeDiagnosticsInRange (shakeExtras state) nfp range
    >>= \case
    Nothing -> do liftIO $ putStrLn "Nothing here..."
                  return []
    Just fileDiags -> do liftIO $ putStrLn "Something here!"
                         return fileDiags

  edit <- case (_magic diags) :: GhcMessage of
     (GhcDsMessage m) -> case m of
       d@DsNonExhaustivePatterns{} -> return (_generateTextEdit d)
       _ -> error "I'll think about it 1"
     _ -> error "I'll think about it 2"

  pure $ InL [InR (CodeAction "Add placeholders for all missing patterns"
                              (Just CodeActionKind_QuickFix)
                              Nothing
                              Nothing
                              Nothing
                              (Just edit)
                              Nothing
                              Nothing)]

suggestCaseSplitProvider _ _ _ = pure $ InL $ []
