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
import Debug.Trace (trace)
import qualified Development.IDE.Core.Shake               as Shake
import Language.LSP.Protocol.Types
import GHC.Driver.Env.Types
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Development.IDE.GHC.Compat.Parser (ParsedModule)
import Development.IDE.GHC.Compat.Core (DynFlags)
import GHC (ParsedModule(pm_parsed_source, pm_mod_summary), MatchGroup (..), Match (m_pats, m_grhss), GRHSs (grhssGRHSs), ModSummary)
import Language.Haskell.GHC.ExactPrint.Transform (HasDecls(hsDecls))
import Development.IDE.GHC.Compat (HasSrcSpan(getLoc), Messages, GhcMessage, TcGblEnv)
import Development.IDE.GHC.Compat.Core (SrcSpan)
import Ide.PluginUtils (subRange)
import GHC.Types.SrcLoc (GenLocated(L))
import GHC.Hs.Decls (HsDecl(..))
import Language.Haskell.Syntax.Binds (HsBindLR(..))
import Language.Haskell.GHC.ExactPrint.Utils
import GHC.Driver.Ppr
import Development.IDE.GHC.Compat.ExactPrint
import GHC.IsList (toList)
import Development.IDE.Core.Shake (getDiagnostics)
import Control.Concurrent.STM.Stats (atomically)
import qualified Data.Foldable as Foldable
import GHC.Utils.Error (MsgEnvelope)
import qualified Development.IDE.Types.Diagnostics as LSP
import Development.IDE.Core.Compile
import Development.IDE.Core.Rules (getParsedModuleDefinition)
import Development.IDE.Types.Options (IdeOptions)

-- TODO: remove this duplication
-- | Check if some `HasSrcSpan` value is in the given range
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
  _
  _
  CodeActionParams{ _textDocument = docId@TextDocumentIdentifier{..}
                  , _context = CodeActionContext{_diagnostics = [Diagnostic{..}]}
                  }
  = do
  nfp <- getNormalizedFilePathE _uri
  let ms = _ :: ModSummary
  let opt = _ :: IdeOptions
  let hsc = _ :: HscEnv
  let tcg = _ :: TcGblEnv

  (diags, mb_pm) <- liftIO $ getParsedModuleDefinition hsc opt nfp ms

  fromCompilation :: Messages GhcMessage <-case mb_pm of
      Nothing -> undefined
      Just pm -> do fmap (_ {- this is easy if I change compileModule's signature -})
                  $ liftIO
                  $ compileModule (RunSimplifier True) hsc (pm_mod_summary pm) tcg

  let msgs = Foldable.toList fromCompilation
  let msgEnv :: MsgEnvelope GhcMessage = _ $ head msgs
  let d :: FileDiagnostic = ideErrorFromLspDiag (LSP.Diagnostic {
                                      _range = noRange,
                                      _severity = Nothing,
                                      _code = Nothing,
                                      _source = Nothing,
                                      _message = "",
                                      _relatedInformation = Nothing,
                                      _tags = Nothing,
                                      _codeDescription = Nothing,
                                      _data_ = Nothing
                                    })
                              nfp
                              (Just msgEnv)

  let Diagnostic{ _message = msg } = fdLspDiagnostic d
  pure $ InL [InR (CodeAction "Add placeholders for all missing patterns"
                              (Just CodeActionKind_QuickFix)
                              Nothing
                              Nothing
                              Nothing
                              (Just $ edit msg)
                              Nothing Nothing)]
        where
          pragmaInsertRange = let p = _end _range in Range p p
          textEdits msg = let extract = T.init
                                      . T.unlines
                                      . reverse
                                      . map (("    " `T.append`) . (`T.append` " -> undefined"))
                                      . take 2
                                      . reverse
                                      . T.lines
                          in [TextEdit pragmaInsertRange $ ("\n" `T.append`) (extract  msg)]
          edit msg =
            WorkspaceEdit
              (Just $ M.singleton _uri (textEdits msg))
              Nothing
              Nothing
suggestCaseSplitProvider _ _ _ = pure $ InL $ []
