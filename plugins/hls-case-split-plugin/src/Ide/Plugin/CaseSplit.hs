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
import GHC (ParsedModule(pm_parsed_source), MatchGroup (..), Match (m_pats, m_grhss), GRHSs (grhssGRHSs))
import Language.Haskell.GHC.ExactPrint.Transform (HasDecls(hsDecls))
import Development.IDE.GHC.Compat (HasSrcSpan(getLoc))
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
  state
  _
  CodeActionParams{ _textDocument = docId@TextDocumentIdentifier{..}
                  , _context = CodeActionContext{_diagnostics = [Diagnostic{..}]}
                  }
  = do
  nfp <- getNormalizedFilePathE _uri
  pm <- runActionE "classplugin.addMethodPlaceholders.GetParsedModule" state
      $ useE GetParsedModule nfp
  let ps =
#if !MIN_VERSION_ghc(9,10,0) || MIN_VERSION_ghc(9,11,0)
            makeDeltaAst $
#endif
                pm_parsed_source pm
#if MIN_VERSION_ghc_exactprint(1,10,0)
  let allDecls = hsDecls ps
#else
  allDecls <- hsDecls ps
#endif
  _ <-  liftIO $ atomically $ getDiagnostics state -- XXX: this too contains
                                                   -- NoStructuredMessage
  verTxtDocId <-
  -- TODO: I think I need some sort of folding, so I can search through
  -- a list for something like this
  --
  --            - `CaseAlt`: this exists only if there's a non-zero list of
  --            alternatives, which means we can just pick the indentation of
  --            one of those and call it a day
  --
  --            - `HsCase`: this exists irrespective of how many alternatives
  --            are already written
    trace (unlines ["_range = " ++ show _range,
                    case break (inRange _range . getLoc) allDecls of
      (before, L l foo : after) -> unlines ["(length before, length after) = "
                                                ++ show (length before, length after),
                                            case foo of
          TyClD _ _ -> "TyClD"
          InstD _ _ -> "InstD"
          DerivD _ _ -> "DerivD"
          ValD _ bind -> unlines ["ValD = " ++ case bind of
            PatBind{} -> "PatBind"
            PatSynBind{} -> "PatSynBind"
            VarBind{} -> "VarBind"
            FunBind{..} -> unlines ["FunBind = " ++
                                    case mg_alts fun_matches of
                              L _ [L _ m] -> case toList (grhssGRHSs (m_grhss m)) of
                                  l -> showAst l
                              _ -> "l is longer than 1"
                                   , ("#matches = " ++)
                                        $ show $ length
                                        $ foldr (:) [] (mg_alts fun_matches)]
            -- TODO: pull out the existing declarations
            ]
          SigD _ _ -> "SigD"
          KindSigD _ _ -> "KindSigD"
          DefD _ _ -> "DefD"
          ForD _ _ -> "ForD"
          WarningD _ _ -> "WarningD"
          AnnD _ _ -> "AnnD"
          RuleD _ _ -> "RuleD"
          SpliceD _ _ -> "SpliceD"
          DocD _ _ -> "DocD"
          RoleAnnotD _ _ -> "RoleAnnotD"
          ]
      _ -> "No elem in range"
        ]) liftIO $ runAction "classplugin.codeAction.getVersionedTextDoc"{- TODO change this -} state $ getVersionedTextDoc docId
  normalizedFilePath <- getNormalizedFilePathE (verTxtDocId ^. L.uri)
  -- TODO: retrieve the structured error message
  activeDiagnosticsInRange (shakeExtras state) normalizedFilePath _range >>= \case
    Just [d] -> let Diagnostic{ _message = msg } = fdLspDiagnostic d
                    strMsg = fdStructuredMessage d -- XXX Unfortunately this is a NoStructuredMessage :(
                in -- trace (">> " ++ show strMsg) $
                  pure $ InL [InR (CodeAction "Add placeholders for all missing patterns" (Just CodeActionKind_QuickFix) Nothing Nothing Nothing (Just $ edit msg) Nothing Nothing)]
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
