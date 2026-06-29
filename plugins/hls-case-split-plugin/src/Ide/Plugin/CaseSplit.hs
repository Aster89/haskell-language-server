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
import Development.IDE.GHC.Compat (GhcMessage (GhcDsMessage), HsMatchContext (CaseAlt), Outputable (ppr), showSDocUnsafe, HscEnv (hsc_dflags))
import Development.IDE.GHC.Compat.Error (DsMessage(DsNonExhaustivePatterns), msgEnvelopeErrorL)
import Data.Maybe (mapMaybe)
import Control.Lens (Fold, prism', (^?), (<&>), (^.))
import GHC.HsToCore.Pmc.Solver.Types
import GHC.Types.Unique.SDFM
import GHC (DynFlags(maxUncoveredPatterns), ParsedModule (pm_parsed_source), ParsedSource, HasLoc (getHasLoc))
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Development.IDE.GHC.Compat.ExactPrint
import Data.Data (Data(), gmapQ)
import Data.Generics.Schemes (everywhere)
import Debug.Trace
import Type.Reflection (eqTypeRep,
                        type (:~~:) (HRefl),
                        typeRep, typeOf)
import GHC.Hs (GhcPs)
import GHC.Types.SrcLoc (SrcSpan)
import Control.Monad (MonadPlus(mplus, mzero))
import Language.Haskell.Syntax.Expr (HsExpr (HsCase))
import Control.Monad.Trans (lift)
import Ide.PluginUtils (diffText, WithDeletions (IncludeDeletions))
import Development.IDE.Core.FileStore (getVersionedTextDoc)
import qualified Language.LSP.Protocol.Lens                   as L

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
  , pluginModifyDynflags = mempty { dynFlagsModifyGlobal = \dynFlags -> dynFlags { maxUncoveredPatterns = 20 } }
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

  (hsc_dflags . hscEnv -> df)
    <- runActionE "classplugin.addMethodPlaceholders.GhcSessionDeps" state
      $ useE GhcSessionDeps nfp

  diags :: [FileDiagnostic] <- activeDiagnosticsInRange (shakeExtras state) nfp range
                                <&> \case Nothing -> error "Oops, no diagnostics!"
                                          Just fileDiags -> fileDiags

  case mapMaybe (\d -> fdStructuredMessage d ^? _SomeStructuredMessage
                                               . msgEnvelopeErrorL
                                               . _DsMessage) diags of
    [dsmsg] -> do
      let pmAltsConApp = dsMsgToPmAlts dsmsg

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
                     , _edit        = Just $ pmAltsToWorkspaceEdit $ pmAltsConApp
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

type MissingPatterns = [PmAltConApp]

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
      f = \node -> case eqTypeRep (typeRep @(HsExpr GhcPs)) (typeOf node) of
                Nothing -> node
                Just HRefl -> case node :: HsExpr GhcPs of
                  (HsCase x e existingPs) | getHasLoc e `inRange` range
                                 -> trace ("I should change this `case`! It's got these patterns already = "
                                           ++ show (exactPrint existingPs)
                                           ++ " but I should add the missing ones coming from the diagnostics")
                                  $ HsCase x e $ -- _ missingPs
                                                      existingPs

                  HsCase _ _ _ -> trace "Leave this `case`"
                                $ node

                  _ -> node

addMissingPatterns :: ParsedSource -> MissingPatterns -> Range -> ParsedSource
addMissingPatterns ps _ _ = ps -- TODO: implement this

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
