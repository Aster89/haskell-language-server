{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Control.Lens                  (Prism', prism', (^.),
                                                (^..), (^?))
import           Data.Foldable                 (find)
import qualified Data.Text                     as T
import qualified Ide.Plugin.CaseSplit          as CS
import qualified Language.LSP.Protocol.Lens    as L
import           System.FilePath
import           Test.Hls
import qualified Test.Hls.FileSystem           as FS

main :: IO ()
main = defaultTestRunner tests

caseSplitPlugin :: PluginTestDescriptor CS.Log
caseSplitPlugin = mkPluginTestDescriptor CS.descriptor "case split"

tests :: TestTree
tests = testGroup
  "case split"
  [ codeActionTests
  ]

codeActionTests :: TestTree
codeActionTests = testGroup
  "code actions"
  [ expectCodeActionsAvailable "Produces addMinimalMethodPlaceholders code actions for one instance" "T1"
      [ "Add placeholders for all missing patterns"
      ]
  , goldenWithClass "No patterns and no braces" "T1" "eq" $
      getActionByTitle "Add placeholders for all missing patterns"
  , goldenWithClass "Some patterns and no braces" "T2" "eq" $
      getActionByTitle "Add placeholders for all missing patterns"
  , goldenWithClass "Some patterns and braces" "T3" "eq" $
      getActionByTitle "Add placeholders for all missing patterns"
  ]

_CACodeAction :: Prism' (Command |? CodeAction) CodeAction
_CACodeAction = prism' InR $ \case
  InR action -> Just action
  _          -> Nothing

goldenWithClass :: TestName -> FilePath -> FilePath -> ([CodeAction] -> Session CodeAction) -> TestTree
goldenWithClass title path desc findAction =
  goldenWithHaskellDocInTmpDir def caseSplitPlugin title (mkFs $ FS.directProject (path <.> "hs")) path (desc <.> "expected") "hs" $ \doc -> do
    _ <- waitForDiagnosticsFrom doc
    actions <- concatMap (^.. _CACodeAction) <$> getAllCodeActions doc
    action <- findAction actions
    executeCodeAction action

getActionByTitle :: T.Text -> [CodeAction] -> Session CodeAction
getActionByTitle title actions = case find (\a -> a ^. L.title == title) actions of
    Just a -> pure a
    Nothing -> liftIO $ assertFailure $ "Action " <> show title <> " not found in " <> show [a ^. L.title | a <- actions]

expectCodeActionsAvailable :: TestName -> FilePath -> [T.Text] -> TestTree
expectCodeActionsAvailable title path actionTitles =
  testCase title $ do
    -- TODO: use runSessionWithServerInTmpDir instead
    runSessionWithServer def caseSplitPlugin testDataDir $ do
      doc <- openDoc (path <.> "hs") "haskell"
      _ <- waitForDiagnosticsFrom doc
      caResults <- getAllCodeActions doc
      liftIO $ map (^? _CACodeAction . L.title) caResults
        @?= expectedActions
    where
      expectedActions = Just <$> actionTitles

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-case-split-plugin" </> "test" </> "testdata"

mkFs :: [FS.FileTree] -> FS.VirtualFileTree
mkFs = FS.mkVirtualFileTree testDataDir
