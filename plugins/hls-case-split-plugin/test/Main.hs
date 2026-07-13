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
  [
  --  expectCodeActionsAvailable "Produces addMinimalMethodPlaceholders code actions for one instance" "T1"
  --    [ "Add placeholders for all missing patterns"
  --    ]
  --, goldenWithClass "No patterns, no braces" "T1" $
  --    getActionByTitle "Add placeholders for all missing patterns"
  --, goldenWithClass "Some patterns, no braces" "T2" $
  --    getActionByTitle "Add placeholders for all missing patterns"
  --, goldenWithClass "Some patterns, with braces" "T3" $
  --    getActionByTitle "Add placeholders for all missing patterns"
  --, goldenWithClass "No patterns, with braces" "T4" $
  --    getActionByTitle "Add placeholders for all missing patterns"
  --, goldenWithClass "Some patterns on one line, no braces" "T5" $
  --    getActionByTitle "Add placeholders for all missing patterns"
  --, goldenWithClass "Some patterns on one line, with braces" "T6" $
  --    getActionByTitle "Add placeholders for all missing patterns"
  --, goldenWithClass "Records' field names are ignored" "T7" $
  --    getActionByTitle "Add placeholders for all missing patterns"
  --,
  goldenWithClass "Inside `where`" "T8" $
      getActionByTitle "Add placeholders for all missing patterns"
  ]

  -- TODO:
  --   1. inside where
  --   2. inside let
  --   3. inside in
  --   4. inside let within a do
  --   5. inside a do
  --   6. inside nested wheres
  --   6. nested cases

_CACodeAction :: Prism' (Command |? CodeAction) CodeAction
_CACodeAction = prism' InR $ \case
  InR action -> Just action
  _          -> Nothing

goldenWithClass :: TestName -> FilePath -> ([CodeAction] -> Session CodeAction) -> TestTree
goldenWithClass title path findAction =
  goldenWithHaskellDocInTmpDir def caseSplitPlugin title (mkFs $ FS.directProject (path <.> "hs")) path "expected" "hs" $ \doc -> do
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
