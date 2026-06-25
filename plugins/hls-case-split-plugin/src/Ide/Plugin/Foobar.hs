{-# LANGUAGE CPP                   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Ide.Plugin.Foobar (descriptor) where

import Development.IDE
import Development.IDE.GHC.Compat (HscEnv (hsc_dflags)) -- TODO: remove hsc_dflags from here, and check the last 2 TODOs below
import Ide.Types
import Language.LSP.Protocol.Message as LSP

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor = _

somefunc1 :: PluginMethodHandler IdeState 'Method_TextDocumentCodeAction
somefunc1 _ _ _ = do
  _ <- hsc_dflags . hscEnv <$> _ -- TODO: ask HSL to put it back in the imports: no error
  pure _

somefunc2 :: PluginMethodHandler IdeState 'LSP.Method_TextDocumentCodeAction
somefunc2 _ _ _ = do
  _ <- hsc_dflags . hscEnv <$> _ -- TODO: ask HSL to put it back in the imports: ERROR!
  pure _
