#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text
-}

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import qualified Language.JavaScript.Parser as Language.JavaScript.Parser

-- Exact replica of the failing test logic
testExactReplica :: IO ()
testExactReplica = do
  putStrLn "Testing exact replica of failing test logic:"

  -- These are the exact test cases from line 138
  let validProgs = ["var x = 1;", "function f() { return 2; } f();", "if (true) { console.log('ok'); }"]

  forM_ validProgs $ \input -> do
    putStrLn $ "\nTesting: " ++ input
    case Language.JavaScript.Parser.parse input "test" of
      Right _ -> putStrLn "✓ Successfully parsed"
      Left err -> putStrLn $ "✗ Failed to parse program: " ++ err

main :: IO ()
main = testExactReplica