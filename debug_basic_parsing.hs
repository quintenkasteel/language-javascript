#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Language.JavaScript.Parser as Language.JavaScript.Parser

-- Debug the most basic parsing failure - using EXACT same imports as failing test
main :: IO ()
main = do
  putStrLn "=== DEBUGGING BASIC PARSING FAILURE - EXACT TEST REPLICA ==="
  putStrLn ""

  -- Test the most basic variable declaration that's failing
  putStrLn "1. Testing basic var declaration (exact test call):"
  testCaseExact "var x = 42;"

  putStrLn ""
  putStrLn "2. Testing different quotes and formatting:"
  testCaseExact "var x=42;"
  testCaseExact " var x = 42; "
  testCaseExact "var x = 42;\n"

testCaseExact :: String -> IO ()
testCaseExact original = do
  case Language.JavaScript.Parser.parse original "test" of
    Right ast -> putStrLn $ "✓ '" ++ original ++ "' - SUCCESS: " ++ take 100 (show ast)
    Left err -> putStrLn $ "✗ '" ++ original ++ "' - FAILED: " ++ show err