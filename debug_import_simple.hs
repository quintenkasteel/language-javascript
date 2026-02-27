#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Language.JavaScript.Parser.Parser as Parser

-- Test simple import parsing
main :: IO ()
main = do
  putStrLn "=== DEBUGGING SIMPLE IMPORT PARSING ==="
  putStrLn ""

  -- Test individual parts
  putStrLn "1. Testing 'import' keyword alone:"
  testCase "import"

  putStrLn ""
  putStrLn "2. Testing simple import statement:"
  testCase "import x;"

  putStrLn ""
  putStrLn "3. Testing export keyword alone:"
  testCase "export"

  putStrLn ""
  putStrLn "4. Testing simple export statement:"
  testCase "export x;"

  putStrLn ""
  putStrLn "5. Testing other keywords for comparison:"
  testCase "var"
  testCase "function"
  testCase "class"

testCase :: String -> IO ()
testCase input = do
  case Parser.parse input "test" of
    Right ast -> putStrLn $ "✓ '" ++ input ++ "' - SUCCESS: " ++ take 50 (show ast)
    Left err -> putStrLn $ "✗ '" ++ input ++ "' - FAILED: " ++ take 80 err