#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text, bytestring, flatparse
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified FlatParse.Basic as FP

-- Test with actual parser functions
import qualified Language.JavaScript.Parser.Parser as Parser

-- Test actual parsing vs our working implementation
main :: IO ()
main = do
  let input = "var x = 42;"

  putStrLn $ "Testing actual parser implementation:"
  putStrLn $ "Input: " ++ input
  putStrLn ""

  -- Test with main parser interface
  putStrLn "1. Main Parser.parse result:"
  case Parser.parse input "test" of
    Right ast -> putStrLn $ "✓ SUCCESS: " ++ take 100 (show ast)
    Left err -> putStrLn $ "✗ FAILED: " ++ err

  putStrLn ""

  -- Test other simple cases
  let testCases = ["x", "42", "x=42", "var x", "var x=42"]
  putStrLn "2. Testing simple cases with main parser:"
  mapM_ testCase testCases

  putStrLn ""

  -- Test parsing other constructs
  let jsTestCases = ["function f(){}", "if(true){}", "for(var i=0;i<10;i++){}", "class C{}"]
  putStrLn "3. Testing other JavaScript constructs:"
  mapM_ testCase jsTestCases

testCase :: String -> IO ()
testCase input = do
  case Parser.parse input "test" of
    Right _ -> putStrLn $ "✓ '" ++ input ++ "' - SUCCESS"
    Left err -> putStrLn $ "✗ '" ++ input ++ "' - FAILED: " ++ take 50 err