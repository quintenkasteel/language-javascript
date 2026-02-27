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

-- Debug core parsing issue
main :: IO ()
main = do
  putStrLn "=== DEBUGGING CORE PARSING ISSUE ==="
  putStrLn ""

  -- Test the simplest possible case
  let basicTests =
        [ "x"
        , "42"
        , "var x"
        , "var x = 42"
        , "var x = 42;"
        , "function f() {}"
        , "if (true) {}"
        ]

  putStrLn "Testing basic parsing with current implementation:"
  mapM_ testBasicParse basicTests

testBasicParse :: String -> IO ()
testBasicParse input = do
  putStrLn $ "\n  Input: " ++ show input
  case Parser.parse input "test" of
    Right ast -> putStrLn $ "  ✓ SUCCESS: " ++ take 100 (show ast)
    Left err -> putStrLn $ "  ✗ FAILED: " ++ err