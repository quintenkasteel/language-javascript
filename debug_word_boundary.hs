#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Language.JavaScript.Parser.Parser as Parser

-- Test word boundary fixes specifically
main :: IO ()
main = do
  putStrLn "=== TESTING WORD BOUNDARY FIXES ==="
  putStrLn ""

  -- Test boolean literals
  putStrLn "Testing boolean literals:"
  testCase "true"       -- Should work
  testCase "false"      -- Should work
  testCase "trueValue"  -- Should NOT parse "true" from this (word boundary)
  testCase "falsehood"  -- Should NOT parse "false" from this (word boundary)

  putStrLn ""
  putStrLn "Testing null literal:"
  testCase "null"       -- Should work
  testCase "nullish"    -- Should NOT parse "null" from this (word boundary)

  putStrLn ""
  putStrLn "Testing variable keywords:"
  testCase "var x = 1"   -- Should work
  testCase "variable"    -- Should NOT parse "var" from this (word boundary)

  putStrLn ""
  putStrLn "Testing control flow keywords:"
  testCase "if (true) {}"  -- Should work
  testCase "iffy"          -- Should NOT parse "if" from this (word boundary)

testCase :: String -> IO ()
testCase input = do
  case Parser.parse input "test" of
    Right ast -> putStrLn $ "✓ '" ++ input ++ "' - SUCCESS: " ++ take 50 (show ast)
    Left err -> putStrLn $ "✗ '" ++ input ++ "' - FAILED: " ++ take 50 err