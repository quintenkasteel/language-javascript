#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Language.JavaScript.Parser.Parser as Parser

-- Debug optional chaining specifically
main :: IO ()
main = do
  putStrLn "=== DEBUGGING OPTIONAL CHAINING ==="
  putStrLn ""

  -- Test simple cases first
  putStrLn "1. Simple member access (should work):"
  testCase "obj.prop"

  putStrLn ""
  putStrLn "2. Simple optional chaining:"
  testCase "obj?.prop"

  putStrLn ""
  putStrLn "3. Progressive complexity:"
  testCase "obj"
  testCase "obj?"
  testCase "obj?."
  testCase "obj?.p"
  testCase "obj?.prop"

  putStrLn ""
  putStrLn "4. Other optional chaining forms:"
  testCase "obj?.[key]"
  testCase "obj?.()"
  testCase "obj?.(args)"

testCase :: String -> IO ()
testCase input = do
  case Parser.parse input "test" of
    Right ast -> putStrLn $ "✓ '" ++ input ++ "' - SUCCESS: " ++ take 80 (show ast)
    Left err -> putStrLn $ "✗ '" ++ input ++ "' - FAILED: " ++ take 80 err