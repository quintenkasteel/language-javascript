#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Language.JavaScript.Parser.Parser as Parser

-- Test complete statements that should work
main :: IO ()
main = do
  putStrLn "=== TESTING COMPLETE STATEMENTS ==="
  putStrLn ""

  -- These should all work because they're complete statements
  putStrLn "1. Complete variable declarations:"
  testCase "var x = 1;"
  testCase "let y = 2;"
  testCase "const z = 3;"

  putStrLn ""
  putStrLn "2. Complete function declarations:"
  testCase "function test() {}"
  testCase "function add(a, b) { return a + b; }"

  putStrLn ""
  putStrLn "3. Complete control flow:"
  testCase "if (true) {}"
  testCase "while (x < 10) { x++; }"
  testCase "for (var i = 0; i < 5; i++) {}"

  putStrLn ""
  putStrLn "4. Complete class declarations:"
  testCase "class Test {}"
  testCase "class Test extends Base {}"

  putStrLn ""
  putStrLn "5. Import/export with complete syntax:"
  testCase "import x from 'module';"
  testCase "export const y = 1;"

testCase :: String -> IO ()
testCase input = do
  case Parser.parse input "test" of
    Right _ -> putStrLn $ "✓ '" ++ input ++ "'"
    Left err -> putStrLn $ "✗ '" ++ input ++ "' - " ++ take 50 err