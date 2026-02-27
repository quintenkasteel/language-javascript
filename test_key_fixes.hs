#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Language.JavaScript.Parser.Parser as Parser

-- Test key fixes that should have massive impact
main :: IO ()
main = do
  putStrLn "=== TESTING KEY FIXES THAT SHOULD ELIMINATE MOST FAILURES ==="
  putStrLn ""

  -- Test 1: Word boundary issues that were causing mass failures
  putStrLn "1. Critical word boundary tests (previously failing massively):"

  -- These should all work now with proper word boundaries
  testExpectedSuccess "var x = 1" "Variable declaration"
  testExpectedSuccess "true" "Boolean literal"
  testExpectedSuccess "false" "Boolean literal"
  testExpectedSuccess "null" "Null literal"
  testExpectedSuccess "if (true) {}" "If statement"
  testExpectedSuccess "function f(){}" "Function declaration"
  testExpectedSuccess "while (true) {}" "While loop"
  testExpectedSuccess "for (var i=0; i<10; i++) {}" "For loop"
  testExpectedSuccess "switch (x) { case 1: break; }" "Switch statement"
  testExpectedSuccess "try { } catch (e) { }" "Try-catch"
  testExpectedSuccess "class C extends B {}" "Class declaration"
  testExpectedSuccess "const x = 1" "Const declaration"
  testExpectedSuccess "let y = 2" "Let declaration"

  -- Test 2: Advanced numeric literals
  putStrLn ""
  putStrLn "2. Advanced numeric literals (ES2021 features):"
  testExpectedSuccess "1_000_000" "Decimal with separators"
  testExpectedSuccess "0x1_BEEF" "Hex with separators"
  testExpectedSuccess "0b1010_1010" "Binary with separators"
  testExpectedSuccess "0o777_123" "Octal with separators"
  testExpectedSuccess "123n" "BigInt literal"
  testExpectedSuccess "0xFF_00n" "Hex BigInt"

  -- Test 3: Complex expressions
  putStrLn ""
  putStrLn "3. Complex expressions:"
  testExpectedSuccess "x + y * z" "Binary expressions"
  testExpectedSuccess "obj.prop" "Member access"
  testExpectedSuccess "arr[index]" "Array access"
  testExpectedSuccess "func(arg1, arg2)" "Function calls"
  testExpectedSuccess "(x + y) * z" "Parenthesized expressions"

  -- Test 4: Advanced statements
  putStrLn ""
  putStrLn "4. Advanced statements:"
  testExpectedSuccess "return x + 1" "Return statement"
  testExpectedSuccess "break" "Break statement"
  testExpectedSuccess "continue" "Continue statement"
  testExpectedSuccess "throw new Error('test')" "Throw statement"

  putStrLn ""
  putStrLn "Summary: If these core tests are passing, the word boundary fixes should have"
  putStrLn "         eliminated hundreds of test failures!"

testExpectedSuccess :: String -> String -> IO ()
testExpectedSuccess input description = do
  case Parser.parse input "test" of
    Right _ -> putStrLn $ "✓ " ++ description ++ ": '" ++ input ++ "'"
    Left err -> putStrLn $ "✗ " ++ description ++ ": '" ++ input ++ "' - FAILED: " ++ take 50 err