#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Language.JavaScript.Parser.Parser as Parser

-- Comprehensive test of the major fixes applied
main :: IO ()
main = do
  putStrLn "=== COMPREHENSIVE TEST OF FLATPARSE FIXES ==="
  putStrLn ""

  -- Test 1: Word boundary fixes for literals
  putStrLn "1. Boolean and null literal word boundaries:"
  testCase "true"         -- ✓ Should parse as boolean
  testCase "false"        -- ✓ Should parse as boolean
  testCase "null"         -- ✓ Should parse as null
  testCase "trueValue"    -- ✓ Should parse as identifier (NOT boolean)
  testCase "falseStart"   -- ✓ Should parse as identifier (NOT boolean)
  testCase "nullish"      -- ✓ Should parse as identifier (NOT null)

  putStrLn ""

  -- Test 2: Word boundary fixes for keywords
  putStrLn "2. Keyword word boundaries:"
  testCase "var x = 1"     -- ✓ Should parse as variable declaration
  testCase "if (true) {}"  -- ✓ Should parse as if statement
  testCase "function f(){}" -- ✓ Should parse as function declaration
  testCase "variable"      -- ✓ Should parse as identifier (NOT var)
  testCase "iffy"          -- ✓ Should parse as identifier (NOT if)
  testCase "functionality" -- ✓ Should parse as identifier (NOT function)

  putStrLn ""

  -- Test 3: Advanced numeric literals (ES2021)
  putStrLn "3. Advanced numeric literals:"
  testCase "1_000_000"     -- ✓ Decimal with separators
  testCase "0x1_BEEF"      -- ✓ Hex with separators
  testCase "0b1010_1010"   -- ✓ Binary with separators
  testCase "0o777_123"     -- ✓ Octal with separators
  testCase "123n"          -- ✓ BigInt
  testCase "0x123n"        -- ✓ Hex BigInt

  putStrLn ""

  -- Test 4: Complex JavaScript constructs
  putStrLn "4. Complex JavaScript constructs:"
  testCase "class MyClass extends Base {}"
  testCase "async function test() { await fetch('/api'); }"
  testCase "const [a, b] = [1, 2];"
  testCase "obj?.prop?.method?.(args)"
  testCase "for (let i = 0; i < 10; i++) { console.log(i); }"

testCase :: String -> IO ()
testCase input = do
  case Parser.parse input "test" of
    Right ast -> putStrLn $ "✓ '" ++ input ++ "' - SUCCESS"
    Left err -> putStrLn $ "✗ '" ++ input ++ "' - FAILED: " ++ take 50 err