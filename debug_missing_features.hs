#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Language.JavaScript.Parser.Parser as Parser

-- Test advanced JavaScript features that might be missing
main :: IO ()
main = do
  putStrLn "=== TESTING ADVANCED JAVASCRIPT FEATURES ==="
  putStrLn ""

  -- Test 1: Template literals
  putStrLn "1. Template literals:"
  testFeature "`hello world`" "Basic template literal"
  testFeature "`hello ${name}`" "Template literal with interpolation"
  testFeature "`multi\nline`" "Multiline template literal"

  putStrLn ""

  -- Test 2: Destructuring
  putStrLn "2. Destructuring assignments:"
  testFeature "const [a, b] = arr" "Array destructuring"
  testFeature "const {x, y} = obj" "Object destructuring"
  testFeature "const [a, ...rest] = arr" "Rest in destructuring"
  testFeature "const {x: newX} = obj" "Destructuring with rename"

  putStrLn ""

  -- Test 3: Spread operator
  putStrLn "3. Spread operator:"
  testFeature "func(...args)" "Spread in function call"
  testFeature "const arr = [...items]" "Spread in array literal"
  testFeature "const obj = {...other}" "Spread in object literal"

  putStrLn ""

  -- Test 4: Arrow functions
  putStrLn "4. Arrow functions:"
  testFeature "x => x + 1" "Simple arrow function"
  testFeature "(x, y) => x + y" "Arrow function with multiple params"
  testFeature "() => 42" "Arrow function with no params"
  testFeature "x => { return x * 2; }" "Arrow function with block body"

  putStrLn ""

  -- Test 5: Modern features
  putStrLn "5. Modern JavaScript features:"
  testFeature "async function f() {}" "Async function"
  testFeature "await promise" "Await expression"
  testFeature "function* gen() {}" "Generator function"
  testFeature "yield 42" "Yield expression"

  putStrLn ""

  -- Test 6: Object/Array methods
  putStrLn "6. Advanced object/array features:"
  testFeature "obj[computed]" "Computed property access"
  testFeature "{[key]: value}" "Computed property name"
  testFeature "class { #private = 42 }" "Private class fields"

  putStrLn ""

  -- Test 7: Import/Export
  putStrLn "7. ES6 modules:"
  testFeature "import { x } from 'module'" "Named import"
  testFeature "import * as mod from 'module'" "Namespace import"
  testFeature "export const x = 1" "Named export"
  testFeature "export default class C {}" "Default export"

testFeature :: String -> String -> IO ()
testFeature input description = do
  case Parser.parse input "test" of
    Right _ -> putStrLn $ "✓ " ++ description ++ ": '" ++ input ++ "'"
    Left err -> putStrLn $ "✗ " ++ description ++ ": '" ++ input ++ "' - " ++ take 50 err