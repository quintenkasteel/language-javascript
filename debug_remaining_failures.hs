#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Language.JavaScript.Parser as Parser
import qualified Data.Text as Text

-- Test patterns to identify remaining parsing gaps
testCases :: [String]
testCases =
  [ -- Basic syntax that should work
    "var x = 42;"
  , "function test() { return 1; }"
  , "if (x > 0) { console.log('positive'); }"

  -- ES6+ features
  , "let x = 42;"
  , "const y = 'hello';"
  , "class Test { constructor() {} }"
  , "class Test { #private = 42; }"
  , "import { x } from 'module';"
  , "export default function() {}"

  -- Object/Array features
  , "const obj = { x: 1, ...other };"
  , "const arr = [1, 2, ...rest];"
  , "const obj = { [key]: value };"

  -- Advanced features that might be missing
  , "async function test() { await promise; }"
  , "function* generator() { yield 1; }"
  , "const arrow = () => 42;"
  , "const { x, y } = obj;"
  , "const [a, b] = arr;"
  , "for (let item of items) {}"
  , "for (let key in obj) {}"
  , "try { } catch (e) { } finally { }"

  -- Template literals
  , "const str = `Hello ${name}`;"

  -- Regular expressions
  , "const regex = /pattern/gi;"

  -- Numbers with separators (ES2021)
  , "const num = 1_000_000;"
  , "const big = 123n;"

  -- Optional chaining
  , "obj?.prop?.method?.()"

  -- Nullish coalescing
  , "const val = x ?? 'default';"

  -- Dynamic imports
  , "import('module').then(mod => {})"
  ]

main :: IO ()
main = do
  putStrLn "=== ANALYZING REMAINING PARSING FAILURES ==="
  putStrLn ""

  let totalTests = length testCases
  results <- mapM testCase testCases
  let (passed, failed) = partition results

  putStrLn $ "Results: " ++ show (length passed) ++ "/" ++ show totalTests ++ " passed"
  putStrLn ""

  putStrLn "FAILING PATTERNS:"
  mapM_ (\(input, err) -> do
    putStrLn $ "❌ " ++ input
    putStrLn $ "   Error: " ++ show err
    putStrLn ""
    ) failed

  putStrLn "PASSING PATTERNS:"
  mapM_ (\input -> putStrLn $ "✅ " ++ input) passed

testCase :: String -> IO (Either (String, String) String)
testCase input = do
  case Parser.parse input "test" of
    Right _ -> pure (Right input)
    Left err -> pure (Left (input, show err))

partition :: [Either (String, String) String] -> ([String], [(String, String)])
partition [] = ([], [])
partition (Right x : xs) = let (ps, fs) = partition xs in (x:ps, fs)
partition (Left x : xs) = let (ps, fs) = partition xs in (ps, x:fs)