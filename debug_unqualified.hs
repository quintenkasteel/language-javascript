#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text
-}

{-# LANGUAGE OverloadedStrings #-}

import Language.JavaScript.Parser  -- Unqualified import
import qualified Language.JavaScript.Parser as Language.JavaScript.Parser  -- Qualified import

-- Debug the difference between qualified and unqualified imports
main :: IO ()
main = do
  putStrLn "=== COMPARING QUALIFIED VS UNQUALIFIED PARSER FUNCTIONS ==="
  putStrLn ""

  -- Test cases that are failing in the tests
  let testCases =
        [ ("true", "expression")
        , ("function f() { return 1; }", "statement")
        , ("if (true) { return; }", "statement")
        , ("var x = 1;", "program")
        ]

  mapM_ testAllFunctions testCases

testAllFunctions :: (String, String) -> IO ()
testAllFunctions (input, expectedType) = do
  putStrLn $ "\n=== Testing: " ++ show input ++ " (expected: " ++ expectedType ++ ") ==="

  -- Test qualified parse
  putStrLn "1. Qualified Language.JavaScript.Parser.parse:"
  case Language.JavaScript.Parser.parse input "test" of
    Right ast -> putStrLn $ "  ✓ SUCCESS: " ++ take 50 (show ast)
    Left err -> putStrLn $ "  ✗ FAILED: " ++ err

  -- Test unqualified parseExpression (if available)
  putStrLn "2. Unqualified parseExpression:"
  case parseExpression input "test" of
    Right ast -> putStrLn $ "  ✓ SUCCESS: " ++ take 50 (show ast)
    Left err -> putStrLn $ "  ✗ FAILED: " ++ err

  -- Test unqualified parseStatement (if available)
  putStrLn "3. Unqualified parseStatement:"
  case parseStatement input "test" of
    Right ast -> putStrLn $ "  ✓ SUCCESS: " ++ take 50 (show ast)
    Left err -> putStrLn $ "  ✗ FAILED: " ++ err

  -- Test qualified parseExpression
  putStrLn "4. Qualified Language.JavaScript.Parser.parseExpression:"
  case Language.JavaScript.Parser.parseExpression input "test" of
    Right ast -> putStrLn $ "  ✓ SUCCESS: " ++ take 50 (show ast)
    Left err -> putStrLn $ "  ✗ FAILED: " ++ err

  -- Test qualified parseStatement
  putStrLn "5. Qualified Language.JavaScript.Parser.parseStatement:"
  case Language.JavaScript.Parser.parseStatement input "test" of
    Right ast -> putStrLn $ "  ✓ SUCCESS: " ++ take 50 (show ast)
    Left err -> putStrLn $ "  ✗ FAILED: " ++ err