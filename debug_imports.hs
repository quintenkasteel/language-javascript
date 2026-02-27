#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text
-}

{-# LANGUAGE OverloadedStrings #-}

import Language.JavaScript.Parser  -- Unqualified import exactly like tests

-- Try to use the same functions the tests are using
main :: IO ()
main = do
  putStrLn "=== TESTING FUNCTIONS AVAILABLE FROM UNQUALIFIED IMPORT ==="
  putStrLn ""

  let input = "true"

  -- Try parseExpression (like the tests use)
  putStrLn "Testing parseExpression:"
  case parseExpression input "test" of
    Right ast -> putStrLn $ "✓ SUCCESS: " ++ take 50 (show ast)
    Left err -> putStrLn $ "✗ FAILED: " ++ err

  -- Try parseStatement (like the tests use)
  putStrLn "Testing parseStatement:"
  case parseStatement input "test" of
    Right ast -> putStrLn $ "✓ SUCCESS: " ++ take 50 (show ast)
    Left err -> putStrLn $ "✗ FAILED: " ++ err