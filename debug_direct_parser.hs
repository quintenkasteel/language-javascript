#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text, bytestring, flatparse-basic
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified FlatParse.Basic as FP
import qualified Language.JavaScript.Parser.Flatparse.Statement as Statement
import qualified Language.JavaScript.Parser.Flatparse.Lexer as Lexer

-- Test individual parsers directly
main :: IO ()
main = do
  putStrLn "=== TESTING INDIVIDUAL PARSERS DIRECTLY ==="
  putStrLn ""

  let input = "var x = 42;"
  let inputText = Text.pack input
  let inputBS = Text.encodeUtf8 inputText

  putStrLn $ "Testing input: " ++ input
  putStrLn ""

  -- Test individual component parsers
  putStrLn "1. Testing whitespace parser:"
  case FP.runParser Lexer.whitespace inputBS of
    FP.OK _ remaining -> do
      putStrLn $ "✓ Whitespace parser succeeded"
      putStrLn $ "  Remaining: " ++ show remaining
    FP.Fail -> putStrLn $ "✗ Whitespace parser failed"
    FP.Err err -> putStrLn $ "✗ Whitespace parser error: " ++ show err

  putStrLn ""
  putStrLn "2. Testing variableDeclaration parser:"
  case FP.runParser Statement.variableDeclaration inputBS of
    FP.OK result remaining -> do
      putStrLn $ "✓ Variable declaration parser succeeded"
      putStrLn $ "  Result: " ++ take 100 (show result)
      putStrLn $ "  Remaining: " ++ show remaining
    FP.Fail -> putStrLn $ "✗ Variable declaration parser failed"
    FP.Err err -> putStrLn $ "✗ Variable declaration parser error: " ++ show err

  putStrLn ""
  putStrLn "3. Testing statement parser:"
  case FP.runParser Statement.statement inputBS of
    FP.OK result remaining -> do
      putStrLn $ "✓ Statement parser succeeded"
      putStrLn $ "  Result: " ++ take 100 (show result)
      putStrLn $ "  Remaining: " ++ show remaining
    FP.Fail -> putStrLn $ "✗ Statement parser failed"
    FP.Err err -> putStrLn $ "✗ Statement parser error: " ++ show err

  putStrLn ""
  putStrLn "4. Testing statementList parser:"
  case FP.runParser Statement.statementList inputBS of
    FP.OK result remaining -> do
      putStrLn $ "✓ Statement list parser succeeded"
      putStrLn $ "  Result: " ++ take 100 (show result)
      putStrLn $ "  Remaining: " ++ show remaining
    FP.Fail -> putStrLn $ "✗ Statement list parser failed"
    FP.Err err -> putStrLn $ "✗ Statement list parser error: " ++ show err