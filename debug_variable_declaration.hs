#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text, bytestring, flatparse
-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified FlatParse.Basic as FP

-- Import the individual parser functions
import qualified Language.JavaScript.Parser.Flatparse.Statement as Statement
import qualified Language.JavaScript.Parser.Flatparse.Lexer as Lexer
import Language.JavaScript.Parser.Flatparse.Primitives (JSParser, runJSParser)

-- Debug variable declaration parsing specifically
main :: IO ()
main = do
  putStrLn "=== DEBUGGING VARIABLE DECLARATION PARSING ==="
  putStrLn ""

  let input = "var x = 1;"
  let byteInput = Text.encodeUtf8 (Text.pack input)

  putStrLn $ "Input: " ++ show input
  putStrLn $ "ByteString: " ++ show byteInput
  putStrLn ""

  -- Test variable declaration directly
  putStrLn "Testing variableDeclaration parser directly:"
  case runJSParser Statement.variableDeclaration byteInput of
    Right (result, remaining, consumed) -> do
      putStrLn $ "✓ SUCCESS: " ++ take 100 (show result)
      putStrLn $ "  Consumed: " ++ show consumed ++ " bytes"
      putStrLn $ "  Remaining: " ++ show remaining
    Left err -> do
      putStrLn $ "✗ FAILED: " ++ show err

  putStrLn ""

  -- Test statement parser (which includes variableDeclaration)
  putStrLn "Testing statement parser:"
  case runJSParser Statement.statement byteInput of
    Right (result, remaining, consumed) -> do
      putStrLn $ "✓ SUCCESS: " ++ take 100 (show result)
      putStrLn $ "  Consumed: " ++ show consumed ++ " bytes"
      putStrLn $ "  Remaining: " ++ show remaining
    Left err -> do
      putStrLn $ "✗ FAILED: " ++ show err

  putStrLn ""

  -- Test with whitespace handling
  putStrLn "Testing with whitespace handling:"
  case runJSParser (Lexer.whitespace *> Statement.statement) byteInput of
    Right (result, remaining, consumed) -> do
      putStrLn $ "✓ SUCCESS: " ++ take 100 (show result)
      putStrLn $ "  Consumed: " ++ show consumed ++ " bytes"
      putStrLn $ "  Remaining: " ++ show remaining
    Left err -> do
      putStrLn $ "✗ FAILED: " ++ show err

  putStrLn ""

  -- Test the full program parser
  putStrLn "Testing full program parser with whitespace and EOF:"
  case runJSParser (Lexer.whitespace *> Statement.statementList <* Lexer.whitespace <* FP.eof) byteInput of
    Right (result, remaining, consumed) -> do
      putStrLn $ "✓ SUCCESS: " ++ take 100 (show result)
      putStrLn $ "  Consumed: " ++ show consumed ++ " bytes"
      putStrLn $ "  Remaining: " ++ show remaining
    Left err -> do
      putStrLn $ "✗ FAILED: " ++ show err