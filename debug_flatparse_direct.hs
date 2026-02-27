#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text, bytestring
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Language.JavaScript.Parser.Flatparse.Parser as FlatParser

-- Debug by calling flatparse directly
main :: IO ()
main = do
  putStrLn "=== DEBUGGING FLATPARSE DIRECTLY ==="
  putStrLn ""

  let input = "var x = 42;"
  let inputText = Text.pack input
  let inputBS = Text.encodeUtf8 inputText

  putStrLn $ "Testing input: " ++ input
  putStrLn $ "Input as Text: " ++ show inputText
  putStrLn $ "Input as ByteString: " ++ show inputBS
  putStrLn ""

  putStrLn "1. Testing parseProgram (Text -> ParseResult):"
  case FlatParser.parseProgram inputText of
    FlatParser.ParseOK success -> do
      putStrLn $ "✓ SUCCESS: parseProgram worked"
      putStrLn $ "  Result: " ++ take 100 (show (FlatParser.parseResult success))
    FlatParser.ParseError failure -> do
      putStrLn $ "✗ FAILED: parseProgram failed"
      putStrLn $ "  Error: " ++ show (FlatParser.parseError failure)

  putStrLn ""
  putStrLn "2. Testing parseProgramByteString directly:"
  case FlatParser.parseProgramByteString inputBS of
    FlatParser.ParseOK success -> do
      putStrLn $ "✓ SUCCESS: parseProgramByteString worked"
      putStrLn $ "  Result: " ++ take 100 (show (FlatParser.parseResult success))
    FlatParser.ParseError failure -> do
      putStrLn $ "✗ FAILED: parseProgramByteString failed"
      putStrLn $ "  Error: " ++ show (FlatParser.parseError failure)