#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Language.JavaScript.Parser.Parser as Parser

-- Debug with detailed error reporting
main :: IO ()
main = do
  putStrLn "=== DEBUGGING WITH DETAILED ERROR REPORTING ==="
  putStrLn ""

  let testCases =
        [ "var x = 1;"
        , "var x = 1"      -- Without semicolon
        , "var x"          -- Just declaration
        , "x"              -- Just identifier
        , "1"              -- Just number
        , ""               -- Empty string
        , " "              -- Just whitespace
        , "  var x = 1;  " -- With surrounding whitespace
        ]

  putStrLn "Testing various input variations:"
  mapM_ testWithDetails testCases

testWithDetails :: String -> IO ()
testWithDetails input = do
  putStrLn $ "\n  Input: " ++ show input
  putStrLn $ "  Length: " ++ show (length input)
  putStrLn $ "  Bytes: " ++ show (map (fromEnum) input)

  case Parser.parse input "test" of
    Right ast -> putStrLn $ "  ✓ SUCCESS: " ++ take 50 (show ast)
    Left err -> do
      putStrLn $ "  ✗ FAILED: " ++ err
      putStrLn $ "  Error length: " ++ show (length err)
      putStrLn $ "  Error words: " ++ show (take 5 (words err))