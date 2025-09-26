#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, containers, text
-}

{-# LANGUAGE OverloadedStrings #-}

import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Pretty.Printer (renderToString)
import Language.JavaScript.Process.TreeShake

main :: IO ()
main = do
  putStrLn "=== Direct Comparison ==="

  -- Working case from Node.js test
  let source1 = "const unused = require('crypto');"
  putStrLn $ "\n--- Working case: " ++ source1
  case parse source1 "test" of
    Right ast -> do
      let optimized = treeShake defaultOptions ast
      let optimizedSource = renderToString optimized
      putStrLn "Optimized:"
      putStrLn $ "'" ++ optimizedSource ++ "'"
      putStrLn $ "Length: " ++ show (length optimizedSource)
      putStrLn $ "Empty: " ++ show (null (filter (/= ' ') (filter (/= '\n') optimizedSource)))
    Left err -> putStrLn $ "Parse error: " ++ err

  -- Failing case from React test
  let source2 = "var useEffect = require('react').useEffect;"
  putStrLn $ "\n--- Failing case: " ++ source2
  case parse source2 "test" of
    Right ast -> do
      let optimized = treeShake defaultOptions ast
      let optimizedSource = renderToString optimized
      putStrLn "Optimized:"
      putStrLn $ "'" ++ optimizedSource ++ "'"
      putStrLn $ "Length: " ++ show (length optimizedSource)
      putStrLn $ "Empty: " ++ show (null (filter (/= ' ') (filter (/= '\n') optimizedSource)))
    Left err -> putStrLn $ "Parse error: " ++ err