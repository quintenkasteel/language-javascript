#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, hspec, containers, text
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Set as Set
import qualified Data.Text as Text
import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Pretty.Printer (renderToString)
import Language.JavaScript.Process.TreeShake
-- import Language.JavaScript.Process.TreeShake.Analysis (analyzeUsage)
import Test.Hspec

main :: IO ()
main = do
  putStrLn "=== UseEffect Elimination Debug ==="

  let source = "var useEffect = require('react').useEffect;"
  putStrLn $ "Source: " ++ source

  case parse source "test" of
    Right ast -> do
      putStrLn "Parse successful!"
      -- let analysis = analyzeUsage ast
      -- putStrLn $ "Usage analysis: " ++ show analysis

      let optimized = treeShake defaultOptions ast
      let optimizedSource = renderToString optimized
      putStrLn "Optimized source:"
      putStrLn optimizedSource

      putStrLn $ "Contains 'useEffect': " ++ show ("useEffect" `elem` words optimizedSource)
    Left err -> putStrLn $ "Parse error: " ++ err