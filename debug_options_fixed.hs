#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, hspec, containers, text, lens
-}

{-# LANGUAGE OverloadedStrings #-}

import Control.Lens ((^.), (.~), (&))
import qualified Data.Set as Set
import qualified Data.Text as Text
import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Pretty.Printer (renderToString)
import Language.JavaScript.Process.TreeShake

main :: IO ()
main = do
  putStrLn "=== Tree Shaking Options Debug ==="

  let source = "var useEffect = require('react').useEffect;"
  putStrLn $ "Source: " ++ source

  case parse source "test" of
    Right ast -> do
      putStrLn "Parse successful!"

      putStrLn "\n--- Test with default options ---"
      let opts1 = defaultOptions
      putStrLn $ "preserveTopLevel: " ++ show (opts1 ^. _preserveTopLevel)
      putStrLn $ "aggressiveShaking: " ++ show (opts1 ^. _aggressiveShaking)
      let optimized1 = treeShake opts1 ast
      let optimizedSource1 = renderToString optimized1
      putStrLn "Optimized source:"
      putStrLn optimizedSource1
      putStrLn $ "Contains useEffect: " ++ show ("useEffect" `elem` words optimizedSource1)

      putStrLn "\n--- Test with preserveTopLevel = False ---"
      let opts2 = defaultOptions & _preserveTopLevel .~ False
      putStrLn $ "preserveTopLevel: " ++ show (opts2 ^. _preserveTopLevel)
      putStrLn $ "aggressiveShaking: " ++ show (opts2 ^. _aggressiveShaking)
      let optimized2 = treeShake opts2 ast
      let optimizedSource2 = renderToString optimized2
      putStrLn "Optimized source:"
      putStrLn optimizedSource2
      putStrLn $ "Contains useEffect: " ++ show ("useEffect" `elem` words optimizedSource2)

    Left err -> putStrLn $ "Parse error: " ++ err