#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, hspec, containers, text, lens
-}

{-# LANGUAGE OverloadedStrings #-}

import Control.Lens ((^.), (.~), (&))
import qualified Data.Text as Text
import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Pretty.Printer (renderToString)
import Language.JavaScript.Process.TreeShake

main :: IO ()
main = do
  putStrLn "=== Simple Tree Shaking Test ==="

  let source = "var useEffect = require('react').useEffect;"
  putStrLn $ "Source: " ++ source

  case parse source "test" of
    Right ast -> do
      putStrLn "Parse successful!"

      -- Test with preserveTopLevel = False
      let opts = defaultOptions & preserveTopLevel .~ False
      let optimized = treeShake opts ast
      let optimizedSource = renderToString optimized
      putStrLn "Optimized source (preserveTopLevel=False):"
      putStrLn optimizedSource
      putStrLn $ "Contains useEffect: " ++ show ("useEffect" `elem` words optimizedSource)
      putStrLn $ "Is empty: " ++ show (null (words optimizedSource))

    Left err -> putStrLn $ "Parse error: " ++ err
