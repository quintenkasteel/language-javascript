#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, containers, text, lens
-}

{-# LANGUAGE OverloadedStrings #-}

import Control.Lens ((^.), (.~), (&))
import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Pretty.Printer (renderToString)
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Process.TreeShake.Types as Types

main :: IO ()
main = do
  putStrLn "=== Simple Eval Logic Test ==="

  -- Exact test case from failing test
  let source = "var x = 1; eval('console.log(x)');"
  putStrLn $ "Source: " ++ source

  case parse source "test" of
    Right ast -> do
      putStrLn "Parse successful"

      -- Test with default options (should be conservative)
      let opts = defaultOptions
      putStrLn $ "aggressiveShaking: " ++ show (opts ^. Types.aggressiveShaking)

      let optimized = treeShake opts ast
      let result = renderToString optimized
      putStrLn $ "Result: '" ++ result ++ "'"
      putStrLn $ "Result words: " ++ show (words result)
      putStrLn $ "Contains 'var': " ++ show ("var" `elem` words result)
      putStrLn $ "Contains 'x': " ++ show ("x" `elem` words result)
      putStrLn $ "Contains 'eval': " ++ show ("eval" `elem` words result)

    Left err -> putStrLn $ "Parse error: " ++ err