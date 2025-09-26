#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, containers, text, lens
-}

{-# LANGUAGE OverloadedStrings #-}

import Control.Lens ((^.), (.~), (&))
import qualified Data.Text as Text
import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Pretty.Printer (renderToString)
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Process.TreeShake.Types as Types

main :: IO ()
main = do
  putStrLn "=== Simple Variable Test ==="

  -- Test just a variable without eval first
  let source1 = "var x = 1;"
  putStrLn $ "\n--- Test 1 (no eval): " ++ source1
  case parse source1 "test" of
    Right ast -> do
      let optimized = treeShake defaultOptions ast
      putStrLn $ "Result: " ++ renderToString optimized
      putStrLn $ "Contains 'x': " ++ show ("x" `elem` words (renderToString optimized))
    Left err -> putStrLn $ "Parse error: " ++ err

  -- Test variable with eval but in aggressive mode
  let source2 = "var x = 1; eval('console.log(x)');"
  let aggressiveOpts = defaultOptions & Types.aggressiveShaking .~ True
  putStrLn $ "\n--- Test 2 (eval + aggressive): " ++ source2
  case parse source2 "test" of
    Right ast -> do
      let optimized = treeShake aggressiveOpts ast
      putStrLn $ "Result: " ++ renderToString optimized
      putStrLn $ "Contains 'x': " ++ show ("x" `elem` words (renderToString optimized))
    Left err -> putStrLn $ "Parse error: " ++ err

  -- Test variable with eval in conservative mode
  putStrLn $ "\n--- Test 3 (eval + conservative): " ++ source2
  case parse source2 "test" of
    Right ast -> do
      let optimized = treeShake defaultOptions ast -- defaultOptions has aggressiveShaking=False
      putStrLn $ "Result: " ++ renderToString optimized
      putStrLn $ "Contains 'x': " ++ show ("x" `elem` words (renderToString optimized))
    Left err -> putStrLn $ "Parse error: " ++ err