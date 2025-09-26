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
  putStrLn "=== Eval Handling Debug ==="

  -- Test 1: aggressiveShaking test case
  let source1 = "var maybeUsed = 1; eval('console.log(maybeUsed)');"
  putStrLn $ "\n--- Test 1 (aggressiveShaking): " ++ source1
  case parse source1 "test" of
    Right ast -> do
      let conservativeOpts = defaultOptions & aggressiveShaking .~ False
      let aggressiveOpts = defaultOptions & aggressiveShaking .~ True

      let conservativeResult = treeShake conservativeOpts ast
      let aggressiveResult = treeShake aggressiveOpts ast

      putStrLn $ "Conservative mode result: " ++ renderToString conservativeResult
      putStrLn $ "Conservative contains 'maybeUsed': " ++ show ("maybeUsed" `elem` words (renderToString conservativeResult))

      putStrLn $ "Aggressive mode result: " ++ renderToString aggressiveResult  
      putStrLn $ "Aggressive contains 'maybeUsed': " ++ show ("maybeUsed" `elem` words (renderToString aggressiveResult))
    Left err -> putStrLn $ "Parse error: " ++ err

  -- Test 2: conservative eval test case
  let source2 = "var x = 1; eval('console.log(x)');"
  putStrLn $ "\n--- Test 2 (conservative eval): " ++ source2
  case parse source2 "test" of
    Right ast -> do
      let optimized = treeShake defaultOptions ast
      putStrLn $ "Optimized result: " ++ renderToString optimized
      putStrLn $ "Contains 'x': " ++ show ("x" `elem` words (renderToString optimized))

      -- Check usage analysis
      let (_, analysis) = treeShakeWithAnalysis defaultOptions ast
      let usageMap = analysis ^. Types.usageMap
      putStrLn $ "Is 'x' used according to analysis? " ++ show (Types.isIdentifierUsed "x" usageMap)
    Left err -> putStrLn $ "Parse error: " ++ err
