#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import Control.Lens ((^.), (.~), (&))
import Language.JavaScript.Parser (parse)
import Language.JavaScript.Pretty.Printer (renderToString)
import Language.JavaScript.Process.TreeShake (treeShake, defaultOptions, analyzeUsage)
import Language.JavaScript.Process.TreeShake.Types

main :: IO ()
main = do
  let source = "function unused() { return 42; } var x = 1;"
  putStrLn "=== OPTIMIZATION LEVEL TEST ==="
  putStrLn $ "Source: " ++ source
  
  case parse source "test" of
    Right ast -> do
      putStrLn "\n=== ORIGINAL AST ===" 
      putStrLn $ renderToString ast
      
      -- Base options that allow optimization level differences to be visible
      let baseOpts = defaultOptions & preserveTopLevel .~ False
      let conservativeOpts = baseOpts & optimizationLevel .~ Conservative
      let aggressiveOpts = baseOpts & optimizationLevel .~ Aggressive
      
      putStrLn "\n=== CONSERVATIVE OPTIONS ==="
      print conservativeOpts
      
      putStrLn "\n=== AGGRESSIVE OPTIONS ==="
      print aggressiveOpts
      
      let conservativeResult = treeShake conservativeOpts ast
      let aggressiveResult = treeShake aggressiveOpts ast
      
      putStrLn "\n=== CONSERVATIVE RESULT ==="
      putStrLn $ renderToString conservativeResult
      
      putStrLn "\n=== AGGRESSIVE RESULT ==="
      putStrLn $ renderToString aggressiveResult
      
      putStrLn "\n=== RESULTS COMPARISON ==="
      putStrLn $ "Results identical: " ++ show (conservativeResult == aggressiveResult)
      
      putStrLn "\n=== USAGE ANALYSIS ==="
      let analysis = analyzeUsage ast
      let usageMap = _usageMap analysis
      putStrLn $ "Total identifiers: " ++ show (_totalIdentifiers analysis)
      putStrLn $ "Unused count: " ++ show (_unusedCount analysis)
      
      putStrLn "\n=== USAGE MAP DETAILS ==="
      Map.foldrWithKey (\name info acc -> do
        putStrLn $ Text.unpack name ++ ": used=" ++ show (_isUsed info) 
                                   ++ ", exported=" ++ show (_isExported info)
                                   ++ ", scope=" ++ show (_scopeDepth info)
                                   ++ ", refs=" ++ show (_directReferences info)
        acc) (pure ()) usageMap
      
    Left err -> putStrLn $ "Parse failed: " ++ err