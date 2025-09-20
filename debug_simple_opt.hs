#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens ((^.), (.~), (&))
import Language.JavaScript.Parser (parse)
import Language.JavaScript.Pretty.Printer (renderToString)
import Language.JavaScript.Process.TreeShake (treeShake, defaultOptions)
import Language.JavaScript.Process.TreeShake.Types

main :: IO ()
main = do
  let source = "function unused() { return 42; }"
  
  case parse source "test" of
    Right ast -> do
      putStrLn "=== SIMPLE OPTIMIZATION TEST ==="
      putStrLn $ "Original: " ++ renderToString ast
      
      -- Test with default options (preserveTopLevel = True)
      let defaultResult = treeShake defaultOptions ast
      putStrLn $ "Default: " ++ renderToString defaultResult
      
      -- Test Conservative with preserveTopLevel = False 
      let conservativeOpts = defaultOptions & preserveTopLevel .~ False & optimizationLevel .~ Conservative
      let conservativeResult = treeShake conservativeOpts ast
      putStrLn $ "Conservative (preserveTopLevel=False): " ++ renderToString conservativeResult
      putStrLn $ "  AST: " ++ show conservativeResult
      
      -- Test Aggressive with preserveTopLevel = False
      let aggressiveOpts = defaultOptions & preserveTopLevel .~ False & optimizationLevel .~ Aggressive  
      let aggressiveResult = treeShake aggressiveOpts ast
      putStrLn $ "Aggressive (preserveTopLevel=False): " ++ renderToString aggressiveResult
      putStrLn $ "  AST: " ++ show aggressiveResult
      
      putStrLn $ "Results identical: " ++ show (conservativeResult == aggressiveResult)
      
      -- Test just changing optimization level (keep preserveTopLevel = True)
      let conservativeDefault = defaultOptions & optimizationLevel .~ Conservative
      let aggressiveDefault = defaultOptions & optimizationLevel .~ Aggressive
      let conservativeDefaultResult = treeShake conservativeDefault ast
      let aggressiveDefaultResult = treeShake aggressiveDefault ast
      putStrLn $ "Conservative (preserveTopLevel=True): " ++ renderToString conservativeDefaultResult
      putStrLn $ "Aggressive (preserveTopLevel=True): " ++ renderToString aggressiveDefaultResult
      
    Left err -> putStrLn $ "Parse failed: " ++ err