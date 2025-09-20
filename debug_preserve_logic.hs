#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import Control.Lens ((^.), (.~), (&))
import Language.JavaScript.Parser (parse)
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.SrcLocation (TokenPosn(..))
import Language.JavaScript.Process.TreeShake (analyzeUsage)
import Language.JavaScript.Process.TreeShake.Types
import Language.JavaScript.Process.TreeShake.Elimination (shouldPreserveStatement, shouldPreserveForOptimizationLevel)

main :: IO ()
main = do
  let source = "function unused() { return 42; } var x = 1;"
  putStrLn "=== PRESERVE LOGIC TEST ==="
  putStrLn $ "Source: " ++ source
  
  case parse source "test" of
    Right (JSAstProgram [funcStmt, varStmt] _) -> do
      putStrLn "\n=== ANALYZING FUNCTION STATEMENT ==="
      
      let analysis = analyzeUsage (JSAstProgram [funcStmt, varStmt] (JSAnnot (TokenPosn 0 0 0) []))
      let usageMap = _usageMap analysis
      
      -- Base options that allow optimization level differences to be visible
      let baseOpts = defaultTreeShakeOptions & preserveTopLevel .~ False
      let conservativeOpts = baseOpts & optimizationLevel .~ Conservative
      let aggressiveOpts = baseOpts & optimizationLevel .~ Aggressive
      
      putStrLn "=== TESTING FUNCTION PRESERVATION ==="
      putStrLn $ "Function statement: " ++ show funcStmt
      
      putStrLn "\n=== CONSERVATIVE MODE ==="
      let conservativePreserve = shouldPreserveStatement conservativeOpts usageMap funcStmt
      putStrLn $ "shouldPreserveStatement: " ++ show conservativePreserve
      let conservativeOptLevel = shouldPreserveForOptimizationLevel conservativeOpts funcStmt
      putStrLn $ "shouldPreserveForOptimizationLevel: " ++ show conservativeOptLevel
      
      putStrLn "\n=== AGGRESSIVE MODE ==="
      let aggressivePreserve = shouldPreserveStatement aggressiveOpts usageMap funcStmt
      putStrLn $ "shouldPreserveStatement: " ++ show aggressivePreserve
      let aggressiveOptLevel = shouldPreserveForOptimizationLevel aggressiveOpts funcStmt
      putStrLn $ "shouldPreserveForOptimizationLevel: " ++ show aggressiveOptLevel
      
      putStrLn "\n=== USAGE MAP ==="
      Map.foldrWithKey (\name info acc -> do
        putStrLn $ Text.unpack name ++ ": used=" ++ show (_isUsed info)
        acc) (pure ()) usageMap
      
    Right otherAst -> putStrLn $ "Unexpected AST structure: " ++ show otherAst
    Left err -> putStrLn $ "Parse failed: " ++ err