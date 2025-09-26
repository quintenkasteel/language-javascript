#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text, lens
-}

import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Process.TreeShake
-- import Language.JavaScript.Process.TreeShake.Elimination (astContainsEval)
import Language.JavaScript.Pretty.Printer (renderToString)
import qualified Data.Text as Text
import Control.Lens ((^.))

main :: IO ()
main = do
  putStrLn "=== Detailed Eval Debug ==="

  let source = "var maybeUsed = 1; eval('console.log(maybeUsed)');"
  putStrLn $ "Source: " ++ source

  case parse source "test" of
    Right ast -> do
      putStrLn "Parse succeeded!"
      putStrLn $ "AST structure: " ++ take 200 (show ast)

      -- Check eval detection - we'll infer from behavior
      putStrLn "Checking if eval affects behavior..."

      -- Test conservative mode (aggressiveShaking = False)
      let conservativeOpts = defaultOptions { _aggressiveShaking = False }
      putStrLn $ "Conservative options: aggressiveShaking = " ++ show (_aggressiveShaking conservativeOpts)

      let (optimized, analysis) = treeShakeWithAnalysis conservativeOpts ast
      let optimizedSource = renderToString optimized

      putStrLn $ "\nAnalysis results:"
      putStrLn $ "  Total identifiers: " ++ show (_totalIdentifiers analysis)
      putStrLn $ "  Unused count: " ++ show (_unusedCount analysis)

      putStrLn $ "\nOptimized source:"
      putStrLn optimizedSource

      putStrLn $ "\nOptimized AST structure:"
      putStrLn $ take 300 (show optimized)

    Left err -> putStrLn $ "Parse error: " ++ err