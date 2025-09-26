#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text, lens
-}

import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Pretty.Printer (renderToString)
import qualified Data.Text as Text
import Control.Lens ((^.), (.~), (&))

main :: IO ()
main = do
  putStrLn "=== Eval Test 1 Debug: aggressiveShaking ==="

  -- Test the exact failing case from Core.hs line 304
  let source = "var maybeUsed = 1; eval('console.log(maybeUsed)');"
  putStrLn $ "Source: " ++ source

  case parse source "test" of
    Right ast -> do
      putStrLn "Parse succeeded!"

      -- Test aggressive vs conservative
      let conservativeOpts = defaultOptions
      let aggressiveOpts = defaultOptions { _aggressiveShaking = True }

      putStrLn "\n=== Conservative mode ==="
      let conservativeResult = treeShake conservativeOpts ast
      let conservativeSource = renderToString conservativeResult
      putStrLn $ "Conservative result:\n" ++ conservativeSource
      putStrLn $ "Contains 'maybeUsed': " ++ show ("maybeUsed" `elem` words conservativeSource)

      putStrLn "\n=== Aggressive mode ==="
      let aggressiveResult = treeShake aggressiveOpts ast
      let aggressiveSource = renderToString aggressiveResult
      putStrLn $ "Aggressive result:\n" ++ aggressiveSource
      putStrLn $ "Contains 'maybeUsed': " ++ show ("maybeUsed" `elem` words aggressiveSource)

    Left err -> putStrLn $ "Parse error: " ++ err