#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text, lens
-}

import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Process.TreeShake.Types
import Language.JavaScript.Pretty.Printer
import Control.Lens ((^.), (.~), (&))
import qualified Data.Text as Text

main :: IO ()
main = do
  putStrLn "=== Eval Test Debug ==="

  -- Test the exact failing case
  let source = "var x = 1; eval('console.log(x)');"
  putStrLn $ "Source: " ++ source

  case parse source "test" of
    Right ast -> do
      putStrLn "\n=== Tree Shaking with eval present ==="

      let opts = defaultOptions
      let result = treeShake opts ast
      let resultText = renderToString result

      putStrLn $ "Result: " ++ resultText
      putStrLn $ "Contains 'x': " ++ show (Text.pack "x" `Text.isInfixOf` Text.pack resultText)
      putStrLn $ "Length: " ++ show (length resultText)

      -- Check if eval is still present
      putStrLn $ "Contains 'eval': " ++ show (Text.pack "eval" `Text.isInfixOf` Text.pack resultText)

      -- Test aggressive shaking behavior
      putStrLn "\n=== Aggressive vs Conservative ==="
      let conservativeOpts = defaultOptions & aggressiveShaking .~ False
      let aggressiveOpts = defaultOptions & aggressiveShaking .~ True

      let conservativeResult = treeShake conservativeOpts ast
      let aggressiveResult = treeShake aggressiveOpts ast

      putStrLn $ "Conservative: " ++ renderToString conservativeResult
      putStrLn $ "Aggressive: " ++ renderToString aggressiveResult
      putStrLn $ "Same result: " ++ show (renderToString conservativeResult == renderToString aggressiveResult)

    Left err -> putStrLn $ "Parse error: " ++ err