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
  putStrLn "=== maybeUsed Test Debug ==="

  -- Test the exact failing case
  let source = "var maybeUsed = 1; eval('console.log(maybeUsed)');"
  putStrLn $ "Source: " ++ source

  case parse source "test" of
    Right ast -> do
      putStrLn "\n=== Tree Shaking with maybeUsed ==="

      let conservativeOpts = defaultOptions & aggressiveShaking .~ False
      let aggressiveOpts = defaultOptions & aggressiveShaking .~ True

      let conservativeResult = treeShake conservativeOpts ast
      let aggressiveResult = treeShake aggressiveOpts ast

      putStrLn $ "Conservative: " ++ renderToString conservativeResult
      putStrLn $ "Aggressive: " ++ renderToString aggressiveResult
      putStrLn $ "Conservative contains 'maybeUsed': " ++ show (Text.pack "maybeUsed" `Text.isInfixOf` Text.pack (renderToString conservativeResult))
      putStrLn $ "Aggressive contains 'maybeUsed': " ++ show (Text.pack "maybeUsed" `Text.isInfixOf` Text.pack (renderToString aggressiveResult))

    Left err -> putStrLn $ "Parse error: " ++ err