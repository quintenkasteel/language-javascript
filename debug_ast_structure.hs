#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text, lens
-}

import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Parser.AST (JSAST)
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Process.TreeShake.Types
import Language.JavaScript.Pretty.Printer
import Control.Lens ((^.), (.~), (&))
import qualified Data.Text as Text

showAST :: JSAST -> String
showAST ast = take 500 (show ast)  -- Truncate for readability

main :: IO ()
main = do
  putStrLn "=== AST Structure Debug ==="

  let source = "var maybeUsed = 1; eval('console.log(maybeUsed)');"
  putStrLn $ "Source: " ++ source

  case parse source "test" of
    Right ast -> do
      putStrLn "\n=== Original AST ==="
      putStrLn $ showAST ast

      let conservativeOpts = defaultOptions & aggressiveShaking .~ False
      let conservativeResult = treeShake conservativeOpts ast

      putStrLn "\n=== Conservative Tree Shaking Result ==="
      putStrLn $ showAST conservativeResult
      putStrLn $ "\nPretty printed: " ++ renderToString conservativeResult

    Left err -> putStrLn $ "Parse error: " ++ err