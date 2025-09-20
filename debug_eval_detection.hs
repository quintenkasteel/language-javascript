#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text, lens
-}

import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Parser.AST (JSAST)
import Language.JavaScript.Process.TreeShake.Elimination (astContainsEval)

main :: IO ()
main = do
  putStrLn "=== Eval Detection Debug ==="

  -- Test the exact failing case
  let source = "var maybeUsed = 1; eval('console.log(maybeUsed)');"
  putStrLn $ "Source: " ++ source

  case parse source "test" of
    Right ast -> do
      let hasEval = astContainsEval ast
      putStrLn $ "AST contains eval: " ++ show hasEval
      putStrLn $ "AST structure (first 200 chars): " ++ take 200 (show ast)

    Left err -> putStrLn $ "Parse error: " ++ err