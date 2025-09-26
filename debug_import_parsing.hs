#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text
-}

import Language.JavaScript.Parser.Parser (parseModule)

main :: IO ()
main = do
  putStrLn "=== Import Parsing Debug ==="

  -- Test the exact failing case
  let source = "import {used, unused} from 'utils';"
  putStrLn $ "Source: " ++ source

  case parseModule source "test" of
    Right ast -> do
      putStrLn "Parse succeeded!"
      putStrLn $ "AST (first 300 chars): " ++ take 300 (show ast)

    Left err -> do
      putStrLn $ "Parse failed: " ++ err
      putStrLn "The parser may not support ES6 import syntax"