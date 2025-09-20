#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript
-}

import Language.JavaScript.Parser.Parser (parse)

main :: IO ()
main = do
  putStrLn "=== Require Expression Parse Debug ==="

  -- Test what require('react').useState parses to
  let source1 = "var useState = require('react').useState;"
  putStrLn $ "Source 1: " ++ source1
  case parse source1 "test" of
    Right ast -> do
      putStrLn "Parse succeeded!"
      putStrLn $ "AST: " ++ show ast
    Left err -> putStrLn $ "Parse error: " ++ err

  putStrLn "\n--- Now let's try simple require ---"
  let source2 = "var unused = require('crypto');"
  putStrLn $ "Source 2: " ++ source2
  case parse source2 "test" of
    Right ast -> do
      putStrLn "Parse succeeded!"
      putStrLn $ "AST: " ++ show ast
    Left err -> putStrLn $ "Parse error: " ++ err