#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript
-}

import Language.JavaScript.Parser.Parser (parse)

main :: IO ()
main = do
  putStrLn "=== Require Expression Debug ==="

  -- Test what require('react').useEffect parses to
  let source = "var useEffect = require('react').useEffect;"
  putStrLn $ "Source: " ++ source

  case parse source "test" of
    Right ast -> do
      putStrLn "Parse succeeded!"
      putStrLn $ "AST: " ++ show ast
    Left err -> putStrLn $ "Parse error: " ++ err