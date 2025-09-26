#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript
-}

import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Parser.AST

main :: IO ()
main = do
  putStrLn "=== Statement Type Debug ==="

  let source1 = "var unused = require('crypto');"
  putStrLn $ "\n--- var statement: " ++ source1
  case parse source1 "test" of
    Right (JSAstProgram [stmt] _) -> putStrLn $ "Statement type: " ++ show stmt
    _ -> putStrLn "Parse failed"

  let source2 = "const unused = require('crypto');"
  putStrLn $ "\n--- const statement: " ++ source2
  case parse source2 "test" of
    Right (JSAstProgram [stmt] _) -> putStrLn $ "Statement type: " ++ show stmt
    _ -> putStrLn "Parse failed"

  let source3 = "let unused = require('crypto');"
  putStrLn $ "\n--- let statement: " ++ source3
  case parse source3 "test" of
    Right (JSAstProgram [stmt] _) -> putStrLn $ "Statement type: " ++ show stmt
    _ -> putStrLn "Parse failed"