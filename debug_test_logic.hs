#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text, hspec
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Language.JavaScript.Parser as Language.JavaScript.Parser
import qualified Language.JavaScript.Parser.AST as AST
import Language.JavaScript.Pretty.Printer (renderToString)
import Control.Monad (forM_)

-- Replicate the exact failing test logic
main :: IO ()
main = do
  putStrLn "=== REPLICATING EXACT FAILING TEST LOGIC ==="
  putStrLn ""

  -- This is the exact test case from CoreProperties.hs
  let testCases =
        [ "var x = 42;",
          "function test() { return 1 + 2; }",
          "if (x > 0) { console.log('positive'); }",
          "var obj = { key: 'value', num: 123 };"
        ]

  putStrLn "Testing each case with exact test logic:"
  forM_ testCases $ \original -> do
    putStrLn $ "Testing: " ++ original
    case Language.JavaScript.Parser.parse original "test" of
      Right ast -> do
        putStrLn $ "✓ Initial parse succeeded for: " ++ original
        let prettyPrinted = renderToString ast
        putStrLn $ "  Pretty printed: " ++ take 50 prettyPrinted
        case Language.JavaScript.Parser.parse prettyPrinted "test" of
          Right reparsed -> do
            putStrLn $ "✓ Reparse succeeded"
            putStrLn $ "  isValidAST would be: " ++ show (isValidAST reparsed)
          Left err -> putStrLn $ "✗ Reparse failed: " ++ show err
      Left err -> putStrLn $ "✗ Initial parse failed: " ++ show err
    putStrLn ""

-- Simplified version of isValidAST for testing
isValidAST :: AST.JSAST -> Bool
isValidAST (AST.JSAstProgram stmts _) = not (null stmts)  -- Basic check
isValidAST _ = True