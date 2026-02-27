#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text
-}

{-# LANGUAGE OverloadedStrings #-}

import Language.JavaScript.Parser
import qualified Language.JavaScript.Parser as Language.JavaScript.Parser
import qualified Language.JavaScript.Parser.AST as AST

-- Exact replica of the failing test logic
testExactReplica :: IO ()
testExactReplica = do
  let testCases =
        [ "var x = 42;",
          "function test() { return 1 + 2; }",
          "if (x > 0) { console.log('positive'); }",
          "var obj = { key: 'value', num: 123 };"
        ]

  putStrLn "Testing exact replica of failing test logic:"

  mapM_ testCase testCases

testCase :: String -> IO ()
testCase original = do
  putStrLn $ "\nTesting: " ++ original
  case Language.JavaScript.Parser.parse original "test" of
    Right ast -> do
      putStrLn "✓ Initial parse succeeded"
      let prettyPrinted = renderToString ast
      putStrLn $ "  Pretty printed: " ++ show prettyPrinted
      case Language.JavaScript.Parser.parse prettyPrinted "test" of
        Right reparsed -> putStrLn "✓ Reparse succeeded"
        Left err -> putStrLn $ "✗ Reparse failed: " ++ show err
    Left err -> putStrLn $ "✗ Initial parse failed: " ++ show err

main :: IO ()
main = testExactReplica