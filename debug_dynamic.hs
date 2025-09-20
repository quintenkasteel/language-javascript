#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text, containers
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Language.JavaScript.Parser
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Process.TreeShake.Types
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  let source = Text.unlines
        [ "var handlers = {"
        , "  method1: function() { return 'handler1'; },"
        , "  method2: function() { return 'handler2'; }"
        , "};"
        , "var methodName = 'method1';"
        , "var result = handlers[methodName]();"
        , "console.log(result);"
        ]

  putStrLn "Source:"
  Text.putStrLn source

  case parseProgram source of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right ast -> do
      putStrLn "\nParsed successfully!"

      let analysis = analyzeUsage ast
      putStrLn $ "\nUsage analysis:"
      putStrLn $ "  Total identifiers: " ++ show (analysis ^. totalIdentifiers)
      putStrLn $ "  Used identifiers: " ++ show (Map.size (analysis ^. usageMap))
      putStrLn $ "  Dynamic access objects: " ++ show (analysis ^. dynamicAccessObjects)

      let usageMapData = analysis ^. usageMap
      putStrLn $ "\nUsage map:"
      Map.traverseWithKey (\k v -> putStrLn $ "  " ++ Text.unpack k ++ ": " ++ show v) usageMapData

      let opts = defaultOptions
      let optimized = treeShake opts ast
      putStrLn $ "\nOptimized AST:"
      print optimized