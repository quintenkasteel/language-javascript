#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, hspec, containers
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Set as Set
import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Pretty.Printer (renderToString)
import Language.JavaScript.Process.TreeShake
import Test.Hspec

main :: IO ()
main = do
  putStrLn "=== Tree Shaking Debug Test ==="

  -- Test 1: React component test
  putStrLn "\n--- Test 1: React component test ---"
  let reactSource = unlines
        [ "var React = require('react');"
        , "var useState = require('react').useState;"
        , "var useEffect = require('react').useEffect;"  -- Should be unused
        , ""
        , "function MyComponent() {"
        , "  var state = useState(0)[0];"
        , "  var setState = useState(0)[1];"
        , "  return React.createElement('div', null, state);"
        , "}"
        , ""
        , "module.exports = MyComponent;"
        ]

  case parse reactSource "test" of
    Right ast -> do
      putStrLn "Parse successful!"
      let optimized = treeShake defaultOptions ast
      let optimizedSource = renderToString optimized
      putStrLn "Optimized source:"
      putStrLn optimizedSource
      putStrLn ""
      putStrLn $ "Contains 'useEffect': " ++ show ("useEffect" `elem` words optimizedSource)
      putStrLn $ "Contains 'React': " ++ show ("React" `elem` words optimizedSource)
      putStrLn $ "Contains 'useState': " ++ show ("useState" `elem` words optimizedSource)
    Left err -> putStrLn $ "Parse error: " ++ err

  -- Test 2: Node.js module test
  putStrLn "\n--- Test 2: Node.js module test ---"
  let nodeSource = unlines
        [ "const fs = require('fs');"
        , "const path = require('path');"
        , "const unused = require('crypto');"  -- Should be unused
        , ""
        , "function readConfig() {"
        , "  return fs.readFileSync(path.join(__dirname, 'config.json'));"
        , "}"
        , ""
        , "module.exports = {readConfig};"
        ]

  case parse nodeSource "test" of
    Right ast -> do
      putStrLn "Parse successful!"
      let optimized = treeShake defaultOptions ast
      let optimizedSource = renderToString optimized
      putStrLn "Optimized source:"
      putStrLn optimizedSource
      putStrLn ""
      putStrLn $ "Contains 'crypto': " ++ show ("crypto" `elem` words optimizedSource)
      putStrLn $ "Contains 'fs': " ++ show ("fs" `elem` words optimizedSource)
      putStrLn $ "Contains 'unused': " ++ show ("unused" `elem` words optimizedSource)
    Left err -> putStrLn $ "Parse error: " ++ err