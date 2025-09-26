#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, hspec, containers, text
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Set as Set
import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Pretty.Printer (renderToString)
import Language.JavaScript.Process.TreeShake
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "TreeShake Integration Tests" $ do
    -- Test that was previously failing - Node.js test 5
    it "handles Node.js module patterns" $ do
      let source = unlines
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

      case parse source "test" of
        Right ast -> do
          let optimized = treeShake defaultOptions ast
          let optimizedSource = renderToString optimized

          -- Used requires should remain
          optimizedSource `shouldContain` "fs"
          optimizedSource `shouldContain` "path"
          -- Unused require should be removed
          optimizedSource `shouldNotContain` "crypto"

        Left err -> expectationFailure $ "Parse failed: " ++ err

    -- Test that's still probably failing - React test 4
    it "optimizes React component code" $ do
      let source = unlines
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

      case parse source "test" of
        Right ast -> do
          let optimized = treeShake defaultOptions ast
          let optimizedSource = renderToString optimized

          -- Used React imports should remain
          optimizedSource `shouldContain` "React"
          optimizedSource `shouldContain` "useState"
          -- Unused React import should be removed
          optimizedSource `shouldNotContain` "useEffect"

        Left err -> expectationFailure $ "Parse failed: " ++ err