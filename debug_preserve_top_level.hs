#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, hspec, containers, text
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Set as Set
import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Pretty.Printer (renderToString)
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Process.TreeShake.Types (defaultTreeShakeOptions)
import qualified Language.JavaScript.Process.TreeShake.Types as Types
import Control.Lens ((.~), (&))

main :: IO ()
main = do
  putStrLn "=== PreserveTopLevel Debug ==="

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
      putStrLn "--- Test with default options (preserveTopLevel=True) ---"
      let opts1 = defaultOptions
      let optimized1 = treeShake opts1 ast
      let optimizedSource1 = renderToString optimized1
      putStrLn optimizedSource1
      putStrLn $ "\nContains 'useEffect': " ++ show ("useEffect" `elem` words optimizedSource1)

      putStrLn "\n--- Test with preserveTopLevel=False ---"
      let opts2 = defaultTreeShakeOptions & Types.preserveTopLevel .~ False
      let optimized2 = treeShake opts2 ast
      let optimizedSource2 = renderToString optimized2
      putStrLn optimizedSource2
      putStrLn $ "\nContains 'useEffect': " ++ show ("useEffect" `elem` words optimizedSource2)

    Left err -> putStrLn $ "Parse error: " ++ err
