#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text, lens
-}

import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Parser.AST (JSAST(..), JSStatement(..), JSExpression(..))
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Process.TreeShake.Types
import Language.JavaScript.Pretty.Printer
import Control.Lens ((^.), (.~), (&))
import qualified Data.Text as Text

main :: IO ()
main = do
  putStrLn "=== AST Search Debug ==="

  let source = "var topLevel = 1; console.log('used');"
  putStrLn $ "Source: " ++ source

  case parse source "test" of
    Right ast -> do
      putStrLn "\n=== BEFORE tree shaking ==="
      let beforeText = renderToString ast
      putStrLn $ "Rendered: " ++ beforeText
      putStrLn $ "Contains 'topLevel' (text search): " ++ show (Text.pack "topLevel" `Text.isInfixOf` Text.pack beforeText)

      -- Test with preserveTopLevel = True
      let opts = defaultOptions & preserveTopLevel .~ True
      let optimized = treeShake opts ast

      putStrLn "\n=== AFTER tree shaking (preserveTopLevel = True) ==="
      let afterText = renderToString optimized
      putStrLn $ "Rendered: " ++ afterText
      putStrLn $ "Contains 'topLevel' (text search): " ++ show (Text.pack "topLevel" `Text.isInfixOf` Text.pack afterText)

      putStrLn "\n=== AST Structure Analysis ==="
      putStrLn "Original AST statements:"
      case ast of
        JSAstProgram statements _ -> mapM_ (\(i, stmt) -> putStrLn $ "  " ++ show i ++ ": " ++ show stmt) (zip [1..] statements)
        _ -> putStrLn "Not a program AST"

      putStrLn "\nOptimized AST statements:"
      case optimized of
        JSAstProgram statements _ -> mapM_ (\(i, stmt) -> putStrLn $ "  " ++ show i ++ ": " ++ show stmt) (zip [1..] statements)
        _ -> putStrLn "Not a program AST"

    Left err -> putStrLn $ "Parse error: " ++ err