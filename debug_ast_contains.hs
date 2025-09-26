#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text, lens
-}

import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Parser.AST (JSAST)
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Pretty.Printer (renderToString)
import qualified Data.Text as Text
import Control.Lens ((.~), (&))

-- Import the test helper (need to implement this since it's not exported)
astContainsIdentifier :: JSAST -> Text.Text -> Bool
astContainsIdentifier ast identifier = identifier `Text.isInfixOf` (Text.pack $ renderToString ast)

main :: IO ()
main = do
  putStrLn "=== AST Contains Debug ==="

  let source = "var maybeUsed = 1; eval('console.log(maybeUsed)');"
  putStrLn $ "Source: " ++ source

  case parse source "test" of
    Right ast -> do
      putStrLn "Parse succeeded!"

      -- Use the exact same options as the test
      let conservativeOpts = defaultOptions { _aggressiveShaking = False }
      putStrLn $ "aggressiveShaking setting: " ++ show (_aggressiveShaking conservativeOpts)

      let conservativeResult = treeShake conservativeOpts ast
      let conservativeSource = renderToString conservativeResult

      putStrLn $ "\nConservative result:\n" ++ conservativeSource

      -- Check if the identifier is found
      let containsMaybeUsed = astContainsIdentifier conservativeResult (Text.pack "maybeUsed")
      putStrLn $ "\nastContainsIdentifier result: " ++ show containsMaybeUsed

      putStrLn $ "\nDirect string search in rendered output:"
      putStrLn $ "  Contains 'maybeUsed': " ++ show ("maybeUsed" `elem` words conservativeSource)
      putStrLn $ "  Rendered source contains 'maybeUsed': " ++ show (Text.pack "maybeUsed" `Text.isInfixOf` Text.pack conservativeSource)

    Left err -> putStrLn $ "Parse error: " ++ err