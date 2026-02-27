#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript
-}

import qualified Language.JavaScript.Parser.Parser as Parser

-- Test the numeric literal improvements
main :: IO ()
main = do
  let numericTests =
        [ "1_000_000",      -- Decimal with separators
          "0x1_BEEF",       -- Hex with separators
          "0b1010_1010",    -- Binary with separators
          "0o777_123",      -- Octal with separators
          "123n",           -- BigInt
          "0x123n",         -- Hex BigInt
          "0b101n",         -- Binary BigInt
          "0o777n"          -- Octal BigInt
        ]

  putStrLn "Testing numeric literal improvements:"
  mapM_ testNumeric numericTests

testNumeric :: String -> IO ()
testNumeric input = do
  case Parser.parse input "test" of
    Right _ -> putStrLn $ "✓ '" ++ input ++ "' - SUCCESS"
    Left err -> putStrLn $ "✗ '" ++ input ++ "' - FAILED: " ++ take 50 err