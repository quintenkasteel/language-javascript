{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Criterion benchmarks for the language-javascript parser.
--
-- Measures parse, print, and minify performance against real JavaScript
-- fixtures of varying sizes and complexity. Run with:
--
-- @
-- cabal bench
-- @
--
-- @since 0.8.0.0
module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Criterion.Main
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Language.JavaScript.Parser.AST (JSAST)
import qualified Language.JavaScript.Parser.Parser as Parser
import qualified Language.JavaScript.Pretty.Printer as Printer
import qualified Language.JavaScript.Process.Minify as Minify

main :: IO ()
main = do
  fixtures <- loadFixtures
  defaultMain (buildBenchmarks fixtures)

-- | All loaded fixture data, pre-read to avoid I/O during benchmarks.
data Fixtures = Fixtures
  { fixtureSmall :: !ByteString,
    fixtureMedium :: !ByteString,
    fixtureLarge :: !ByteString,
    fixtureES6 :: !ByteString,
    fixtureES2023 :: !ByteString,
    fixtureReact :: !ByteString,
    fixtureExpress :: !ByteString,
    fixtureSynthLarge :: !ByteString
  }

loadFixtures :: IO Fixtures
loadFixtures = do
  small <- BS.readFile "test/fixtures/simple-es5.js"
  medium <- BS.readFile "test/fixtures/large-sample.js"
  large <- BS.readFile "test/fixtures/throughput-sample.js"
  es6 <- BS.readFile "test/fixtures/es6-module-sample.js"
  es2023 <- BS.readFile "test/fixtures/es2023-features.js"
  react <- BS.readFile "test/fixtures/react-sample.js"
  express <- BS.readFile "test/fixtures/express-sample.js"
  synthLarge <- evaluate (force (generateLargeJS 500))
  return
    Fixtures
      { fixtureSmall = small,
        fixtureMedium = medium,
        fixtureLarge = large,
        fixtureES6 = es6,
        fixtureES2023 = es2023,
        fixtureReact = react,
        fixtureExpress = express,
        fixtureSynthLarge = synthLarge
      }

-- | Generate a large synthetic JavaScript file with many declarations.
generateLargeJS :: Int -> ByteString
generateLargeJS n = BS8.unlines (map generateFunction [1 .. n])
  where
    generateFunction :: Int -> ByteString
    generateFunction i =
      BS8.pack $
        "function func_" ++ show i ++ "(a, b) { "
          ++ "var result = a + b * " ++ show i ++ "; "
          ++ "if (result > 0) { return result; } "
          ++ "else { return -result; } "
          ++ "}"

buildBenchmarks :: Fixtures -> [Benchmark]
buildBenchmarks fixtures =
  [ parseBenchmarks fixtures,
    printBenchmarks fixtures,
    minifyBenchmarks fixtures,
    roundTripBenchmarks fixtures,
    scalingBenchmarks
  ]

-- | Parsing benchmarks across fixture sizes and JS feature sets.
parseBenchmarks :: Fixtures -> Benchmark
parseBenchmarks fixtures =
  bgroup
    "parse"
    [ bgroup
        "by-size"
        [ bench "small (5KB ES5)" $ nf parseFixture (fixtureSmall fixtures),
          bench "medium (11KB mixed)" $ nf parseFixture (fixtureMedium fixtures),
          bench "react (1KB)" $ nf parseFixture (fixtureReact fixtures),
          bench "express (1.5KB)" $ nf parseFixture (fixtureExpress fixtures),
          bench "synthetic (large)" $ nf parseFixture (fixtureSynthLarge fixtures)
        ],
      bgroup
        "by-feature"
        [ bench "ES6 modules" $ nf parseFixture (fixtureES6 fixtures),
          bench "ES2023 features" $ nf parseFixture (fixtureES2023 fixtures)
        ]
    ]

-- | Pretty-printing benchmarks: parse then render.
printBenchmarks :: Fixtures -> Benchmark
printBenchmarks fixtures =
  bgroup
    "print"
    [ bench "small (5KB)" $ nfIO (parseThenPrint (fixtureSmall fixtures)),
      bench "medium (11KB)" $ nfIO (parseThenPrint (fixtureMedium fixtures)),
      bench "synthetic (large)" $ nfIO (parseThenPrint (fixtureSynthLarge fixtures))
    ]

-- | Minification benchmarks: parse then minify then render.
minifyBenchmarks :: Fixtures -> Benchmark
minifyBenchmarks fixtures =
  bgroup
    "minify"
    [ bench "small (5KB)" $ nfIO (parseThenMinify (fixtureSmall fixtures)),
      bench "medium (11KB)" $ nfIO (parseThenMinify (fixtureMedium fixtures)),
      bench "synthetic (large)" $ nfIO (parseThenMinify (fixtureSynthLarge fixtures))
    ]

-- | Round-trip benchmarks: parse, render, re-parse.
roundTripBenchmarks :: Fixtures -> Benchmark
roundTripBenchmarks fixtures =
  bgroup
    "round-trip"
    [ bench "small (5KB)" $ nfIO (parseRoundTrip (fixtureSmall fixtures)),
      bench "medium (11KB)" $ nfIO (parseRoundTrip (fixtureMedium fixtures))
    ]

-- | Scaling benchmarks: parse increasingly large synthetic inputs.
scalingBenchmarks :: Benchmark
scalingBenchmarks =
  bgroup
    "scaling"
    [ env (evaluate (force (generateLargeJS n))) (bench label . nf parseFixture)
      | (label, n) <- scalingSizes
    ]
  where
    scalingSizes :: [(String, Int)]
    scalingSizes =
      [ ("50 functions", 50),
        ("100 functions", 100),
        ("250 functions", 250),
        ("500 functions", 500)
      ]

-- | Parse a ByteString fixture, returning the result.
parseFixture :: ByteString -> Either String JSAST
parseFixture = Parser.parseBS

-- | Parse then pretty-print a fixture.
parseThenPrint :: ByteString -> IO String
parseThenPrint bs = case Parser.parseBS bs of
  Left err -> return err
  Right ast -> evaluate (force (Printer.renderToString ast))

-- | Parse, minify, then render a fixture.
parseThenMinify :: ByteString -> IO String
parseThenMinify bs = case Parser.parseBS bs of
  Left err -> return err
  Right ast -> evaluate (force (Printer.renderToString (Minify.minifyJS ast)))

-- | Parse, render, then re-parse a fixture.
parseRoundTrip :: ByteString -> IO (Either String JSAST)
parseRoundTrip bs = case Parser.parseBS bs of
  Left err -> return (Left err)
  Right ast -> evaluate (force (Parser.parse (Printer.renderToString ast) "bench"))
