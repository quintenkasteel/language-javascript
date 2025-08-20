{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Golden test infrastructure for JavaScript parser regression testing.
--
-- This module provides comprehensive golden test infrastructure to ensure
-- parser output consistency, error message stability, and pretty printer
-- round-trip correctness across versions.
--
-- Golden tests help prevent regressions by comparing current parser output
-- against stored baseline outputs. When changes are intentional, baselines
-- can be updated easily.
--
-- @since 0.7.1.0
module Test.Language.Javascript.GoldenTest
  ( -- * Test suites
    goldenTests
  , ecmascriptGoldenTests
  , errorGoldenTests
  , prettyPrinterGoldenTests
  , realWorldGoldenTests
  -- * Test utilities
  , parseJavaScriptGolden
  , formatParseResult
  , formatErrorMessage
  ) where

import Test.Hspec
import Test.Hspec.Golden
import Control.Exception (try, SomeException)
import System.FilePath (takeBaseName, (</>))
import System.Directory (listDirectory)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import qualified Language.JavaScript.Parser as Parser
import qualified Language.JavaScript.Parser.AST as AST
import qualified Language.JavaScript.Pretty.Printer as Printer

-- | Main golden test suite combining all categories.
goldenTests :: Spec
goldenTests = describe "Golden Tests" $ do
  ecmascriptGoldenTests
  errorGoldenTests
  prettyPrinterGoldenTests
  realWorldGoldenTests

-- | ECMAScript specification example golden tests.
--
-- Tests parser output consistency for standard JavaScript constructs
-- defined in the ECMAScript specification.
ecmascriptGoldenTests :: Spec
ecmascriptGoldenTests = describe "ECMAScript Golden Tests" $ do
  runGoldenTestsFor "ecmascript" parseJavaScriptGolden

-- | Error message consistency golden tests.
--
-- Ensures error messages remain stable and helpful across parser versions.
-- Tests both lexer and parser error scenarios.
errorGoldenTests :: Spec
errorGoldenTests = describe "Error Message Golden Tests" $ do
  runGoldenTestsFor "errors" parseWithErrorCapture

-- | Pretty printer output stability golden tests.
--
-- Validates that pretty printer output remains consistent for parsed ASTs.
-- Includes round-trip testing (parse -> pretty print -> parse).
prettyPrinterGoldenTests :: Spec
prettyPrinterGoldenTests = describe "Pretty Printer Golden Tests" $ do
  runGoldenTestsFor "pretty-printer" parseAndPrettyPrint

-- | Real-world JavaScript parsing golden tests.
--
-- Tests parser behavior on realistic JavaScript code including modern
-- features, frameworks, and complex constructs.
realWorldGoldenTests :: Spec
realWorldGoldenTests = describe "Real-world JavaScript Golden Tests" $ do
  runGoldenTestsFor "real-world" parseJavaScriptGolden

-- | Run golden tests for a specific category.
--
-- Discovers all input files in the category directory and creates
-- corresponding golden tests.
runGoldenTestsFor :: String -> (FilePath -> IO String) -> Spec
runGoldenTestsFor category processor = do
  inputFiles <- runIO (discoverInputFiles category)
  mapM_ (createGoldenTest category processor) inputFiles

-- | Discover input files for a test category.
discoverInputFiles :: String -> IO [FilePath]
discoverInputFiles category = do
  let inputDir = "test/golden" </> category </> "inputs"
  files <- listDirectory inputDir
  pure $ map (inputDir </>) $ filter isJavaScriptFile files
  where
    isJavaScriptFile name = 
      ".js" `Text.isSuffixOf` Text.pack name

-- | Create a golden test for a specific input file.
createGoldenTest :: String -> (FilePath -> IO String) -> FilePath -> Spec
createGoldenTest category processor inputFile = do
  let testName = takeBaseName inputFile
  let expectedFile = expectedFilePath category testName
  it ("golden test: " ++ testName) $
    defaultGolden expectedFile (processor inputFile)

-- | Generate expected file path for golden test output.
expectedFilePath :: String -> String -> FilePath
expectedFilePath category testName =
  "test/golden" </> category </> "expected" </> testName ++ ".golden"

-- | Parse JavaScript and format result for golden test comparison.
--
-- Parses JavaScript source and formats the result in a stable,
-- human-readable format suitable for golden test baselines.
parseJavaScriptGolden :: FilePath -> IO String
parseJavaScriptGolden inputFile = do
  content <- readFile inputFile
  pure $ formatParseResult $ Parser.parse content inputFile

-- | Parse JavaScript with error capture for error message testing.
--
-- Attempts to parse JavaScript and captures both successful parses
-- and error messages in a consistent format.
parseWithErrorCapture :: FilePath -> IO String
parseWithErrorCapture inputFile = do
  content <- readFile inputFile
  result <- try (evaluate $ Parser.parse content inputFile)
  case result of
    Left (e :: SomeException) -> 
      pure $ "EXCEPTION: " ++ show e
    Right parseResult -> 
      pure $ formatParseResult parseResult
  where
    evaluate (Left err) = error err
    evaluate (Right ast) = pure ast

-- | Parse JavaScript and format pretty printer output.
--
-- Parses JavaScript, pretty prints the AST, and formats the result
-- for golden test comparison. Includes round-trip validation.
parseAndPrettyPrint :: FilePath -> IO String
parseAndPrettyPrint inputFile = do
  content <- readFile inputFile
  case Parser.parse content inputFile of
    Left err -> pure $ "PARSE_ERROR: " ++ err
    Right ast -> do
      let prettyOutput = Printer.renderToString ast
      let roundTripResult = Parser.parse prettyOutput "round-trip"
      pure $ formatPrettyPrintResult prettyOutput roundTripResult

-- | Format parse result for golden test output.
--
-- Creates a stable, readable representation of parse results
-- suitable for golden test baselines.
formatParseResult :: Either String AST.JSAST -> String
formatParseResult (Left err) = "PARSE_ERROR:\n" ++ err
formatParseResult (Right ast) = "PARSE_SUCCESS:\n" ++ show ast

-- | Format pretty printer result with round-trip validation.
formatPrettyPrintResult :: String -> Either String AST.JSAST -> String
formatPrettyPrintResult prettyOutput (Left roundTripError) =
  unlines 
    [ "PRETTY_PRINT_OUTPUT:"
    , prettyOutput
    , ""
    , "ROUND_TRIP_ERROR:"
    , roundTripError
    ]
formatPrettyPrintResult prettyOutput (Right _) =
  unlines
    [ "PRETTY_PRINT_OUTPUT:"
    , prettyOutput
    , ""
    , "ROUND_TRIP: SUCCESS"
    ]

-- | Format error message for consistent golden test output.
--
-- Standardizes error message format for golden test comparison,
-- removing volatile elements like timestamps or memory addresses.
formatErrorMessage :: String -> String
formatErrorMessage = Text.unpack . cleanErrorMessage . Text.pack
  where
    cleanErrorMessage = id  -- For now, use as-is; can add cleaning later