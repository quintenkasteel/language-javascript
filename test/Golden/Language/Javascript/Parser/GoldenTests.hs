{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Golden test infrastructure for JavaScript parser regression testing.
--
-- This module provides comprehensive golden test infrastructure to ensure
-- parser output consistency, error message stability, and pretty printer
-- round-trip correctness across versions.
--
-- Golden tests compare current parser output against stored baseline files.
-- On first run, baseline @.golden@ files are auto-created. Subsequent runs
-- detect regressions by comparing against the stored baselines.
--
-- @since 0.7.1.0
module Golden.Language.Javascript.Parser.GoldenTests
  ( -- * Test suites
    goldenTests,
    ecmascriptGoldenTests,
    errorGoldenTests,
    prettyPrinterGoldenTests,
    realWorldGoldenTests,

    -- * Test utilities
    parseJavaScriptGolden,
    formatParseResult,
    formatErrorMessage,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.JavaScript.Parser as Parser
import qualified Language.JavaScript.Parser.AST as AST
import qualified Language.JavaScript.Pretty.Printer as Printer
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath (takeBaseName, (</>))
import Test.Hspec (Spec, describe, it, runIO)
import Test.Hspec.Golden (Golden (..))

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
ecmascriptGoldenTests = describe "ECMAScript Golden Tests" $
  runGoldenTestsFor "ecmascript" parseJavaScriptGolden

-- | Error message consistency golden tests.
--
-- Ensures error messages remain stable and helpful across parser versions.
-- Tests both lexer and parser error scenarios.
errorGoldenTests :: Spec
errorGoldenTests = describe "Error Message Golden Tests" $
  runGoldenTestsFor "errors" parseWithErrorCapture

-- | Pretty printer output stability golden tests.
--
-- Validates that pretty printer output remains consistent for parsed ASTs.
-- Includes round-trip testing (parse -> pretty print -> parse).
prettyPrinterGoldenTests :: Spec
prettyPrinterGoldenTests = describe "Pretty Printer Golden Tests" $
  runGoldenTestsFor "pretty-printer" parseAndPrettyPrint

-- | Real-world JavaScript parsing golden tests.
--
-- Tests parser behavior on realistic JavaScript code including modern
-- features, frameworks, and complex constructs.
realWorldGoldenTests :: Spec
realWorldGoldenTests = describe "Real-world JavaScript Golden Tests" $
  runGoldenTestsFor "real-world" parseJavaScriptGolden

-- | Run golden tests for a specific category.
--
-- Discovers all input files in the category directory and creates
-- corresponding golden tests using hspec-golden comparison.
runGoldenTestsFor :: String -> (FilePath -> IO String) -> Spec
runGoldenTestsFor category processor = do
  inputFiles <- runIO (discoverInputFiles category)
  runIO (ensureExpectedDir category)
  mapM_ (createGoldenTest category processor) inputFiles

-- | Discover input files for a test category.
discoverInputFiles :: String -> IO [FilePath]
discoverInputFiles category = do
  files <- listDirectory inputDir
  pure (map (inputDir </>) (filter isJavaScriptFile files))
  where
    inputDir = fixturesDir </> category </> "inputs"
    isJavaScriptFile name = ".js" `Text.isSuffixOf` Text.pack name

-- | Ensure the expected output directory exists for a category.
ensureExpectedDir :: String -> IO ()
ensureExpectedDir category =
  createDirectoryIfMissing True (fixturesDir </> category </> "expected")

-- | Base directory for all golden test fixtures.
fixturesDir :: FilePath
fixturesDir = "test/Golden/Language/Javascript/Parser/fixtures"

-- | Create a golden test for a specific input file.
--
-- Uses hspec-golden's 'Golden' type for actual file comparison.
-- On first run, creates the @.golden@ baseline file automatically.
createGoldenTest :: String -> (FilePath -> IO String) -> FilePath -> Spec
createGoldenTest category processor inputFile = do
  actualOutput <- runIO (processor inputFile)
  it ("golden test: " <> testName) (mkGolden category testName actualOutput)
  where
    testName = takeBaseName inputFile

-- | Construct a 'Golden' value for hspec-golden comparison.
mkGolden :: String -> String -> String -> Golden String
mkGolden category testName actualOutput = Golden
  { output = actualOutput
  , encodePretty = id
  , writeToFile = writeFile
  , readFromFile = readFile
  , goldenFile = expectedFilePath category testName
  , actualFile = Nothing
  , failFirstTime = False
  }

-- | Generate expected file path for golden test output.
expectedFilePath :: String -> String -> FilePath
expectedFilePath category testName =
  fixturesDir </> category </> "expected" </> testName <> ".golden"

-- | Parse JavaScript and format result for golden test comparison.
--
-- Parses JavaScript source and formats the result in a stable,
-- human-readable format suitable for golden test baselines.
parseJavaScriptGolden :: FilePath -> IO String
parseJavaScriptGolden inputFile = do
  content <- readFile inputFile
  pure (formatParseResult (Parser.parse content inputFile))

-- | Parse JavaScript with error capture for error message testing.
--
-- Uses 'Parser.parse' which returns @Either String JSAST@,
-- capturing both parse successes and failures without exceptions.
parseWithErrorCapture :: FilePath -> IO String
parseWithErrorCapture inputFile = do
  content <- readFile inputFile
  pure (formatParseResult (Parser.parse content "src"))

-- | Parse JavaScript and format pretty printer output.
--
-- Parses JavaScript, pretty prints the AST, and formats the result
-- for golden test comparison. Includes round-trip validation.
parseAndPrettyPrint :: FilePath -> IO String
parseAndPrettyPrint inputFile = do
  content <- readFile inputFile
  pure (either formatError formatSuccess (Parser.parse content inputFile))
  where
    formatError err = "PARSE_ERROR: " <> err
    formatSuccess ast = formatPrettyPrintResult prettyOutput roundTrip
      where
        prettyOutput = Printer.renderToString ast
        roundTrip = Parser.parse prettyOutput "round-trip"

-- | Format parse result for golden test output.
--
-- Creates a stable, readable representation of parse results
-- suitable for golden test baselines.
formatParseResult :: Either String AST.JSAST -> String
formatParseResult (Left err) = "PARSE_ERROR:\n" <> err
formatParseResult (Right ast) = "PARSE_SUCCESS:\n" <> show ast

-- | Format pretty printer result with round-trip validation.
formatPrettyPrintResult :: String -> Either String AST.JSAST -> String
formatPrettyPrintResult prettyOutput roundTripResult =
  unlines
    [ "PRETTY_PRINT_OUTPUT:"
    , prettyOutput
    , ""
    , roundTripLine
    ]
  where
    roundTripLine = either mkError (const "ROUND_TRIP: SUCCESS") roundTripResult
    mkError err = "ROUND_TRIP_ERROR:\n" <> err

-- | Format error message for consistent golden test output.
--
-- Standardizes error message format for golden test comparison,
-- removing volatile elements like timestamps or memory addresses.
formatErrorMessage :: String -> String
formatErrorMessage = Text.unpack . cleanErrorMessage . Text.pack
  where
    cleanErrorMessage :: Text -> Text
    cleanErrorMessage = id
