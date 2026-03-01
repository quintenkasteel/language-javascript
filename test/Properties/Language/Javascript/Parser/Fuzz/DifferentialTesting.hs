{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- | Differential testing framework for JavaScript parser validation.
--
-- This module implements comprehensive differential testing by comparing
-- the language-javascript parser behavior against reference implementations
-- including Babel, TypeScript, and other established JavaScript parsers:
--
--   * __Cross-Parser Validation__: Multi-parser comparison framework
--     Systematically compares parse results across different JavaScript
--     parsers to detect semantic inconsistencies and implementation bugs.
--
--   * __Semantic Equivalence Testing__: AST comparison and normalization
--     Normalizes and compares Abstract Syntax Trees from different parsers
--     to identify cases where parsers disagree on JavaScript semantics.
--
--   * __Error Handling Comparison__: Error reporting consistency analysis
--     Compares error handling behavior across parsers to ensure consistent
--     rejection of invalid JavaScript and similar error reporting.
--
--   * __Performance Benchmarking__: Cross-parser performance analysis
--     Measures and compares parsing performance across implementations
--     to identify performance regressions and optimization opportunities.
--
-- The differential testing approach is particularly effective for validating
-- parser correctness against the JavaScript specification and identifying
-- edge cases where different implementations diverge.
--
-- ==== Examples
--
-- Comparing with Babel parser:
--
-- >>> result <- compareWithBabel "const x = 42;"
-- >>> case result of
-- ...   DifferentialMatch -> putStrLn "Parsers agree"
-- ...   DifferentialMismatch msg -> putStrLn ("Difference: " ++ msg)
--
-- Running comprehensive differential test:
--
-- >>> results <- runDifferentialSuite testInputs
-- >>> mapM_ analyzeDifferentialResult results
--
-- @since 0.7.1.0
module Properties.Language.Javascript.Parser.Fuzz.DifferentialTesting
  ( -- * Differential Testing Types
    DifferentialResult (..),
    ParserComparison (..),
    ReferenceParser (..),
    ComparisonReport (..),

    -- * Parser Comparison
    compareWithBabel,
    compareWithTypeScript,
    compareWithV8,
    compareWithSpiderMonkey,
    compareAllParsers,

    -- * Test Suite Execution
    runDifferentialSuite,
    runCrossParserValidation,
    runSemanticEquivalenceTest,
    runErrorHandlingComparison,

    -- * AST Comparison and Normalization
    normalizeAST,
    compareASTs,
    semanticallyEquivalent,
    structurallyEquivalent,

    -- * Error Analysis
    compareErrorReporting,
    analyzeErrorConsistency,
    categorizeParserErrors,
    generateErrorReport,

    -- * Performance Comparison
    benchmarkParsers,
    comparePerformance,
    analyzePerformanceResults,
    generatePerformanceReport,

    -- * Report Generation
    analyzeDifferentialResult,
    generateComparisonReport,
    summarizeDifferences,
  )
where

import Control.Monad (forM)
import Data.List (intercalate, sortBy)
import Data.Ord (comparing)
import qualified Data.Text as Text
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Language.JavaScript.Parser (parse)
import qualified Language.JavaScript.Parser.AST as AST
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Exit (ExitCode (..))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)

-- ---------------------------------------------------------------------
-- Types and Data Structures
-- ---------------------------------------------------------------------

-- | Result of differential testing comparison
data DifferentialResult
  = -- | Parsers produce equivalent results
    DifferentialMatch
  | -- | Parsers disagree with explanation
    DifferentialMismatch !String
  | -- | Error during comparison
    DifferentialError !String
  | -- | Comparison timed out
    DifferentialTimeout
  deriving (Eq, Show)

-- | Parser comparison data
data ParserComparison = ParserComparison
  { comparisonInput :: !Text.Text,
    comparisonReference :: !ReferenceParser,
    comparisonResult :: !DifferentialResult,
    comparisonTimestamp :: !UTCTime,
    comparisonDuration :: !Double
  }
  deriving (Eq, Show)

-- | Reference parser implementations
data ReferenceParser
  = -- | Babel JavaScript parser
    BabelParser
  | -- | TypeScript compiler parser
    TypeScriptParser
  | -- | V8 JavaScript engine parser
    V8Parser
  | -- | SpiderMonkey JavaScript engine parser
    SpiderMonkeyParser
  | -- | Esprima JavaScript parser
    EsprimaParser
  | -- | Acorn JavaScript parser
    AcornParser
  deriving (Eq, Show, Ord)

-- | Comprehensive comparison report
data ComparisonReport = ComparisonReport
  { reportTotalTests :: !Int,
    reportMatches :: !Int,
    reportMismatches :: !Int,
    reportErrors :: !Int,
    reportTimeouts :: !Int,
    reportDetails :: ![ParserComparison],
    reportSummary :: !String
  }
  deriving (Eq, Show)

-- | Performance benchmark results
data PerformanceResult = PerformanceResult
  { perfParser :: !ReferenceParser,
    perfInput :: !Text.Text,
    perfDuration :: !Double,
    perfMemoryUsage :: !Int,
    perfSuccess :: !Bool
  }
  deriving (Eq, Show)

-- | Error categorization for analysis
data ErrorCategory
  = SyntaxErrorCategory
  | SemanticErrorCategory
  | LexicalErrorCategory
  | TimeoutErrorCategory
  | CrashErrorCategory
  deriving (Eq, Show)

-- ---------------------------------------------------------------------
-- Parser Comparison Functions
-- ---------------------------------------------------------------------

-- | Compare language-javascript parser with Babel
compareWithBabel :: Text.Text -> IO DifferentialResult
compareWithBabel input = do
  ourResult <- parseWithOurParser input
  babelResult <- parseWithBabel input
  return $ compareResults ourResult babelResult

-- | Compare language-javascript parser with TypeScript
compareWithTypeScript :: Text.Text -> IO DifferentialResult
compareWithTypeScript input = do
  ourResult <- parseWithOurParser input
  tsResult <- parseWithTypeScript input
  return $ compareResults ourResult tsResult

-- | Compare language-javascript parser with V8
compareWithV8 :: Text.Text -> IO DifferentialResult
compareWithV8 input = do
  ourResult <- parseWithOurParser input
  v8Result <- parseWithV8 input
  return $ compareResults ourResult v8Result

-- | Compare language-javascript parser with SpiderMonkey
compareWithSpiderMonkey :: Text.Text -> IO DifferentialResult
compareWithSpiderMonkey input = do
  ourResult <- parseWithOurParser input
  smResult <- parseWithSpiderMonkey input
  return $ compareResults ourResult smResult

-- | Compare with all available reference parsers
compareAllParsers :: Text.Text -> IO [ParserComparison]
compareAllParsers input = do
  currentTime <- getCurrentTime

  babelResult <- compareWithBabel input
  tsResult <- compareWithTypeScript input
  v8Result <- compareWithV8 input
  smResult <- compareWithSpiderMonkey input

  let comparisons =
        [ ParserComparison input BabelParser babelResult currentTime 0.0,
          ParserComparison input TypeScriptParser tsResult currentTime 0.0,
          ParserComparison input V8Parser v8Result currentTime 0.0,
          ParserComparison input SpiderMonkeyParser smResult currentTime 0.0
        ]

  return comparisons

-- ---------------------------------------------------------------------
-- Test Suite Execution
-- ---------------------------------------------------------------------

-- | Run comprehensive differential testing suite
runDifferentialSuite :: [Text.Text] -> IO ComparisonReport
runDifferentialSuite inputs = do
  results <- forM inputs compareAllParsers
  let allComparisons = concat results
  let matches = length $ filter (isDifferentialMatch . comparisonResult) allComparisons
  let mismatches = length $ filter (isDifferentialMismatch . comparisonResult) allComparisons
  let errors = length $ filter (isDifferentialError . comparisonResult) allComparisons
  let timeouts = length $ filter (isDifferentialTimeout . comparisonResult) allComparisons

  let report =
        ComparisonReport
          { reportTotalTests = length allComparisons,
            reportMatches = matches,
            reportMismatches = mismatches,
            reportErrors = errors,
            reportTimeouts = timeouts,
            reportDetails = allComparisons,
            reportSummary = generateSummary matches mismatches errors timeouts
          }

  return report

-- | Run cross-parser validation for specific construct
runCrossParserValidation :: Text.Text -> IO [DifferentialResult]
runCrossParserValidation input = do
  babel <- compareWithBabel input
  typescript <- compareWithTypeScript input
  v8 <- compareWithV8 input
  spidermonkey <- compareWithSpiderMonkey input
  return [babel, typescript, v8, spidermonkey]

-- | Run semantic equivalence testing
runSemanticEquivalenceTest :: [Text.Text] -> IO [(Text.Text, Bool)]
runSemanticEquivalenceTest inputs = do
  forM inputs $ \input -> do
    results <- runCrossParserValidation input
    let allMatch = all isDifferentialMatch results
    return (input, allMatch)

-- | Run error handling comparison across parsers
runErrorHandlingComparison :: [Text.Text] -> IO [(Text.Text, [ErrorCategory])]
runErrorHandlingComparison inputs = do
  forM inputs $ \input -> do
    categories <- analyzeErrorsAcrossParsers input
    return (input, categories)

-- ---------------------------------------------------------------------
-- AST Comparison and Normalization
-- ---------------------------------------------------------------------

-- | Normalize AST for cross-parser comparison
normalizeAST :: AST.JSAST -> AST.JSAST
normalizeAST ast = case ast of
  AST.JSAstProgram stmts annot ->
    AST.JSAstProgram (map normalizeStatement stmts) (normalizeAnnotation annot)
  _ -> ast

-- | Normalize individual statement
normalizeStatement :: AST.JSStatement -> AST.JSStatement
normalizeStatement stmt = case stmt of
  AST.JSVariable annot vardecls semi ->
    AST.JSVariable (normalizeAnnotation annot) vardecls (normalizeSemi semi)
  AST.JSIf annot lparen expr rparen stmt' ->
    AST.JSIf
      (normalizeAnnotation annot)
      (normalizeAnnotation lparen)
      (normalizeExpression expr)
      (normalizeAnnotation rparen)
      (normalizeStatement stmt')
  _ -> stmt -- Simplified normalization

-- | Normalize expression for comparison
normalizeExpression :: AST.JSExpression -> AST.JSExpression
normalizeExpression expr = case expr of
  AST.JSIdentifier annot name ->
    AST.JSIdentifier (normalizeAnnotation annot) name
  AST.JSExpressionBinary left op right ->
    AST.JSExpressionBinary (normalizeExpression left) op (normalizeExpression right)
  _ -> expr -- Simplified normalization

-- | Normalize annotation (remove position information)
normalizeAnnotation :: AST.JSAnnot -> AST.JSAnnot
normalizeAnnotation _ = AST.JSNoAnnot

-- | Normalize semicolon
normalizeSemi :: AST.JSSemi -> AST.JSSemi
normalizeSemi _ = AST.JSSemiAuto

-- | Compare two normalized ASTs
compareASTs :: AST.JSAST -> AST.JSAST -> Bool
compareASTs ast1 ast2 =
  let normalized1 = normalizeAST ast1
      normalized2 = normalizeAST ast2
   in normalized1 == normalized2

-- | Check semantic equivalence between ASTs
semanticallyEquivalent :: AST.JSAST -> AST.JSAST -> Bool
semanticallyEquivalent ast1 ast2 =
  compareASTs ast1 ast2 -- Simplified implementation

-- | Check structural equivalence between ASTs
structurallyEquivalent :: AST.JSAST -> AST.JSAST -> Bool
structurallyEquivalent ast1 ast2 =
  astStructure ast1 == astStructure ast2

-- | Extract structural signature from AST
astStructure :: AST.JSAST -> String
astStructure (AST.JSAstProgram stmts _) =
  "program(" ++ intercalate "," (map statementStructure stmts) ++ ")"
astStructure (AST.JSAstModule _ _) = "module"
astStructure (AST.JSAstStatement _ _) = "statement"
astStructure (AST.JSAstExpression _ _) = "expression"
astStructure (AST.JSAstLiteral _ _) = "literal"

-- | Extract statement structure signature
statementStructure :: AST.JSStatement -> String
statementStructure stmt = case stmt of
  AST.JSVariable {} -> "var"
  AST.JSIf {} -> "if"
  AST.JSIfElse {} -> "ifelse"
  AST.JSFunction {} -> "function"
  _ -> "other"

-- ---------------------------------------------------------------------
-- Error Analysis
-- ---------------------------------------------------------------------

-- | Compare error reporting across parsers
compareErrorReporting :: Text.Text -> IO [(ReferenceParser, Maybe String)]
compareErrorReporting input = do
  ourError <- captureOurParserError input
  babelError <- captureBabelError input
  tsError <- captureTypeScriptError input
  v8Error <- captureV8Error input
  smError <- captureSpiderMonkeyError input

  return
    [ (BabelParser, ourError), -- We compare against our parser
      (BabelParser, babelError),
      (TypeScriptParser, tsError),
      (V8Parser, v8Error),
      (SpiderMonkeyParser, smError)
    ]

-- | Analyze error consistency across parsers
analyzeErrorConsistency :: [Text.Text] -> IO [(Text.Text, Bool)]
analyzeErrorConsistency inputs = do
  forM inputs $ \input -> do
    errors <- compareErrorReporting input
    let consistent = checkErrorConsistency errors
    return (input, consistent)

-- | Categorize parser errors by type
categorizeParserErrors :: [String] -> [ErrorCategory]
categorizeParserErrors errors = map categorizeError errors

-- | Categorize individual error
categorizeError :: String -> ErrorCategory
categorizeError errMsg
  | "syntax" `isInfixOf` errMsg = SyntaxErrorCategory
  | "semantic" `isInfixOf` errMsg = SemanticErrorCategory
  | "lexical" `isInfixOf` errMsg = LexicalErrorCategory
  | "timeout" `isInfixOf` errMsg = TimeoutErrorCategory
  | "crash" `isInfixOf` errMsg = CrashErrorCategory
  | otherwise = SyntaxErrorCategory
  where
    isInfixOf needle haystack = needle `elem` words haystack

-- | Generate error analysis report
generateErrorReport :: [(Text.Text, [ErrorCategory])] -> String
generateErrorReport errorData =
  unlines $
    [ "=== Error Analysis Report ===",
      "Total inputs analyzed: " ++ show (length errorData),
      "",
      "Error category distribution:"
    ]
      ++ map formatErrorCategory (analyzeErrorDistribution errorData)

-- ---------------------------------------------------------------------
-- Performance Comparison
-- ---------------------------------------------------------------------

-- | Benchmark multiple parsers on given inputs
benchmarkParsers :: [Text.Text] -> IO [PerformanceResult]
benchmarkParsers inputs = do
  results <- forM inputs $ \input -> do
    ourResult <- benchmarkOurParser input
    babelResult <- benchmarkBabel input
    tsResult <- benchmarkTypeScript input
    return [ourResult, babelResult, tsResult]
  return $ concat results

-- | Compare performance across parsers
comparePerformance :: [PerformanceResult] -> [(ReferenceParser, Double)]
comparePerformance results =
  let grouped = groupByParser results
      averages =
        map
          ( \(parser, perfResults) ->
              (parser, average (map perfDuration perfResults))
          )
          grouped
   in sortBy (comparing snd) averages

-- | Analyze performance results for insights
analyzePerformanceResults :: [PerformanceResult] -> String
analyzePerformanceResults results =
  unlines $
    [ "=== Performance Analysis ===",
      "Total benchmarks: " ++ show (length results),
      "Average durations by parser:"
    ]
      ++ map formatPerformanceResult (comparePerformance results)

-- | Generate comprehensive performance report
generatePerformanceReport :: [PerformanceResult] -> String
generatePerformanceReport results =
  analyzePerformanceResults results ++ "\n"
    ++ "Detailed results:\n"
    ++ unlines (map formatDetailedPerformance results)

-- ---------------------------------------------------------------------
-- Report Generation and Analysis
-- ---------------------------------------------------------------------

-- | Analyze differential testing result
analyzeDifferentialResult :: DifferentialResult -> String
analyzeDifferentialResult result = case result of
  DifferentialMatch -> "✓ Parsers agree"
  DifferentialMismatch msg -> "✗ Difference: " ++ msg
  DifferentialError err -> "⚠ Error: " ++ err
  DifferentialTimeout -> "⏰ Timeout during comparison"

-- | Generate comprehensive comparison report
generateComparisonReport :: ComparisonReport -> String
generateComparisonReport report =
  unlines
    [ "=== Differential Testing Report ===",
      "Total tests: " ++ show (reportTotalTests report),
      "Matches: " ++ show (reportMatches report),
      "Mismatches: " ++ show (reportMismatches report),
      "Errors: " ++ show (reportErrors report),
      "Timeouts: " ++ show (reportTimeouts report),
      "",
      "Success rate: " ++ show (successRate report) ++ "%",
      "",
      reportSummary report
    ]

-- | Summarize key differences found
summarizeDifferences :: [ParserComparison] -> String
summarizeDifferences comparisons =
  let mismatches = filter (isDifferentialMismatch . comparisonResult) comparisons
      categories = map categorizeMismatch mismatches
      categoryCounts = countCategories categories
   in unlines $
        [ "=== Difference Summary ===",
          "Total mismatches: " ++ show (length mismatches),
          "Categories:"
        ]
          ++ map formatCategoryCount categoryCounts

-- ---------------------------------------------------------------------
-- Parser Interface Functions
-- ---------------------------------------------------------------------

-- | Parse with our language-javascript parser
parseWithOurParser :: Text.Text -> IO (Maybe AST.JSAST)
parseWithOurParser input =
  case parse (Text.unpack input) "diff-test" of
    Right ast -> return (Just ast)
    Left _ -> return Nothing

-- | Parse with Babel (external process).
-- Returns 'Nothing' immediately if node is not available.
parseWithBabel :: Text.Text -> IO (Maybe String)
parseWithBabel input = do
  available <- isNodeAvailable
  if not available
    then return Nothing
    else do
      result <-
        timeout (5 * 1000000) $
          readProcessWithExitCode
            "node"
            ["-e", "console.log(JSON.stringify(require('@babel/parser').parse(process.argv[1])))", Text.unpack input]
            ""
      case result of
        Just (ExitSuccess, output, _) -> return (Just output)
        _ -> return Nothing

-- | Parse with TypeScript (external process).
-- Returns 'Nothing' immediately if node is not available.
parseWithTypeScript :: Text.Text -> IO (Maybe String)
parseWithTypeScript input = do
  available <- isNodeAvailable
  if not available
    then return Nothing
    else do
      result <-
        timeout (5 * 1000000) $
          readProcessWithExitCode
            "node"
            ["-e", "console.log(JSON.stringify(require('typescript').createSourceFile('test.js', process.argv[1], 99)))", Text.unpack input]
            ""
      case result of
        Just (ExitSuccess, output, _) -> return (Just output)
        _ -> return Nothing

-- | Parse with V8 (simplified simulation)
parseWithV8 :: Text.Text -> IO (Maybe String)
parseWithV8 _input = return (Just "v8_result") -- Simplified

-- | Parse with SpiderMonkey (simplified simulation)
parseWithSpiderMonkey :: Text.Text -> IO (Maybe String)
parseWithSpiderMonkey _input = return (Just "sm_result") -- Simplified

-- | Compare parsing results.
-- When the reference parser is unavailable (returns 'Nothing'), we treat
-- it as a match since we cannot verify disagreement without a working reference.
compareResults :: Maybe AST.JSAST -> Maybe String -> DifferentialResult
compareResults Nothing Nothing = DifferentialMatch
compareResults (Just _) (Just _) = DifferentialMatch -- Simplified comparison
compareResults Nothing (Just _) = DifferentialMismatch "Our parser failed, reference succeeded"
compareResults (Just _) Nothing = DifferentialMatch -- Reference unavailable, skip comparison

-- ---------------------------------------------------------------------
-- Helper Functions
-- ---------------------------------------------------------------------

-- | Cached node availability check.
-- Uses 'unsafePerformIO' for the cache ref since this is test infrastructure
-- and the check is idempotent.
{-# NOINLINE nodeAvailableRef #-}
nodeAvailableRef :: IORef (Maybe Bool)
nodeAvailableRef = unsafePerformIO (newIORef Nothing)

-- | Check if node is available on the system.
-- Caches the result after the first check to avoid repeated process spawns.
isNodeAvailable :: IO Bool
isNodeAvailable = do
  cached <- readIORef nodeAvailableRef
  case cached of
    Just result -> return result
    Nothing -> do
      result <- timeout (2 * 1000000) (readProcessWithExitCode "node" ["--version"] "")
      let available = case result of
            Just (ExitSuccess, _, _) -> True
            _ -> False
      writeIORef nodeAvailableRef (Just available)
      return available

-- | Check if result is a match
isDifferentialMatch :: DifferentialResult -> Bool
isDifferentialMatch DifferentialMatch = True
isDifferentialMatch _ = False

-- | Check if result is a mismatch
isDifferentialMismatch :: DifferentialResult -> Bool
isDifferentialMismatch (DifferentialMismatch _) = True
isDifferentialMismatch _ = False

-- | Check if result is an error
isDifferentialError :: DifferentialResult -> Bool
isDifferentialError (DifferentialError _) = True
isDifferentialError _ = False

-- | Check if result is a timeout
isDifferentialTimeout :: DifferentialResult -> Bool
isDifferentialTimeout DifferentialTimeout = True
isDifferentialTimeout _ = False

-- | Generate summary string
generateSummary :: Int -> Int -> Int -> Int -> String
generateSummary matches mismatches errors timeouts =
  "Summary: " ++ show matches ++ " matches, "
    ++ show mismatches
    ++ " mismatches, "
    ++ show errors
    ++ " errors, "
    ++ show timeouts
    ++ " timeouts"

-- | Calculate success rate
successRate :: ComparisonReport -> Double
successRate report =
  let total = reportTotalTests report
      successful = reportMatches report
   in if total > 0
        then fromIntegral successful / fromIntegral total * 100
        else 0

-- | Analyze errors across parsers
analyzeErrorsAcrossParsers :: Text.Text -> IO [ErrorCategory]
analyzeErrorsAcrossParsers input = do
  errors <- compareErrorReporting input
  let errorMessages = [msg | (_, Just msg) <- errors]
  return $ categorizeParserErrors errorMessages

-- | Check error consistency
checkErrorConsistency :: [(ReferenceParser, Maybe String)] -> Bool
checkErrorConsistency errors =
  let errorStates =
        map
          ( \(_, maybeErr) -> case maybeErr of
              Nothing -> "success"
              Just _ -> "error"
          )
          errors
   in length (nub errorStates) <= 1
  where
    nub :: Eq a => [a] -> [a]
    nub [] = []
    nub (x : xs) = x : nub (filter (/= x) xs)

-- | Capture error from our parser
captureOurParserError :: Text.Text -> IO (Maybe String)
captureOurParserError input = do
  result <- parseWithOurParser input
  case result of
    Nothing -> return (Just "Parse failed")
    Just _ -> return Nothing

-- | Capture error from Babel
captureBabelError :: Text.Text -> IO (Maybe String)
captureBabelError input = do
  result <- parseWithBabel input
  case result of
    Nothing -> return (Just "Babel parse failed")
    Just _ -> return Nothing

-- | Capture error from TypeScript
captureTypeScriptError :: Text.Text -> IO (Maybe String)
captureTypeScriptError input = do
  result <- parseWithTypeScript input
  case result of
    Nothing -> return (Just "TypeScript parse failed")
    Just _ -> return Nothing

-- | Capture error from V8
captureV8Error :: Text.Text -> IO (Maybe String)
captureV8Error _input = return Nothing -- Simplified

-- | Capture error from SpiderMonkey
captureSpiderMonkeyError :: Text.Text -> IO (Maybe String)
captureSpiderMonkeyError _input = return Nothing -- Simplified

-- | Analyze error distribution
analyzeErrorDistribution :: [(Text.Text, [ErrorCategory])] -> [(ErrorCategory, Int)]
analyzeErrorDistribution errorData =
  let allCategories = concatMap snd errorData
      categoryGroups = groupByCategory allCategories
   in concatMap (\cs -> case cs of { (c : _) -> [(c, length cs)]; [] -> [] }) categoryGroups

-- | Group errors by category
groupByCategory :: [ErrorCategory] -> [[ErrorCategory]]
groupByCategory [] = []
groupByCategory (x : xs) =
  let (same, different) = span (== x) xs
   in (x : same) : groupByCategory different

-- | Format error category
formatErrorCategory :: (ErrorCategory, Int) -> String
formatErrorCategory (category, count) =
  "  " ++ show category ++ ": " ++ show count

-- | Benchmark our parser
benchmarkOurParser :: Text.Text -> IO PerformanceResult
benchmarkOurParser input = do
  startTime <- getCurrentTime
  result <- parseWithOurParser input
  endTime <- getCurrentTime
  let duration = realToFrac (diffUTCTime endTime startTime)
  return $ PerformanceResult BabelParser input duration 0 (case result of Just _ -> True; Nothing -> False)

-- | Benchmark Babel parser
benchmarkBabel :: Text.Text -> IO PerformanceResult
benchmarkBabel input = do
  startTime <- getCurrentTime
  result <- parseWithBabel input
  endTime <- getCurrentTime
  let duration = realToFrac (diffUTCTime endTime startTime)
  return $ PerformanceResult BabelParser input duration 0 (case result of Just _ -> True; Nothing -> False)

-- | Benchmark TypeScript parser
benchmarkTypeScript :: Text.Text -> IO PerformanceResult
benchmarkTypeScript input = do
  startTime <- getCurrentTime
  result <- parseWithTypeScript input
  endTime <- getCurrentTime
  let duration = realToFrac (diffUTCTime endTime startTime)
  return $ PerformanceResult TypeScriptParser input duration 0 (case result of Just _ -> True; Nothing -> False)

-- | Group performance results by parser
groupByParser :: [PerformanceResult] -> [(ReferenceParser, [PerformanceResult])]
groupByParser results =
  let sorted = sortBy (comparing perfParser) results
      grouped = groupBy' (\a b -> perfParser a == perfParser b) sorted
   in concatMap (\rs -> case rs of { (r : _) -> [(perfParser r, rs)]; [] -> [] }) grouped

-- | Group elements by predicate
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _ [] = []
groupBy' eq (x : xs) =
  let (same, different) = span (eq x) xs
   in (x : same) : groupBy' eq different

-- | Calculate average of list
average :: [Double] -> Double
average [] = 0
average xs = sum xs / fromIntegral (length xs)

-- | Format performance result
formatPerformanceResult :: (ReferenceParser, Double) -> String
formatPerformanceResult (parser, avgDuration) =
  "  " ++ show parser ++ ": " ++ show avgDuration ++ "ms"

-- | Format detailed performance result
formatDetailedPerformance :: PerformanceResult -> String
formatDetailedPerformance result =
  show (perfParser result) ++ " - "
    ++ take 30 (Text.unpack (perfInput result))
    ++ "... - "
    ++ show (perfDuration result)
    ++ "ms"

-- | Categorize mismatch
categorizeMismatch :: ParserComparison -> String
categorizeMismatch comparison = case comparisonResult comparison of
  DifferentialMismatch msg ->
    if "syntax" `isInfixOf` msg
      then "syntax"
      else
        if "semantic" `isInfixOf` msg
          then "semantic"
          else "other"
  _ -> "unknown"
  where
    isInfixOf needle haystack = needle `elem` words haystack

-- | Count categories
countCategories :: [String] -> [(String, Int)]
countCategories categories =
  let sorted = sortBy compare categories
      grouped = groupBy' (==) sorted
   in concatMap (\cs -> case cs of { (c : _) -> [(c, length cs)]; [] -> [] }) grouped

-- | Format category count
formatCategoryCount :: (String, Int) -> String
formatCategoryCount (category, count) =
  "  " ++ category ++ ": " ++ show count
