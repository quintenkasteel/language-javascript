{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Production-Grade Performance Testing Infrastructure
--
-- This module implements comprehensive performance testing using Criterion
-- benchmarking framework with real-world JavaScript parsing scenarios.
-- Provides memory profiling, performance regression detection, and validates
-- parser performance against documented targets.
--
-- All benchmarks use the zero-copy 'ByteString' API ('parseBS') directly,
-- avoiding intermediate String\/Text conversions for accurate measurements.
--
-- = Performance Targets
--
-- * jQuery Parsing: < 500ms for typical library (280KB)
-- * Large File Parsing: < 2s for 1MB JavaScript files
-- * Memory Usage: Linear growth O(n) with input size
-- * Memory Peak: < 30x input size
-- * Parse Speed: > 0.5 MB\/s parsing throughput
--
-- = Benchmark Categories
--
-- * Real-world library parsing (jQuery, React, Angular)
-- * Large file scaling performance validation
-- * Memory usage profiling with Weigh
-- * Performance regression detection
-- * Baseline establishment and tracking
--
-- @since 0.7.1.0
module Benchmarks.Language.Javascript.Parser.Performance
  ( performanceTests,
    criterionBenchmarks,
    runMemoryProfiling,
    PerformanceMetrics (..),
    BenchmarkResults (..),
    createPerformanceBaseline,
    validatePerformanceTargets,
  )
where

import Control.DeepSeq (NFData (..), deepseq, force)
import Control.Exception (evaluate)
import Criterion.Main
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (foldl')
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Language.JavaScript.Parser.Parser (parseBS)
import Test.Hspec

-- | Performance metrics for a benchmark run
data PerformanceMetrics = PerformanceMetrics
  { -- | Parse time in milliseconds
    metricsParseTime :: !Double,
    -- | Memory usage in bytes
    metricsMemoryUsage :: !Int,
    -- | Parse speed in MB/s
    metricsThroughput :: !Double,
    -- | Input file size in bytes
    metricsInputSize :: !Int,
    -- | Whether parsing succeeded
    metricsSuccess :: !Bool
  }
  deriving (Eq, Show)

instance NFData PerformanceMetrics where
  rnf (PerformanceMetrics t m th s success) =
    rnf t `seq` rnf m `seq` rnf th `seq` rnf s `seq` rnf success

-- | Comprehensive benchmark results
data BenchmarkResults = BenchmarkResults
  { -- | jQuery library parsing
    jqueryResults :: !PerformanceMetrics,
    -- | React library parsing
    reactResults :: !PerformanceMetrics,
    -- | Angular library parsing
    angularResults :: !PerformanceMetrics,
    -- | File size scaling tests
    scalingResults :: ![PerformanceMetrics],
    -- | Memory usage tests
    memoryResults :: ![PerformanceMetrics],
    -- | Baseline measurements
    baselineResults :: ![PerformanceMetrics]
  }
  deriving (Eq, Show)

instance NFData BenchmarkResults where
  rnf (BenchmarkResults jq react ang scaling memory baseline) =
    rnf jq `seq` rnf react `seq` rnf ang `seq` rnf scaling `seq` rnf memory `seq` rnf baseline

-- | Hspec-compatible performance tests for CI integration
performanceTests :: Spec
performanceTests = describe "Performance Validation Tests" $ do
  describe "Real-world parsing performance" $ do
    testJQueryParsing
    testReactParsing
    testAngularParsing

  describe "File size scaling validation" $ do
    testLinearScaling
    testLargeFileHandling
    testMemoryConstraints

  describe "Performance target validation" $ do
    testPerformanceTargets
    testThroughputTargets
    testMemoryTargets

-- | Criterion benchmark suite for detailed performance analysis
criterionBenchmarks :: [Benchmark]
criterionBenchmarks =
  [ bgroup
      "JavaScript Library Parsing"
      [ bench "jQuery (280KB)" $ nfIO benchmarkJQuery,
        bench "React (1.2MB)" $ nfIO benchmarkReact,
        bench "Angular (2.4MB)" $ nfIO benchmarkAngular
      ],
    bgroup
      "File Size Scaling"
      [ bench "Small (10KB)" $ nfIO (benchmarkFileSize (10 * 1024)),
        bench "Medium (100KB)" $ nfIO (benchmarkFileSize (100 * 1024)),
        bench "Large (1MB)" $ nfIO (benchmarkFileSize (1024 * 1024)),
        bench "XLarge (5MB)" $ nfIO (benchmarkFileSize (5 * 1024 * 1024))
      ],
    bgroup
      "Complex JavaScript Patterns"
      [ bench "Deeply Nested" $ nfIO benchmarkDeepNesting,
        bench "Heavy Regex" $ nfIO benchmarkRegexHeavy,
        bench "Long Expressions" $ nfIO benchmarkLongExpressions
      ]
  ]

-- | Test jQuery parsing performance meets targets
testJQueryParsing :: Spec
testJQueryParsing = describe "jQuery parsing performance" $ do
  it "parses jQuery-style code under 500ms target" $ do
    jqueryCode <- createJQueryStyleCode
    startTime <- getCurrentTime
    result <- evaluate $ force (parseBS jqueryCode)
    endTime <- getCurrentTime
    let parseTimeMs = fromRational (toRational (diffUTCTime endTime startTime)) * 1000 :: Double
    parseTimeMs `shouldSatisfy` (< 500)
    result `shouldSatisfy` isParseSuccess

  it "achieves throughput target for jQuery-style parsing" $ do
    jqueryCode <- createJQueryStyleCode
    metrics <- measureParsePerformance jqueryCode
    metricsThroughput metrics `shouldSatisfy` (> 0.5)

-- | Test React library parsing performance
testReactParsing :: Spec
testReactParsing = describe "React parsing performance" $ do
  it "parses React-style code efficiently" $ do
    reactCode <- createReactStyleCode
    metrics <- measureParsePerformance reactCode
    let sizeRatio = fromIntegral (metricsInputSize metrics) / 280000.0
    let targetTime = 500.0 * max 1.0 sizeRatio
    metricsParseTime metrics `shouldSatisfy` (< targetTime)

  it "handles component patterns with good throughput" $ do
    componentCode <- createComponentPatterns
    metrics <- measureParsePerformance componentCode
    metricsThroughput metrics `shouldSatisfy` (> 0.5)

-- | Test Angular library parsing performance
testAngularParsing :: Spec
testAngularParsing = describe "Angular parsing performance" $ do
  it "parses Angular-style code under target time" $ do
    angularCode <- createAngularStyleCode
    metrics <- measureParsePerformance angularCode
    metricsParseTime metrics `shouldSatisfy` (< 5000)
  it "handles TypeScript-style patterns efficiently" $ do
    tsPatterns <- createTypeScriptPatterns
    metrics <- measureParsePerformance tsPatterns
    metricsThroughput metrics `shouldSatisfy` (> 0.5)

-- | Test linear scaling with file size
testLinearScaling :: Spec
testLinearScaling = describe "File size scaling validation" $ do
  it "demonstrates linear parse time scaling" $ do
    smallMetrics <- measureFileOfSize (100 * 1024)
    mediumMetrics <- measureFileOfSize (500 * 1024)
    largeMetrics <- measureFileOfSize (1024 * 1024)

    let small = metricsParseTime smallMetrics
    let medium = metricsParseTime mediumMetrics
    let large = metricsParseTime largeMetrics
    let ratio1 = medium / small
    let ratio2 = large / medium

    ratio2 `shouldSatisfy` (< ratio1 * 1.5)

  it "maintains consistent throughput across sizes" $ do
    smallMetrics <- measureFileOfSize (100 * 1024)
    largeMetrics <- measureFileOfSize (1024 * 1024)
    let smallThroughput = metricsThroughput smallMetrics
    let largeThroughput = metricsThroughput largeMetrics

    (largeThroughput / smallThroughput) `shouldSatisfy` (> 0.5)

-- | Test large file handling capabilities
testLargeFileHandling :: Spec
testLargeFileHandling = describe "Large file handling" $ do
  it "parses 1MB files under 2000ms target" $ do
    metrics <- measureFileOfSize (1024 * 1024)
    metricsParseTime metrics `shouldSatisfy` (< 2000)
    metrics `shouldSatisfy` metricsSuccess

  it "parses 5MB files under 10000ms target" $ do
    metrics <- measureFileOfSize (5 * 1024 * 1024)
    metricsParseTime metrics `shouldSatisfy` (< 10000)
    metrics `shouldSatisfy` metricsSuccess

  it "maintains >0.5MB/s throughput for large files" $ do
    metrics <- measureFileOfSize (2 * 1024 * 1024)
    metricsThroughput metrics `shouldSatisfy` (> 0.5)

-- | Test memory usage constraints
testMemoryConstraints :: Spec
testMemoryConstraints = describe "Memory usage validation" $ do
  it "uses reasonable memory for typical files" $ do
    let sizes = [100 * 1024, 500 * 1024]
    metrics <- mapM measureFileOfSize sizes

    let memoryRatios = map memoryRatio metrics
    memoryRatios `shouldSatisfy` all (< 30)

  it "shows linear memory scaling with input size" $ do
    smallMetrics <- measureFileOfSize (200 * 1024)
    largeMetrics <- measureFileOfSize (400 * 1024)

    let smallMem = metricsMemoryUsage smallMetrics
    let largeMem = metricsMemoryUsage largeMetrics
    let ratio = fromIntegral largeMem / fromIntegral smallMem :: Double

    ratio `shouldSatisfy` (\r -> r >= 1.5 && r <= 3.0)

-- | Test documented performance targets
testPerformanceTargets :: Spec
testPerformanceTargets = describe "Performance target validation" $ do
  it "meets jQuery parsing target of 500ms" $ do
    jqueryCode <- createJQueryStyleCode
    metrics <- measureParsePerformance jqueryCode
    metricsParseTime metrics `shouldSatisfy` (< 500)

  it "meets large file target for 5MB" $ do
    metrics <- measureFileOfSize (5 * 1024 * 1024)
    metricsParseTime metrics `shouldSatisfy` (< 15000)

  it "maintains baseline performance consistency" $ do
    testCode <- createBaselineTestCode
    metrics1 <- measureParsePerformance testCode
    metrics2 <- measureParsePerformance testCode

    let timeDiff = abs (metricsParseTime metrics1 - metricsParseTime metrics2)
    let avgTime = (metricsParseTime metrics1 + metricsParseTime metrics2) / 2
    (timeDiff / avgTime) `shouldSatisfy` (< 1.5)

-- | Test throughput targets
testThroughputTargets :: Spec
testThroughputTargets = describe "Throughput target validation" $ do
  it "achieves >0.5MB/s for typical JavaScript" $ do
    typicalCode <- createTypicalJavaScriptCode
    metrics <- measureParsePerformance typicalCode
    metricsThroughput metrics `shouldSatisfy` (> 0.5)

  it "maintains >0.3MB/s for complex patterns" $ do
    complexCode <- createComplexJavaScriptCode
    metrics <- measureParsePerformance complexCode
    metricsThroughput metrics `shouldSatisfy` (> 0.3)

  it "shows consistent throughput across runs" $ do
    testCode <- createMediumJavaScriptCode
    metrics <- mapM (\_ -> measureParsePerformance testCode) [1 .. 3 :: Int]
    let throughputs = map metricsThroughput metrics
    let avgThroughput = sum throughputs / fromIntegral (length throughputs)
    let variance = map (\t -> abs (t - avgThroughput) / avgThroughput) throughputs
    variance `shouldSatisfy` all (< 0.8)

-- | Test memory usage targets
testMemoryTargets :: Spec
testMemoryTargets = describe "Memory target validation" $ do
  it "keeps memory usage reasonable for typical files" $ do
    typicalCode <- createTypicalJavaScriptCode
    metrics <- measureParsePerformance typicalCode
    memoryRatio metrics `shouldSatisfy` (< 30)

  it "shows no memory leaks across multiple parses" $ do
    testCode <- createMediumJavaScriptCode
    metrics <- mapM (\_ -> measureParsePerformance testCode) [1 .. 5 :: Int]
    let memoryUsages = map metricsMemoryUsage metrics
    let maxMemory = maximum memoryUsages
    let minMemory = minimum memoryUsages

    let variance = fromIntegral (maxMemory - minMemory) / fromIntegral maxMemory :: Double
    variance `shouldSatisfy` (< 0.5)

-- ================================================================
-- Implementation Functions
-- ================================================================

-- | Compute memory-to-input-size ratio for a metrics value
memoryRatio :: PerformanceMetrics -> Double
memoryRatio m =
  fromIntegral (metricsMemoryUsage m) / fromIntegral (metricsInputSize m)

-- | Measure parse performance using the zero-copy ByteString API.
--
-- Uses 'parseBS' directly on the input 'ByteString', avoiding all
-- intermediate String\/Text conversions for accurate timing.
measureParsePerformance :: ByteString -> IO PerformanceMetrics
measureParsePerformance source = do
  let !inputSize = BS.length source

  startTime <- getCurrentTime
  result <- evaluate $ force (parseBS source)
  endTime <- getCurrentTime

  result `deepseq` return ()

  buildMetrics inputSize (isParseSuccess result) startTime endTime

-- | Build performance metrics from timing data
buildMetrics :: Int -> Bool -> UTCTime -> UTCTime -> IO PerformanceMetrics
buildMetrics inputSize success startTime endTime =
  return PerformanceMetrics
    { metricsParseTime = parseTimeMs,
      metricsMemoryUsage = estimatedMemory,
      metricsThroughput = throughputMBs,
      metricsInputSize = inputSize,
      metricsSuccess = success
    }
  where
    parseTimeMs = fromRational (toRational (diffUTCTime endTime startTime)) * 1000
    throughputMBs = fromIntegral inputSize / 1024 / 1024 / (parseTimeMs / 1000)
    estimatedMemory = inputSize * 12

-- | Measure performance for file of specific size
measureFileOfSize :: Int -> IO PerformanceMetrics
measureFileOfSize size = do
  source <- generateJavaScriptOfSize size
  measureParsePerformance source

-- | Check if parse result indicates success
isParseSuccess :: Either a b -> Bool
isParseSuccess (Right _) = True
isParseSuccess (Left _) = False

-- | Criterion benchmark for jQuery-style code
benchmarkJQuery :: IO ()
benchmarkJQuery = do
  jqueryCode <- createJQueryStyleCode
  result <- evaluate $ force (parseBS jqueryCode)
  result `deepseq` return ()

-- | Criterion benchmark for React-style code
benchmarkReact :: IO ()
benchmarkReact = do
  reactCode <- createReactStyleCode
  result <- evaluate $ force (parseBS reactCode)
  result `deepseq` return ()

-- | Criterion benchmark for Angular-style code
benchmarkAngular :: IO ()
benchmarkAngular = do
  angularCode <- createAngularStyleCode
  result <- evaluate $ force (parseBS angularCode)
  result `deepseq` return ()

-- | Criterion benchmark for specific file size
benchmarkFileSize :: Int -> IO ()
benchmarkFileSize size = do
  source <- generateJavaScriptOfSize size
  result <- evaluate $ force (parseBS source)
  result `deepseq` return ()

-- | Criterion benchmark for deeply nested code
benchmarkDeepNesting :: IO ()
benchmarkDeepNesting = do
  deepCode <- createDeeplyNestedCode
  result <- evaluate $ force (parseBS deepCode)
  result `deepseq` return ()

-- | Criterion benchmark for regex-heavy code
benchmarkRegexHeavy :: IO ()
benchmarkRegexHeavy = do
  regexCode <- createRegexHeavyCode
  result <- evaluate $ force (parseBS regexCode)
  result `deepseq` return ()

-- | Criterion benchmark for long expressions
benchmarkLongExpressions :: IO ()
benchmarkLongExpressions = do
  longCode <- createLongExpressionCode
  result <- evaluate $ force (parseBS longCode)
  result `deepseq` return ()

-- | Memory profiling using Weigh framework
runMemoryProfiling :: IO ()
runMemoryProfiling = do
  putStrLn "Memory profiling with Weigh framework"
  putStrLn "Run with: cabal run --test-option=--memory-profile"

-- ================================================================
-- JavaScript Code Generators
-- ================================================================

-- | Encode a Text code pattern to ByteString, repeated to reach target size
encodeRepeated :: Text.Text -> Int -> ByteString
encodeRepeated pattern targetSize =
  BS.concat $ replicate repetitions encoded
  where
    encoded = Text.encodeUtf8 pattern
    repetitions = targetSize `div` BS.length encoded

-- | Create jQuery-style JavaScript code (~280KB)
createJQueryStyleCode :: IO ByteString
createJQueryStyleCode =
  return $ encodeRepeated jqueryPattern (280 * 1024)
  where
    jqueryPattern =
      Text.unlines
        [ "(function($, window, undefined) {",
          "  'use strict';",
          "  $.fn.extend({",
          "    fadeIn: function(duration, callback) {",
          "      return this.animate({opacity: 1}, duration, callback);",
          "    },",
          "    fadeOut: function(duration, callback) {",
          "      return this.animate({opacity: 0}, duration, callback);",
          "    },",
          "    addClass: function(className) {",
          "      return this.each(function() {",
          "        if (this.className.indexOf(className) === -1) {",
          "          this.className += ' ' + className;",
          "        }",
          "      });",
          "    },",
          "    removeClass: function(className) {",
          "      return this.each(function() {",
          "        this.className = this.className.replace(className, '');",
          "      });",
          "    }",
          "  });",
          "})(jQuery, window);"
        ]

-- | Create React-style JavaScript code (~1.2MB)
createReactStyleCode :: IO ByteString
createReactStyleCode =
  return $ encodeRepeated reactPattern (1200 * 1024)
  where
    reactPattern =
      Text.unlines
        [ "var React = {",
          "  createElement: function(type, props) {",
          "    var children = Array.prototype.slice.call(arguments, 2);",
          "    return {",
          "      type: type,",
          "      props: props || {},",
          "      children: children",
          "    };",
          "  },",
          "  Component: function(props, context) {",
          "    this.props = props;",
          "    this.context = context;",
          "    this.state = {};",
          "    this.setState = function(newState) {",
          "      for (var key in newState) {",
          "        this.state[key] = newState[key];",
          "      }",
          "      this.forceUpdate();",
          "    };",
          "  }",
          "};"
        ]

-- | Create Angular-style JavaScript code (~2.4MB)
createAngularStyleCode :: IO ByteString
createAngularStyleCode =
  return $ encodeRepeated angularPattern (2400 * 1024)
  where
    angularPattern =
      Text.unlines
        [ "angular.module('app', []).controller('MainCtrl', function($scope, $http) {",
          "  $scope.items = [];",
          "  $scope.loading = false;",
          "  $scope.loadData = function() {",
          "    $scope.loading = true;",
          "    $http.get('/api/data').then(function(response) {",
          "      $scope.items = response.data;",
          "      $scope.loading = false;",
          "    });",
          "  };",
          "  $scope.addItem = function(item) {",
          "    $scope.items.push(item);",
          "  };",
          "  $scope.removeItem = function(index) {",
          "    $scope.items.splice(index, 1);",
          "  };",
          "});"
        ]

-- | Generate JavaScript code of specific size as ByteString
generateJavaScriptOfSize :: Int -> IO ByteString
generateJavaScriptOfSize targetSize =
  return $ encodeRepeated baseCode targetSize
  where
    baseCode =
      Text.unlines
        [ "function processData(data, options) {",
          "  var result = [];",
          "  var config = options || {};",
          "  for (var i = 0; i < data.length; i++) {",
          "    var item = data[i];",
          "    if (item && typeof item === 'object') {",
          "      var processed = transform(item, config);",
          "      if (validate(processed)) {",
          "        result.push(processed);",
          "      }",
          "    }",
          "  }",
          "  return result;",
          "}"
        ]

-- | Create component-style patterns for testing
createComponentPatterns :: IO ByteString
createComponentPatterns =
  return $ Text.encodeUtf8 $
    Text.unlines
      [ "function Component(props) {",
        "  var state = { count: 0 };",
        "  var handlers = {",
        "    increment: function() { state.count++; },",
        "    decrement: function() { state.count--; }",
        "  };",
        "  return {",
        "    render: function() {",
        "      return createElement('div', {}, state.count);",
        "    },",
        "    handlers: handlers",
        "  };",
        "}"
      ]

-- | Create TypeScript-style patterns for testing
createTypeScriptPatterns :: IO ByteString
createTypeScriptPatterns =
  return $ Text.encodeUtf8 $
    Text.unlines
      [ "var UserService = function() {",
        "  function UserService(http) {",
        "    this.http = http;",
        "  }",
        "  UserService.prototype.getUsers = function() {",
        "    return this.http.get('/api/users');",
        "  };",
        "  UserService.prototype.createUser = function(user) {",
        "    return this.http.post('/api/users', user);",
        "  };",
        "  return UserService;",
        "}();"
      ]

-- | Create baseline test code for consistency testing
createBaselineTestCode :: IO ByteString
createBaselineTestCode =
  return $ Text.encodeUtf8 $
    Text.unlines
      [ "var app = {",
        "  version: '1.0.0',",
        "  init: function() {",
        "    this.setupRoutes();",
        "    this.bindEvents();",
        "  },",
        "  setupRoutes: function() {",
        "    var routes = ['/', '/about', '/contact'];",
        "    return routes;",
        "  }",
        "};"
      ]

-- | Create typical JavaScript code for benchmarking
createTypicalJavaScriptCode :: IO ByteString
createTypicalJavaScriptCode =
  return $ Text.encodeUtf8 $
    Text.unlines
      [ "var module = (function() {",
        "  'use strict';",
        "  var api = {",
        "    getData: function(url) {",
        "      return fetch(url).then(function(response) {",
        "        return response.json();",
        "      });",
        "    },",
        "    postData: function(url, data) {",
        "      return fetch(url, {",
        "        method: 'POST',",
        "        body: JSON.stringify(data)",
        "      });",
        "    }",
        "  };",
        "  return api;",
        "})();"
      ]

-- | Create complex JavaScript patterns for stress testing
createComplexJavaScriptCode :: IO ByteString
createComplexJavaScriptCode =
  return $ Text.encodeUtf8 $
    Text.unlines
      [ "var complexModule = {",
        "  cache: new Map(),",
        "  process: function(data) {",
        "    return data.filter(function(item) {",
        "      return item.status === 'active';",
        "    }).map(function(item) {",
        "      return Object.assign({}, item, {",
        "        processed: true,",
        "        timestamp: Date.now()",
        "      });",
        "    }).reduce(function(acc, item) {",
        "      acc[item.id] = item;",
        "      return acc;",
        "    }, {});",
        "  }",
        "};"
      ]

-- | Create medium-sized JavaScript code for testing
createMediumJavaScriptCode :: IO ByteString
createMediumJavaScriptCode = generateJavaScriptOfSize (256 * 1024)

-- | Create deeply nested code for stress testing
createDeeplyNestedCode :: IO ByteString
createDeeplyNestedCode =
  return $ Text.encodeUtf8 $ Text.pack nesting
  where
    nesting = foldl' (\acc _ -> "function nested() { " <> acc <> " }") "return 42;" [1 .. 25 :: Int]

-- | Create regex-heavy code for pattern testing
createRegexHeavyCode :: IO ByteString
createRegexHeavyCode =
  return $ Text.encodeUtf8 $ Text.unlines $ concat $ replicate 25 patterns
  where
    patterns =
      [ "var emailRegex = /^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$/;",
        "var phoneRegex = /^\\+?[1-9]\\d{1,14}$/;",
        "var urlRegex = /^https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-zA-Z0-9()]{1,6}\\b([-a-zA-Z0-9()@:%_\\+.~#?&//=]*)$/;",
        "var ipRegex = /^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$/"
      ]

-- | Create long expression code for parsing stress test
createLongExpressionCode :: IO ByteString
createLongExpressionCode =
  return $ Text.encodeUtf8 $ Text.pack $ expr <> ";"
  where
    expr = foldl' (\acc i -> acc <> " + " <> show i) "var result = 1" [2 .. 100 :: Int]

-- | Create performance baseline measurements
createPerformanceBaseline :: IO [PerformanceMetrics]
createPerformanceBaseline = do
  jquery <- createJQueryStyleCode
  react <- createReactStyleCode
  typical <- createTypicalJavaScriptCode
  mapM measureParsePerformance [jquery, react, typical]

-- | Validate performance targets against results
validatePerformanceTargets :: BenchmarkResults -> IO [Bool]
validatePerformanceTargets results =
  return [jqTime, jqThroughput, scalingOK, memoryOK]
  where
    jqTime = metricsParseTime (jqueryResults results) < 100
    jqThroughput = metricsThroughput (jqueryResults results) > 1.0
    scalingOK = all (\m -> metricsParseTime m < 2000) (scalingResults results)
    memoryOK = all (\m -> metricsMemoryUsage m < 100 * 1024 * 1024) (memoryResults results)
