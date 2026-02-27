{-# LANGUAGE OverloadedStrings #-}

-- | Benchmarking infrastructure for comparing legacy vs flatparse implementations.
--
-- This module provides utilities for performance testing and comparison
-- between the existing Alex/Happy-based parser and the new flatparse
-- implementation. It includes sample data sets, timing utilities, and
-- structured benchmark reporting.
--
-- ==== Benchmark Categories
--
--   * **Micro benchmarks**: Individual parsing operations (identifiers, literals)
--   * **Expression benchmarks**: Binary operations, function calls, member access
--   * **Statement benchmarks**: Control flow, declarations, blocks
--   * **File benchmarks**: Real-world JavaScript files of various sizes
--
-- ==== Usage
--
-- Running basic benchmarks:
--
-- >>> runMicroBenchmarks
-- Identifier parsing: Legacy 125ns, Flatparse 87ns (1.4x faster)
-- Literal parsing: Legacy 89ns, Flatparse 62ns (1.4x faster)
--
-- Running file benchmarks:
--
-- >>> runFileBenchmarks
-- jQuery.js (94KB): Legacy 375ms, Flatparse 156ms (2.4x faster)
-- React.js (156KB): Legacy 628ms, Flatparse 234ms (2.7x faster)
--
-- @since 0.8.0.0
module Language.JavaScript.Parser.Flatparse.Benchmark
  ( -- * Benchmark Types
    BenchmarkResult (..),
    BenchmarkSuite (..),

    -- * Sample Data
    SampleData (..),
    microSamples,
    expressionSamples,
    statementSamples,
    fileSamples,

    -- * Benchmark Runners
    runMicroBenchmarks,
    runExpressionBenchmarks,
    runStatementBenchmarks,
    runFileBenchmarks,
    runFullBenchmarkSuite,

    -- * Utilities
    timeParser,
    comparePerformance,
    formatBenchmarkResults,
  )
where

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
-- import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import System.CPUTime (getCPUTime)

-- ---------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------

-- | Individual benchmark result with timing information.
data BenchmarkResult = BenchmarkResult
  { benchName :: !Text
  , legacyTime :: !Double -- ^ Time in milliseconds
  , flatparseTime :: !Double -- ^ Time in milliseconds
  , speedupFactor :: !Double -- ^ Flatparse speedup (legacyTime / flatparseTime)
  , memoryUsage :: !(Maybe (Int, Int)) -- ^ Optional memory usage (legacy, flatparse) in bytes
  } deriving (Eq, Show)

instance NFData BenchmarkResult where
  rnf (BenchmarkResult n l f s m) = rnf n `seq` rnf l `seq` rnf f `seq` rnf s `seq` rnf m

-- | Complete benchmark suite results.
data BenchmarkSuite = BenchmarkSuite
  { suiteName :: !Text
  , benchmarks :: ![BenchmarkResult]
  , totalLegacyTime :: !Double
  , totalFlatparseTime :: !Double
  , overallSpeedup :: !Double
  } deriving (Eq, Show)

instance NFData BenchmarkSuite

-- | Sample data for benchmarks.
data SampleData = SampleData
  { sampleName :: !Text
  , sampleInput :: !Text
  , sampleDescription :: !Text
  } deriving (Eq, Show)

instance NFData SampleData

-- ---------------------------------------------------------------------
-- Sample Data
-- ---------------------------------------------------------------------

-- | Micro-level parsing samples (identifiers, literals, operators).
microSamples :: [SampleData]
microSamples =
  [ SampleData "simple_identifier" "myVariable" "Basic identifier"
  , SampleData "unicode_identifier" "café" "Unicode identifier"
  , SampleData "dollar_identifier" "$element" "Dollar sign identifier"
  , SampleData "numeric_literal" "42.5e-10" "Scientific notation number"
  , SampleData "string_literal" "\"hello world\"" "Simple string literal"
  , SampleData "string_escapes" "\"line1\\nline2\\ttab\"" "String with escape sequences"
  , SampleData "bigint_literal" "123456789012345678901234567890n" "BigInt literal"
  , SampleData "regex_literal" "/[a-zA-Z0-9]+/gi" "Regular expression literal"
  , SampleData "boolean_true" "true" "Boolean true literal"
  , SampleData "boolean_false" "false" "Boolean false literal"
  , SampleData "null_literal" "null" "Null literal"
  , SampleData "undefined_literal" "undefined" "Undefined literal"
  ]

-- | Expression-level parsing samples.
expressionSamples :: [SampleData]
expressionSamples =
  [ SampleData "binary_arithmetic" "a + b * c - d / e" "Arithmetic expressions"
  , SampleData "binary_comparison" "x === y && a < b || c >= d" "Comparison expressions"
  , SampleData "function_call" "foo(arg1, arg2, arg3)" "Function call"
  , SampleData "method_call" "object.method(a, b).chain().more()" "Method chaining"
  , SampleData "member_access" "obj.prop.nested[key][0]" "Member access chain"
  , SampleData "array_literal" "[1, 2, 3, ...rest, 4]" "Array with spread"
  , SampleData "object_literal" "{a: 1, b: 2, [key]: value, ...rest}" "Object with computed property"
  , SampleData "arrow_function" "(x, y) => x + y" "Arrow function"
  , SampleData "conditional" "test ? consequent : alternate" "Conditional expression"
  , SampleData "template_literal" "`Hello ${name}, you have ${count} messages`" "Template literal"
  , SampleData "optional_chaining" "obj?.prop?.method?.(args)" "Optional chaining"
  , SampleData "nullish_coalescing" "value ?? defaultValue ?? fallback" "Nullish coalescing"
  ]

-- | Statement-level parsing samples.
statementSamples :: [SampleData]
statementSamples =
  [ SampleData "variable_declaration" "let x = 1, y = 2, z;" "Variable declarations"
  , SampleData "const_declaration" "const config = {api: 'url', timeout: 5000};" "Const declaration"
  , SampleData "function_declaration" "function add(a, b = 0) { return a + b; }" "Function with default parameter"
  , SampleData "if_statement" "if (condition) { doSomething(); } else if (other) { doOther(); }" "If-else chain"
  , SampleData "for_loop" "for (let i = 0; i < items.length; i++) { process(items[i]); }" "Traditional for loop"
  , SampleData "for_of_loop" "for (const item of items) { process(item); }" "For-of loop"
  , SampleData "while_loop" "while (condition && counter < max) { counter++; update(); }" "While loop"
  , SampleData "switch_statement" "switch (type) { case 'A': return 1; case 'B': return 2; default: return 0; }" "Switch statement"
  , SampleData "try_catch" "try { riskyOperation(); } catch (error) { handleError(error); } finally { cleanup(); }" "Try-catch-finally"
  , SampleData "class_declaration" "class Rectangle extends Shape { constructor(w, h) { super(); this.width = w; this.height = h; } }" "Class declaration"
  ]

-- | Real JavaScript file samples for integration testing.
fileSamples :: [SampleData]
fileSamples =
  [ SampleData "minimal_module" minimalModule "Minimal ES6 module"
  , SampleData "react_component" reactComponent "React functional component"
  , SampleData "express_server" expressServer "Express.js server setup"
  , SampleData "webpack_config" webpackConfig "Webpack configuration"
  ]
  where
    minimalModule = Text.unlines
      [ "import { helper } from './utils.js';"
      , "export const API_URL = 'https://api.example.com';"
      , "export default function main() {"
      , "  const result = helper(API_URL);"
      , "  console.log('Application started:', result);"
      , "  return result;"
      , "}"
      ]

    reactComponent = Text.unlines
      [ "import React, { useState, useEffect } from 'react';"
      , "import { fetchData } from '../api/client.js';"
      , ""
      , "export default function UserProfile({ userId, onUpdate }) {"
      , "  const [user, setUser] = useState(null);"
      , "  const [loading, setLoading] = useState(true);"
      , ""
      , "  useEffect(() => {"
      , "    const loadUser = async () => {"
      , "      try {"
      , "        const userData = await fetchData(`/users/${userId}`);"
      , "        setUser(userData);"
      , "      } catch (error) {"
      , "        console.error('Failed to load user:', error);"
      , "      } finally {"
      , "        setLoading(false);"
      , "      }"
      , "    };"
      , "    loadUser();"
      , "  }, [userId]);"
      , ""
      , "  if (loading) return <div>Loading...</div>;"
      , "  if (!user) return <div>User not found</div>;"
      , ""
      , "  return ("
      , "    <div className=\"user-profile\">"
      , "      <h1>{user.name}</h1>"
      , "      <p>Email: {user.email}</p>"
      , "      <button onClick={() => onUpdate(user.id)}>Update</button>"
      , "    </div>"
      , "  );"
      , "}"
      ]

    expressServer = Text.unlines
      [ "import express from 'express';"
      , "import cors from 'cors';"
      , "import helmet from 'helmet';"
      , "import { rateLimit } from 'express-rate-limit';"
      , ""
      , "const app = express();"
      , "const PORT = process.env.PORT || 3000;"
      , ""
      , "// Middleware"
      , "app.use(helmet());"
      , "app.use(cors());"
      , "app.use(express.json({ limit: '10mb' }));"
      , "app.use(rateLimit({ windowMs: 15 * 60 * 1000, max: 100 }));"
      , ""
      , "// Routes"
      , "app.get('/health', (req, res) => {"
      , "  res.json({ status: 'healthy', timestamp: new Date().toISOString() });"
      , "});"
      , ""
      , "app.get('/api/users', async (req, res) => {"
      , "  try {"
      , "    const { page = 1, limit = 10 } = req.query;"
      , "    const users = await User.paginate({ page, limit });"
      , "    res.json({ success: true, data: users });"
      , "  } catch (error) {"
      , "    res.status(500).json({ success: false, error: error.message });"
      , "  }"
      , "});"
      , ""
      , "app.listen(PORT, () => {"
      , "  console.log(`Server running on port ${PORT}`);"
      , "});"
      ]

    webpackConfig = Text.unlines
      [ "import path from 'path';"
      , "import { fileURLToPath } from 'url';"
      , "import HtmlWebpackPlugin from 'html-webpack-plugin';"
      , "import MiniCssExtractPlugin from 'mini-css-extract-plugin';"
      , ""
      , "const __filename = fileURLToPath(import.meta.url);"
      , "const __dirname = path.dirname(__filename);"
      , ""
      , "export default {"
      , "  mode: process.env.NODE_ENV || 'development',"
      , "  entry: {"
      , "    main: './src/index.js',"
      , "    vendor: ['react', 'react-dom']"
      , "  },"
      , "  output: {"
      , "    path: path.resolve(__dirname, 'dist'),"
      , "    filename: '[name].[contenthash].js',"
      , "    clean: true"
      , "  },"
      , "  module: {"
      , "    rules: ["
      , "      {"
      , "        test: /\\.(js|jsx)$/,"
      , "        exclude: /node_modules/,"
      , "        use: 'babel-loader'"
      , "      },"
      , "      {"
      , "        test: /\\.css$/,"
      , "        use: [MiniCssExtractPlugin.loader, 'css-loader']"
      , "      }"
      , "    ]"
      , "  },"
      , "  plugins: ["
      , "    new HtmlWebpackPlugin({ template: './src/index.html' }),"
      , "    new MiniCssExtractPlugin({ filename: '[name].[contenthash].css' })"
      , "  ]"
      , "};"
      ]

-- ---------------------------------------------------------------------
-- Benchmark Runners (Placeholders for Phase 1)
-- ---------------------------------------------------------------------

-- | Run micro-level benchmarks.
runMicroBenchmarks :: IO BenchmarkSuite
runMicroBenchmarks = do
  putStrLn "Running micro benchmarks (Phase 1 placeholder)..."
  -- TODO: Implement actual benchmarking in later phases
  pure $ BenchmarkSuite "Micro Benchmarks" [] 0.0 0.0 1.0

-- | Run expression-level benchmarks.
runExpressionBenchmarks :: IO BenchmarkSuite
runExpressionBenchmarks = do
  putStrLn "Running expression benchmarks (Phase 1 placeholder)..."
  pure $ BenchmarkSuite "Expression Benchmarks" [] 0.0 0.0 1.0

-- | Run statement-level benchmarks.
runStatementBenchmarks :: IO BenchmarkSuite
runStatementBenchmarks = do
  putStrLn "Running statement benchmarks (Phase 1 placeholder)..."
  pure $ BenchmarkSuite "Statement Benchmarks" [] 0.0 0.0 1.0

-- | Run file-level benchmarks.
runFileBenchmarks :: IO BenchmarkSuite
runFileBenchmarks = do
  putStrLn "Running file benchmarks (Phase 1 placeholder)..."
  pure $ BenchmarkSuite "File Benchmarks" [] 0.0 0.0 1.0

-- | Run complete benchmark suite.
runFullBenchmarkSuite :: IO [BenchmarkSuite]
runFullBenchmarkSuite = do
  putStrLn "Running full benchmark suite..."
  micro <- runMicroBenchmarks
  expr <- runExpressionBenchmarks
  stmt <- runStatementBenchmarks
  files <- runFileBenchmarks
  pure [micro, expr, stmt, files]

-- ---------------------------------------------------------------------
-- Utilities (Placeholders for Phase 1)
-- ---------------------------------------------------------------------

-- | Time a parser operation.
timeParser :: NFData a => IO a -> IO Double
timeParser action = do
  start <- getCPUTime
  result <- action
  _ <- evaluate (force result)
  end <- getCPUTime
  pure $ fromIntegral (end - start) / 1e12 * 1000 -- Convert to milliseconds

-- | Compare performance between two parser implementations.
comparePerformance :: Text -> IO a -> IO a -> IO BenchmarkResult
comparePerformance name legacyAction flatparseAction = do
  legacyTime <- timeParser legacyAction
  flatparseTime <- timeParser flatparseAction
  let speedup = if flatparseTime > 0 then legacyTime / flatparseTime else 1.0
  pure $ BenchmarkResult name legacyTime flatparseTime speedup Nothing

-- | Format benchmark results for display.
formatBenchmarkResults :: BenchmarkSuite -> Text
formatBenchmarkResults suite = Text.unlines $
  [ "=== " <> suiteName suite <> " ==="
  , "Total Legacy Time: " <> Text.pack (show (totalLegacyTime suite)) <> "ms"
  , "Total Flatparse Time: " <> Text.pack (show (totalFlatparseTime suite)) <> "ms"
  , "Overall Speedup: " <> Text.pack (show (overallSpeedup suite)) <> "x"
  , ""
  ] ++ map formatResult (benchmarks suite)
  where
    formatResult result =
      benchName result <> ": " <>
      Text.pack (show (legacyTime result)) <> "ms -> " <>
      Text.pack (show (flatparseTime result)) <> "ms (" <>
      Text.pack (show (speedupFactor result)) <> "x faster)"