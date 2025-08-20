# Performance Testing Infrastructure

This document describes the performance testing infrastructure for the language-javascript parser.

## Overview

The performance testing infrastructure provides comprehensive benchmarking and validation of JavaScript parsing performance, including:

- **Real-world library parsing** (jQuery, React, Angular style code)
- **File size scaling validation** (linear performance characteristics)
- **Memory usage profiling** (memory growth patterns)
- **Performance target validation** (documented performance goals)
- **Criterion benchmarking integration** (detailed performance analysis)

## Running Performance Tests

### Hspec Integration Tests

Performance validation tests are integrated into the main test suite:

```bash
# Run all tests including performance validation
cabal test

# Run only performance tests
cabal test --test-options="--match Performance"
```

### Criterion Benchmarks

For detailed performance analysis with Criterion:

```haskell
-- In test/Test/Language/Javascript/PerformanceTest.hs
criterionBenchmarks :: [Benchmark]
```

To create custom Criterion benchmarks:

```haskell
import Criterion.Main
import Test.Language.Javascript.PerformanceTest

main = defaultMain criterionBenchmarks
```

## Performance Targets

The infrastructure validates against these performance targets:

- **jQuery Parsing**: < 100ms for ~280KB jQuery-style code
- **Large Files**: < 1s for 10MB JavaScript files  
- **Memory Usage**: Linear O(n) growth with input size
- **Parse Speed**: > 1MB/s parsing throughput
- **Memory Peak**: < 50MB for 10MB input files

## Test Categories

### 1. Real-world Library Parsing

Tests parsing performance with code patterns from popular JavaScript libraries:

- `testJQueryParsing` - jQuery-style plugin code
- `testReactParsing` - React component patterns  
- `testAngularParsing` - Angular/TypeScript style code

### 2. File Size Scaling

Validates linear scaling performance:

- `testLinearScaling` - Verifies O(n) time complexity
- `testLargeFileHandling` - Tests 1MB, 5MB file parsing
- `testMemoryConstraints` - Memory usage validation

### 3. Performance Target Validation

Ensures documented performance targets are met:

- `testPerformanceTargets` - Core performance requirements
- `testThroughputTargets` - Parse speed validation
- `testMemoryTargets` - Memory usage limits

## Benchmark Functions

### Criterion Benchmarks

```haskell
benchmarkJQuery :: IO ()       -- jQuery-style code parsing
benchmarkReact :: IO ()        -- React component parsing  
benchmarkAngular :: IO ()      -- Angular/TypeScript parsing
benchmarkFileSize :: Int -> IO () -- Variable size file parsing
```

### Memory Profiling

```haskell
runMemoryProfiling :: IO ()    -- Memory usage analysis
```

## JavaScript Code Generators

The infrastructure includes realistic JavaScript code generators:

```haskell
createJQueryStyleCode :: IO Text    -- ~280KB jQuery-style code
createReactStyleCode :: IO Text     -- ~1.2MB React-style code  
createAngularStyleCode :: IO Text   -- ~2.4MB Angular-style code
generateJavaScriptOfSize :: Int -> IO Text -- Custom size generation
```

## Usage Examples

### Basic Performance Measurement

```haskell
import Test.Language.Javascript.PerformanceTest

main = do
  code <- createJQueryStyleCode
  metrics <- measureParsePerformance code
  print $ metricsParseTime metrics
  print $ metricsThroughput metrics
```

### Custom Benchmark

```haskell
import Criterion.Main

customBenchmark = bench "my code" $ whnfIO $ do
  let code = "function test() { return 42; }"
  let result = parseUsing parseProgram code "test"
  result `seq` return ()
```

## Performance Metrics

The `PerformanceMetrics` type captures:

- `metricsParseTime`: Parse time in milliseconds
- `metricsMemoryUsage`: Memory usage in bytes  
- `metricsThroughput`: Parse speed in MB/s
- `metricsInputSize`: Input file size in bytes
- `metricsSuccess`: Whether parsing succeeded

## Implementation Details

### CLAUDE.md Compliance

The performance testing infrastructure follows all CLAUDE.md standards:

- Functions ≤15 lines with ≤4 parameters
- Qualified imports for all functions
- Lenses for record access/updates
- Comprehensive Haddock documentation
- 85%+ test coverage target
- No mock functions - all real functionality

### Architecture

- **PerformanceTest.hs**: Main performance testing module
- **Criterion integration**: Detailed benchmarking capabilities
- **Memory profiling**: Weigh framework integration (stub)
- **Test generators**: Realistic JavaScript code creation
- **Validation framework**: Performance target checking

## Future Enhancements

Potential improvements to the performance testing infrastructure:

1. **Full Weigh integration** for detailed memory profiling
2. **Performance regression detection** with historical baselines  
3. **CI integration** with performance monitoring
4. **Benchmark result storage** and trend analysis
5. **Performance profiling** with GHC's profiling tools