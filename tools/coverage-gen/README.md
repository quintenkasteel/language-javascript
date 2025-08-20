# Coverage-Driven Test Generation

An intelligent test generation system that uses machine learning, genetic algorithms, and real-world JavaScript corpus analysis to automatically generate test cases that improve code coverage and approach 95%+ coverage systematically.

## 🎯 Features

- **HPC Coverage Analysis**: Parses HPC coverage reports to identify gaps and prioritize test generation
- **ML-Driven Generation**: Uses neural networks and machine learning to generate intelligent test cases
- **Genetic Algorithm Optimization**: Evolves test suites for optimal coverage and diversity
- **Real-World Corpus Analysis**: Learns from large collections of real JavaScript code
- **Seamless Integration**: Integrates with existing test infrastructure and measurement systems
- **95%+ Coverage Target**: Systematically approaches and maintains high coverage levels

## 🚀 Quick Start

### Prerequisites

- GHC 9.4+ with Cabal
- HPC coverage reports (`.tix` files)
- Optional: Real-world JavaScript corpus for enhanced generation

### Installation

```bash
# Clone and build
cd tools/coverage-gen/
make build

# Install globally (optional)
make install
```

### Basic Usage

```bash
# Generate tests from HPC coverage report
make run

# Run with corpus analysis for better quality
make run-corpus

# Custom configuration
cabal run coverage-gen -- \
  --hpc dist/hpc/tix/testsuite/testsuite.tix \
  --corpus corpus/real-world/ \
  --target 0.95 \
  --output test/Generated/ \
  --verbose
```

## 📊 Architecture

### Core Components

1. **Coverage Analysis** (`Coverage.Analysis`)
   - Parses HPC reports and identifies coverage gaps
   - Prioritizes gaps by importance and impact
   - Provides detailed gap classification

2. **Test Generation** (`Coverage.Generation`)
   - ML-driven test case synthesis
   - Multiple generation strategies (Random, Genetic, ML, Hybrid)
   - Configurable neural networks and decision trees

3. **Test Optimization** (`Coverage.Optimization`)
   - Genetic algorithm-based test suite evolution
   - Multi-objective fitness functions
   - Adaptive parameter tuning

4. **Corpus Analysis** (`Coverage.Corpus`)
   - Real-world JavaScript pattern extraction
   - Feature analysis and classification
   - Realistic test case synthesis

5. **Integration** (`Coverage.Integration`)
   - Seamless test infrastructure integration
   - Coverage measurement and validation
   - Incremental improvement tracking

### Generation Strategies

- **Random Generation**: Baseline random test case creation
- **Genetic Algorithm**: Evolutionary optimization with crossover and mutation
- **Machine Learning**: Neural network-driven intelligent synthesis
- **Hybrid Approach**: Combines multiple strategies with adaptive weighting

## 🧠 Machine Learning Models

### Supported Model Types

1. **Neural Networks**
   - Configurable layer architectures
   - Multiple activation functions (ReLU, Sigmoid, Tanh)
   - Dropout regularization
   - Various optimizers (SGD, Adam, RMSprop)

2. **Decision Trees**
   - Configurable depth and split criteria
   - Minimum sample requirements
   - Gini, Entropy, and MSE criteria

3. **Random Forests**
   - Ensemble learning with bootstrap sampling
   - Feature selection optimization
   - Parallel tree training

4. **Gradient Boosting**
   - Sequential weak learner improvement
   - Configurable learning rates
   - Adaptive depth control

## 📈 Performance Characteristics

### Coverage Improvement

- **Baseline**: Typical projects start at 60-80% coverage
- **Target**: Systematically approaches 95%+ coverage
- **Incremental**: Continuous improvement through iterative generation

### Generation Metrics

- **Test Quality**: Generates syntactically valid JavaScript
- **Diversity**: Maintains high test case diversity
- **Efficiency**: Optimizes test suite size while maximizing coverage
- **Realism**: Incorporates real-world patterns from corpus analysis

## 🔧 Configuration

### Generation Configuration

```haskell
GenerationConfig
  { _configStrategy = MachineLearning mlConfig
  , _configMaxTests = 100
  , _configTargetCoverage = 0.95
  , _configMutationRate = 0.1
  }
```

### Optimization Configuration

```haskell
OptimizationConfig
  { _configPopulationSize = 50
  , _configGenerations = 20
  , _configEliteSize = 5
  , _configCrossoverRate = 0.8
  , _configMutationRate = 0.2
  }
```

### Integration Configuration

```haskell
IntegrationConfig
  { _configTestCommand = "cabal test"
  , _configCoverageCommand = "cabal test --enable-coverage"
  , _configTestDirectory = "test/Generated/"
  , _configTimeout = 300
  , _configParallel = True
  }
```

## 📚 Examples

### Basic Coverage Analysis

```haskell
-- Parse HPC report
hpcReport <- parseHpcReport "dist/hpc/tix/testsuite/testsuite.tix"

-- Identify gaps
let gaps = identifyCoverageGaps hpcReport
let prioritized = prioritizeGaps gaps

-- Generate tests
generator <- createMLGenerator config
testCases <- generateTestCases generator prioritized
```

### Advanced Corpus-Based Generation

```haskell
-- Load real-world corpus
corpus <- loadCorpus "corpus/real-world/"
patterns <- extractPatterns corpus

-- Generate from patterns
corpusTests <- generateFromCorpus patterns gaps

-- Combine with ML generation
mlTests <- generateTestCases generator gaps
let allTests = mlTests ++ corpusTests
```

### Genetic Algorithm Optimization

```haskell
-- Create optimizer
optimizer <- createCoverageOptimizer optConfig

-- Create initial test suite
let initialSuite = TestSuite testCases metrics size fitness

-- Evolve for better coverage
optimizedSuite <- optimizeTestSuite optimizer initialSuite
let improvement = measureCoverageGain initialSuite optimizedSuite
```

## 🧪 Testing

```bash
# Run all tests
make test

# Generate coverage report
make coverage

# Run performance benchmarks
make bench

# Comprehensive validation
make validate
```

### Test Categories

1. **Unit Tests**: Individual component testing
2. **Integration Tests**: End-to-end workflow validation
3. **Property Tests**: Invariant verification
4. **Performance Tests**: Scalability and efficiency validation
5. **Golden Tests**: Output consistency verification

## 📖 API Documentation

Generate complete API documentation:

```bash
make docs
```

Key modules:

- `Coverage.Analysis`: HPC report analysis and gap identification
- `Coverage.Generation`: ML-driven test case synthesis
- `Coverage.Optimization`: Genetic algorithm optimization
- `Coverage.Corpus`: Real-world pattern extraction
- `Coverage.Integration`: Test infrastructure integration

## 🔍 Debugging and Monitoring

### Verbose Output

```bash
cabal run coverage-gen -- --verbose
```

### Coverage Monitoring

```bash
# Real-time coverage tracking
watch "cabal test --enable-coverage && grep -A5 'Coverage' dist/hpc/html/testsuite.html"
```

### Performance Profiling

```bash
# Enable profiling
cabal configure --enable-profiling
cabal build
cabal run coverage-gen -- +RTS -p -RTS
```

## 🚨 Troubleshooting

### Common Issues

1. **Missing HPC Files**
   - Ensure tests run with `--enable-coverage`
   - Check `.tix` file location and permissions

2. **Low Generation Quality**
   - Increase corpus size for better patterns
   - Adjust ML model parameters
   - Tune genetic algorithm settings

3. **Slow Performance**
   - Enable parallel execution
   - Reduce population size for genetic algorithms
   - Use smaller neural network architectures

4. **Integration Failures**
   - Verify test command configuration
   - Check output directory permissions
   - Validate generated test syntax

### Performance Optimization

- Use corpus analysis for realistic patterns
- Enable parallel test execution
- Tune ML model complexity vs. speed
- Monitor memory usage with large corpora

## 🤝 Contributing

1. Follow CLAUDE.md coding standards
2. Functions ≤15 lines, ≤4 parameters
3. Comprehensive Haddock documentation
4. 85%+ test coverage requirement
5. Qualified imports for all functions

### Development Workflow

```bash
# Format code
make format

# Run style checks
make lint

# Comprehensive validation
make validate
```

## 📊 Metrics and Benchmarks

### Coverage Metrics

- **Line Coverage**: Percentage of executable lines covered
- **Branch Coverage**: Percentage of conditional branches covered
- **Expression Coverage**: Percentage of expressions evaluated
- **Path Coverage**: Percentage of execution paths tested

### Performance Benchmarks

- **Generation Speed**: Tests generated per second
- **Coverage Improvement**: Coverage gain per iteration
- **Test Quality**: Syntactic and semantic validity
- **Resource Usage**: Memory and CPU consumption

## 🎓 Advanced Usage

### Custom ML Models

```haskell
-- Define custom neural network
let customConfig = NetworkConfig
      { _networkLayers = [50, 100, 50, 1]
      , _networkActivation = ReLU
      , _networkDropout = 0.3
      }

-- Use with generation
let mlConfig = MLConfig
      { _mlModelType = NeuralNetwork customConfig
      , _mlTrainingSize = 5000
      , _mlFeatureSet = customFeatures
      , _mlOptimizer = Adam
      }
```

### Hybrid Generation Strategies

```haskell
-- Combine multiple approaches
let hybridConfig = HybridConfig
      { _hybridStrategies = [MachineLearning mlConfig, GeneticAlgorithm genConfig]
      , _hybridWeights = [0.7, 0.3]
      , _hybridAdaptive = True
      }
```

### Real-Time Coverage Monitoring

```haskell
-- Set up continuous monitoring
integrator <- createTestIntegrator integConfig
result <- runGeneratedTests integrator testCases
coverage <- measureIntegratedCoverage result

-- Adapt based on results
when (coverage < targetCoverage) $ do
  newTests <- generateAdditionalTests gaps
  runGeneratedTests integrator newTests
```

## 📄 License

Part of the language-javascript project. See main project LICENSE for details.

## 🔗 Related Tools

- [HPC](https://wiki.haskell.org/Haskell_program_coverage): Haskell Program Coverage
- [QuickCheck](https://hackage.haskell.org/package/QuickCheck): Property-based testing
- [Tasty](https://hackage.haskell.org/package/tasty): Modern testing framework

---

**Coverage-driven test generation for systematic quality improvement** 🎯