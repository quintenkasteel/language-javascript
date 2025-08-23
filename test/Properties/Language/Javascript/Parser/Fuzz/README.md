# JavaScript Parser Fuzzing Infrastructure

This directory contains a comprehensive fuzzing infrastructure designed to discover edge cases, crashes, and vulnerabilities in the language-javascript parser through systematic input generation and testing.

## Overview

The fuzzing infrastructure implements multiple complementary strategies:

- **Crash Testing**: AFL-style mutation fuzzing for parser robustness
- **Coverage-Guided Fuzzing**: Feedback-driven input generation to explore uncovered code paths  
- **Property-Based Fuzzing**: AST invariant validation through systematic input mutation
- **Differential Testing**: Cross-parser validation against Babel, TypeScript, and other reference implementations

## Architecture

### Core Components

- **`FuzzHarness.hs`**: Main fuzzing orchestration and result analysis
- **`FuzzGenerators.hs`**: Advanced input generation strategies and mutation techniques
- **`CoverageGuided.hs`**: Coverage measurement and feedback-driven generation
- **`DifferentialTesting.hs`**: Cross-parser comparison and validation framework
- **`FuzzTest.hs`**: Integration test suite with CI/development configurations

### Test Integration

- **`Test/Language/Javascript/FuzzingSuite.hs`**: Main test interface integrated with Hspec
- Automatic integration with existing test suite via `testsuite.hs`
- Environment-specific configurations for CI vs development testing

## Usage

### Running Fuzzing Tests

Basic fuzzing as part of test suite:
```bash
cabal test testsuite
```

Environment-specific fuzzing:
```bash
# CI mode (fast, lightweight)
FUZZ_TEST_ENV=ci cabal test testsuite

# Development mode (intensive, comprehensive)  
FUZZ_TEST_ENV=development cabal test testsuite

# Regression mode (focused on known issues)
FUZZ_TEST_ENV=regression cabal test testsuite
```

### Standalone Fuzzing

Run intensive fuzzing campaign:
```bash
cabal run fuzzing-campaign -- --iterations 10000
```

Generate coverage report:
```bash
cabal run coverage-analysis -- --output coverage-report.html
```

Run differential testing:
```bash
cabal run differential-testing -- --parsers babel,typescript
```

## Configuration

### Test Environments

The fuzzing infrastructure adapts to different testing environments:

- **CI Environment**: Fast, lightweight tests suitable for continuous integration
  - 200 iterations per strategy
  - 2-second timeout per test
  - Minimal failure analysis
  
- **Development Environment**: Comprehensive testing for thorough validation
  - 5000 iterations per strategy  
  - 10-second timeout per test
  - Full failure analysis and minimization
  
- **Regression Environment**: Focused testing of known edge cases
  - 500 iterations focused on regression corpus
  - Validates previously discovered issues remain fixed

### Fuzzing Strategies

Each strategy can be configured independently:

```haskell
FuzzConfig {
  fuzzStrategy = Comprehensive,      -- Strategy selection
  fuzzIterations = 1000,            -- Number of test iterations
  fuzzTimeout = 5000,               -- Timeout per test (ms)
  fuzzMinimizeFailures = True,      -- Minimize failing inputs
  fuzzSeedInputs = [...]            -- Seed inputs for mutation
}
```

## Input Generation

### Malformed Input Generation

Systematically generates inputs designed to trigger parser edge cases:

- Syntax-breaking mutations (unmatched parentheses, incomplete constructs)
- Invalid character sequences (null bytes, control characters, broken Unicode)
- Boundary condition violations (deeply nested structures, extremely long identifiers)

### Coverage-Guided Generation

Uses feedback from code coverage measurements to guide input generation:

- Measures line, branch, and path coverage during parser execution
- Biases input generation toward areas that increase coverage
- Implements genetic algorithms for evolutionary input optimization

### Property-Based Generation

Generates inputs designed to test AST invariants and parser properties:

- Round-trip preservation (parse → print → parse consistency)
- AST structural integrity validation
- Semantic equivalence testing under transformations

### Differential Generation

Creates inputs for cross-parser validation:

- Standard JavaScript constructs for compatibility testing
- Edge case features for implementation comparison
- Error condition inputs for consistent error handling validation

## Failure Analysis

### Automatic Classification

Failures are automatically categorized by type:

- **Parser Crashes**: Segmentation faults, stack overflows, infinite loops
- **Parser Timeouts**: Inputs that cause parser to hang or run indefinitely  
- **Memory Exhaustion**: Inputs that consume excessive memory
- **Property Violations**: AST invariant violations or inconsistencies
- **Differential Mismatches**: Disagreements with reference parser implementations

### Failure Minimization

Complex failing inputs are automatically minimized to smallest reproduction:

```
Original: function f(x,y,z,w,a,b,c,d,e,f) { return ((((((x+y)+z)+w)+a)+b)+c)+d)+e)+f); }
Minimized: function f(x) { return ((x; }
```

### Regression Corpus

Discovered failures are automatically added to regression corpus:

- `test/fuzz/corpus/crashes/` - Known crash cases
- `test/fuzz/corpus/timeouts/` - Known timeout cases  
- `test/fuzz/corpus/properties/` - Known property violations
- `test/fuzz/corpus/differential/` - Known differential mismatches

## Performance Monitoring

### Resource Limits

Fuzzing operates within strict resource limits to prevent system exhaustion:

- Maximum 128MB memory per test
- 1-second timeout per input by default
- Maximum input size of 1MB
- Maximum parse depth of 100 levels

### Performance Validation

Continuous monitoring ensures fuzzing doesn't impact parser performance:

- Baseline performance measurement and regression detection
- Memory usage monitoring and leak detection  
- Parsing rate validation (minimum inputs per second)

## Integration with CI

### Automatic Execution

Fuzzing tests run automatically in CI with appropriate resource limits:

- Fast mode: 200 iterations across all strategies (~30 seconds)
- Comprehensive validation of parser robustness
- Automatic failure reporting and artifact collection

### Failure Reporting

CI integration provides detailed failure analysis:

- Categorized failure reports with minimized reproduction cases
- Coverage analysis showing newly discovered code paths
- Performance regression detection and alerting

## Development Workflow

### Local Development

For intensive local testing:

```bash
# Run comprehensive fuzzing
make fuzz-comprehensive

# Analyze specific failure
make fuzz-analyze FAILURE=crash_001.js

# Update regression corpus  
make fuzz-update-corpus

# Generate coverage report
make fuzz-coverage-report
```

### Debugging Failures

When fuzzing discovers a failure:

1. **Reproduce**: Verify the failure is reproducible
2. **Minimize**: Reduce input to smallest failing case
3. **Analyze**: Determine root cause (crash, hang, property violation)
4. **Fix**: Implement parser fix for the issue
5. **Validate**: Ensure fix doesn't break existing functionality
6. **Regression**: Add minimized case to regression corpus

## Extending the Infrastructure

### Adding New Generators

To add new input generation strategies:

1. Implement generator in `FuzzGenerators.hs`
2. Add strategy to `FuzzHarness.hs` 
3. Update test configuration in `FuzzTest.hs`
4. Add integration tests in `FuzzingSuite.hs`

### Adding New Properties

To test additional AST properties:

1. Define property in `PropertyTest.hs`
2. Add validation function to `FuzzHarness.hs`
3. Update failure classification in `FuzzTest.hs`

### Adding Reference Parsers

To compare against additional parsers:

1. Implement parser interface in `DifferentialTesting.hs`
2. Add comparison logic and result analysis
3. Update test configuration for new parser

## Best Practices

### Resource Management

- Always use timeouts to prevent infinite loops
- Monitor memory usage to prevent system exhaustion
- Implement early termination for resource-intensive operations

### Test Isolation

- Each fuzzing test should be independent and isolated
- Clean up any state between tests
- Use fresh parser instances for each test

### Failure Handling

- Never ignore failures - all crashes and hangs are bugs
- Minimize failures before analysis to reduce complexity
- Maintain comprehensive regression corpus

### Performance Considerations

- Balance test coverage with execution time
- Use appropriate iteration counts for different environments
- Monitor and report on fuzzing performance metrics

## Security Considerations

The fuzzing infrastructure is designed to safely test parser robustness:

- All inputs are treated as untrusted and potentially malicious
- Resource limits prevent system compromise
- Timeout mechanisms prevent denial-of-service
- Input validation prevents injection attacks

## Troubleshooting

### Common Issues

**Fuzzing tests timeout in CI:**
- Reduce iteration counts in CI configuration
- Check for infinite loops in input generation
- Verify timeout settings are appropriate

**High memory usage:**
- Review memory limits in configuration
- Check for memory leaks in parser or fuzzing code
- Monitor garbage collection behavior

**False positive failures:**
- Review failure classification logic
- Check for non-deterministic behavior
- Validate property definitions are correct

**Poor coverage improvement:**
- Review coverage measurement implementation
- Check that feedback loop is working correctly
- Validate input generation diversity

### Performance Issues

**Slow fuzzing execution:**
- Profile input generation performance
- Optimize mutation strategies
- Consider parallel execution where appropriate

**Coverage measurement overhead:**
- Use sampling for coverage measurement
- Implement efficient coverage data structures
- Cache coverage results where possible

## Contributing

When contributing to the fuzzing infrastructure:

1. Follow the established coding standards from `CLAUDE.md`
2. Add comprehensive tests for new functionality
3. Update documentation for any interface changes
4. Ensure new components integrate with CI
5. Validate performance impact of changes

### Code Review Checklist

- [ ] Functions are ≤15 lines and ≤4 parameters
- [ ] Qualified imports follow project conventions
- [ ] Comprehensive Haddock documentation
- [ ] Property tests for new generators
- [ ] Integration tests for new strategies
- [ ] Performance impact assessment
- [ ] Security considerations addressed

## Future Enhancements

Planned improvements to the fuzzing infrastructure:

- **Structured Input Generation**: Grammar-based generation for more realistic JavaScript
- **Guided Genetic Algorithms**: More sophisticated evolutionary approaches
- **Distributed Fuzzing**: Parallel execution across multiple machines
- **Machine Learning Integration**: ML-guided input generation
- **Advanced Coverage Metrics**: Function-level and data-flow coverage
- **Real-world Corpus Integration**: Fuzzing with real JavaScript codebases

---

For questions or issues with the fuzzing infrastructure, please open an issue in the project repository or contact the maintainers.