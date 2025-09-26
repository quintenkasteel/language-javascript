# Golden Test Infrastructure

This directory contains golden test infrastructure for the language-javascript parser. Golden tests help prevent regressions by comparing current parser output against stored baseline outputs.

## Directory Structure

```
test/golden/
├── README.md                    # This file
├── ecmascript/                  # ECMAScript specification examples
│   ├── inputs/                  # JavaScript source files
│   │   ├── literals.js          # Literal value tests
│   │   ├── expressions.js       # Expression parsing tests
│   │   ├── statements.js        # Statement parsing tests
│   │   └── edge-cases.js        # Complex language constructs
│   └── expected/                # Golden baseline files (auto-generated)
├── errors/                      # Error message consistency tests
│   ├── inputs/                  # Invalid JavaScript files
│   │   ├── syntax-errors.js     # Syntax error scenarios
│   │   └── lexer-errors.js      # Lexer error scenarios
│   └── expected/                # Error message baselines
├── pretty-printer/              # Pretty printer output stability
│   ├── inputs/                  # JavaScript files for pretty printing
│   └── expected/                # Pretty printer output baselines
└── real-world/                  # Real-world JavaScript examples
    ├── inputs/                  # Complex JavaScript files
    │   ├── react-component.js   # React component example
    │   ├── node-module.js       # Node.js module example
    │   └── modern-javascript.js # Modern JS features
    └── expected/                # Parser output baselines
```

## Test Categories

### 1. ECMAScript Golden Tests
Tests parser output consistency for standard JavaScript constructs defined in the ECMAScript specification:
- Numeric, string, boolean, and regex literals
- Binary, unary, and conditional expressions  
- Control flow statements (if/else, loops, try/catch)
- Function and class declarations
- Import/export statements
- Edge cases and complex constructs

### 2. Error Message Golden Tests
Ensures error messages remain stable and helpful across parser versions:
- Lexer errors (invalid tokens, unterminated strings)
- Syntax errors (missing brackets, invalid assignments)
- Semantic errors (context violations)

### 3. Pretty Printer Golden Tests
Validates that pretty printer output remains consistent:
- Round-trip testing (parse → pretty print → parse)
- Output formatting stability
- AST reconstruction accuracy

### 4. Real-World Golden Tests
Tests parser behavior on realistic JavaScript code:
- Modern JavaScript features (ES6+, async/await, destructuring)
- Framework patterns (React components, Node.js modules)
- Complex language constructs (generators, decorators, private fields)

## Running Golden Tests

Golden tests are integrated into the main test suite:

```bash
# Run all tests including golden tests
cabal test

# Run only golden tests
cabal test --test-options="--match 'Golden Tests'"

# Run specific golden test category
cabal test --test-options="--match 'ECMAScript Golden Tests'"
```

## Updating Golden Baselines

When parser changes are intentional and golden tests fail:

1. **Review the differences** to ensure they're expected
2. **Update baselines** by deleting expected files and re-running tests:
   ```bash
   rm test/golden/*/expected/*.golden
   cabal test
   ```
3. **Commit the updated baselines** with your parser changes

## Adding New Golden Tests

To add a new golden test:

1. **Create input file** in appropriate `inputs/` directory:
   ```bash
   echo "new test code" > test/golden/ecmascript/inputs/new-feature.js
   ```

2. **Run tests** to generate baseline:
   ```bash
   cabal test
   ```

3. **Verify baseline** in `expected/` directory is correct

4. **Commit both input and expected files**

## Golden Test Implementation

The golden test infrastructure is implemented in:
- `test/Test/Language/Javascript/GoldenTest.hs` - Main golden test module
- Uses `hspec-golden` library for baseline management
- Automatically discovers input files and generates corresponding tests
- Formats parser output in stable, human-readable format

## Benefits

✅ **Regression Prevention**: Detects unexpected changes in parser behavior  
✅ **Output Stability**: Ensures consistent formatting across versions  
✅ **Error Message Quality**: Maintains helpful error messages  
✅ **Documentation**: Test inputs serve as parser capability examples  
✅ **Confidence**: Enables safe refactoring with comprehensive coverage

## Best Practices

- **Review baselines** carefully when updating
- **Add tests** for new JavaScript features  
- **Use realistic examples** in real-world tests
- **Keep inputs focused** - one concept per test file
- **Document complex cases** with comments in input files