# ChangeLog for `language-javascript`

## 0.8.0.0 -- 2025-08-29

### ✨ New Features
+ **ES2021 Numeric Separators**: Full support for underscore (_) separators in all numeric literals:
  - Decimal literals: `1_000_000`, `3.14_15_92`
  - Binary literals: `0b1010_0001`, `0B1111_0000`
  - Octal literals: `0o755_123`, `0O644_777`
  - Hexadecimal literals: `0xFF_AA_BB`, `0xDEAD_BEEF`
  - BigInt literals: `123_456_789n`, `0xFF_AA_BBn`

### 🔧 Improvements
+ **Enhanced String Escape Sequences**: Improved handling of escape sequences including forward slash (`\/`) and octal sequences
+ **Unicode Support**: Resolved Unicode character encoding issues for better international character handling
+ **Parser Robustness**: Removed overly restrictive lexer error patterns to better follow JavaScript lexical analysis rules

### ⚠️ Breaking Changes
+ **ByteString → String Migration**: Complete migration from `ByteString` to `String` throughout the codebase
  - This affects all public APIs that previously used `ByteString` for JavaScript source code
  - Users should now pass `String` or `Text` values instead of `ByteString`
  - Pretty printer outputs now use `String` instead of `ByteString`
+ **Test Suite Reorganization**: Test fixtures moved to `test/fixtures/` directory
  - Affects users who relied on test files at the old locations

### 🏗️ Internal Improvements
+ **Code Modernization**: Comprehensive refactoring following CLAUDE.md coding standards:
  - Lens usage for all record operations
  - Qualified imports throughout codebase  
  - Function size and complexity limits enforced
  - Enhanced error handling and validation
+ **Build System**: Updated Makefile with comprehensive development commands
+ **Test Coverage**: Expanded test suite with 85%+ coverage target:
  - Enhanced unit tests for all lexer features
  - Improved property-based testing
  - Better golden test coverage
  - Comprehensive fuzzing and benchmark tests
+ **Documentation**: Complete Haddock documentation for all public APIs
+ **Linting**: Added comprehensive `.hlint.yaml` configuration

### 🧹 Cleanup  
+ Removed deprecated files: `.travis.yml`, `Setup.hs`, legacy test files
+ Removed outdated Unicode generation tools and coverage generation utilities
+ Cleaned up build artifacts and temporary files

### 📋 Development
+ **Build Configuration**: Improved cabal configuration to properly isolate build artifacts
+ **Code Quality**: Enforced consistent formatting and linting across entire codebase
+ **Performance**: Optimized parsing performance while maintaining code clarity

## 0.7.1.0 -- 2020-03-22
+ Add support for `async` function specifiers and `await` keyword.

## 0.7.0.0 -- 2019-10-10

+ Add support for (Ryan Hendrickson):
  - Destructuring in var declarations
  - `const` in for statements
  - ES6 property shorthand syntax
  - Template literals
  - Computed property names
  - Bare import declarations
  - Exotic parameter syntaxes
  - Generators and `yield`
  - Method definitions
  - classes
 `- super` keyword

## 0.6.0.13 -- 2019-06-17

+ Add support for (Cyril Sobierajewicz):
  - Unparenthesized arrow functions of one parameter
  - Export from declarations
  - Add back support for identifiers named `as`

## 0.6.0.12 -- 2019-05-03

+ Add support for for..of and friends (Franco Bulgarelli)
