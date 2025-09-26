# ChangeLog for `language-javascript`

## 0.8.0.0 -- 2025-09-26

### ✨ New Features
+ **ES2021 Numeric Separators**: Full support for underscore (_) separators in all numeric literals:
  - Decimal literals: `1_000_000`, `3.14_15_92`
  - Binary literals: `0b1010_0001`, `0B1111_0000`
  - Octal literals: `0o755_123`, `0O644_777`
  - Hexadecimal literals: `0xFF_AA_BB`, `0xDEAD_BEEF`
  - BigInt literals: `123_456_789n`, `0xFF_AA_BBn`

+ **Multiple Output Formats**: New serialization formats for AST export:
  - **JSON Serialization**: Complete JSON export with `renderToJSON` function
  - **XML Serialization**: Structured XML output with `renderToXML` function
  - **S-Expression Serialization**: Lisp-style output with `renderToSExpr` function
  - All formats support complete AST representation with position and comment preservation

+ **Enhanced Error System**: Comprehensive parse error types with detailed diagnostics:
  - `InvalidNumericLiteral` for malformed numeric patterns
  - `InvalidPropertyAccess` for invalid property access patterns
  - `InvalidAssignmentTarget` for invalid assignment targets
  - `InvalidControlFlowLabel` for invalid break/continue labels
  - `MissingConstInitializer` for const declarations without initializers
  - `InvalidIdentifier` for malformed identifiers
  - `InvalidArrowParameter` for invalid arrow function parameters
  - `InvalidEscapeSequence` for malformed escape sequences
  - `InvalidRegexPattern` for invalid regex patterns
  - `InvalidUnicodeSequence` for malformed Unicode escapes
  - Error context and suggestions for better debugging experience

### 🔧 Improvements
+ **Enhanced String Escape Sequences**: Improved handling of escape sequences including forward slash (`\/`) and octal sequences
+ **Unicode Support**: Resolved Unicode character encoding issues for better international character handling (addresses GitHub issues related to Unicode parsing)
+ **Parser Robustness**: Removed overly restrictive lexer error patterns to better follow JavaScript lexical analysis rules
+ **Position Tracking**: Enhanced source position tracking with filename information in `TokenPosn`
+ **String Escape Module**: Made `Language.JavaScript.Parser.StringEscape` module publicly accessible for external use

### ⚠️ Breaking Changes
+ **ByteString → String Migration**: Complete migration from `ByteString` to `String` throughout the codebase
  - This affects all public APIs that previously used `ByteString` for JavaScript source code
  - Users should now pass `String` or `Text` values instead of `ByteString`
  - Pretty printer outputs now use `String` instead of `ByteString`
+ **Test Suite Reorganization**: Test fixtures moved to `test/fixtures/` directory
  - Affects users who relied on test files at the old locations

### 🐛 Bug Fixes & Resolved Issues
+ **Template Literal Parsing**: Fixed misrecognition of strings with backticks as template literals
+ **Parenthesized Expressions**: Resolved parsing issues with parenthesized identifiers in expressions  
+ **Lexer Escape Sequences**: Corrected lexer handling of various escaped character sequences
+ **Unicode Character Encoding**: Fixed Unicode character processing throughout the parser pipeline
+ **Validator Position Extraction**: Improved position handling in context-sensitive validation

### 🏗️ Internal Improvements
+ **Code Modernization**: Comprehensive refactoring following CLAUDE.md coding standards:
  - Lens usage for all record operations
  - Qualified imports throughout codebase  
  - Function size and complexity limits enforced
  - Enhanced error handling and validation
+ **Build System**: Updated Makefile with comprehensive development commands
+ **Test Coverage**: Expanded test suite with 85%+ coverage target:
  - **42 new JSON serialization tests** across 9 categories
  - **560+ XML serialization tests** covering all AST nodes
  - **498+ S-Expression tests** with complete validation
  - Enhanced unit tests for all lexer features including numeric separators
  - **ES2021 compliance tests** for modern JavaScript features
  - Improved property-based testing with better generators
  - Better golden test coverage with updated expectations
  - Comprehensive fuzzing and benchmark tests
  - **Negative test updates** for modern JavaScript error handling
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
