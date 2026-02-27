# JavaScript Parser Migration to Flatparse

## Executive Summary

This document outlines a comprehensive plan to migrate the language-javascript parser from the current Alex/Happy-based implementation to [flatparse](https://hackage.haskell.org/package/flatparse), a high-performance parsing library. The migration promises **5-10x performance improvements** and significant memory efficiency gains while maintaining full JavaScript syntax support.

### Current Performance Issues
- jQuery parsing: **1.5x slower** than 250ms target (actual ~375ms)
- Large file throughput: **4.7x slower** than 1MB/s target (actual 0.5-0.8MB/s)
- Memory usage: **12x input size overhead** with frequent GC pressure
- Parsing 5MB files takes **11.9s** (should be <2s based on targets)

### Expected Improvements
- **5-10x faster parsing** based on flatparse benchmarks
- **50%+ memory reduction** through zero-allocation patterns
- **Better error messages** with precise source locations
- **Improved maintainability** through simpler combinator architecture

## Current Implementation Analysis

### Architecture Overview

The current parser uses a traditional two-stage approach:

```haskell
-- Current flow:
Text -> Alex Lexer -> [Token] -> Happy Parser -> AST
```

**Key Components:**
- **Lexer**: `src/Language/JavaScript/Parser/Lexer.x` (~400 lines Alex spec)
- **Parser**: `src/Language/JavaScript/Parser/Grammar7.y` (1000+ lines Happy grammar)
- **AST**: `src/Language/JavaScript/Parser/AST.hs` (comprehensive type hierarchy)
- **Tokens**: `src/Language/JavaScript/Parser/Token.hs` (40+ token types)

### Performance Bottlenecks Identified

1. **Lexer Inefficiencies**
   - Unicode character class processing
   - Regex pattern backtracking
   - Template literal state transitions
   - Full token stream materialization

2. **Parser Overhead**
   - Happy-generated state machine suboptimality
   - Extensive error recovery mechanisms
   - Multiple parsing passes for disambiguation
   - Heavy AST node wrapping

3. **Memory Allocation Patterns**
   - Token retention throughout parsing
   - AST annotation overhead
   - Intermediate parsing structures
   - High GC pressure from small object allocation

### Current AST Structure Analysis

**Strengths:**
- Comprehensive JavaScript coverage (ES5 + ES6+ features)
- Strong type safety preventing malformed ASTs
- Rich source location tracking via `JSAnnot`
- Support for comments and formatting preservation

**Improvement Opportunities:**
- **Memory efficiency**: Reduce annotation overhead
- **Construction performance**: Minimize allocations during parsing
- **Traversal optimization**: Better generic programming support
- **Size reduction**: Some intermediate wrappers could be eliminated

## Proposed Flatparse Architecture

### New Parsing Flow

```haskell
-- Proposed flow:
ByteString -> Flatparse Parser -> AST
```

**Key Advantages:**
- **Direct ByteString parsing**: No intermediate tokenization
- **Zero allocation**: Possible for pure validation phases
- **Template Haskell optimization**: Compile-time switch generation
- **Efficient combinators**: Performance-optimized primitives

### Enhanced AST Design

#### AST Improvements

1. **Streamlined Annotations**
```haskell
-- Current: Heavy annotation wrapping
data JSExpression = JSIdentifier JSAnnot String
                  | JSLiteral JSAnnot JSLiteral
                  | ...

-- Proposed: Lightweight positioning
data JSExpression = JSIdentifier !Pos !Text
                  | JSLiteral !Pos !JSLiteral
                  | ...

-- Where Pos is a compact position type
newtype Pos = Pos Word64  -- Encoded line/column
```

2. **Memory-Efficient Strings**
```haskell
-- Use Text instead of String throughout
-- Use ByteString for raw source retention when needed
data JSLiteral = JSStringLiteral !Text
               | JSNumericLiteral !Text  -- Preserve exact representation
               | JSBigIntLiteral !Text
               | ...
```

3. **Optimized Node Structure**
```haskell
-- Group related fields to improve cache locality
data JSStatement = JSExpressionStatement !Pos !JSExpression
                 | JSBlock !Pos !(Vector JSStatement)  -- Use Vector for better performance
                 | JSReturn !Pos !(Maybe JSExpression)
                 | ...
```

4. **Smart Constructors for Common Patterns**
```haskell
-- Provide efficient construction helpers
mkIdentifier :: Pos -> Text -> JSExpression
mkBinaryOp :: Pos -> JSBinOp -> JSExpression -> JSExpression -> JSExpression
mkFunctionCall :: Pos -> JSExpression -> [JSExpression] -> JSExpression
```

## Detailed Migration Plan

### Phase 1: Foundation and Preparation (3 weeks)

#### Week 1: Project Setup and Dependencies
- **Day 1-2**: Add flatparse dependency and build configuration
- **Day 3-4**: Create new module structure under `Language.JavaScript.Parser.Flatparse`
- **Day 5**: Set up performance benchmarking infrastructure

**Deliverables:**
- Working build with flatparse dependency
- Benchmark harness for comparing old vs new implementations
- Module structure planning document

#### Week 2: AST Modernization
- **Day 1-3**: Design and implement improved AST structure
- **Day 4-5**: Create smart constructors and utility functions

**Implementation Details:**
```haskell
-- New AST module: Language.JavaScript.Parser.Flatparse.AST

-- Compact position encoding
newtype Pos = Pos Word64
  deriving (Eq, Ord, Show, Storable)

-- Efficient position creation
mkPos :: Int -> Int -> Pos  -- line, column
mkPos line col = Pos (fromIntegral line `shiftL` 32 .|. fromIntegral col)

-- Position extraction
posLine, posColumn :: Pos -> Int
posLine (Pos w) = fromIntegral (w `shiftR` 32)
posColumn (Pos w) = fromIntegral (w .&. 0xFFFFFFFF)

-- Streamlined expression types
data JSExpression
  = JSIdentifier !Pos !Text
  | JSLiteral !Pos !JSLiteral
  | JSBinaryOp !Pos !JSBinOp !JSExpression !JSExpression
  | JSCallExpression !Pos !JSExpression !(Vector JSExpression)
  -- ... other expressions
  deriving (Eq, Show, Generic)
```

#### Week 3: Basic Parser Infrastructure
- **Day 1-3**: Implement basic flatparse utilities and primitives
- **Day 4-5**: Create lexer-equivalent functions for common tokens

**Core Utilities:**
```haskell
-- Language.JavaScript.Parser.Flatparse.Lexer

-- Position tracking parser
type JSParser = Parser ParseError

-- Basic lexical elements
ws :: JSParser ()  -- whitespace
ws = $(switch [| case _ of
  " "  -> ws
  "\t" -> ws
  "\n" -> ws
  "\r" -> ws
  _    -> pure () |])

identifier :: JSParser Text
keyword :: Text -> JSParser ()
stringLiteral :: JSParser Text
numericLiteral :: JSParser Text
```

### Phase 2: Core Expression Parsing (4 weeks)

#### Week 4: Literal and Identifier Parsing
- **Day 1-2**: Implement string literal parsing with escape sequences
- **Day 3-4**: Implement numeric literal parsing (decimal, hex, binary, octal, BigInt)
- **Day 5**: Implement identifier and keyword parsing

**String Literal Implementation:**
```haskell
-- Handle all JavaScript string literal formats
stringLiteral :: JSParser Text
stringLiteral = do
  quote <- satisfy (\c -> c == '"' || c == '\'')
  content <- Text.pack <$> many (stringChar quote)
  char quote
  pure content
  where
    stringChar quote =
      (char '\\' *> escapeSequence) <|>
      satisfy (\c -> c /= quote && c /= '\\' && c /= '\n')

escapeSequence :: JSParser Char
escapeSequence = $(switch [| case _ of
  "n"  -> pure '\n'
  "t"  -> pure '\t'
  "r"  -> pure '\r'
  "\\" -> pure '\\'
  "'"  -> pure '\''
  "\"" -> pure '"'
  "u"  -> unicodeEscape
  _    -> empty |])
```

#### Week 5: Binary and Unary Expressions
- **Day 1-3**: Implement operator precedence parsing
- **Day 4-5**: Add unary expression support

**Operator Precedence Implementation:**
```haskell
-- Efficient operator precedence using precedence climbing
expr :: JSParser JSExpression
expr = precedence atom
  where
    atom = parenthesized <|> literal <|> identifier'

    precedence = flip chainl1 $ do
      op <- binaryOp
      pure (\l r -> JSBinaryOp (getPos l) op l r)

binaryOp :: JSParser JSBinOp
binaryOp = $(switch [| case _ of
  "+"  -> pure JSBinOpPlus
  "-"  -> pure JSBinOpMinus
  "*"  -> pure JSBinOpTimes
  "/"  -> pure JSBinOpDivide
  "==" -> pure JSBinOpEq
  -- ... all operators
  _    -> empty |])
```

#### Week 6: Member Access and Function Calls
- **Day 1-3**: Implement member access (dot and bracket notation)
- **Day 4-5**: Add function call parsing with argument lists

#### Week 7: Advanced Expressions
- **Day 1-2**: Template literal parsing
- **Day 3-4**: Arrow function expressions
- **Day 5**: Optional chaining and nullish coalescing

**Template Literal Parsing:**
```haskell
templateLiteral :: JSParser JSExpression
templateLiteral = do
  char '`'
  parts <- many templatePart
  char '`'
  pure (JSTemplateLiteral pos parts)
  where
    templatePart = templateString <|> templateExpression

    templateString = JSTemplateString <$> takeWhile (\c -> c /= '`' && c /= '$')

    templateExpression = do
      text "${"; expr <- expression; char '}'
      pure (JSTemplateExpression expr)
```

### Phase 3: Statement and Declaration Parsing (3 weeks)

#### Week 8: Basic Statements
- **Day 1-2**: Expression statements and block statements
- **Day 3-4**: Variable declarations (var, let, const)
- **Day 5**: Return, break, continue statements

#### Week 9: Control Flow Statements
- **Day 1-2**: If/else statements
- **Day 3-4**: Loop statements (for, while, do-while)
- **Day 5**: Switch statements

#### Week 10: Function and Class Declarations
- **Day 1-3**: Function declarations and expressions
- **Day 4-5**: Class declarations with methods and properties

### Phase 4: Modern JavaScript Features (3 weeks)

#### Week 11: Async/Await and Generators
- **Day 1-3**: Async function parsing
- **Day 4-5**: Generator function parsing and yield expressions

#### Week 12: Module System
- **Day 1-3**: Import declarations and specifiers
- **Day 4-5**: Export declarations and default exports

#### Week 13: ES6+ Features
- **Day 1-2**: Destructuring patterns
- **Day 3-4**: Spread syntax and rest parameters
- **Day 5**: Private class fields and methods

### Phase 5: Error Handling and Recovery (2 weeks)

#### Week 14: Error System Design
- **Day 1-3**: Design flatparse-compatible error system
- **Day 4-5**: Implement error recovery strategies

**Error System Implementation:**
```haskell
-- Rich error types maintaining current quality
data ParseError
  = SyntaxError !Pos !Text ![Text]  -- position, message, suggestions
  | UnexpectedEOF !Pos
  | UnexpectedToken !Pos !Text !Text  -- found, expected
  | SemanticError !Pos !SemanticError
  deriving (Eq, Show)

-- Error recovery combinators
recover :: JSParser a -> JSParser a -> JSParser a
syncTo :: JSParser () -> JSParser ()
skipToSemicolon :: JSParser ()
```

#### Week 15: Error Quality Assurance
- **Day 1-3**: Implement helpful error messages
- **Day 4-5**: Add suggestion system for common errors

### Phase 6: Integration and Testing (3 weeks)

#### Week 16: API Compatibility Layer
- **Day 1-3**: Create compatibility wrapper for existing API
- **Day 4-5**: Ensure smooth migration path

**Compatibility Layer:**
```haskell
-- Language.JavaScript.Parser (updated)
-- Maintain existing API while using flatparse internally

parseProgram :: Text -> Either ParseError JSAST
parseProgram text =
  case parseByteString flatparseProgram mempty (Text.encodeUtf8 text) of
    OK ast _      -> Right (convertAST ast)
    Fail msg      -> Left (convertError msg)
    Err err       -> Left (convertError err)

-- AST conversion between old and new formats
convertAST :: FlatparseAST -> JSAST
convertError :: FlatparseError -> ParseError
```

#### Week 17: Performance Validation
- **Day 1-3**: Comprehensive performance benchmarking
- **Day 4-5**: Memory usage analysis and optimization

#### Week 18: Test Suite Integration
- **Day 1-3**: Run full existing test suite
- **Day 4-5**: Add flatparse-specific tests

### Phase 7: Optimization and Polish (2 weeks)

#### Week 19: Performance Optimization
- **Day 1-3**: Apply Template Haskell optimizations
- **Day 4-5**: Fine-tune parser combinators

**Template Haskell Optimizations:**
```haskell
-- Use TH for optimal switch generation
keywordSwitch :: JSParser Text
keywordSwitch = $(switch [| case _ of
  "function" -> pure "function"
  "class"    -> pure "class"
  "import"   -> pure "import"
  "export"   -> pure "export"
  "async"    -> pure "async"
  "await"    -> pure "await"
  -- ... all keywords
  _ -> empty |])
```

#### Week 20: Documentation and Release Preparation
- **Day 1-3**: Update all documentation
- **Day 4-5**: Create migration guide and release notes

## Technical Implementation Details

### Key Parser Combinators

```haskell
-- Core parsing utilities
module Language.JavaScript.Parser.Flatparse.Combinators where

-- Position-aware parsing
withPos :: JSParser a -> JSParser (Pos, a)
atPos :: Pos -> JSParser a -> JSParser a

-- JavaScript-specific combinators
jsIdentifier :: JSParser Text
jsKeyword :: Text -> JSParser ()
jsOperator :: Text -> JSParser ()
jsLiteral :: JSParser JSLiteral

-- List parsing with proper separation
commaSeparated :: JSParser a -> JSParser [a]
commaSeparated p = sepBy p (ws *> char ',' <* ws)

-- Statement separation
semicolonSeparated :: JSParser a -> JSParser [a]
semiOrNewline :: JSParser ()

-- Block parsing
block :: JSParser a -> JSParser [a]
block p = between (char '{' <* ws) (char '}') (many (p <* ws))
```

### Error Recovery Strategies

```haskell
-- Sophisticated error recovery maintaining current quality
module Language.JavaScript.Parser.Flatparse.Recovery where

-- Panic-mode recovery
syncToKeyword :: [Text] -> JSParser ()
syncToKeyword keywords = skipManyTill anyChar (lookAhead (choice (map jsKeyword keywords)))

-- Statement-level recovery
recoverStatement :: JSParser JSStatement -> JSParser JSStatement
recoverStatement p = p <|> do
  err <- getCurrentError
  syncToSemicolon
  pure (JSErrorStatement err)

-- Expression-level recovery
recoverExpression :: JSParser JSExpression -> JSParser JSExpression
recoverExpression p = p <|> do
  err <- getCurrentError
  skipToCommaOrSemicolon
  pure (JSErrorExpression err)
```

### Memory Optimization Techniques

1. **Strict Fields**: Use strict fields throughout AST
2. **Unboxed Types**: Use unboxed types for positions and small values
3. **Vector Storage**: Use Vector instead of lists for large collections
4. **ByteString Slicing**: Reuse source ByteString for string literals
5. **Compact Representations**: Pack multiple flags into single words

## Performance Expectations

### Benchmarking Strategy

```haskell
-- Comprehensive benchmarking setup
module Benchmark.JavaScript.Parser where

import Criterion

benchmarkSuite :: Benchmark
benchmarkSuite = bgroup "JavaScript Parser"
  [ bgroup "Current Implementation" currentBenchmarks
  , bgroup "Flatparse Implementation" flatparseBenchmarks
  , bgroup "Memory Usage" memoryBenchmarks
  ]

-- Test various JavaScript patterns
testFiles :: [(String, FilePath)]
testFiles =
  [ ("jQuery", "test/data/jquery.js")
  , ("React", "test/data/react.js")
  , ("Angular", "test/data/angular.js")
  , ("Large Bundle", "test/data/bundle.js")
  ]
```

### Expected Performance Gains

Based on flatparse benchmarks and current bottleneck analysis:

- **Small files (<10KB)**: 3-5x speedup
- **Medium files (10-100KB)**: 5-8x speedup
- **Large files (>100KB)**: 8-12x speedup
- **Memory usage**: 50-70% reduction
- **GC pressure**: 80%+ reduction

### Performance Targets

- **jQuery parsing**: <75ms (5x improvement from 375ms)
- **Large file throughput**: >2.5MB/s (5x improvement from 0.5MB/s)
- **Memory overhead**: <5x input size (down from 12x)
- **5MB file parsing**: <2s (6x improvement from 11.9s)

## Risk Assessment and Mitigation

### High-Risk Items

1. **API Compatibility**
   - **Risk**: Breaking changes for downstream users
   - **Mitigation**: Comprehensive compatibility layer with gradual migration path
   - **Timeline Impact**: +1 week for compatibility layer development

2. **Error Message Quality**
   - **Risk**: Loss of sophisticated error reporting
   - **Mitigation**: Careful error system redesign preserving key features
   - **Timeline Impact**: +1 week for error system polish

3. **Feature Completeness**
   - **Risk**: Missing edge cases during manual conversion
   - **Mitigation**: Comprehensive test coverage and systematic validation
   - **Timeline Impact**: +2 weeks for thorough testing

### Medium-Risk Items

1. **Performance Regression in Specific Cases**
   - **Risk**: Some parsing patterns might not benefit from flatparse
   - **Mitigation**: Extensive benchmarking and optimization tuning
   - **Timeline Impact**: +1 week for optimization

2. **Memory Usage in Edge Cases**
   - **Risk**: Certain input patterns might use more memory
   - **Mitigation**: Memory profiling and targeted optimization
   - **Timeline Impact**: +0.5 weeks for memory optimization

### Low-Risk Items

1. **Technical Feasibility**: Flatparse is proven technology with good Haskell ecosystem integration
2. **Development Velocity**: Clear plan with incremental milestones
3. **Rollback Capability**: Can maintain both implementations during transition

## Success Metrics and Validation

### Performance Metrics
- [ ] **5x+ parsing speed improvement** on representative JavaScript files
- [ ] **50%+ memory usage reduction** across all test cases
- [ ] **Meet all documented performance targets** (jQuery <250ms, 1MB/s+ throughput)
- [ ] **80%+ GC pressure reduction** measured via GC statistics

### Quality Metrics
- [ ] **100% test suite pass rate** with new implementation
- [ ] **Error message quality maintained** or improved vs current implementation
- [ ] **No functionality regression** across all supported JavaScript features
- [ ] **API compatibility preserved** for existing users

### Development Metrics
- [ ] **On-time delivery** within 20-week timeline
- [ ] **Incremental milestone validation** at each phase
- [ ] **Continuous integration** with performance regression detection
- [ ] **Comprehensive documentation** for new implementation

## Migration Timeline Summary

| Phase | Duration | Focus | Key Deliverables |
|-------|----------|--------|------------------|
| 1 | 3 weeks | Foundation | Project setup, improved AST, basic infrastructure |
| 2 | 4 weeks | Core Parsing | Expression parsing, operators, literals |
| 3 | 3 weeks | Statements | Declarations, control flow, functions |
| 4 | 3 weeks | Modern JS | Async/await, modules, ES6+ features |
| 5 | 2 weeks | Error Handling | Error recovery, quality messages |
| 6 | 3 weeks | Integration | API compatibility, testing, validation |
| 7 | 2 weeks | Optimization | Performance tuning, documentation |

**Total Timeline: 20 weeks (5 months)**

## Post-Migration Roadmap

### Phase 8: Advanced Optimizations (Future)
- **Incremental Parsing**: Support for partial source updates
- **Parallel Parsing**: Multi-threaded parsing for very large files
- **Stream Processing**: Parsing from streams rather than full ByteString
- **Custom Backends**: Specialized output formats beyond AST

### Phase 9: Ecosystem Integration
- **Language Server Protocol**: Enhanced LSP support with faster parsing
- **IDE Integration**: Real-time parsing for IDEs and editors
- **Tooling Support**: Integration with JavaScript tooling ecosystem
- **WebAssembly**: Potential WASM compilation for browser use

## Conclusion

The migration to flatparse represents a significant architectural improvement that will:

1. **Solve current performance problems** (1.5-4.7x slower than targets)
2. **Provide substantial performance gains** (5-10x speedup expected)
3. **Improve memory efficiency** (50%+ reduction expected)
4. **Simplify maintenance** through cleaner combinator architecture
5. **Enable future optimizations** not possible with current Happy/Alex approach

The **20-week timeline** is realistic given the scope, and the **phased approach** minimizes risk while delivering incremental value. The **compatibility layer** ensures existing users can migrate smoothly.

This migration will establish the language-javascript parser as a **high-performance reference implementation** capable of handling large-scale JavaScript codebases efficiently.