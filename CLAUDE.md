# CLAUDE.md - JavaScript Parser Development Standards

This document defines comprehensive coding standards, best practices, and development guidelines for the language-javascript parser project (Haskell-based JavaScript AST parser). These standards ensure the highest code quality, maintainability, performance, and team collaboration.

## 🎯 Core Principles

**Code Excellence**: Write clear, efficient, and self-documenting code that serves as the gold standard for parser implementation
**Consistency**: Maintain uniform style and patterns throughout the entire codebase
**Modularity**: Design with single responsibility principle and clear separation of concerns
**Performance**: Optimize parsing hot paths while maintaining readability; profile-driven optimization
**Robustness**: Comprehensive error handling with rich error types and graceful failure
**Testability**: Write code designed for thorough testing with high coverage (85%+ target)
**Documentation**: Extensive Haddock documentation for all public APIs
**Security**: Treat all JavaScript input as untrusted; validate and sanitize rigorously
**Collaboration**: Enable effective teamwork through clear standards and practices

## 🚫 Non-Negotiable Guardrails

These constraints are enforced by CI and must be followed without exception:

1. **Function size**: ≤ 15 lines (excluding blank lines and comments)
2. **Parameters**: ≤ 4 per function (use records/newtypes for grouping)
3. **Branching complexity**: ≤ 4 branching points (sum of if/case arms, guards, boolean splits)
4. **No duplication (DRY)**: Extract common logic into reusable functions
5. **Single responsibility**: One clear purpose per module
6. **Lens usage**: Use lenses for record access/updates; record construction for initial creation
7. **Qualified imports**: Everything qualified except types, lenses, and pragmas
8. **Test coverage**: Minimum 85% coverage for all modules (higher than typical projects due to parser criticality)
9. **Add documentation to each module in Haddock style**: Complete module-level documentation with purpose, examples, and function-level docs with type explanations

## 📁 Project Structure

```
language-javascript/
├── src/
│   └── Language/
│       └── JavaScript/
│           ├── Parser.hs              -- Main parser interface
│           ├── Parser/
│           │   ├── AST.hs             -- Abstract syntax tree definitions
│           │   ├── Grammar.y          -- Happy grammar (legacy)
│           │   ├── Grammar7.y         -- Current Happy grammar
│           │   ├── Lexer.x            -- Alex lexer definition
│           │   ├── LexerUtils.hs      -- Lexer utility functions
│           │   ├── ParseError.hs      -- Error types and handling
│           │   ├── Parser.hs          -- Core parser implementation
│           │   ├── ParserMonad.hs     -- Parser monad and state
│           │   ├── SrcLocation.hs     -- Source location tracking
│           │   └── Token.hs           -- Token definitions
│           ├── Pretty/
│           │   ├── Printer.hs         -- JavaScript pretty printer
│           │   ├── JSON.hs            -- JSON serialization (planned)
│           │   ├── XML.hs             -- XML serialization (planned)
│           │   └── SExpr.hs           -- S-expression serialization (planned)
│           └── Process/
│               └── Minify.hs          -- JavaScript minification
└── test/
    ├── Test/
    │   └── Language/
    │       └── Javascript/
    │           ├── ExpressionParser.hs -- Expression parsing tests
    │           ├── Lexer.hs           -- Lexer tests
    │           ├── LiteralParser.hs   -- Literal parsing tests
    │           ├── Minify.hs          -- Minification tests
    │           ├── ModuleParser.hs    -- Module parsing tests
    │           ├── ProgramParser.hs   -- Program parsing tests
    │           ├── RoundTrip.hs       -- Round-trip tests
    │           └── StatementParser.hs -- Statement parsing tests
    ├── testsuite.hs                   -- Main test runner
    ├── Unicode.js                     -- Unicode test data
    └── k.js                           -- Test JavaScript file
```

## 🎨 Haskell Style Guide

### Import Style

**MANDATORY PATTERN: Import types/constructors unqualified, functions qualified**

This is the ONLY acceptable import pattern for the ENTIRE language-javascript codebase. NO EXCEPTIONS.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Language.JavaScript.Parser.Expression
  ( parseExpression
  , JSExpression(..)
  ) where

-- Pattern 1: Types unqualified + module qualified
import Control.Monad.State.Strict (StateT)
import qualified Control.Monad.State.Strict as State
import qualified Control.Monad.Trans as Trans

-- Pattern 2: Multiple types from same module + qualified alias
import Data.Text (Text)
import qualified Data.Text as Text

-- Pattern 3: Specific operators unqualified + module qualified
import Data.List (intercalate)
import qualified Data.List as List

-- Pattern 4: Local project modules with selective type imports
import Language.JavaScript.Parser.Token
  ( Token(..)
  , CommentAnnotation(..)
  , TokenPosn(..)
  )
import qualified Language.JavaScript.Parser.Token as Token

-- Pattern 5: Standard library modules (types + qualified)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
```

**Usage Rules (APPLY TO ENTIRE CODEBASE):**

- **Type signatures**: Use unqualified types (`Text`, `Map`, `JSExpression`, `Token`, etc.)
- **Constructors**: Use qualified prefix (`Token.IdentifierToken`, `AST.JSLiteral`, `Map.empty`)
- **Functions**: ALWAYS use qualified (`Text.pack`, `List.map`, `State.put`, `Map.insert`)
- **Operators**: Import unqualified when frequently used (`(<+>)`, `(</>)`)
- **Import aliases**: Use meaningful names, NOT abbreviations (`as Parser` NOT `as P`, `as Text` NOT `as T`)
- **NO COMMENTS**: NEVER add comments like "-- Qualified imports" or "-- Local imports" in import blocks

### Lens Usage (Mandatory)

**Use lenses for record access and updates. Use record construction for initial creation.**

```haskell
-- Define records with lens support
data ParseState = ParseState
  { _stateTokens :: ![Token]
  , _statePosition :: !Int
  , _stateErrors :: ![ParseError]
  , _stateContext :: !ParseContext
  } deriving (Eq, Show)

-- Generate lenses
makeLenses ''ParseState

-- GOOD: Initial construction with record syntax
createInitialState :: [Token] -> ParseState
createInitialState tokens = ParseState
  { _stateTokens = tokens
  , _statePosition = 0
  , _stateErrors = []
  , _stateContext = TopLevel
  }

-- Access with (^.)
getCurrentToken :: ParseState -> Maybe Token
getCurrentToken state = 
  let pos = state ^. statePosition
      tokens = state ^. stateTokens
  in tokens !? pos

-- Update with (.~)
setPosition :: Int -> ParseState -> ParseState
setPosition pos state = state & statePosition .~ pos

-- Modify with (%~)
addError :: ParseError -> ParseState -> ParseState
addError err state = state & stateErrors %~ (err :)

-- Complex updates with (&)
advanceParser :: ParseState -> ParseState
advanceParser state = state
  & statePosition %~ (+1)
  & stateContext .~ InExpression
  & stateErrors .~ []

-- NEVER DO THIS
-- BAD: state.stateTokens (record access)
-- BAD: state { stateTokens = newTokens } (record update)
```

### Function Composition Style

**Prefer binds (>>=, >=>) over do-notation when linear and readable:**

```haskell
-- GOOD: Linear bind composition for parsing
parseExpression :: Text -> Either ParseError JSExpression
parseExpression input =
  Text.unpack input
    |> tokenize
    >>= parseTokens
    >>= validateExpression
  where
    validateExpression expr
      | isValidExpression expr = Right expr
      | otherwise = Left (InvalidExpression expr)

-- GOOD: Kleisli composition for parser combinators
parseCall :: Parser JSExpression
parseCall = parseIdentifier >=> parseArguments >=> buildCallExpression

-- GOOD: Using fmap and where for parser combinations
parseWithLocation :: Parser a -> Parser (Located a)
parseWithLocation parser = do
  start <- getPosition
  result <- parser
  end <- getPosition
  pure (Located (SrcSpan start end) result)
```

### Where vs Let

**Always prefer `where` over `let`:**

```haskell
-- GOOD: Using where for parser functions
parseJSBinaryOp :: Parser JSBinOp
parseJSBinaryOp = do
  pos <- getPosition
  token <- getCurrentToken
  either (Left . parseError pos) Right (tokenToBinOp token)
  where
    tokenToBinOp (PlusToken {}) = Right (JSBinOpPlus (JSAnnot pos []))
    tokenToBinOp (MinusToken {}) = Right (JSBinOpMinus (JSAnnot pos []))
    tokenToBinOp _ = Left "Expected binary operator"
    
    parseError pos msg = ParseError pos msg
```

## 📊 Function Design Rules

### Size and Complexity Limits

Every function must adhere to these limits:

```haskell
-- GOOD: Focused parser function under 15 lines
parseIdentifier :: Parser JSExpression
parseIdentifier = do
  token <- expectToken isIdentifier
  pos <- getTokenPosition token
  case token of
    IdentifierToken _ name _ -> pure (JSIdentifier (JSAnnot pos []) name)
    _ -> parseError "Expected identifier"
  where
    isIdentifier (IdentifierToken {}) = True
    isIdentifier _ = False

-- Extract complex parsing logic to separate functions
parseCallExpression :: Parser JSExpression
parseCallExpression =
  parseIdentifier
    >>= parseArgumentList
    >>= buildCallExpression

-- BAD: Function too long and complex (would exceed 15 lines)
-- Split into smaller, focused functions instead
```

### Parser-Specific Patterns

```haskell
-- Use record types for complex parser configuration
data ParseOptions = ParseOptions
  { _optionsStrictMode :: Bool
  , _optionsES6Features :: Bool
  , _optionsJSXSupport :: Bool
  , _optionsSourceMaps :: Bool
  }
makeLenses ''ParseOptions

-- Use sum types for parser states
data ParseContext 
  = TopLevel 
  | InFunction 
  | InClass 
  | InExpression
  deriving (Eq, Show)

-- Factor out validation logic
isValidModuleDeclaration :: JSStatement -> Bool
isValidModuleDeclaration stmt =
  case stmt of
    JSModuleImportDeclaration {} -> True
    JSModuleExportDeclaration {} -> True
    _ -> False
```

## 🧪 Testing Strategy

### Test Organization

```haskell
-- Unit test structure
module Test.Language.Javascript.ExpressionParserTest where

import Test.Hspec
import Language.JavaScript.Parser
import qualified Language.JavaScript.Parser.AST as AST

tests :: Spec
tests = describe "Expression Parser Tests" $ do
  describe "literal expressions" $ do
    it "parses numeric literals" $ do
      parseExpression "42" `shouldBe` 
        Right (AST.JSLiteral (AST.JSNumericLiteral noAnnot "42"))
    it "parses string literals" $ do
      parseExpression "\"hello\"" `shouldBe`
        Right (AST.JSLiteral (AST.JSStringLiteral noAnnot "hello"))
  
  describe "binary expressions" $ do
    it "parses addition" $ do
      case parseExpression "1 + 2" of
        Right (AST.JSExpressionBinary _ _ (AST.JSBinOpPlus _) _) -> 
          pure ()
        _ -> expectationFailure "Expected binary addition expression"

-- Property test example
module Test.Property.Language.Javascript.RoundTripProps where

import Test.Hspec
import Test.QuickCheck
import Language.JavaScript.Parser
import Language.JavaScript.Pretty.Printer

props :: Spec  
props = describe "Round-trip Properties" $ do
  it "parse then pretty-print preserves semantics" $ property $ \validJS ->
    case parseProgram validJS of
      Right ast -> 
        case parseProgram (renderToString ast) of
          Right ast' -> astEquivalent ast ast'
          Left _ -> False
      Left _ -> True  -- Skip invalid input

-- Golden test example
module Test.Golden.JSGeneration where

import Test.Hspec.Golden
import Language.JavaScript.Parser
import Language.JavaScript.Pretty.Printer

goldenTest :: Spec
goldenTest = describe "JavaScript Generation" $ do
  golden "simple expression" $ do
    let input = "function add(a, b) { return a + b; }"
    case parseProgram input of
      Right ast -> pure (renderToString ast)
      Left err -> error ("Parse failed: " + show err)
```

### Anti-Patterns: NEVER Mock Real Functionality

**❌ FORBIDDEN: Mock functions that always return True/False**

```haskell
-- BAD: This provides no actual testing value
isValidJavaScript :: Text -> Bool
isValidJavaScript _ = True  -- This is worthless!

isValidExpression :: JSExpression -> Bool  
isValidExpression _ = True  -- This tests nothing!

-- BAD: Fake validation that doesn't validate
validateSyntax :: Text -> Bool
validateSyntax _ = True      -- Completely useless
```

**❌ FORBIDDEN: Reflexive equality tests (testing if x == x)**

```haskell
-- BAD: These test nothing meaningful
testCase "expression equals itself" $ do
  let expr = JSLiteral (JSNumericLiteral noAnnot "42")
  expr `shouldBe` expr  -- Useless!

testCase "token is reflexive" $ do
  let token = IdentifierToken pos "test" []
  token `shouldBe` token  -- Tests nothing!
```

**✅ REQUIRED: Test actual functionality**

```haskell
-- GOOD: Test actual parsing behavior
testParseLiterals :: Spec
testParseLiterals = describe "Literal parsing" $ do
  it "parses integer literals correctly" $ do
    parseExpression "123" `shouldBe`
      Right (JSLiteral (JSNumericLiteral noAnnot "123"))
  
  it "parses string literals with quotes" $ do
    parseExpression "\"hello world\"" `shouldBe` 
      Right (JSLiteral (JSStringLiteral noAnnot "hello world"))
  
  it "handles escape sequences in strings" $ do
    parseExpression "\"hello\\nworld\"" `shouldBe`
      Right (JSLiteral (JSStringLiteral noAnnot "hello\nworld"))

-- GOOD: Test error conditions
testParseErrors :: Spec  
testParseErrors = describe "Parse error handling" $ do
  it "reports unclosed string literals" $ do
    case parseExpression "\"unclosed" of
      Left (ParseError _ msg) -> 
        msg `shouldContain` "unclosed string"
      _ -> expectationFailure "Expected parse error"
```

### Testing Requirements

1. **Unit tests** for every public function - NO MOCK FUNCTIONS
2. **Property tests** for parser invariants and round-trip properties  
3. **Golden tests** for pretty printer output and error messages
4. **Integration tests** for end-to-end parsing
5. **Performance tests** for parsing large JavaScript files

### Test Commands

```bash
# Build project
cabal build

# Run all tests - ALWAYS VERIFY NO MOCKING BEFORE COMMIT  
cabal test

# Run specific test suite
cabal test testsuite

# Run with coverage - MUST BE ≥85% REAL COVERAGE
cabal test --enable-coverage

# Run specific test pattern
cabal test --test-options="--match Expression"

# MANDATORY: Check for mock functions before any commit
grep -r "_ = True" test/    # Should return NOTHING
grep -r "_ = False" test/   # Should return NOTHING

# MANDATORY: Check for reflexive equality tests  
grep -r "shouldBe.*\b\(\w\+\)\b.*\b\1\b" test/  # Should return NOTHING
```

## 🚨 Error Handling

### Rich Error Types

```haskell
-- Define comprehensive error types for parsing
data ParseError
  = LexError !TokenPosn !Text
  | SyntaxError !TokenPosn !Text ![Text]  -- position, message, suggestions
  | SemanticError !TokenPosn !SemanticProblem
  | UnexpectedEOF !TokenPosn
  deriving (Eq, Show)

data SemanticProblem
  = DuplicateIdentifier !Text
  | UndefinedIdentifier !Text  
  | InvalidAssignmentTarget
  | InvalidBreakContext
  | InvalidContinueContext
  deriving (Eq, Show)

-- Use structured error information
renderParseError :: ParseError -> Text
renderParseError (SyntaxError pos msg suggestions) =
  Text.unlines $
    [ "Syntax Error at " <> showPosition pos
    , "  " <> msg
    ] ++ map ("  Suggestion: " <>) suggestions

-- Provide helpful error messages
parseExpected :: Text -> Parser a
parseExpected expected = do
  pos <- getPosition
  parseError (SyntaxError pos ("Expected " <> expected) [])
```

### Validation and Safety

```haskell
-- Validate all JavaScript input
validateJavaScriptInput :: Text -> Either ValidationError Text
validateJavaScriptInput input
  | Text.null input =
      Left (ValidationError "Empty input")
  | Text.length input > maxInputSize =
      Left (ValidationError "Input too large")
  | hasInvalidChars input =
      Left (ValidationError "Input contains invalid characters")
  | otherwise = Right input
  where
    maxInputSize = 10 * 1024 * 1024  -- 10MB limit
    hasInvalidChars = Text.any isInvalidChar
    isInvalidChar c = c `elem` ['\0', '\r\n']

-- Use total functions, document partial ones
safeHead :: [a] -> Maybe a
safeHead = listToMaybe

-- If partial function is necessary, document it clearly
-- | Get first token.
-- PARTIAL: Fails on empty list. Only use when token stream is guaranteed non-empty.
unsafeHeadToken :: [Token] -> Token
unsafeHeadToken (x:_) = x
unsafeHeadToken [] = error "unsafeHeadToken: empty token stream"
```

## 📝 Documentation Standards

### Haddock Documentation

Every public function must have comprehensive Haddock documentation:

```haskell
-- | Parse a JavaScript program from source text.
--
-- This function performs the following steps:
--   1. Tokenizes the input using Alex-generated lexer
--   2. Parses the token stream using Happy-generated parser
--   3. Constructs the JavaScript AST
--   4. Validates the AST structure
--
-- The parser supports ECMAScript 5 with some ES6+ features.
--
-- ==== Examples
--
-- >>> parseProgram "var x = 42;"
-- Right (JSAstProgram [JSVariable ...])
--
-- >>> parseProgram "invalid syntax here"
-- Left (SyntaxError ...)
--
-- ==== Errors
--
-- Returns 'ParseError' for:
--   * Lexical errors (invalid tokens)
--   * Syntax errors (invalid grammar)
--   * Semantic errors (context violations)
--
-- @since 0.7.1.0
parseProgram
  :: Text
  -- ^ JavaScript source code to parse
  -> Either ParseError JSAST
  -- ^ Parsed AST or error
parseProgram input =
  runParser programParser input
```

## ⚡ Performance Guidelines

### Parser-Specific Optimizations

```haskell
-- Use strict fields in parser state
data ParseState = ParseState
  { _stateTokens :: ![Token]
  , _statePosition :: !Int
  , _stateErrors :: ![ParseError]
  } deriving (Eq, Show)

-- Use BangPatterns for strict evaluation in parsing
parseTokens :: [Token] -> Either ParseError JSAST
parseTokens = go []
  where
    go !acc [] = Right (buildAST (reverse acc))
    go !acc (t:ts) = 
      case parseToken t of
        Right node -> go (node : acc) ts
        Left err -> Left err

-- Prefer Text over String for source input
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- Use efficient token representation
data Token = Token
  { tokenType :: !TokenType
  , tokenSpan :: !TokenPosn
  , tokenLiteral :: !Text
  , tokenComment :: ![CommentAnnotation]
  } deriving (Eq, Show)
```

### Memory Management for Large Files

```haskell
-- Stream large JavaScript files
parseFileStreaming :: FilePath -> IO (Either ParseError JSAST)
parseFileStreaming path = do
  content <- Text.readFile path
  let tokens = tokenize content
  pure (parseTokens tokens)

-- Clear parser state between uses
clearParseState :: ParseState -> ParseState  
clearParseState state = state
  & stateTokens .~ []
  & stateErrors .~ []
  & statePosition .~ 0
```

## 🔒 Security Considerations

### Input Validation for JavaScript

```haskell
-- Validate and sanitize JavaScript input
validateJavaScriptSource :: Text -> Either SecurityError Text
validateJavaScriptSource input
  | Text.length input > maxFileSize =
      Left (SecurityError "File too large")
  | containsUnsafePatterns input =
      Left (SecurityError "Input contains potentially unsafe patterns") 
  | exceedsNestingLimit input =
      Left (SecurityError "Nesting too deep")
  | otherwise = Right input
  where
    maxFileSize = 50 * 1024 * 1024  -- 50MB
    maxNestingDepth = 1000

-- Limit parsing resources
parseWithLimits :: Text -> Either ParseError JSAST
parseWithLimits input
  | Text.length input > maxSourceSize =
      Left (ParseError noPos "Source file too large")
  | estimatedComplexity input > maxComplexity =
      Left (ParseError noPos "Source too complex")
  | otherwise = parseProgram input
  where
    maxSourceSize = 10000000  -- 10MB
    maxComplexity = 100000
```

## 🔄 Version Control & Collaboration

### Commit Message Format

Follow conventional commits strictly:

```bash
feat(parser): add support for optional chaining (?.)
fix(lexer): handle BigInt literals correctly  
perf(parser): improve expression parsing by 20%
docs(api): add examples for Pretty.Printer module
refactor(ast): split JSExpression into separate modules
test(integration): add tests for ES2020 features
build(deps): update to ghc 9.8.4
ci(github): add parser performance benchmarks
style(format): apply ormolu to all modules
```

### Pull Request Checklist

- [ ] All CI checks pass
- [ ] Functions meet size/complexity limits  
- [ ] Lenses used for all record operations
- [ ] Qualified imports follow conventions
- [ ] Unit tests added/updated (coverage ≥85%)
- [ ] Property tests for parser invariants
- [ ] Golden tests updated if output changed
- [ ] Haddock documentation complete
- [ ] Performance impact assessed for parsing
- [ ] Security implications considered for JavaScript input
- [ ] CHANGELOG.md updated

## 🛠️ Development Workflow

### Daily Development

```bash
# Start work on feature
git checkout -b feature/es2020-bigint

# Ensure code quality before commit
make format           # Auto-format code (ormolu)
make lint            # Check for issues (hlint)
cabal test           # Run all tests

# Commit with conventional format
git commit -m "feat(parser): add BigInt literal support"

# Before pushing
cabal test --enable-coverage  # Check coverage ≥85%

# Push and create PR
git push origin feature/es2020-bigint
```

### JavaScript Parser Specific Workflow

```bash
# Test parser with specific JavaScript
echo "const x = 42n;" | cabal run language-javascript

# Run lexer tests
cabal test --test-options="--match Lexer"

# Run round-trip property tests  
cabal test --test-options="--match RoundTrip"

# Generate parser from grammar (when grammar changes)
happy src/Language/JavaScript/Parser/Grammar7.y
alex src/Language/JavaScript/Parser/Lexer.x
```

## 📋 Quick Reference

### Mandatory Practices

✅ **ALWAYS**:

- Use lenses for record access/updates; record construction for initial creation
- Qualify imports (except types/lenses/pragmas)
- Keep functions ≤15 lines, ≤4 params, ≤4 branches
- Write tests first (TDD) - especially for new JavaScript features
- Document with Haddock - critical for parser APIs
- Use `where` over `let`
- Prefer `()` over `$`
- Validate all JavaScript input for security
- Handle all parse error cases with helpful messages
- Target 85%+ test coverage

❌ **NEVER**:

- Use record syntax for access/updates (use lenses instead)
- Write functions >15 lines
- Use partial functions without documentation
- Parse untrusted JavaScript without validation
- Commit without tests  
- Ignore parser warnings or errors
- Skip code review for parser changes
- Use String (prefer Text for source code)

### Common Parser Patterns

```haskell
-- Module header
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}  
{-# OPTIONS_GHC -Wall #-}
module Language.JavaScript.Parser.Feature (api) where

-- Parser state with lenses
data ParseState = ParseState { _stateField :: !Type }
makeLenses ''ParseState

-- Error handling
parseFeature :: Text -> Either ParseError JSExpression  
parseFeature = tokenize >=> parseTokens >=> validate

-- Testing
spec :: Spec
spec = describe "Feature parsing" $
  it "parses correctly" $
    parseFeature input `shouldBe` Right expected
```

## 🎓 Learning Resources

- [Happy Parser Generator Guide](https://www.haskell.org/happy/)
- [Alex Lexer Generator Guide](https://www.haskell.org/alex/)
- [JavaScript Language Specification](https://tc39.es/ecma262/)
- [Haskell Parsing Libraries](https://wiki.haskell.org/Parsing)
- [Property Testing with QuickCheck](https://www.fpcomplete.com/haskell/library/quickcheck/)

## 📜 License and Credits

This coding standard incorporates best practices from:

- The language-javascript library maintainers
- Haskell parsing community standards  
- JavaScript language specification requirements
- Modern parser construction techniques

---

**Remember**: These standards exist to help us build a robust, maintainable, and high-performance JavaScript parser. When in doubt, prioritize parse correctness and input safety over performance optimizations.

For questions or suggestions, please open an issue in the project repository.