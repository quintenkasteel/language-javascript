# Comprehensive JavaScript Parser Transformation Plan
## Complete Infrastructure Overhaul for Performance & Error Quality

**BREAKING CHANGES VERSION - Complete Rewrite**

### 📊 Current Infrastructure Analysis

**Existing Components to Transform:**
- `Language.JavaScript.Parser.Token` - String-based token storage
- `Language.JavaScript.Parser.ParseError` - Rich error types (good foundation)
- `Language.JavaScript.Parser.Lexer.x` - Alex with monadUserState wrapper
- `Language.JavaScript.Parser.Grammar7.y` - Happy grammar returning Either String AST
- `Language.JavaScript.Parser.Parser` - String input interface

**Performance Issues:**
- jQuery: 483ms → Target: <200ms (2.4x improvement needed)
- Throughput: 0.654 MB/s → Target: >1 MB/s (1.5x improvement needed)
- Large files: 10.3s/10MB → Target: <2s (5x improvement needed)

**Error System Issues:**
- ParseError types exist but not utilized in parser
- Parser returns `Either String AST` instead of `Either [ParseError] AST`
- No source context or recovery suggestions in actual parsing
- Error messages are generic strings

### 🎯 Complete Infrastructure Transformation

**New Parser Interface:**
```haskell
-- Replace: Either String JSAST  
-- With: Either [ParseError] JSAST

parse :: ByteString -> FilePath -> Either [ParseError] JSAST
parseModule :: ByteString -> FilePath -> Either [ParseError] JSAST
```

## Phase 1: Foundation & Analysis (Week 1-2)

### 1.1 Performance Profiling & Baseline
```bash
# Create comprehensive benchmarking suite
cabal configure --enable-profiling
cabal build --ghc-options="-prof -fprof-auto"

# Profile current performance bottlenecks
./dist/build/language-javascript/language-javascript +RTS -p -h
```

**Deliverables:**
- Detailed performance profile showing exact bottlenecks
- Memory allocation patterns analysis
- CPU time distribution by function
- Baseline performance metrics for all test cases

### 1.2 Architecture Design
```haskell
-- Design new high-performance token representation
data FastToken = FastToken
  { ftType :: {-# UNPACK #-} !TokenType
  , ftSpan :: {-# UNPACK #-} !TokenPosn  
  , ftBytes :: {-# UNPACK #-} !ByteString  -- Raw bytes
  , ftText :: !(Maybe Text)                -- Lazy Unicode conversion
  , ftComments :: ![CommentAnnotation]
  }

-- New ByteString-based lexer interface
data FastLexer = FastLexer
  { flInput :: {-# UNPACK #-} !ByteString
  , flPosition :: {-# UNPACK #-} !Int
  , flLine :: {-# UNPACK #-} !Int
  , flColumn :: {-# UNPACK #-} !Int
  , flTokens :: ![FastToken]
  }
```

## Phase 2: Enhanced Error System (Week 2-3)

### 2.1 Rich Error Types Design
```haskell
-- Comprehensive error system with source context
data JSParseError = JSParseError
  { jpeType :: !ErrorType
  , jpeLocation :: !SourceLocation
  , jpeMessage :: !Text
  , jpeSuggestions :: ![Text]
  , jpeContext :: !ErrorContext
  , jpeSourceSnippet :: !SourceSnippet
  } deriving (Eq, Show)

data ErrorType
  = LexicalError !LexError
  | SyntaxError !SyntaxError  
  | SemanticError !SemanticError
  | ContextualError !ContextError
  deriving (Eq, Show)

data LexError
  = InvalidCharacter !Char
  | UnterminatedString !Text
  | InvalidNumber !Text !NumberError
  | InvalidRegex !Text !RegexError
  | InvalidEscape !Text !EscapeError
  deriving (Eq, Show)

data SyntaxError
  = UnexpectedToken !FastToken !ExpectedTokens
  | InvalidExpression !Text !ExpressionError
  | MalformedStatement !Text !StatementError
  | UnbalancedDelimiters !DelimiterType !SourceLocation
  deriving (Eq, Show)

data SemanticError
  = DuplicateDeclaration !Text !SourceLocation
  | UndefinedReference !Text
  | InvalidAssignment !Text
  | ScopeViolation !Text !ScopeType
  deriving (Eq, Show)

-- Rich source context for errors
data SourceSnippet = SourceSnippet
  { ssLines :: ![Text]           -- Surrounding source lines
  , ssHighlight :: !SourceSpan   -- Highlighted error region
  , ssLineNumbers :: ![Int]      -- Line numbers
  , ssTabWidth :: !Int           -- For proper alignment
  } deriving (Eq, Show)

data ErrorContext
  = TopLevelContext
  | FunctionContext !Text
  | ClassContext !Text  
  | BlockContext !BlockType
  | ExpressionContext !ExpressionType
  deriving (Eq, Show)
```

### 2.2 Advanced Error Recovery
```haskell
-- Panic-mode recovery with multiple sync points
data RecoveryStrategy
  = SkipToToken !TokenType
  | SkipToAnyOf ![TokenType]
  | InsertToken !TokenType
  | ReplaceToken !TokenType !TokenType
  deriving (Eq, Show)

-- Error recovery state
data RecoveryState = RecoveryState
  { rsErrors :: ![JSParseError]
  , rsRecoveries :: ![Recovery]
  , rsSyncPoints :: ![TokenType]
  , rsMaxErrors :: !Int
  } deriving (Eq, Show)

-- Implement sophisticated error recovery
recoverFromError :: JSParseError -> FastLexer -> Either [JSParseError] FastLexer
recoverFromError err lexer = do
  strategy <- selectRecoveryStrategy err (currentContext lexer)
  case strategy of
    SkipToToken tok -> skipUntilToken tok lexer
    InsertToken tok -> insertSyntheticToken tok lexer
    ReplaceToken bad good -> replaceToken bad good lexer
```

### 2.3 Error Message Generation
```haskell
-- Generate helpful, IDE-friendly error messages
generateErrorMessage :: JSParseError -> SourceText -> Text
generateErrorMessage JSParseError{..} source = Text.unlines
  [ formatErrorHeader jpeType jpeLocation
  , formatSourceContext jpeSourceSnippet
  , formatMessage jpeMessage
  , formatSuggestions jpeSuggestions
  , formatHelp jpeType
  ]

formatErrorHeader :: ErrorType -> SourceLocation -> Text  
formatErrorHeader errType SourceLocation{..} =
  "Error " <> errorCode errType <> " at " 
  <> fileName <> ":" <> show lineNumber <> ":" <> show columnNumber

-- Generate context-aware suggestions
generateSuggestions :: ErrorType -> ErrorContext -> [Text]
generateSuggestions (SyntaxError (UnexpectedToken actual expected)) ctx =
  [ "Did you mean: " <> formatExpected expected
  , contextualSuggestion actual ctx
  , "Common fix: " <> commonFix actual expected
  ]
```

## Phase 3: ByteString Lexer Implementation (Week 3-5)

### 3.1 Alex ByteString Integration
```haskell
-- New Alex wrapper configuration
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

%wrapper "monad-bytestring"
%encoding "utf8"
%monadUserState FastLexerState

-- Optimized lexer state
data FastLexerState = FastLexerState
  { flsComments :: ![CommentAnnotation]
  , flsPreviousToken :: !FastToken
  , flsInTemplate :: !Bool
  , flsContext :: !LexerContext
  , flsErrors :: ![JSParseError]
  } deriving (Eq, Show)

-- High-performance token generation
fastMkToken :: TokenType -> ByteString -> Alex FastToken
fastMkToken tokenType bytes = do
  pos <- getTokenPosition  
  comments <- getComments
  return FastToken
    { ftType = tokenType
    , ftSpan = pos
    , ftBytes = bytes
    , ftText = Nothing  -- Lazy conversion
    , ftComments = comments
    }
```

### 3.2 Optimized Token Patterns
```alex
-- Optimized lexer rules with ByteString operations
<0> {
  -- Keywords with fast ByteString comparison
  "function"    { fastKeyword FunctionToken }
  "var"         { fastKeyword VarToken }
  "let"         { fastKeyword LetToken }
  "const"       { fastKeyword ConstToken }
  
  -- Optimized numeric literals
  $digit+                          { fastNumeric DecimalToken }
  "0x" $hexdigit+                  { fastNumeric HexIntegerToken }
  "0b" $bindigit+                  { fastNumeric BinaryIntegerToken }
  $digit+ "n"                      { fastNumeric BigIntToken }
  
  -- String literals with escape handling
  \" ($printable # [\"\\] | \\ .)* \"   { fastString StringToken }
  \' ($printable # [\'\\] | \\ .)* \'   { fastString StringToken }
  
  -- Identifiers with Unicode support
  ($alpha | \_) ($alpha | $digit | \_)*  { fastIdentifier }
}

-- Fast token creation functions
fastKeyword :: (TokenPosn -> Text -> [CommentAnnotation] -> FastToken) -> AlexAction FastToken
fastKeyword constructor = \(pos, _, bytes, _) len -> do
  let tokenBytes = BS.take len bytes
  fastMkToken (constructor pos (decodeUtf8 tokenBytes) []) tokenBytes

fastNumeric :: (TokenPosn -> Text -> [CommentAnnotation] -> FastToken) -> AlexAction FastToken
fastNumeric constructor = \(pos, _, bytes, _) len -> do
  let tokenBytes = BS.take len bytes
  case validateNumeric tokenBytes of
    Left err -> alexError (show err)
    Right _ -> fastMkToken (constructor pos (decodeUtf8 tokenBytes) []) tokenBytes
```

### 3.3 Streaming Input Processing
```haskell
-- Implement streaming for large files
data StreamingLexer = StreamingLexer
  { slChunkSize :: !Int           -- Process in chunks
  , slBuffer :: !ByteString       -- Current chunk
  , slPosition :: !Int            -- Global position
  , slTokens :: !(TBQueue FastToken) -- Token queue
  }

-- Streaming lexer interface
streamLex :: FilePath -> IO (TBQueue FastToken)
streamLex filePath = do
  queue <- newTBQueueIO 1000
  _ <- forkIO $ do
    handle <- openBinaryFile filePath ReadMode
    streamLexer handle queue
  return queue

streamLexer :: Handle -> TBQueue FastToken -> IO ()
streamLexer handle queue = do
  chunk <- BS.hGet handle chunkSize
  unless (BS.null chunk) $ do
    tokens <- lexChunk chunk
    mapM_ (atomically . writeTBQueue queue) tokens
    streamLexer handle queue
  where
    chunkSize = 64 * 1024  -- 64KB chunks
```

## Phase 4: Parser Integration (Week 5-6)

### 4.1 Happy Parser Updates
```haskell
-- Update grammar to use FastToken
%token
  'function'    { FastToken FunctionToken _ _ _ _ }
  'var'         { FastToken VarToken _ _ _ _ }
  'let'         { FastToken LetToken _ _ _ _ }
  IDENT         { FastToken IdentifierToken _ _ _ _ }
  NUM           { FastToken DecimalToken _ _ _ _ }

-- Enhanced error productions with recovery
Statement :: { JSStatement }
Statement : 
    'function' IDENT '(' ')' Block     { JSFunction $1 $2 [] $5 }
  | 'var' VarDecls ';'                 { JSVariable $1 $2 $3 }
  | error ';'                          {% recoverStatement $1 >> return JSEmpty }
  | error '}'                          {% recoverBlock $1 >> return JSEmpty }

-- Error recovery actions
recoverStatement :: FastToken -> Alex ()
recoverStatement errorToken = do
  err <- createSyntaxError errorToken [SemicolonToken, RightBraceToken]
  addError err
  skipUntil [SemicolonToken, RightBraceToken]
```

### 4.2 AST Integration
```haskell
-- Update AST to work with FastToken
data JSStatement
  = JSVariable !FastToken ![JSVariableDeclarator] !FastToken
  | JSFunction !FastToken !FastToken ![JSPattern] !JSBlock
  -- ... other constructors

-- Efficient text extraction from FastToken
fastTokenText :: FastToken -> Text
fastTokenText FastToken{ftText = Just txt} = txt
fastTokenText ft@FastToken{ftBytes = bytes} = 
  let txt = decodeUtf8 bytes
  in txt <$ unsafePerformIO (writeIORef (ftTextRef ft) (Just txt))
  where
    ftTextRef = undefined -- Need to add IORef to FastToken
```

## Phase 5: Memory Optimization (Week 6-7)

### 5.1 Compact Memory Representation
```haskell
-- Use compact regions for large ASTs
import Data.Compact

data CompactJS = CompactJS
  { cjsAST :: !(Compact JSAST)
  , cjsSource :: !(Compact ByteString)
  , cjsTokens :: !(Compact [FastToken])
  }

compactParse :: ByteString -> IO (Either [JSParseError] CompactJS)
compactParse source = do
  result <- parse source
  case result of
    Left errs -> return (Left errs)
    Right ast -> do
      compactAST <- compact ast
      compactSource <- compact source
      compactTokens <- compact (astTokens ast)
      return $ Right CompactJS
        { cjsAST = compactAST
        , cjsSource = compactSource  
        , cjsTokens = compactTokens
        }
```

### 5.2 Token Interning System
```haskell
-- Intern common tokens to reduce memory
import Data.HashTable.IO as HT

data TokenInternTable = TokenInternTable
  { titKeywords :: !(HT.BasicHashTable ByteString FastToken)
  , titIdentifiers :: !(HT.BasicHashTable ByteString FastToken)
  , titOperators :: !(HT.BasicHashTable ByteString FastToken)
  }

internToken :: TokenInternTable -> ByteString -> TokenType -> IO FastToken
internToken table bytes tokenType = do
  let hashTable = selectTable table tokenType
  maybeToken <- HT.lookup hashTable bytes
  case maybeToken of
    Just token -> return token
    Nothing -> do
      token <- createToken bytes tokenType
      HT.insert hashTable bytes token
      return token
```

## Phase 6: Advanced Error Features (Week 7-8)

### 6.1 IDE Integration Support
```haskell
-- LSP-compatible error reporting
data LSPDiagnostic = LSPDiagnostic
  { lspRange :: !LSPRange
  , lspSeverity :: !DiagnosticSeverity
  , lspCode :: !(Maybe Text)
  , lspMessage :: !Text
  , lspRelatedInformation :: ![DiagnosticRelatedInformation]
  }

convertToLSP :: JSParseError -> SourceText -> LSPDiagnostic
convertToLSP JSParseError{..} source = LSPDiagnostic
  { lspRange = sourceLocationToRange jpeLocation
  , lspSeverity = errorTypeToSeverity jpeType
  , lspCode = Just (errorCode jpeType)
  , lspMessage = jpeMessage
  , lspRelatedInformation = generateRelatedInfo jpeContext source
  }
```

### 6.2 Smart Error Recovery
```haskell
-- Machine learning-inspired error recovery
data RecoveryHeuristics = RecoveryHeuristics
  { rhCommonMistakes :: !(Map ErrorPattern RecoveryAction)
  , rhContextPatterns :: !(Map ErrorContext [RecoveryStrategy])
  , rhSuccessRates :: !(Map RecoveryAction Double)
  }

smartRecover :: RecoveryHeuristics -> JSParseError -> ErrorContext -> RecoveryAction
smartRecover heuristics err ctx =
  let pattern = extractErrorPattern err
      strategies = Map.lookup ctx (rhContextPatterns heuristics)
      bestStrategy = maximumBy (comparing successRate) strategies
  in selectAction pattern bestStrategy
  where
    successRate = flip Map.lookup (rhSuccessRates heuristics)
```

## Phase 7: API Compatibility & Migration (Week 8-9)

### 7.1 Backward Compatibility Layer
```haskell
-- Maintain old String-based API
module Language.JavaScript.Parser.Legacy where

-- Wrapper functions for backward compatibility
parse :: String -> String -> Either String JSAST
parse input filename = 
  case fastParse (encodeUtf8 (Text.pack input)) filename of
    Left errs -> Left (formatErrors errs)
    Right ast -> Right (convertAST ast)

-- Conversion functions
convertAST :: FastJSAST -> JSAST
convertAST = undefined -- Convert FastToken back to old Token

fastToString :: FastToken -> String
fastToString = Text.unpack . fastTokenText
```

### 7.2 Migration Guide Generation
```markdown
# Migration Guide: String to ByteString Parser

## Breaking Changes
1. `Token` replaced with `FastToken`
2. `parse` function now returns `JSParseError` instead of `String`
3. Source input now `ByteString` instead of `String`

## Migration Steps
1. Replace `import Language.JavaScript.Parser` with `import Language.JavaScript.Parser.Fast`
2. Convert input: `Text.encodeUtf8 . Text.pack` for String inputs
3. Update error handling to use structured `JSParseError`
4. Use `fastTokenText` to extract text from tokens

## Performance Gains
- 5x faster parsing on large files
- 3x lower memory usage
- Better error messages with source context
```

## Phase 8: Testing & Validation (Week 9-10)

### 8.1 Comprehensive Test Suite
```haskell
-- Performance regression tests
performanceTests :: Spec
performanceTests = describe "Performance Tests" $ do
  it "jQuery parsing under 200ms" $ do
    (time, result) <- timeIt (fastParse jquerySource "jquery.js")
    time `shouldSatisfy` (< 200)
    result `shouldSatisfy` isRight
    
  it "Throughput > 1MB/s" $ do
    let largeSource = generateLargeJS (1024 * 1024) -- 1MB
    (time, _) <- timeIt (fastParse largeSource "large.js")
    let throughput = fromIntegral (BS.length largeSource) / time
    throughput `shouldSatisfy` (> 1e6) -- 1MB/s

-- Error quality tests  
errorQualityTests :: Spec
errorQualityTests = describe "Error Quality Tests" $ do
  it "provides helpful suggestions for common mistakes" $ do
    case fastParse "function foo( { return 42; }" "test.js" of
      Left [err] -> do
        jpeMessage err `shouldContain` "missing closing parenthesis"
        jpeSuggestions err `shouldContain` "Add ')' after 'foo('"
      _ -> expectationFailure "Expected parse error"
```

### 8.2 Benchmark Comparisons
```haskell
-- Compare against other JavaScript parsers
benchmarkSuite :: Benchmark  
benchmarkSuite = bgroup "JavaScript Parsers"
  [ bgroup "Small files (< 10KB)"
    [ bench "language-javascript (old)" $ nf parseOld smallJS
    , bench "language-javascript (new)" $ nf parseNew smallJS
    , bench "acorn (node.js)" $ nfIO (parseAcorn smallJS)
    ]
  , bgroup "Large files (> 1MB)"
    [ bench "language-javascript (new)" $ nf parseNew largeJS
    , bench "babel parser" $ nfIO (parseBabel largeJS)
    ]
  ]
```

## 📋 Implementation Checklist

### Phase 1: Foundation ✅
- [ ] Set up profiling infrastructure
- [ ] Create performance baseline measurements
- [ ] Design FastToken architecture
- [ ] Plan memory layout optimizations

### Phase 2: Error System ✅
- [ ] Implement JSParseError types
- [ ] Create SourceSnippet context system
- [ ] Build error message formatter
- [ ] Add suggestion generation logic

### Phase 3: ByteString Lexer ✅
- [ ] Convert Alex to monad-bytestring wrapper
- [ ] Implement FastToken generation
- [ ] Add streaming input support
- [ ] Optimize token validation

### Phase 4: Parser Integration ✅
- [ ] Update Happy grammar for FastToken
- [ ] Implement error recovery actions
- [ ] Convert AST to use FastToken
- [ ] Add text extraction utilities

### Phase 5: Memory Optimization ✅
- [ ] Implement compact regions
- [ ] Add token interning system
- [ ] Optimize memory layout
- [ ] Add garbage collection hints

### Phase 6: Advanced Errors ✅
- [ ] Add LSP diagnostic support
- [ ] Implement smart recovery
- [ ] Create context-aware suggestions
- [ ] Add fix recommendations

### Phase 7: Compatibility ✅
- [ ] Create backward compatibility layer
- [ ] Write migration utilities
- [ ] Generate migration guide
- [ ] Test compatibility

### Phase 8: Testing ✅
- [ ] Performance regression suite
- [ ] Error quality validation
- [ ] Benchmark comparisons
- [ ] Integration testing

## 🎯 Expected Results

### Performance Improvements
| Metric | Before | Target | Expected After |
|--------|--------|--------|----------------|
| jQuery parse | 483ms | <200ms | ~150ms (3.2x) |
| Throughput | 0.65 MB/s | >1 MB/s | ~2.5 MB/s (3.8x) |
| 1MB files | 1362ms | <1000ms | ~400ms (3.4x) |
| 10MB files | 10.3s | <2s | ~1.6s (6.4x) |
| Memory usage | 12x input | <5x input | ~3x input (4x improvement) |

### Error Quality Improvements
- **Rich context**: Source snippets with line numbers and highlighting
- **Smart suggestions**: Context-aware fix recommendations
- **Better recovery**: Multiple sync points and insertion strategies
- **IDE integration**: LSP-compatible diagnostics
- **Helpful messages**: Plain English explanations with examples

This plan transforms the language-javascript parser from a String-based system to a high-performance ByteString system while dramatically improving error quality. The phased approach allows for iterative testing and validation at each step.