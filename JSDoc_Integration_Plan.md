# JSDoc Integration Plan for language-javascript

## Executive Summary

This document provides a comprehensive plan for integrating JSDoc parsing capabilities into the language-javascript Haskell library. The integration will extend the existing comment annotation system to parse and validate JSDoc comments, making structured documentation information available through the AST.

## Current Architecture Analysis

### Existing Comment System

The library already has a robust foundation for comment handling:

1. **Token Level**: `CommentAnnotation` data type in `Token.hs`
   ```haskell
   data CommentAnnotation
     = CommentA TokenPosn String
     | WhiteSpace TokenPosn String
     | NoComment
   ```

2. **Lexer Level**: Alex lexer in `Lexer.x` recognizes comment patterns
   - Single-line comments: `//...`
   - Multi-line comments: `/* ... */`
   - Comments stored in `AlexUserState.comment :: [Token]`

3. **AST Level**: `JSAnnot` stores position and comment information
   ```haskell
   data JSAnnot
     = JSAnnot !TokenPosn ![CommentAnnotation]
     | JSAnnotSpace
     | JSNoAnnot
   ```

### Integration Point

JSDoc comments are standard JavaScript block comments (`/** ... */`) with structured content. The integration will:
- Detect JSDoc patterns during lexical analysis
- Parse JSDoc content into structured data
- Attach parsed JSDoc to appropriate AST nodes
- Provide validation for JSDoc tags and types

## JSDoc Data Model Design

### Core JSDoc AST Types

```haskell
-- | JSDoc comment structure
data JSDocComment = JSDocComment
  { _jsDocPosition :: !TokenPosn
  , _jsDocDescription :: !(Maybe Text)
  , _jsDocTags :: ![JSDocTag]
  } deriving (Eq, Show, Generic, NFData)

-- | JSDoc tag representation
data JSDocTag = JSDocTag
  { _jsDocTagName :: !Text
  , _jsDocTagType :: !(Maybe JSDocType)
  , _jsDocTagName :: !(Maybe Text)
  , _jsDocTagDescription :: !(Maybe Text)
  , _jsDocTagPosition :: !TokenPosn
  } deriving (Eq, Show, Generic, NFData)

-- | JSDoc type expressions
data JSDocType
  = JSDocBasicType !Text                      -- string, number, boolean
  | JSDocArrayType !JSDocType                 -- Array<T>
  | JSDocUnionType ![JSDocType]               -- T | U | V
  | JSDocObjectType ![JSDocObjectField]       -- {prop: type}
  | JSDocFunctionType ![JSDocType] !JSDocType -- (arg1, arg2) => RetType
  | JSDocGenericType !Text ![JSDocType]       -- Map<K, V>
  | JSDocOptionalType !JSDocType              -- T?
  | JSDocNullableType !JSDocType              -- ?T
  | JSDocNonNullableType !JSDocType           -- !T
  deriving (Eq, Show, Generic, NFData)

-- | Object field in JSDoc type
data JSDocObjectField = JSDocObjectField
  { _jsDocFieldName :: !Text
  , _jsDocFieldType :: !JSDocType
  , _jsDocFieldOptional :: !Bool
  } deriving (Eq, Show, Generic, NFData)

-- Make lenses for all types
makeLenses ''JSDocComment
makeLenses ''JSDocTag
makeLenses ''JSDocObjectField
```

### Extended Comment Annotation

```haskell
-- Extend existing CommentAnnotation to include JSDoc
data CommentAnnotation
  = CommentA TokenPosn String
  | WhiteSpace TokenPosn String
  | JSDocA TokenPosn JSDocComment  -- New JSDoc variant
  | NoComment
  deriving (Eq, Generic, NFData, Show, Typeable, Data, Read)
```

## Implementation Plan

### Phase 1: JSDoc Detection and Lexing (Week 1)

**Files to modify:**
- `src/Language/JavaScript/Parser/Lexer.x`
- `src/Language/JavaScript/Parser/LexerUtils.hs`

**Tasks:**
1. Add JSDoc comment pattern recognition
   ```alex
   -- JSDoc comment pattern (/** ... */)
   <reg,divide> "/**" (($MultiLineNotAsteriskChar)*| ("*")+ ($MultiLineNotForwardSlashOrAsteriskChar) )* ("*")+ "/"
     { adapt (mkString jsDocCommentToken) }
   ```

2. Implement `jsDocCommentToken` function
   ```haskell
   jsDocCommentToken :: TokenPosn -> String -> Token
   jsDocCommentToken loc content =
     case parseJSDocContent content of
       Right jsDoc -> CommentToken loc content [JSDocA loc jsDoc]
       Left _ -> CommentToken loc content [CommentA loc content]
   ```

### Phase 2: JSDoc Parser Implementation (Week 2)

**New file:** `src/Language/JavaScript/Parser/JSDoc.hs`

**Core functions:**
```haskell
-- | Parse JSDoc content from comment string
parseJSDocContent :: String -> Either JSDocError JSDocComment
parseJSDocContent content =
  runParser jsDocCommentParser (Text.pack content)

-- | Main JSDoc comment parser
jsDocCommentParser :: Parser JSDocComment
jsDocCommentParser = do
  description <- optional parseDescription
  tags <- many parseJSDocTag
  pure (JSDocComment position description tags)

-- | Parse individual JSDoc tags
parseJSDocTag :: Parser JSDocTag
parseJSDocTag = do
  skipWhitespace
  char '@'
  tagName <- parseTagName
  case tagName of
    "param" -> parseParamTag tagName
    "returns" -> parseReturnsTag tagName
    "type" -> parseTypeTag tagName
    _ -> parseGenericTag tagName
```

**Supported Tags (60+ standard JSDoc tags):**
- Core: `@param`, `@returns`, `@type`, `@description`
- Functions: `@function`, `@method`, `@callback`, `@async`
- Classes: `@class`, `@constructor`, `@extends`, `@implements`
- Modules: `@module`, `@namespace`, `@exports`, `@imports`
- Types: `@typedef`, `@enum`, `@interface`, `@generic`
- Access: `@public`, `@private`, `@protected`, `@readonly`
- Lifecycle: `@deprecated`, `@since`, `@version`
- Documentation: `@author`, `@see`, `@example`, `@todo`

### Phase 3: Type Expression Parser (Week 2)

**Type parsing functions:**
```haskell
-- | Parse JSDoc type expressions
parseJSDocType :: Parser JSDocType
parseJSDocType = choice
  [ parseUnionType
  , parseArrayType
  , parseObjectType
  , parseFunctionType
  , parseGenericType
  , parseBasicType
  ]

-- | Parse union types: string | number | boolean
parseUnionType :: Parser JSDocType
parseUnionType = JSDocUnionType <$> sepBy1 parseSimpleType (symbol "|")

-- | Parse array types: Array<T>, T[], Array
parseArrayType :: Parser JSDocType
parseArrayType = choice
  [ JSDocArrayType <$> (parseSimpleType <* symbol "[]")
  , do
      void (symbol "Array")
      optional (between (symbol "<") (symbol ">") parseJSDocType)
        >>= \case
          Nothing -> pure (JSDocBasicType "Array")
          Just t -> pure (JSDocArrayType t)
  ]
```

### Phase 4: AST Integration (Week 3)

**Modify:** `src/Language/JavaScript/Parser/AST.hs`

**Integration strategies:**
1. **Function-level JSDoc**: Attach to function declarations/expressions
   ```haskell
   data JSFunction a = JSFunction
     { _jsFunctionAnnot :: a
     , _jsFunctionIdent :: JSIdent a
     , _jsFunctionLParen :: a
     , _jsFunctionParams :: [JSExpression a]
     , _jsFunctionRParen :: a
     , _jsFunctionBody :: JSBlock a
     , _jsFunctionJSDoc :: Maybe JSDocComment  -- New field
     } deriving (Eq, Show, Generic, NFData)
   ```

2. **Variable-level JSDoc**: Attach to variable declarations
   ```haskell
   data JSVarInitializer a = JSVarInitializer
     { _jsVarInitializerName :: JSExpression a
     , _jsVarInitializerEqual :: a
     , _jsVarInitializerExpr :: JSExpression a
     , _jsVarInitializerJSDoc :: Maybe JSDocComment  -- New field
     } deriving (Eq, Show, Generic, NFData)
   ```

3. **Class-level JSDoc**: Attach to class declarations
   ```haskell
   data JSClass a = JSClass
     { _jsClassAnnot :: a
     , _jsClassExtends :: Maybe (JSExpression a)
     , _jsClassLCurly :: a
     , _jsClassBody :: [JSClassElement a]
     , _jsClassRCurly :: a
     , _jsClassJSDoc :: Maybe JSDocComment  -- New field
     } deriving (Eq, Show, Generic, NFData)
   ```

### Phase 5: Parser Integration (Week 3)

**Modify:** `src/Language/JavaScript/Parser/Parser.hs`

**Comment-to-AST association logic:**
```haskell
-- | Extract JSDoc from preceding comments
extractJSDoc :: [CommentAnnotation] -> Maybe JSDocComment
extractJSDoc comments = listToMaybe
  [ jsDoc | JSDocA _ jsDoc <- reverse comments ]

-- | Attach JSDoc to function declarations
parseFunctionDeclaration :: Parser (JSStatement a)
parseFunctionDeclaration = do
  comments <- getCurrentComments
  func <- parseFunctionDeclarationBase
  let jsDoc = extractJSDoc comments
  pure (func & jsFunctionJSDoc .~ jsDoc)
```

### Phase 6: Validation Functions (Week 4)

**New file:** `src/Language/JavaScript/Parser/JSDocValidation.hs`

**Validation functions:**
```haskell
-- | Validate JSDoc comment for consistency
validateJSDocComment :: JSDocComment -> [JSDocValidationError]
validateJSDocComment jsDoc = concat
  [ validateRequiredTags jsDoc
  , validateTypeConsistency jsDoc
  , validateParameterConsistency jsDoc
  , validateTagCombinations jsDoc
  ]

-- | Check parameter consistency between @param tags and function signature
validateParameterConsistency :: JSDocComment -> JSFunction a -> [JSDocValidationError]
validateParameterConsistency jsDoc func =
  let paramTags = getParamTags jsDoc
      functionParams = getFunctionParams func
  in checkParameterAlignment paramTags functionParams

-- | Validate type expressions
validateJSDocType :: JSDocType -> [JSDocValidationError]
validateJSDocType jsDocType = case jsDocType of
  JSDocBasicType name -> validateBasicTypeName name
  JSDocUnionType types -> concatMap validateJSDocType types
  JSDocObjectType fields -> concatMap validateObjectField fields
  JSDocFunctionType args ret ->
    concatMap validateJSDocType args ++ validateJSDocType ret
```

### Phase 7: Pretty Printing & Serialization (Week 4)

**Modify:** `src/Language/JavaScript/Pretty/Printer.hs`

**JSDoc rendering:**
```haskell
-- | Render JSDoc comment back to text
renderJSDocComment :: JSDocComment -> Text
renderJSDocComment jsDoc = Text.unlines $ concat
  [ ["/**"]
  , maybeToList (fmap (" * " <>) (_jsDocDescription jsDoc))
  , if null (_jsDocTags jsDoc) then [] else [" *"]
  , map renderJSDocTag (_jsDocTags jsDoc)
  , [" */"]
  ]

-- | Render individual JSDoc tags
renderJSDocTag :: JSDocTag -> Text
renderJSDocTag tag = Text.concat
  [ " * @", _jsDocTagName tag
  , maybe "" ((" {" <>) . (<> "}") . renderJSDocType) (_jsDocTagType tag)
  , maybe "" (" " <>) (_jsDocTagName tag)
  , maybe "" (" " <>) (_jsDocTagDescription tag)
  ]
```

## Testing Strategy

### Unit Tests

**File:** `test/Test/Language/Javascript/JSDocParser.hs`

```haskell
jsDocParserTests :: Spec
jsDocParserTests = describe "JSDoc Parser Tests" $ do
  describe "basic JSDoc parsing" $ do
    it "parses simple function documentation" $ do
      let input = "/** Add two numbers @param {number} a First number @param {number} b Second number @returns {number} Sum */"
      parseJSDocContent input `shouldSatisfy` isRight

  describe "type expression parsing" $ do
    it "parses union types" $ do
      parseJSDocType "string | number | boolean" `shouldBe`
        Right (JSDocUnionType [JSDocBasicType "string", JSDocBasicType "number", JSDocBasicType "boolean"])

    it "parses array types" $ do
      parseJSDocType "Array<string>" `shouldBe`
        Right (JSDocArrayType (JSDocBasicType "string"))
```

### Integration Tests

**File:** `test/Test/Language/Javascript/JSDocIntegration.hs`

```haskell
jsDocIntegrationTests :: Spec
jsDocIntegrationTests = describe "JSDoc Integration Tests" $ do
  it "attaches JSDoc to function declarations" $ do
    let input = Text.unlines
          [ "/** Add two numbers"
          , " * @param {number} a First number  "
          , " * @param {number} b Second number"
          , " * @returns {number} Sum"
          , " */"
          , "function add(a, b) { return a + b; }"
          ]
    case parseProgram input of
      Right ast ->
        getJSDocFromFunction ast `shouldSatisfy` isJust
      Left err -> expectationFailure ("Parse failed: " ++ show err)
```

### Property Tests

**File:** `test/Test/Language/Javascript/JSDocProperties.hs`

```haskell
jsDocPropertyTests :: Spec
jsDocPropertyTests = describe "JSDoc Property Tests" $ do
  it "round-trip property: parse then render preserves content" $ property $ \validJSDoc ->
    case parseJSDocContent validJSDoc of
      Right jsDoc ->
        parseJSDocContent (Text.unpack (renderJSDocComment jsDoc)) `shouldBe` Right jsDoc
      Left _ -> True
```

### Golden Tests

**File:** `test/Test/Language/Javascript/JSDocGolden.hs`

Store reference JSDoc parsing outputs for regression testing.

## Performance Considerations

### Optimization Strategies

1. **Lazy Parsing**: Parse JSDoc content only when accessed
   ```haskell
   data JSDocComment = JSDocComment
     { _jsDocRawContent :: !Text
     , _jsDocParsed :: !(IORef (Maybe (Either JSDocError JSDocParsedContent)))
     }
   ```

2. **Caching**: Cache parsed JSDoc results per comment
3. **Streaming**: Handle large files with many JSDoc comments efficiently
4. **Memory**: Use strict fields and optimize data structures

### Benchmarking

Track parsing performance impact:
- JSDoc parsing time vs. total parse time
- Memory usage with JSDoc vs. without
- Large file handling (files with 1000+ JSDoc comments)

## Migration Strategy

### Backward Compatibility

1. **Existing API**: All existing functions remain unchanged
2. **Optional JSDoc**: JSDoc fields are `Maybe` types, defaulting to `Nothing`
3. **Incremental Adoption**: Users can opt-in to JSDoc parsing via flags

### Configuration

```haskell
data ParseOptions = ParseOptions
  { _parseOptionsJSDocEnabled :: Bool
  , _parseOptionsJSDocValidation :: Bool
  , _parseOptionsJSDocStrict :: Bool
  } deriving (Eq, Show)

-- Default: JSDoc disabled for backward compatibility
defaultParseOptions :: ParseOptions
defaultParseOptions = ParseOptions False False False
```

## Error Handling

### JSDoc-Specific Errors

```haskell
data JSDocError
  = JSDocSyntaxError !TokenPosn !Text
  | JSDocTypeParseError !TokenPosn !Text
  | JSDocValidationError !JSDocValidationError
  | JSDocUnknownTag !TokenPosn !Text
  deriving (Eq, Show)

data JSDocValidationError
  = MissingRequiredTag !Text
  | DuplicateTag !Text
  | ParameterMismatch !Text !Text
  | InvalidType !Text
  | IncompatibleTags !Text !Text
  deriving (Eq, Show)
```

### Error Recovery

- Continue parsing even with malformed JSDoc
- Collect multiple JSDoc errors per file
- Provide helpful error messages with suggestions

## Documentation

### Module Documentation

Complete Haddock documentation for all JSDoc-related modules:
- `Language.JavaScript.Parser.JSDoc`
- `Language.JavaScript.Parser.JSDocValidation`
- API examples and usage patterns

### User Guide

Documentation covering:
- Enabling JSDoc parsing
- Accessing JSDoc from AST
- Writing JSDoc validation rules
- Performance implications
- Migration from other tools

## Timeline

### 4-Week Implementation Schedule

**Week 1: Foundation**
- JSDoc detection in lexer
- Basic comment parsing infrastructure
- Core data types and lenses

**Week 2: Parsing**
- JSDoc content parser
- Type expression parsing
- Tag parsing for all 60+ standard tags

**Week 3: Integration**
- AST integration
- Parser modifications
- Comment-to-AST association logic

**Week 4: Validation & Polish**
- Validation functions
- Pretty printing
- Comprehensive testing
- Documentation

### Success Metrics

1. **Functionality**: Parse and validate all standard JSDoc tags
2. **Performance**: <10% parsing performance impact
3. **Coverage**: 90%+ test coverage for JSDoc modules
4. **Compatibility**: Zero breaking changes to existing API
5. **Quality**: Pass all existing tests + comprehensive JSDoc tests

## Future Enhancements

### Post-MVP Features

1. **TypeScript Support**: Parse TypeScript-style type annotations
2. **Custom Tags**: Support for project-specific JSDoc tags
3. **IDE Integration**: Language server protocol support
4. **Documentation Generation**: HTML/Markdown output
5. **Type Checking**: Runtime type validation from JSDoc

### Advanced Type System

1. **Template Types**: Generic type parameters
2. **Conditional Types**: Conditional type expressions
3. **Mapped Types**: Object type transformations
4. **Utility Types**: Built-in utility types (Partial<T>, Pick<T, K>)

## Conclusion

This comprehensive JSDoc integration plan provides a robust foundation for adding structured documentation parsing to the language-javascript library. The design leverages the existing comment infrastructure while adding powerful new capabilities for documentation analysis and validation.

The phased implementation approach ensures minimal disruption to existing code while providing immediate value through incremental JSDoc support. The extensive testing strategy and performance considerations ensure production-ready quality.

With this implementation, language-javascript will offer best-in-class JSDoc parsing capabilities, enabling rich documentation analysis and tooling in the Haskell ecosystem.