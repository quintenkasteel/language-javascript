# Plan 10: Use `byteStringOf` for Zero-Copy Token Capture

## Summary

Replace character-by-character token building in the Flatparse lexer with `FP.byteStringOf` for zero-copy slicing directly from the input buffer. This is the core optimization that makes the ByteString AST migration (Plan 06) worthwhile.

## Current Problem

The lexer builds tokens character by character:
```haskell
identifier :: JSParser Text
identifier = do
  first <- satisfy isIdentifierStart
  rest <- many (satisfy isIdentifierContinue)
  let ident = Text.pack (first : rest)  -- allocates [Char] list, then copies to Text
  ...
```

This requires: `[Char] allocation → Text.pack copy → Text.unpack copy → String allocation` (3 allocations per token).

## Solution: `byteStringOf`

```haskell
byteStringOf :: ParserT st e a -> ParserT st e ByteString
```

Runs a parser and returns the consumed input span as a `ByteString` **sharing memory with the original input buffer**. Zero allocation, zero copy.

## Changes

**File:** `src/Language/JavaScript/Parser/Flatparse/Lexer.hs`

### Identifier
```haskell
-- Before:
identifier :: JSParser Text
identifier = do
  first <- satisfy isIdentifierStart
  rest <- many (satisfy isIdentifierContinue)
  let ident = Text.pack (first : rest)
  if isKeyword ident then empty else pure ident

-- After:
identifier :: JSParser ByteString
identifier = do
  bs <- FP.byteStringOf $ do
    _ <- satisfy isIdentifierStart
    FP.skipMany (satisfy isIdentifierContinue)
  if isKeywordBS bs then FP.empty else pure bs
```

### Numeric Literals
```haskell
-- Before:
decimalLiteral :: JSParser Text
decimalLiteral = do
  digits <- some (satisfy isDigit)
  ...
  pure (Text.pack ...)

-- After:
decimalLiteral :: JSParser ByteString
decimalLiteral = FP.byteStringOf $ do
  FP.skipSome (satisfy isDigit)
  FP.optional_ $ do
    _ <- FP.word8 0x2E  -- '.'
    FP.skipMany (satisfy isDigit)
  FP.optional_ scientificSuffix
```

### String Literals
```haskell
-- Before:
stringLiteral :: JSParser Text
stringLiteral = singleQuotedString FP.<|> doubleQuotedString

-- After:
stringLiteral :: JSParser ByteString
stringLiteral = FP.byteStringOf (singleQuotedString FP.<|> doubleQuotedString)
```

**Note for strings:** `byteStringOf` captures the raw bytes including escape sequences. If the AST needs processed escape sequences (e.g., `\n` → newline), additional processing is needed. For the AST representation, storing the raw source bytes is correct — escape processing happens at runtime/evaluation, not during parsing.

### All Functions to Convert

| Function | Line | From | To |
|----------|------|------|----|
| `stringLiteral` | 148 | `JSParser Text` | `JSParser ByteString` via `byteStringOf` |
| `singleQuotedString` | 152 | `JSParser Text` | `JSParser ByteString` |
| `doubleQuotedString` | 160 | `JSParser Text` | `JSParser ByteString` |
| `numericLiteral` | 239 | `JSParser Text` | `JSParser ByteString` via `byteStringOf` |
| `decimalLiteral` | 252 | `JSParser Text` | `JSParser ByteString` |
| `hexLiteral` | 290 | `JSParser Text` | `JSParser ByteString` |
| `binaryLiteral` | 298 | `JSParser Text` | `JSParser ByteString` |
| `octalLiteral` | 308 | `JSParser Text` | `JSParser ByteString` |
| `bigIntLiteral` | 317 | `JSParser Text` | `JSParser ByteString` |
| `scientificNotation` | 273 | `JSParser Text` | `JSParser ByteString` |
| `identifier` | 367 | `JSParser Text` | `JSParser ByteString` via `byteStringOf` |
| `rawIdentifier` | 389 | `JSParser Text` | `JSParser ByteString` |

### Keyword Checking

```haskell
-- Before:
isKeyword :: Text -> Bool
isKeyword t = Set.member t keywordSet

-- After:
isKeywordBS :: ByteString -> Bool
isKeywordBS bs = Set.member bs keywordSetBS
  where
    keywordSetBS = Set.fromList (map Text.encodeUtf8 keywordList)
```

## Coordination with Other Plans

- **Plan 06 (AST to ByteString):** This plan feeds directly into Plan 06. The lexer returns ByteString, Grammar.hs passes it directly to AST constructors.
- **Plan 07 (Remove fixPositions):** Independent — can be done in either order.

## Files Changed

| File | Change |
|------|--------|
| `src/.../Flatparse/Lexer.hs` | Convert 12 functions to use `byteStringOf`, return `ByteString` |
| `src/.../Flatparse/Grammar.hs` | Update call sites (already covered by Plan 06) |

## Verification

- `grep -rn 'Text\.pack' src/Language/JavaScript/Parser/Flatparse/Lexer.hs` returns 0 results
- All lexer functions return `ByteString`
- Parsing produces identical AST structure (just with ByteString instead of String)
- Memory benchmark shows reduced allocation rate
