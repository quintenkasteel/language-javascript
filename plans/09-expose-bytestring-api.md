# Plan 09: Expose ByteString Entry Points in Public API

## Summary

The Flatparse parser already has `parseProgramByteString` and `parseExpressionByteString` internally. Expose these through the main `Language.JavaScript.Parser` module so consumers can skip the String → Text → ByteString conversion chain.

## Current State

**Internal (Flatparse/Parser.hs):**
```haskell
parseProgramByteString :: ByteString -> ParseResult JSAST
parseExpressionByteString :: ByteString -> ParseResult JSAST
parseModuleProgramByteString :: ByteString -> ParseResult JSAST
```

**Public (Parser.hs):**
```haskell
parse :: String -> String -> Either String JSAST       -- String only!
parseModule :: String -> String -> Either String JSAST  -- String only!
```

## Changes

### Add to `src/Language/JavaScript/Parser/Parser.hs`:

```haskell
-- | Parse a JavaScript program from a UTF-8 encoded ByteString.
-- This is the most efficient entry point, avoiding String/Text conversion.
parseBS :: ByteString -> Either String JSAST
parseBS input =
  case FlatParser.parseProgramByteString input of
    FlatParser.ParseOK success -> Right (FlatParser.parseResult success)
    FlatParser.ParseError failure -> Left (show (FlatParser.parseError failure))

-- | Parse a JavaScript module from a UTF-8 encoded ByteString.
parseModuleBS :: ByteString -> Either String JSAST
parseModuleBS input =
  case FlatParser.parseModuleProgramByteString input of
    FlatParser.ParseOK success -> Right (FlatParser.parseResult success)
    FlatParser.ParseError failure -> Left (show (FlatParser.parseError failure))

-- | Parse a JavaScript expression from a UTF-8 encoded ByteString.
parseExpressionBS :: ByteString -> Either String JSAST
parseExpressionBS input =
  case FlatParser.parseExpressionByteString input of
    FlatParser.ParseOK success -> Right (FlatParser.parseResult success)
    FlatParser.ParseError failure -> Left (show (FlatParser.parseError failure))

-- | Read and parse a JavaScript file efficiently using ByteString IO.
parseFileBS :: FilePath -> IO (Either String JSAST)
parseFileBS path = parseBS <$> BS.readFile path
```

### Add to `src/Language/JavaScript/Parser.hs` exports:

```haskell
module Language.JavaScript.Parser
  ( ...
  , parseBS
  , parseModuleBS
  , parseExpressionBS
  , parseFileBS
  ) where
```

### Update existing String-based API to use ByteString internally:

```haskell
parse :: String -> String -> Either String JSAST
parse input _srcName = parseBS (Text.encodeUtf8 (Text.pack input))
```

## Files Changed

| File | Change |
|------|--------|
| `src/Language/JavaScript/Parser/Parser.hs` | Add 4 new functions |
| `src/Language/JavaScript/Parser.hs` | Export new functions |

## Verification

- New functions are exported and documented with Haddock
- `parseBS` produces identical results to `parse` for the same input
- `parseFileBS` reads and parses without intermediate String allocation
