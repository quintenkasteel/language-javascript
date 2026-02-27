# Plan 06: Switch AST from String to ByteString

## Summary

Replace all 21 `!String` fields in the AST, all 49 `!String` fields in Token constructors, and 2 `String` fields in `CommentAnnotation` with strict `ByteString`. This eliminates the `ByteString -> [Char] -> Text -> [Char] -> String` conversion chain and enables zero-copy slicing from FlatParse's input buffer via `byteStringOf`.

**Expected impact:** 8-12x memory reduction for string storage, elimination of 5-6 O(n) copies per token, throughput improvement from ~0.5 MB/s to ~5-10 MB/s.

## Why ByteString (Not Text)

- FlatParse operates on `ByteString` natively — `byteStringOf` returns zero-copy `ByteString` slices of the input buffer
- Zero allocation: `byteStringOf` shares memory with the original input, no copying
- The blaze-builder in Printer.hs already produces `ByteString` internally
- `ShortByteString` copies data (defeating zero-copy), `Text` requires UTF-16 re-encoding from UTF-8
- For typical parse-transform-render pipelines, slices that share the input buffer are optimal

**Caveat:** If ASTs are cached long-term, retained slices keep the entire input alive. Use `BS.copy` at storage boundaries for that use case.

## Complete Change Inventory

### Phase A: AST.hs (21 fields)

**File:** `src/Language/JavaScript/Parser/AST.hs`

Change `!String` to `!ByteString` in:

| Line | Constructor | Field |
|------|-------------|-------|
| 142 | `JSImportDeclarationBare` | module specifier |
| 170 | `JSFromClause` | from path |
| 296 | `JSIdentifier` | name |
| 297 | `JSDecimal` | value |
| 298 | `JSLiteral` | value |
| 299 | `JSHexInteger` | value |
| 300 | `JSBinaryInteger` | value |
| 301 | `JSOctal` | value |
| 302 | `JSBigIntLiteral` | value |
| 303 | `JSStringLiteral` | value |
| 304 | `JSRegEx` | value |
| 356 | `JSTemplateLiteral` | raw string |
| 479 | `JSPropertyIdentRef` | name |
| 496 | `JSPropertyIdent` | name |
| 497 | `JSPropertyString` | value |
| 498 | `JSPropertyNumber` | value |
| 512 | `JSIdentName` | name |
| 538 | `JSTemplatePart` | raw string |
| 551 | `JSPrivateField` | name |
| 553 | `JSPrivateMethod` | name |
| 555 | `JSPrivateAccessor` | name |

Add import:
```haskell
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
```

Update `ShowStripped` class instances to decode ByteString for display:
```haskell
-- Helper for ShowStripped
bsToString :: ByteString -> String
bsToString = Text.unpack . Text.decodeUtf8
```

### Phase B: Token.hs (49 constructors + 2 CommentAnnotation)

**File:** `src/Language/JavaScript/Parser/Token.hs`

Change `tokenLiteral :: !String` to `tokenLiteral :: !ByteString` in all 49 literal-bearing token constructors (lines 523-667).

Change `CommentAnnotation` fields (lines 510-514):
```haskell
-- Before:
data CommentAnnotation
  = CommentA TokenPosn String
  | WhiteSpace TokenPosn String

-- After:
data CommentAnnotation
  = CommentA !TokenPosn !ByteString
  | WhiteSpace !TokenPosn !ByteString
```

Note: Also add missing strictness annotations (`!`) to fix the lazy-field issue.

### Phase C: Lexer.hs (12 functions → ByteString)

**File:** `src/Language/JavaScript/Parser/Flatparse/Lexer.hs`

Replace character-by-character Text building with `byteStringOf` for zero-copy capture:

```haskell
-- Before (3 allocations: [Char], Text, then Text.unpack to String):
identifier :: JSParser Text
identifier = do
  first <- satisfy isIdentifierStart
  rest <- many (satisfy isIdentifierContinue)
  let ident = Text.pack (first : rest)
  if isKeyword ident then empty else pure ident

-- After (0 allocations: slice of input buffer):
identifier :: JSParser ByteString
identifier = do
  bs <- byteStringOf $ do
    _ <- satisfy isIdentifierStart
    skipMany (satisfy isIdentifierContinue)
  if isKeywordBS bs then empty else pure bs
```

Functions to change:

| Function | Line | Change |
|----------|------|--------|
| `stringLiteral` | 148 | Return `ByteString` via `byteStringOf` |
| `singleQuotedString` | 152 | Return `ByteString` |
| `doubleQuotedString` | 160 | Return `ByteString` |
| `numericLiteral` | 239 | Return `ByteString` via `byteStringOf` |
| `decimalLiteral` | 252 | Return `ByteString` |
| `hexLiteral` | 290 | Return `ByteString` |
| `binaryLiteral` | 298 | Return `ByteString` |
| `octalLiteral` | 308 | Return `ByteString` |
| `bigIntLiteral` | 317 | Return `ByteString` |
| `scientificNotation` | 273 | Return `ByteString` |
| `identifier` | 367 | Return `ByteString` via `byteStringOf` |
| `rawIdentifier` | 389 | Return `ByteString` |

**Keyword checking:** `isKeyword` currently takes `Text`. Change to `isKeywordBS :: ByteString -> Bool` using ByteString comparison.

### Phase D: Grammar.hs (28 `Text.unpack` → direct ByteString)

**File:** `src/Language/JavaScript/Parser/Flatparse/Grammar.hs`

Remove all 28 `Text.unpack` calls. Since the lexer now returns `ByteString` and the AST takes `ByteString`, the conversion is eliminated:

```haskell
-- Before:
pos <- FP.getPos
name <- identifier  -- returns Text
pure (JSIdentifier (fpPosToAnnot pos) (Text.unpack name))  -- Text -> String

-- After:
pos <- FP.getPos
name <- identifier  -- returns ByteString
pure (JSIdentifier (fpPosToAnnot pos) name)  -- direct, zero-copy
```

Also remove `import qualified Data.Text as Text` if no longer needed (or reduce to minimal usage).

### Phase E: Modern.hs (24 `Text.unpack` → direct ByteString)

**File:** `src/Language/JavaScript/Parser/Flatparse/Modern.hs`

Same pattern as Grammar.hs — remove all 24 `Text.unpack` calls.

### Phase F: Printer.hs (RenderJS instance)

**File:** `src/Language/JavaScript/Pretty/Printer.hs`

```haskell
-- Before:
str :: String -> Builder
str = BS.fromString

instance RenderJS String where
  (|>) (PosAccum (r, c) bb) s = PosAccum (r', c') (bb <> str s)
    where (r', c') = foldl' (\(row, col) ch -> go (row, col) ch) (r, c) s

-- After:
bsBuilder :: ByteString -> Builder
bsBuilder = Blaze.ByteString.Builder.fromByteString

instance RenderJS ByteString where
  (|>) (PosAccum (r, c) bb) bs = PosAccum (r', c') (bb <> bsBuilder bs)
    where (r', c') = BS.foldl' (\(row, col) w -> go (row, col) w) (r, c) bs
          go (rx, _) 0x0A = (rx + 1, 1)   -- newline
          go (rx, cx) 0x09 = (rx, cx + 8)  -- tab
          go (rx, cx) _ = (rx, cx + 1)     -- other byte
```

Note: Position tracking now counts bytes, not characters. For ASCII-heavy JavaScript this is equivalent. For Unicode identifiers, byte count differs from character count, but this matches the existing behavior where positions are byte offsets.

`renderToString` and `renderToText` stay the same — they already go through Builder → ByteString → decode.

### Phase G: Parser.hs (Public API)

**File:** `src/Language/JavaScript/Parser/Parser.hs`

Add ByteString-native entry points:
```haskell
-- New primary API:
parseBS :: ByteString -> ByteString -> Either String JSAST
parseBS input _srcName = parseFlatparseBS input

parseModuleBS :: ByteString -> ByteString -> Either String JSAST

-- Keep String wrappers for backwards compatibility:
parse :: String -> String -> Either String JSAST
parse input srcName = parseBS (Text.encodeUtf8 (Text.pack input)) (BS.pack (map c2w srcName))
```

Expose `parseProgramByteString` through the main `Language.JavaScript.Parser` module.

### Phase H: Downstream Consumers

| Module | Approximate Changes |
|--------|-------------------|
| `Minify.hs` | ~15 pattern matches on String fields → ByteString |
| `Validator.hs` | ~35 pattern matches + ~20 validation functions → ByteString comparisons |
| `TreeShake/Analysis.hs` | ~15 pattern matches → ByteString |
| `TreeShake/Elimination.hs` | ~15 pattern matches → ByteString |
| `Pretty/JSON.hs` | Escape functions → ByteString |
| `Pretty/XML.hs` | Rendering → ByteString |
| `Pretty/SExpr.hs` | Rendering → ByteString |
| `QQ/*.hs` | Template Haskell quotes → ByteString |
| Test files (29+) | ~1751 AST constructor references with String literals → ByteString |

For test files, use `OverloadedStrings` with `{-# LANGUAGE OverloadedStrings #-}` so `"foo"` works as `ByteString` automatically.

For Validator.hs, string comparisons change from `==` on `String` to `==` on `ByteString` (faster).

### Phase I: Cabal File

`bytestring` is already a dependency. Ensure `bytestring >= 0.10.0` for `BS.foldl'` and other functions.

May be able to remove `utf8-string` dependency if no longer needed after migration.

## Migration Order

1. **AST.hs + Token.hs** — Change types (breaks everything)
2. **Lexer.hs** — Switch to `byteStringOf` returns
3. **Grammar.hs + Modern.hs** — Remove `Text.unpack` calls
4. **Printer.hs** — Update RenderJS instance
5. **Parser.hs** — Add ByteString API
6. **Validator.hs + Minify.hs + TreeShake** — Fix downstream
7. **JSON/XML/SExpr** — Fix serializers
8. **Test files** — Add `OverloadedStrings`, fix literals
9. **Cabal** — Update deps, remove `utf8-string` if possible

## Verification

- `cabal build` compiles without warnings
- `cabal test` passes
- `grep -rn 'Text\.unpack' src/Language/JavaScript/Parser/Flatparse/` returns 0 results
- `grep -rn ':: !String' src/Language/JavaScript/Parser/AST.hs` returns 0 results
- Memory benchmark shows significant reduction for large file parsing
- Throughput benchmark shows improvement from ~0.5 MB/s toward 5+ MB/s
