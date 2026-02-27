# Plan 01: Replace All `error` Calls with Safe Alternatives

## Summary

Remove all 5 `error` calls and 3 `head`/`!!` uses, 2 `Text.head`/`Text.tail` uses, 1 `read`, and 2 `VU.unsafeIndex` calls from library source code. Replace with total functions.

## Inventory (14 unsafe calls across 5 files)

### Critical: Public API crash

| Location | Call | Impact |
|----------|------|--------|
| `src/Language/JavaScript/Parser/Parser.hs:121` | `error (show msg)` in `readJsWith` | `readJs "invalid"` crashes at runtime |

### Medium: Internal library code

| Location | Call |
|----------|------|
| `src/Language/JavaScript/Parser/LexerUtils.hs:53` | `error ("Invalid decimal literal: " ...)` |
| `src/Language/JavaScript/Parser/LexerUtils.hs:66` | `error ("Invalid hex literal: " ...)` |
| `src/Language/JavaScript/Parser/LexerUtils.hs:81` | `error ("Invalid binary literal: " ...)` |
| `src/Language/JavaScript/Parser/LexerUtils.hs:96` | `error ("Invalid octal literal: " ...)` |
| `src/Language/JavaScript/Parser/Validator.hs:832` | `head positions` |
| `src/Language/JavaScript/Parser/Validator.hs:2315` | `head values` |
| `src/Language/JavaScript/Parser/Validator.hs:2346` | `head types` + `types !! 1` |
| `src/Language/JavaScript/Parser/Token.hs:906` | `Text.head` + `Text.tail` |
| `src/Language/JavaScript/Parser/Token.hs:953` | `Text.head` + `Text.tail` |
| `src/Language/JavaScript/Parser/Validator.hs:1440` | `read ("0x" ++ hexDigits)` |
| `src/Language/JavaScript/Parser/Flatparse/Parser.hs:311,314` | `VU.unsafeIndex` (2 calls) |

## Changes

### 1. `Parser.hs:121` — `readJsWith`

**Current:**
```haskell
readJsWith f input =
  case f input "src" of
    Left msg -> error (show msg)
    Right p -> p
```

**Fix:** Change return type to `Either String JSAST`. Add safe wrappers:
```haskell
readJsSafe :: String -> Either String AST.JSAST
readJsSafe input = parse input "src"

readJsModuleSafe :: String -> Either String AST.JSAST
readJsModuleSafe input = parseModule input "src"
```

Keep `readJs`/`readJsModule` but document partiality and add `{-# WARNING #-}`:
```haskell
{-# WARNING readJs "Partial function: crashes on parse failure. Use readJsSafe instead." #-}
```

Update `parseFile`/`parseFileUtf8` to return `IO (Either String JSAST)`.

### 2. `LexerUtils.hs:53,66,81,96` — Token constructors

**Current (4 functions, same pattern):**
```haskell
decimalToken loc str
  | isValidDecimal str = DecimalToken loc str []
  | otherwise = error ("Invalid decimal literal: " ...)
```

**Fix:** Return `Either String Token`:
```haskell
decimalToken :: TokenPosn -> String -> Either String Token
decimalToken loc str
  | isValidDecimal str = Right (DecimalToken loc str [])
  | otherwise = Left ("Invalid decimal literal: " <> str <> " at " <> show loc)
```

Note: Since the Flatparse parser doesn't use LexerUtils (it's dead code from the Alex era), an alternative is to simply remove the file. Verify no imports exist first.

### 3. `Validator.hs:832` — `head positions`

**Current:**
```haskell
in [(name, head positions) | (name, positions) <- Map.toList duplicateEntries]
```

**Fix:** Pattern match:
```haskell
in [(name, p) | (name, p:_) <- Map.toList duplicateEntries]
```

### 4. `Validator.hs:2315` — `head values`

**Current:**
```haskell
in map (\name -> JSDocEnumValueDuplicate name (jsDocEnumValueName (head values)) pos) duplicates
```

**Fix:** Guard against empty:
```haskell
findDuplicateEnumValues [] _ = []
findDuplicateEnumValues (v:_) pos =
  let valueNames = map jsDocEnumValueName (v : vs)
      duplicates = findDuplicatesInList valueNames
  in map (\name -> JSDocEnumValueDuplicate name (jsDocEnumValueName v) pos) duplicates
```

### 5. `Validator.hs:2346` — `head types` + `types !! 1`

**Current:**
```haskell
in if length uniqueTypes > 1
   then [JSDocEnumValueTypeMismatch enumName (head types) (types !! 1) pos]
   else []
```

**Fix:** Pattern match:
```haskell
in case uniqueTypes of
     (t1:t2:_) -> [JSDocEnumValueTypeMismatch enumName t1 t2 pos]
     _ -> []
```

### 6. `Token.hs:906,953` — `Text.head`/`Text.tail`

**Current (2 locations, same pattern):**
```haskell
let (char, rest') = (Text.head remaining, Text.tail remaining)
```

**Fix:** Use `Text.uncons`:
```haskell
case Text.uncons remaining of
  Nothing -> if Text.null current then acc else acc <> [Text.strip current]
  Just (char, rest') -> case char of ...
```

### 7. `Validator.hs:1440` — `read`

**Current:**
```haskell
let codePoint = read ("0x" ++ hexDigits) :: Int
```

**Fix:** Use `Numeric.readHex`:
```haskell
import qualified Numeric as Numeric

case Numeric.readHex hexDigits of
  [(codePoint, "")] -> if codePoint <= 0x10FFFF then go remaining else ...
  _ -> [InvalidEscapeSequence ...]
```

### 8. `Flatparse/Parser.hs:311,314` — `VU.unsafeIndex`

**Current:**
```haskell
col = offset - VU.unsafeIndex lineStarts (line - 1) + 1
```

**Fix:** Use bounds-checked `VU.!`:
```haskell
col = offset - (lineStarts VU.! (line - 1)) + 1
```

## Verification

- `grep -rn 'error\s*(' src/ | grep -v '"error"' | grep -v '-- '` should return 0 results
- `grep -rn '\bhead\b' src/ | grep -v 'import\|--\|Header\|header\|thead'` should return 0 results
- `grep -rn 'Text\.head\|Text\.tail' src/` should return 0 results
- `grep -rn '\bread\b\s' src/ | grep -v 'import\|Read\|read-\|readM\|readFile'` should return 0 results
- `grep -rn 'unsafeIndex' src/` should return 0 results
- `cabal build` compiles without warnings
- `cabal test` passes
