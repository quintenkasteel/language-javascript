# Plan 14: Fix or Remove Incomplete JSON/XML/SExpr Serializers

## Summary

The JSON, XML, and S-Expression serializers silently lose data — most AST node types hit a wildcard fallback returning `{"type":"JSExpression","unsupported":true}` or equivalent. Either complete them or remove them from the exposed API.

## Current State

### JSON (`Pretty/JSON.hs`)
- `renderExpressionToJSON`: Only handles ~10 expression types. Wildcard at line 133 returns `renderUnsupportedExpression`.
- `renderStatementToJSON`: Only handles `JSExpressionStatement`. All others → `{"type":"JSStatement","unsupported":true}`.
- `escapeJSONString`: Doesn't escape control characters below U+0020 (except 5 specific ones). Produces invalid JSON for `\0`, `\x1B`, etc.
- Hand-rolled JSON — no `aeson` dependency.

### XML (`Pretty/XML.hs`)
- Import/export rendering completely stubbed (lines 249, 253): `<!-- not yet implemented -->`.
- Same wildcard fallback pattern for expressions and statements.

### SExpr (`Pretty/SExpr.hs`)
- Same incomplete pattern: wildcard fallbacks at lines 230 (expressions) and 274 (statements).
- Import/export declarations stubbed (lines 299-312).

## Decision: Complete or Remove

### Option A (Recommended): Remove from exposed API, mark as experimental

Move JSON/XML/SExpr modules to `other-modules` in cabal. Remove `renderToJSON`, `renderToXML`, `renderToSExpr` from the main `Language.JavaScript.Parser` re-exports. Add a note that these are experimental and incomplete.

### Option B: Complete all three serializers

This is significant work — each serializer needs coverage for all ~50 expression types, ~30 statement types, and ~20 module/class/method types. Estimate: ~500-800 lines per serializer.

### Option C: Complete JSON only, remove XML/SExpr

JSON is the most useful format. Complete the JSON serializer to cover all AST types. Remove XML and SExpr.

## If Completing JSON (Option B or C)

### Missing Expression Types (~30)
Object literals, array literals, function expressions, arrow expressions, class expressions, template literals, spread, unary expressions, postfix expressions, ternary, assignment, comma, member access, call expressions, new expressions, optional chaining, yield, await, async function expressions, generator expressions, import.meta, import(), BigInt, regex.

### Missing Statement Types (~25)
Variable declarations, if/else, while, do-while, for variants (12), switch/case, return, break, continue, throw, try/catch/finally, function declarations, class declarations, async functions, generators, labeled, with, empty.

### Fix `escapeJSONString`
Add missing control character escaping:
```haskell
escapeJSONChar c
  | c < '\x20' = "\\u" <> printf "%04x" (ord c)
  | c == '"' = "\\\""
  | c == '\\' = "\\\\"
  | otherwise = [c]
```

## Files Changed

| File | Change |
|------|--------|
| `language-javascript.cabal` | Move JSON/XML/SExpr to `other-modules` (Option A) |
| `src/Language/JavaScript/Parser.hs` | Remove re-exports of `renderToJSON`, `renderToXML`, `renderToSExpr` |
| Or: `src/Language/JavaScript/Pretty/JSON.hs` | Complete all AST coverage (Option B/C) |

## Verification

- If removing: modules no longer importable, no data loss possible
- If completing: `renderToJSON (readJs "var x = 1;")` produces valid, complete JSON
- `escapeJSONString` handles all control characters correctly
