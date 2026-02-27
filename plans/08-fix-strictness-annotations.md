# Plan 08: Add Missing Strictness Annotations

## Summary

Fix lazy fields in `CommentAnnotation` and `JSExportDeclaration` that can cause space leaks when ASTs are retained in memory.

## Inventory

### CommentAnnotation (Token.hs:510-514)

```haskell
-- Current (LAZY fields):
data CommentAnnotation
  = CommentA TokenPosn String      -- Both fields lazy!
  | WhiteSpace TokenPosn String    -- Both fields lazy!
  | JSDocA TokenPosn JSDocComment
  | NoComment
```

```haskell
-- Fixed:
data CommentAnnotation
  = CommentA !TokenPosn !ByteString   -- Strict (ByteString after Plan 06)
  | WhiteSpace !TokenPosn !ByteString -- Strict
  | JSDocA !TokenPosn !JSDocComment
  | NoComment
```

### JSExportDeclaration (AST.hs, various lines)

```haskell
-- Current (some fields missing !):
JSExportAllFrom !JSBinOp JSFromClause !JSSemi        -- JSFromClause NOT strict
JSExportFrom JSExportClause JSFromClause !JSSemi      -- JSExportClause AND JSFromClause NOT strict
```

```haskell
-- Fixed:
JSExportAllFrom !JSBinOp !JSFromClause !JSSemi
JSExportFrom !JSExportClause !JSFromClause !JSSemi
```

## Files Changed

| File | Change |
|------|--------|
| `src/Language/JavaScript/Parser/Token.hs:510-514` | Add `!` to CommentAnnotation fields |
| `src/Language/JavaScript/Parser/AST.hs` | Add `!` to JSExportDeclaration fields |

## Verification

- `cabal build` with `-funbox-strict-fields` (already set in cabal) optimizes these
- No behavioral change — strictness only affects evaluation order
