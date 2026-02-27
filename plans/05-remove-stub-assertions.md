# Plan 05: Remove Stub Assertions from TreeShake Tests

## Summary

10 assertion helpers in `test/Unit/.../TreeShake/Core.hs` are `pure ()` no-ops, making any test that uses them incapable of failing. Replace with real AST-walking assertions.

## Inventory

All in `test/Unit/Language/Javascript/Process/TreeShake/Core.hs`:

| Function | Current Implementation | Should Do |
|----------|----------------------|-----------|
| `astShouldContainCall` | `pure ()` | Walk AST, check for `JSMemberExpression`/`JSCallExpression` with matching name |
| `astShouldNotContainCall` | `pure ()` | Walk AST, assert no matching call expression exists |
| `astShouldContainAssignment` | `pure ()` | Walk AST, check for `JSAssignExpression`/`JSAssignStatement` |
| `astShouldContainNew` | `pure ()` | Walk AST, check for `JSMemberNew`/`JSNewExpression` |
| `astShouldContainDelete` | `pure ()` | Walk AST, check for `JSUnaryExpression` with `JSUnaryOpDelete` |
| `astShouldContainImport` | `pure ()` | Walk AST, check for `JSImportDeclaration` with matching module |
| `astShouldNotContainImport` | `pure ()` | Walk AST, assert no matching import |
| `astShouldContainSideEffectImport` | `pure ()` | Walk AST, check for `JSImportDeclarationBare` |
| `astShouldContainExport` | `pure ()` | Walk AST, check for `JSExportDeclaration` with matching name |
| `astShouldNotContainExport` | `pure ()` | Walk AST, assert no matching export |

Note: `astShouldContainIdentifier` and `astShouldNotContainIdentifier` already have proper implementations using `showStripped` and `Data.Text.isInfixOf`.

## Implementation

### Approach: Use `showStripped` (Consistent with Working Helpers)

The existing working helpers use `showStripped` to get a string representation and then search for substrings. Follow the same pattern:

```haskell
astShouldContainCall :: JSAST -> Text -> Expectation
astShouldContainCall ast functionName = do
  let stripped = Text.pack (showStripped ast)
  unless (functionName `Text.isInfixOf` stripped)
    (expectationFailure
      ("Expected AST to contain call to '" <> Text.unpack functionName
       <> "' but it was not found in:\n" <> Text.unpack stripped))

astShouldNotContainCall :: JSAST -> Text -> Expectation
astShouldNotContainCall ast functionName = do
  let stripped = Text.pack (showStripped ast)
  when (functionName `Text.isInfixOf` stripped)
    (expectationFailure
      ("Expected AST to NOT contain call to '" <> Text.unpack functionName
       <> "' but it was found in:\n" <> Text.unpack stripped))
```

### More Precise Approach (SYB traversal)

For assertions that need structural precision (not just substring matching), use `Data.Generics`:

```haskell
import Data.Generics (everything, mkQ)

containsCallTo :: JSAST -> Text -> Bool
containsCallTo ast name = everything (||) (mkQ False checkExpr) ast
  where
    checkExpr (JSMemberExpression (JSIdentifier _ n) _ _ _) = Text.pack n == name
    checkExpr (JSMethodCall (JSIdentifier _ n) _ _ _ _) = Text.pack n == name
    checkExpr _ = False
```

## Files Changed

| File | Change |
|------|--------|
| `test/Unit/.../TreeShake/Core.hs` | Replace 10 `pure ()` stubs with real implementations |

## Verification

- `grep -rn 'pure ()' test/Unit/Language/Javascript/Process/TreeShake/Core.hs` returns 0 results
- Tests that previously used these stubs now actually verify behavior
- `cabal test` still passes (if tree shaking is correct, tests pass; if not, they fail meaningfully)
