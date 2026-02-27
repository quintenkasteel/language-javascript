# Plan 03: Enable Pending Tests and Remove Stub Helpers

## Summary

Address 117 `pendingWith` disabled tests, 37 always-True stub predicates, 25 no-op "simplified" functions, and 17 test files not wired into the test suite. Either enable them with real implementations or delete the dead code.

## Inventory

### A. `pendingWith` Disabled Tests (117 across 7 files)

| File | Count | Reason |
|------|-------|--------|
| `test/Unit/.../Lexer/BasicLexer.hs` | 18 | "Waiting for flatparse lexer migration" |
| `test/Unit/.../Lexer/AdvancedLexer.hs` | 4 | Same |
| `test/Unit/.../Lexer/UnicodeSupport.hs` | 7 | Same |
| `test/Unit/.../Lexer/StringLiterals.hs` | 11 | Same |
| `test/Unit/.../Lexer/ASIHandling.hs` | 5 | Same |
| `test/Unit/.../Flatparse/LexerTest.hs` | 30 | "Waiting for flatparse build" |
| `test/Unit/.../Flatparse/ExpressionTest.hs` | 42 | "Waiting for flatparse build" |

### B. Always-True Stub Predicates (37 total)

**ExpressionTest.hs (30 stubs):** `isStringLiteral`, `isNumericLiteral`, `isBooleanLiteral`, `isNullLiteral`, `isUndefinedLiteral`, `isBinaryOp`, `isUnaryOp`, `isMemberAccess`, `isChainedAccess`, `isOptionalChaining`, `isFunctionCall`, `isFunctionCallWithArgs`, `isMethodCall`, `isChainedCall`, `isTemplateLiteral`, `isTemplateWithExpression`, `isMultiLineTemplate`, `isComplexTemplate`, `isArrowFunction`, `isArrowFunctionMultiParam`, `isArrowFunctionNoParam`, `isArrowFunctionDefault`, `hasCorrectPrecedence`, `hasCorrectExponentiation`, `hasRightAssociativity`, `hasParenthesesPrecedence`, `isObjectLiteral`, `isArrayLiteral`, `isConditionalExpression`, `isAssignmentExpression` — ALL return `_ = True`.

**CoreProperties.hs (7 stubs):** `commentPositionsPreserved`, `isValidExpressionInStatement`, `statementOrderValid` (2 branches), `alphaEquivalent`, `expressionAnnotationsMatch`, `statementAnnotationsMatch` — ALL return `True` unconditionally.

### C. No-Op "Simplified" Functions (25 total)

**CoreProperties.hs (23 stubs):** `countComments _ = 0`, `countBlockComments _ = 0`, `extractActualPositions _ = []`, `calculatePositions _ = []`, `renameVariable stmt _ _ = stmt`, `extractFreeVariables _ = []`, `canonicalizeAST = id`, `extractBindingStructure _ = "bindings"`, `hasVariableCapture _ = False`, `applyRenamingMap ast _ = ast`, plus 13 more `= id` functions.

**FuzzHarness.hs (2 stubs):** `checkASTInvariants _ast = return []`, `minimizeAllFailures _results = return ()`

### D. Test Files Not Wired Into testsuite.hs (17 total)

**Never imported (11):**
- `Unit/.../Flatparse/LexerTest.hs`
- `Unit/.../Flatparse/ExpressionTest.hs`
- `Unit/.../Flatparse/Test.hs`
- `Unit/.../Flatparse/ModernTest.hs`
- `Unit/.../Process/TreeShake/ModernJS.hs`
- `Properties/.../Fuzz/FuzzTest.hs`
- `Properties/.../Fuzz/FuzzGenerators.hs`
- `Properties/.../Fuzz/DifferentialTesting.hs`
- `Properties/.../Fuzz/FuzzHarness.hs`
- `Properties/.../Fuzz/CoverageGuided.hs`
- `Properties/.../Generators.hs`

**Commented-out imports (6):**
- `Integration/.../AdvancedFeatures` — "constructor issues"
- `Unit/.../TreeShake/Usage` — no reason
- `Unit/.../TreeShake/Elimination` — no reason
- `Unit/.../QQ/Validate` — "flatparse+TH compatibility"
- `Unit/.../QQ/Compile` — same
- `Unit/.../QQ/Antiquote` — same

---

## Strategy

### Phase 1: Triage — Delete or Fix

For each disabled test file, determine:
1. **Is the feature it tests working?** (The Flatparse parser IS the parser now — lexer migration IS complete)
2. **Are the test assertions meaningful?** (Not if they use stub predicates)

### Phase 2: Lexer Unit Tests (45 tests across 5 files)

The lexer tests test the OLD Alex lexer API. Since the Flatparse lexer has replaced it:

**Option A (Recommended):** Rewrite tests to exercise the Flatparse parser through the public `parse` API. Instead of testing individual tokens, test that parse results contain the expected AST nodes. Remove `pendingWith`.

**Option B:** Delete the old lexer tests entirely and rely on the parser-level tests to cover lexer behavior indirectly.

### Phase 3: Flatparse ExpressionTest.hs (42 tests)

This file is 100% stub predicates + 100% pending. **Delete the entire file and write real expression tests** using the same pattern as the working `Statements.hs` and `Expressions.hs` files — parse real JS strings, pattern-match on expected AST constructors.

### Phase 4: Flatparse LexerTest.hs (30 tests)

All pending. Rewrite with real assertions using `parse`/`readJs` or the internal Flatparse lexer functions. Remove `pendingWith`.

### Phase 5: CoreProperties.hs Stubs (23 no-ops + 7 always-True)

For each stub function, either:
1. **Implement it properly** if the property it tests is valuable
2. **Delete the property test** if it provides no real coverage

Priority stubs to implement: `alphaEquivalent`, `countComments`, `extractFreeVariables`.
Delete: all 13 `= id` functions (they exist only to make broken property tests compile).

### Phase 6: TreeShake/Core.hs Stub Assertions (10 stubs)

Replace the 10 `pure ()` assertion helpers with real AST-walking assertions:
```haskell
-- Current (useless):
astShouldContainCall _ast _functionName = pure ()

-- Required:
astShouldContainCall ast functionName =
  unless (containsCall ast functionName)
    (expectationFailure ("AST should contain call to " <> Text.unpack functionName))
```

### Phase 7: Wire Missing Test Files

For each of the 17 unwired files:
1. If the tests compile and pass: add import to `testsuite.hs`
2. If the tests don't compile: fix compilation errors first
3. If the module tests a dead feature: delete the file

## Changes Per File

| File | Action |
|------|--------|
| `test/testsuite.hs` | Add 5-8 missing imports, uncomment 3-6 disabled imports |
| Lexer/*.hs (5 files) | Remove `pendingWith`, rewrite for Flatparse API |
| `Flatparse/ExpressionTest.hs` | Delete and rewrite completely |
| `Flatparse/LexerTest.hs` | Remove `pendingWith`, fix assertions |
| `CoreProperties.hs` | Implement 3 stubs, delete 20+ unused stubs + their tests |
| `TreeShake/Core.hs` | Implement 10 assertion helpers |
| `FuzzHarness.hs` | Implement `checkASTInvariants` or delete |

## Verification

- `grep -rn 'pendingWith' test/` returns 0 results
- `grep -rn '_ = True' test/` returns only legitimate wildcard-False predicates
- `grep -rn 'pure ()' test/ | grep -v 'import\|it "'` returns 0 assertion stubs
- `cabal test` passes with increased real test count
- All test files under `test/` are imported in `testsuite.hs` or deleted
