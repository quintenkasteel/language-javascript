# Plan 19: Add Real Golden Test Files

## Summary

The golden test infrastructure (`Golden.Language.Javascript.Parser.GoldenTests`) uses `hspec-golden` and expects `.js` files in `test/golden/` directories. No golden files exist, so all golden tests silently pass with zero comparisons.

## Current Infrastructure

The golden test module reads files from:
- `test/golden/ecmascript/` — ECMAScript feature tests
- `test/golden/errors/` — Error message golden tests
- `test/golden/prettyprint/` — Pretty printer output tests
- `test/golden/minify/` — Minification output tests

When directories are empty/absent, `listDirectory` returns `[]` and all tests pass vacuously.

## Plan

### Step 1: Create Directory Structure

```bash
mkdir -p test/golden/{ecmascript,errors,prettyprint,minify}/{inputs,expected}
```

### Step 2: ECMAScript Golden Tests

Create `.js` input files covering key language features:

```
test/golden/ecmascript/inputs/
├── arrow-functions.js
├── async-await.js
├── classes.js
├── destructuring.js
├── for-of.js
├── generators.js
├── modules.js
├── optional-chaining.js
├── rest-spread.js
├── template-literals.js
└── variables.js
```

For each, generate expected output by running the parser and saving the `showStripped` output:
```haskell
-- Generate golden files:
writeFile "test/golden/ecmascript/expected/arrow-functions.golden"
  (showStripped (readJs arrowFunctionInput))
```

### Step 3: Error Golden Tests

Create inputs that should produce specific error messages:

```
test/golden/errors/inputs/
├── unclosed-string.js        -- "unterminated string
├── unexpected-token.js       -- var = ;
├── missing-semicolon.js      -- var x = 1 var y = 2
└── invalid-assignment.js     -- 5 = x
```

Expected files contain the exact error message text.

### Step 4: Pretty Printer Golden Tests

Round-trip tests: input JS → parse → render → compare with expected:

```
test/golden/prettyprint/inputs/
├── simple-function.js
├── class-definition.js
├── module-imports.js
└── complex-expression.js
```

### Step 5: Minification Golden Tests

```
test/golden/minify/inputs/
├── simple.js
├── with-comments.js
└── multiline.js
```

Expected files contain the minified output.

## Files Changed

| File/Directory | Change |
|----------------|--------|
| `test/golden/` | Create directory structure with 20-30 golden files |
| `test/golden/ecmascript/inputs/*.js` | Input JS files |
| `test/golden/ecmascript/expected/*.golden` | Expected parsed output |
| `test/golden/errors/inputs/*.js` | Error case inputs |
| `test/golden/errors/expected/*.golden` | Expected error messages |
| `test/golden/prettyprint/inputs/*.js` | Pretty print inputs |
| `test/golden/prettyprint/expected/*.golden` | Expected pretty printed output |
| `test/golden/minify/inputs/*.js` | Minify inputs |
| `test/golden/minify/expected/*.golden` | Expected minified output |

## Verification

- Golden test suite runs with actual file comparisons
- `cabal test --test-options="--match Golden"` reports actual test counts (not 0)
- Golden files checked into git for regression detection
