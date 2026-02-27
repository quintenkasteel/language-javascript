# Plan 21: Add Negative Tests for All Parser Features

## Summary

The test suite has good positive coverage (parse valid JS, check AST structure) but limited negative testing. Add comprehensive tests that verify the parser correctly rejects invalid JavaScript with appropriate error messages.

## Current Negative Test Coverage

The only dedicated negative test file is `test/Unit/.../Error/Negative.hs` which uses `shouldFailToParse`. Coverage is thin.

## Categories of Negative Tests Needed

### 1. Syntax Errors — Missing Tokens
```javascript
// Missing closing paren
function foo( { }

// Missing closing brace
function foo() {

// Missing semicolons where required (ASI cannot help)
var x = 1 var y = 2

// Unterminated string
var s = "hello

// Unterminated template literal
var t = `hello ${world

// Unterminated regex
var r = /pattern
```

### 2. Invalid Expressions
```javascript
// Invalid assignment targets
5 = x
"hello" = x
(a + b) = c

// Invalid unary operands
delete 5
typeof = 5

// Invalid arrow parameters
(5) => x
([a, ...b, c]) => x   // rest must be last

// Double spread
var x = [...a, ...b, ...]  // trailing spread
```

### 3. Invalid Statements
```javascript
// break outside loop/switch
break;

// continue outside loop
continue;

// return outside function
return 5;

// yield outside generator
yield 5;

// await outside async
await promise;

// duplicate labels
label: label: x;

// const without initializer
const x;
```

### 4. Invalid Class Constructs
```javascript
// Duplicate constructor
class Foo { constructor() {} constructor() {} }

// Generator constructor
class Foo { *constructor() {} }

// Private name outside class
x.#field;
```

### 5. Invalid Module Syntax
```javascript
// Import in non-module context (when parsing as script)
import x from 'y';

// Duplicate exports
export { x }; export { x };

// Export non-existent
export { nonExistent };
```

### 6. Numeric Literal Errors
```javascript
// Invalid octal
089

// BigInt with decimal
1.5n

// Hex without digits
0x
```

## Implementation Pattern

```haskell
shouldFailToParse :: String -> Expectation
shouldFailToParse input =
  case parse input "test" of
    Left _ -> pure ()
    Right ast -> expectationFailure
      ("Expected parse failure but got: " <> showStripped ast)

shouldFailWithMessage :: String -> String -> Expectation
shouldFailWithMessage input expectedMsg =
  case parse input "test" of
    Left msg -> msg `shouldContain` expectedMsg
    Right ast -> expectationFailure
      ("Expected parse failure but got: " <> showStripped ast)
```

## Files Changed

| File | Change |
|------|--------|
| `test/Unit/.../Error/Negative.hs` | Expand with ~50-80 new negative test cases |
| `test/Unit/.../Parser/Expressions.hs` | Add negative expression tests |
| `test/Unit/.../Parser/Statements.hs` | Add negative statement tests |

## Verification

- All negative tests fail to parse (not accidentally passing)
- Error messages are meaningful (not just "parse error")
- `cabal test` passes
- No false positives (valid JS incorrectly rejected)
