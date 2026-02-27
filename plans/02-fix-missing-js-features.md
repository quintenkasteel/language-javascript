# Plan 02: Fix Missing JavaScript Features

## Summary

Add parser support for 7 missing JavaScript features that cause parse failures on valid modern JS. Each feature requires AST additions, Grammar.hs parser rules, Pretty Printer instances, and tests.

## Files Affected

- `src/Language/JavaScript/Parser/AST.hs` — New constructors + ShowStripped instances
- `src/Language/JavaScript/Parser/Flatparse/Grammar.hs` — New parser rules
- `src/Language/JavaScript/Pretty/Printer.hs` — New RenderJS instances
- `src/Language/JavaScript/Parser/Validator.hs` — Validation for new constructs
- `test/Unit/Language/Javascript/Parser/Parser/Statements.hs` — Update expected-failure tests
- `test/Unit/Language/Javascript/Parser/Parser/Expressions.hs` — New tests

---

## Feature 1: `debugger` Statement (Low difficulty)

### AST Change
```haskell
-- Add to JSStatement in AST.hs after JSWith:
| JSDebugger !JSAnnot !JSSemi
```

### Grammar.hs Change
Add `debuggerStatement` parser and wire into `statement` combinator (before `expressionStatement`):
```haskell
debuggerStatement :: JSParser JSStatement
debuggerStatement = do
  pos <- FP.getPos
  keyword "debugger"
  semi <- expectStatementEnd
  pure (JSDebugger (fpPosToAnnot pos) semi)
```

### Printer.hs Change
```haskell
instance RenderJS JSStatement where
  ...
  (|>) pacc (JSDebugger annot semi) = pacc |> annot |> "debugger" |> semi
```

---

## Feature 2: Destructuring in `catch` (Low difficulty)

### AST Change
None — `JSCatch` already takes `!JSExpression` which can hold array/object literals.

### Grammar.hs Change
In `catchSimple` (line 2255) and `catchWithGuard` (line 2241), replace `identifier` with `catchParam`:
```haskell
catchParam :: JSParser JSExpression
catchParam = arrayLiteral FP.<|> objectLiteral FP.<|> identifierExpression
  where
    identifierExpression = do
      pos <- FP.getPos
      name <- identifier
      pure (JSIdentifier (fpPosToAnnot pos) (Text.unpack name))
```

---

## Feature 3: Public Class Fields (Medium difficulty)

### AST Change
```haskell
-- Add to JSClassElement in AST.hs:
| JSClassField !JSPropertyName !JSAnnot !(Maybe JSExpression) !JSSemi
-- name, equals, optional initializer, semi
| JSClassStaticField !JSAnnot !JSPropertyName !JSAnnot !(Maybe JSExpression) !JSSemi
-- static, name, equals, optional initializer, semi
```

### Grammar.hs Change
In `instanceElement` (line 1484), add `publicField` before `classMethodDef`:
```haskell
instanceElement = publicField FP.<|> (JSClassInstanceMethod <$> classMethodDef)

publicField :: JSParser JSClassElement
publicField = do
  name <- propertyName
  whitespace
  initAndSemi <- fieldInitializer
  pure (uncurry (JSClassField name) initAndSemi)

fieldInitializer :: JSParser (JSAnnot, Maybe JSExpression, JSSemi)
fieldInitializer = withInit FP.<|> withoutInit
  where
    withInit = do
      eq <- parseCharAnnot '='
      whitespace
      val <- assignmentExpression
      semi <- expectStatementEnd
      pure (eq, Just val, semi)
    withoutInit = do
      semi <- expectStatementEnd
      pure (defaultAnnot, Nothing, semi)
```

In `staticElement` (line 1427), add static field variant before static method.

**Disambiguation:** After `propertyName`, check for `=` or `;` (field) vs `(` (method). Try `publicField` first since it's more specific.

---

## Feature 4: Async Arrow Functions (High difficulty)

### AST Change
```haskell
-- Add to JSExpression in AST.hs:
| JSAsyncArrowExpression !JSAnnot !JSArrowParameterList !JSAnnot !JSConciseBody
-- async annot, params, arrow annot, body
```

### Grammar.hs Change
Add `asyncArrowFunction` and wire into `assignmentExpression` (line 381) before `arrowFunction`:

```haskell
asyncArrowFunction :: JSParser JSExpression
asyncArrowFunction = asyncParenArrow FP.<|> asyncSingleParamArrow

asyncParenArrow :: JSParser JSExpression
asyncParenArrow = do
  asyncPos <- FP.getPos
  keyword "async"
  whitespace
  -- Must NOT be followed by 'function' (that's asyncFunctionExpr)
  pos <- FP.getPos
  parseChar '('
  whitespace
  params <- sepBy arrowParam (whitespace *> parseChar ',' *> whitespace)
  whitespace
  parseChar ')'
  whitespace
  parseString "=>"  -- This disambiguates from async(x) function call
  whitespace
  body <- arrowBody
  pure (JSAsyncArrowExpression (fpPosToAnnot asyncPos)
    (JSParenthesizedArrowParameterList (fpPosToAnnot pos) (listToCommaList params) defaultAnnot)
    defaultAnnot body)

asyncSingleParamArrow :: JSParser JSExpression
asyncSingleParamArrow = do
  asyncPos <- FP.getPos
  keyword "async"
  whitespace
  pos <- FP.getPos
  name <- identifier
  whitespace
  parseString "=>"
  whitespace
  body <- arrowBody
  pure (JSAsyncArrowExpression (fpPosToAnnot asyncPos)
    (JSUnparenthesizedArrowParameter (JSIdentName (fpPosToAnnot pos) (Text.unpack name)))
    defaultAnnot body)
```

**Disambiguation challenge:** `async(x)` (function call) vs `async (x) => {}` (async arrow). The `=>` after `)` disambiguates. FlatParse's backtracking handles this — if `parseString "=>"` fails after the params, the entire `asyncParenArrow` fails and falls through to other alternatives.

---

## Feature 5: Async Generators (Medium difficulty)

### AST Changes
```haskell
-- In JSStatement:
| JSAsyncGenerator !JSAnnot !JSAnnot !JSAnnot !JSIdent !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock !JSSemi
-- async, function, *, name, lb, params, rb, block, semi

-- In JSExpression:
| JSAsyncGeneratorExpression !JSAnnot !JSAnnot !JSAnnot !JSIdent !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock
-- async, function, *, name, lb, params, rb, block

-- In JSMethodDefinition:
| JSAsyncGeneratorMethodDefinition !JSAnnot !JSAnnot !JSPropertyName !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock
-- async, *, name, lb, params, rb, block
```

### Grammar.hs Changes
1. Add `asyncGeneratorDecl` parser, wire into `statement` before `asyncFunctionDecl`
2. Add `asyncGeneratorExpr` parser, wire into primary expressions
3. Add `asyncGeneratorMethodDef` parser, wire into `classMethodDef` and `methodProp`

The parser checks for `async function*` by looking for `*` after `function`:
```haskell
asyncGeneratorDecl :: JSParser JSStatement
asyncGeneratorDecl = do
  pos <- FP.getPos
  keyword "async"
  whitespace
  funcAnnot <- keywordAnnot "function"
  whitespace
  star <- parseCharAnnot '*'
  whitespace
  name <- identName
  -- ... params, body ...
  pure (JSAsyncGenerator (fpPosToAnnot pos) funcAnnot star name ...)
```

---

## Feature 6: `for await...of` (Medium difficulty)

### AST Changes
```haskell
-- Add 4 new constructors (one per declaration form):
| JSForAwaitOf !JSAnnot !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement
| JSForAwaitVarOf !JSAnnot !JSAnnot !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement
| JSForAwaitLetOf !JSAnnot !JSAnnot !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement
| JSForAwaitConstOf !JSAnnot !JSAnnot !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement
```

### Grammar.hs Change
In `forStatement` (line 1766), optionally parse `await` between `for` and `(`:
```haskell
forStatement = do
  pos <- FP.getPos
  keyword "for"
  whitespace
  mAwait <- FP.optional (keywordAnnot "await" <* whitespace)
  lp <- parseCharAnnot '('
  whitespace
  -- Thread mAwait into *Of variants, producing ForAwait*Of when Just
  ...
```

---

## Feature 7: Static Class Blocks (Low-Medium difficulty)

### AST Change
```haskell
-- Add to JSClassElement:
| JSClassStaticBlock !JSAnnot !JSBlock
-- static, { stmts }
```

### Grammar.hs Change
In `staticElement` (line 1427), add `staticBlock` as first alternative (before static method), since `static {` is disambiguated by the `{` character (methods start with a property name):
```haskell
staticElement = do
  pos <- FP.getPos
  keyword "static"
  whitespace
  staticBlock pos FP.<|> staticMethod pos

staticBlock :: FP.Pos -> JSParser JSClassElement
staticBlock pos = do
  body <- blockBody
  pure (JSClassStaticBlock (fpPosToAnnot pos) body)
```

---

## Test Updates

1. Update tests in `Statements.hs` that currently expect parse errors for these features (lines 355-369)
2. Add positive tests for each feature
3. Add round-trip tests (parse -> render -> parse)
4. Add negative tests (malformed variants)

## Verification

- `cabal build` compiles without warnings
- `cabal test` passes with new tests
- Manual test: parse `async (x) => { await x; }` successfully
- Manual test: parse `class Foo { x = 5; static y = 10; }` successfully
- Manual test: parse `debugger;` successfully
- Manual test: parse `async function* gen() { yield 1; }` successfully
