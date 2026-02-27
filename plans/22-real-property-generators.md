# Plan 22: Build Real Property-Based Test Generators

## Summary

The current QuickCheck generators produce fixed, hardcoded strings from small pools (~8 strings). They provide no more coverage than deterministic unit tests. Build generators that produce novel, structurally diverse JavaScript syntax.

## Current Problems

### Hardcoded Generators
```haskell
-- Current (useless): always generates from 8 fixed strings
instance Arbitrary ValidJSInput where
  arbitrary = ValidJSInput <$> elements
    [ "var x = 1;"
    , "function f() {}"
    , "if (true) { x = 1; }"
    -- ... 5 more
    ]
```

### Stub Properties
```haskell
-- Current (vacuous):
alphaEquivalent _ _ = True  -- always passes
commentPositionsPreserved _ _ = True  -- always passes
```

## Plan: Structural AST Generators

### Step 1: Expression Generator

Generate arbitrary `JSExpression` AST nodes:

```haskell
genExpression :: Int -> Gen JSExpression
genExpression 0 = oneof
  [ genIdentifier
  , genNumericLiteral
  , genStringLiteral
  , genBoolLiteral
  ]
genExpression n = oneof
  [ genIdentifier
  , genNumericLiteral
  , genStringLiteral
  , genBoolLiteral
  , genBinaryExpr (n - 1)
  , genUnaryExpr (n - 1)
  , genParenExpr (n - 1)
  , genCallExpr (n - 1)
  , genMemberExpr (n - 1)
  , genArrayLiteral (n - 1)
  , genObjectLiteral (n - 1)
  ]

genIdentifier :: Gen JSExpression
genIdentifier = do
  first <- elements (['a'..'z'] <> ['A'..'Z'] <> ['_'])
  rest <- listOf (elements (['a'..'z'] <> ['0'..'9'] <> ['_']))
  let name = first : take 10 rest
  -- Reject keywords
  if isKeyword name then genIdentifier
  else pure (JSIdentifier noAnnot name)

genNumericLiteral :: Gen JSExpression
genNumericLiteral = do
  n <- arbitrary :: Gen (NonNegative Int)
  pure (JSDecimal noAnnot (show (getNonNegative n)))

genBinaryExpr :: Int -> Gen JSExpression
genBinaryExpr n = do
  left <- genExpression (n `div` 2)
  right <- genExpression (n `div` 2)
  op <- genBinOp
  pure (JSExpressionBinary left op right)
```

### Step 2: Statement Generator

```haskell
genStatement :: Int -> Gen JSStatement
genStatement 0 = genExpressionStatement
genStatement n = oneof
  [ genExpressionStatement
  , genVarDeclaration (n - 1)
  , genIfStatement (n - 1)
  , genWhileStatement (n - 1)
  , genBlockStatement (n - 1)
  , genReturnStatement (n - 1)
  ]
```

### Step 3: Source Code Generator (String-based)

Generate syntactically valid JavaScript source strings:

```haskell
genValidJS :: Gen String
genValidJS = do
  stmts <- listOf1 genStatementString
  pure (unlines stmts)

genStatementString :: Gen String
genStatementString = oneof
  [ genVarDeclString
  , genFunctionDeclString
  , genExprStmtString
  , genIfString
  ]

genVarDeclString :: Gen String
genVarDeclString = do
  keyword <- elements ["var", "let", "const"]
  name <- genIdentName
  value <- genExprString 3
  pure (keyword <> " " <> name <> " = " <> value <> ";")
```

### Step 4: Real Properties to Test

```haskell
-- Round-trip: parse -> render -> parse produces same AST
prop_roundTrip :: Property
prop_roundTrip = forAll genValidJS $ \input ->
  case parse input "test" of
    Left _ -> discard  -- skip invalid inputs
    Right ast1 ->
      let rendered = renderToString ast1
      in case parse rendered "test" of
        Left err -> counterexample ("Re-parse failed: " <> err) False
        Right ast2 -> showStripped ast1 === showStripped ast2

-- Idempotent rendering: rendering twice gives same result
prop_renderIdempotent :: Property
prop_renderIdempotent = forAll genValidJS $ \input ->
  case parse input "test" of
    Left _ -> discard
    Right ast ->
      let r1 = renderToString ast
          r2 = renderToString (readJs r1)
      in r1 === r2

-- Parse never crashes (even on random input)
prop_noCrash :: Property
prop_noCrash = forAll (arbitrary :: Gen String) $ \input ->
  case parse input "test" of
    Left _ -> True
    Right _ -> True
  -- Should never throw an exception

-- Minification preserves parseability
prop_minifyParseable :: Property
prop_minifyParseable = forAll genValidJS $ \input ->
  case parse input "test" of
    Left _ -> discard
    Right ast ->
      let minified = minifyJS ast
      in case parse (renderToString minified) "test" of
        Left err -> counterexample ("Minified output fails to parse: " <> err) False
        Right _ -> True
```

### Step 5: Shrinking

Implement `shrink` for the generators so QuickCheck can minimize failing cases:

```haskell
instance Arbitrary ValidJSProgram where
  arbitrary = ValidJSProgram <$> genValidJS
  shrink (ValidJSProgram s) =
    -- Try removing lines
    [ ValidJSProgram (unlines ls')
    | ls' <- shrinkList (const []) (lines s)
    , not (null ls')
    ]
```

## Files Changed

| File | Change |
|------|--------|
| `test/Properties/.../Generators.hs` | Rewrite with structural generators |
| `test/Properties/.../CoreProperties.hs` | Replace stub properties with real ones |
| `test/Properties/.../Fuzzing.hs` | Use real generators instead of fixed pools |

## Verification

- Properties find at least 1 real bug when first run (validates the generators work)
- `quickCheckWith stdArgs { maxSuccess = 1000 }` completes in reasonable time
- No stub predicates remain (`grep '_ = True\|_ _ = True' test/Properties/`)
- Shrinking produces minimal counterexamples
