# Plan 08: Pretty Printer and Process Module Documentation

**Priority**: P2 (Enhancement — incomplete public API docs)
**Effort**: Medium (2-3 hours)
**Risk**: Zero (documentation only)

## Problem

### Pretty Printer (`src/Language/JavaScript/Pretty/Printer.hs`, 492 lines)

The printer exports three public functions:
```haskell
renderJS :: JSAST -> Doc
renderToString :: JSAST -> String
renderToText :: JSAST -> Text
```

But the module-level documentation is sparse and the internal `(|>)` operator and `RenderJS` typeclass are documented minimally. Users looking at Haddock docs get:
- No examples of rendering different AST types
- No explanation of whitespace/formatting decisions
- No guidance on when to use `renderJS` vs `renderToString` vs `renderToText`

### Minifier (`src/Language/JavaScript/Process/Minify.hs`, 508 lines)

The `MinifyJS` typeclass and `minifyJS` function lack:
- Documentation of what transformations are applied
- Examples of minification behavior
- Documentation of edge cases (string concatenation, quote normalization)

## Solution

### Printer.hs Documentation

Add module-level documentation:
```haskell
-- | JavaScript pretty printer.
--
-- Converts JavaScript AST back to source code. The printer preserves
-- semantic equivalence while normalizing whitespace and formatting.
--
-- ==== Usage
--
-- >>> let Right ast = parse "var  x  =  42 ;" "test"
-- >>> renderToString ast
-- "var x=42"
--
-- ==== Rendering targets
--
-- * 'renderToString' — most convenient, returns 'String'
-- * 'renderToText' — returns 'Text', avoids String allocation
-- * 'renderJS' — returns 'Doc' for composition with other documents
--
-- @since 0.5.0.0
```

Document the `RenderJS` typeclass:
```haskell
-- | Typeclass for rendering AST nodes to JavaScript source.
--
-- Each AST node type implements this class to define how it
-- should be serialized back to JavaScript text. The rendering
-- is done via a left-fold accumulator pattern using '(|>)'.
class RenderJS a where
  (|>) :: Doc -> a -> Doc
```

### Minify.hs Documentation

```haskell
-- | JavaScript minification.
--
-- Reduces JavaScript source size by:
--
-- * Removing all comments and unnecessary whitespace
-- * Normalizing string quotes to single quotes
-- * Concatenating adjacent string literals
-- * Removing trailing semicolons where ASI applies
--
-- The minifier operates on the AST level, so it preserves
-- semantic equivalence with the original source.
--
-- ==== Usage
--
-- >>> let Right ast = parse "var  x  =  'hello'  +  'world' ;" "test"
-- >>> renderToString (minifyJS ast)
-- "var x='helloworld'"
```

## Files Changed

- `src/Language/JavaScript/Pretty/Printer.hs` — module and function documentation
- `src/Language/JavaScript/Process/Minify.hs` — module and function documentation

## Verification

1. `cabal haddock` — renders without warnings
2. `cabal build` — compiles (doc-only changes)
3. Verify examples in documentation are accurate by testing in GHCi
