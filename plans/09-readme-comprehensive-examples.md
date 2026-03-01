# Plan 09: Comprehensive README with Real-World Examples

**Priority**: P2 (Enhancement — first impression for new users)
**Effort**: Medium (2-3 hours)
**Risk**: Zero (documentation only)

## Problem

The current README is minimal. For a library with multiple API tiers, a rich AST, minification support, quasi-quoters, and tree-shaking, the README needs to demonstrate:

1. **Quick start** — parse "Hello World" in 3 lines
2. **All three API tiers** — String, Text, ByteString with when to use each
3. **Module parsing** — ES6 import/export
4. **AST inspection** — pattern matching on common constructs
5. **Pretty printing** — render AST back to source
6. **Minification** — reduce JavaScript size
7. **Error handling** — structured vs string errors
8. **Quasi-quoters** — compile-time JavaScript parsing
9. **Performance** — benchmark numbers and guidance

## Current README Gaps

- No ByteString API example
- No Text API example
- No module parsing example
- No minification example
- No error handling example
- No quasi-quoter example
- No performance numbers
- No "when to use which API" guidance

## Solution

### README Structure

```markdown
# language-javascript

Fast JavaScript parser for Haskell. Parses ES5 through ES2022+ syntax
into a typed AST with pretty printing and minification support.

## Features
- Complete ES5-ES2022 JavaScript syntax support
- Three API tiers: ByteString (fastest), Text, String
- Pretty printer with round-trip fidelity
- JavaScript minification
- Compile-time quasi-quoters
- ~4-6 MB/s parsing throughput

## Quick Start

## Parsing JavaScript

### String API (simplest)
### ByteString API (fastest)
### Text API
### Module Parsing (ES6 import/export)
### File Parsing

## Working with the AST

### Pattern Matching
### Common AST Patterns

## Pretty Printing

## Minification

## Error Handling

### String Errors (simple)
### Structured Errors (rich)

## Quasi-Quoters

## Performance

## API Reference

## License
```

### Key Examples to Include

```haskell
-- Quick start
import Language.JavaScript.Parser (parse, renderToString)

main :: IO ()
main = case parse "var x = 42;" "example" of
  Left err  -> putStrLn ("Error: " <> err)
  Right ast -> putStrLn (renderToString ast)

-- ByteString API (production use)
import qualified Data.ByteString as BS
import Language.JavaScript.Parser (parseBS)

parseFile :: FilePath -> IO ()
parseFile path = do
  content <- BS.readFile path
  case parseBS content of
    Left err  -> putStrLn err
    Right ast -> print ast

-- Minification
import Language.JavaScript.Parser (parse, renderToString)
import Language.JavaScript.Process.Minify (minifyJS)

minify :: String -> Either String String
minify input = renderToString . minifyJS <$> parse input "input"
```

## Files Changed

- `README.md` — complete rewrite with comprehensive examples

## Verification

1. All code examples compile (test in GHCi or a scratch file)
2. All code examples produce the stated output
3. Benchmark numbers match actual `cabal bench` output
