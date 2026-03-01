# language-javascript

Fast, production-ready JavaScript parser for Haskell. Parses ECMAScript 5 through ES2022+ syntax into a typed AST with pretty printing, minification, and tree shaking.

## Features

- **Complete JavaScript syntax support** — ES5 through ES2022+ including async/await, optional chaining, nullish coalescing, BigInt, and more
- **Three API tiers** — ByteString (fastest, zero-copy), Text, and String interfaces
- **High-performance FlatParse backend** — 5-10x faster than the previous Alex/Happy implementation
- **Round-trip pretty printer** — reconstructs JavaScript from the AST preserving semantic equivalence
- **JavaScript minifier** — whitespace/comment stripping, string concatenation, variable merging
- **Tree shaker** — scope analysis and dead code elimination
- **Template Haskell quasi-quoters** — compile-time JavaScript parsing with `jsast` and `jsx`
- **Smart constructors** — ergonomic AST construction via `Language.JavaScript.Parser.Build`
- **Serialization** — JSON, XML, and S-expression output formats
- **Input validation** — size limits (10 MB) and UTF-8 validation for production safety

## Quick Start

```haskell
import Language.JavaScript.Parser (parse, renderToString)

main :: IO ()
main =
  case parse "function add(a, b) { return a + b; }" "example.js" of
    Left err  -> putStrLn ("Parse error: " ++ err)
    Right ast -> putStrLn (renderToString ast)
```

## Installation

Add to your `.cabal` file:

```cabal
build-depends: language-javascript >= 0.8 && < 0.9
```

## Parsing JavaScript

### String API (simplest)

```haskell
import Language.JavaScript.Parser (parse, parseModule)

-- Parse a JavaScript program
parseResult :: Either String JSAST
parseResult = parse "var x = 42;" "source.js"

-- Parse an ES6 module (import/export declarations)
moduleResult :: Either String JSAST
moduleResult = parseModule "import { foo } from './bar'; export default foo;" "module.js"
```

### ByteString API (highest performance)

For production use where performance matters. Avoids intermediate String/Text conversions.

```haskell
import qualified Data.ByteString as BS
import Language.JavaScript.Parser (parseByteString, parseModuleByteString)

parseFromFile :: FilePath -> IO (Either String JSAST)
parseFromFile path = do
  content <- BS.readFile path
  pure (parseByteString content)
```

### Text API

Convenient for applications already working with `Text` values.

```haskell
import Data.Text (Text)
import Language.JavaScript.Parser (parseText, parseModuleText)

parseTextInput :: Text -> Either String JSAST
parseTextInput = parseText
```

### File Parsing

Safe file parsing that returns errors as values (never throws on parse failure):

```haskell
import Language.JavaScript.Parser (parseFileSafe, parseFileUtf8Safe)

-- Uses system locale encoding
result1 <- parseFileSafe "script.js"

-- Explicit UTF-8 encoding
result2 <- parseFileUtf8Safe "script.js"
```

### Structured Error API

For applications that need rich error information:

```haskell
import Language.JavaScript.Parser
  ( parseProgramByteString
  , ParseResult(..), ParseSuccess(..), ParseFailure(..)
  , formatParseError
  )

case parseProgramByteString input of
  ParseOK success -> process (parseResult success)
  ParseError failure -> reportError (formatParseError failure)
```

## Working with the AST

### Pattern Matching

```haskell
import Language.JavaScript.Parser (parse)
import Language.JavaScript.Parser.AST

extractFunctionNames :: JSAST -> [String]
extractFunctionNames (JSAstProgram stmts _) = concatMap stmtFunctions stmts
extractFunctionNames _ = []

stmtFunctions :: JSStatement -> [String]
stmtFunctions (JSFunction _ (JSIdentName _ name) _ _ _ _ _) = [show name]
stmtFunctions _ = []
```

### Displaying Stripped AST

The `showStripped` function renders the AST without position/annotation noise, useful for testing and debugging:

```haskell
import Language.JavaScript.Parser (parse, showStripped)

case parse "var x = 1 + 2;" "test" of
  Right ast -> putStrLn (showStripped ast)
  Left err  -> putStrLn err
-- Output: JSAstProgram (JSVariable (JSLOne (JSVarInitExpression (JSIdentifier 'x') ...)))
```

## Pretty Printing

Render an AST back to JavaScript source text:

```haskell
import Language.JavaScript.Parser (parse)
import Language.JavaScript.Pretty.Printer (renderToString, renderToText, renderJS)

-- renderToString :: JSAST -> String       (most convenient)
-- renderToText   :: JSAST -> Text         (avoids String allocation)
-- renderJS       :: JSAST -> Doc          (for composition with other documents)

roundTrip :: String -> Either String String
roundTrip src = renderToString <$> parse src "input"
```

## Minification

Reduce JavaScript size by stripping whitespace, comments, normalizing quotes, and merging declarations:

```haskell
import Language.JavaScript.Parser (parse)
import Language.JavaScript.Pretty.Printer (renderToString)
import Language.JavaScript.Process.Minify (minifyJS)

minify :: String -> Either String String
minify src = renderToString . minifyJS <$> parse src "input"

-- Example:
-- minify "var  x  =  'hello'  +  'world' ;"
-- Right "var x='helloworld'"
```

## Tree Shaking

Eliminate dead code with scope-aware analysis:

```haskell
import Language.JavaScript.Parser (parse)
import Language.JavaScript.Pretty.Printer (renderToString)
import Language.JavaScript.Process.TreeShake (treeShake, defaultShakeConfig)

shake :: String -> Either String String
shake src = renderToString . treeShake defaultShakeConfig <$> parse src "input"
```

## Error Handling

Parse errors include source position and descriptive messages:

```haskell
case parse "var x = ;" "test.js" of
  Left err -> putStrLn err
  -- Output: "test.js: Syntax Error at 1:9\n  unexpected input"
  Right ast -> process ast
```

## Quasi-Quoters

Embed JavaScript in Haskell source with compile-time parsing:

```haskell
{-# LANGUAGE QuasiQuotes #-}
import Language.JavaScript.QQ (jsast, jsx)

-- Compile-time parsed AST (parse errors become compile errors)
myAst :: JSAST
myAst = [jsast| var x = 42; |]

-- With Haskell expression splicing
buildGreeting :: JSExpression -> JSAST
buildGreeting nameExpr = [jsx| console.log("Hello " + ${nameExpr}); |]
```

## API Reference

| Function | Type | Description |
|----------|------|-------------|
| `parse` | `String -> String -> Either String JSAST` | Parse JS program from String |
| `parseModule` | `String -> String -> Either String JSAST` | Parse ES6 module from String |
| `parseByteString` | `ByteString -> Either String JSAST` | Parse JS program from ByteString (fastest) |
| `parseModuleByteString` | `ByteString -> Either String JSAST` | Parse ES6 module from ByteString |
| `parseText` | `Text -> Either String JSAST` | Parse JS program from Text |
| `parseModuleText` | `Text -> Either String JSAST` | Parse ES6 module from Text |
| `parseFileSafe` | `FilePath -> IO (Either String JSAST)` | Parse JS file (safe) |
| `parseFileUtf8Safe` | `FilePath -> IO (Either String JSAST)` | Parse JS file with UTF-8 (safe) |
| `renderToString` | `JSAST -> String` | Render AST to JavaScript source |
| `renderToText` | `JSAST -> Text` | Render AST to JavaScript Text |
| `minifyJS` | `JSAST -> JSAST` | Minify AST |
| `treeShake` | `ShakeConfig -> JSAST -> JSAST` | Tree-shake AST |
| `showStripped` | `JSAST -> String` | Display AST without annotations |

## Building

```bash
cabal build
```

## Testing

```bash
cabal test
```

## License

BSD-3-Clause. See [LICENSE](LICENSE) for details.
