# language-javascript

A Haskell library for parsing JavaScript source code into an Abstract Syntax Tree (AST).

## Features

- **ES5 through ES2021** syntax coverage (near-complete)
- **FlatParse-based lexer/parser** for high-performance zero-copy parsing
- **Faithful pretty printer** that reconstructs JavaScript from the AST (round-trip safe)
- **JavaScript minifier** (whitespace/comment stripping, string concat, var merging)
- **Tree shaker** with scope analysis and dead code elimination
- **Template Haskell quasi-quoters** (`jsast` and `jsx`) for compile-time JavaScript embedding
- **Smart constructors** (`Language.JavaScript.Parser.Build`) for ergonomic AST construction
- **Serialization** to JSON, XML, and S-expression formats

## Quick Start

```haskell
import Language.JavaScript.Parser (parse, renderToString)

main :: IO ()
main = do
  case parse "function add(a, b) { return a + b; }" "example.js" of
    Left err  -> putStrLn ("Parse error: " ++ err)
    Right ast -> putStrLn (renderToString ast)
```

## Installation

Add to your `.cabal` file:

```cabal
build-depends: language-javascript >= 0.8 && < 0.9
```

## API Overview

### Parsing

```haskell
import Language.JavaScript.Parser

-- Parse JavaScript source code
parse :: String -> String -> Either String JSAST

-- Parse from ByteString (recommended for performance)
parseBS :: ByteString -> String -> Either String JSAST
```

### Pretty Printing

```haskell
import Language.JavaScript.Pretty.Printer (renderToString)

-- Render AST back to JavaScript source
renderToString :: JSAST -> String
```

### Minification

```haskell
import Language.JavaScript.Process.Minify (minifyJS)

-- Minify JavaScript (strip whitespace, comments, merge vars)
minifyJS :: JSAST -> JSAST
```

### Quasi-Quoters

```haskell
{-# LANGUAGE QuasiQuotes #-}
import Language.JavaScript.QQ (jsast, jsx)

-- Compile-time parsed AST
myAst :: JSAST
myAst = [jsast| var x = 42; |]

-- With Haskell expression splicing
buildGreeting :: JSExpression -> JSAST
buildGreeting nameExpr = [jsx| console.log("Hello " + ${nameExpr}); |]
```

### Smart Constructors

```haskell
import Language.JavaScript.Parser.Build

-- Build AST nodes without manual annotation threading
let ident = mkIdent "x"
    num   = mkDecimal 42
    binOp = mkBinOp ident JSBinOpPlus num
```

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
