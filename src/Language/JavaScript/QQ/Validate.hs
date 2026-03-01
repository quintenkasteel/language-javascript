{-# LANGUAGE TemplateHaskell #-}

-- | Compile-time JavaScript validation quasi-quoter.
--
-- The @js@ quasi-quoter parses JavaScript source code at compile time,
-- reporting any syntax errors as Haskell compilation errors. On success,
-- the original source text is embedded as a 'String' literal.
--
-- This is useful when you want to ensure JavaScript snippets in your
-- Haskell source are syntactically valid without paying the cost of
-- carrying the parsed AST at runtime.
--
-- ==== Examples
--
-- @
-- {-\# LANGUAGE QuasiQuotes \#-}
-- import Language.JavaScript.QQ (js)
--
-- myScript :: String
-- myScript = [js| var x = 42; console.log(x); |]
-- -- Compile error if JS is invalid
-- @
--
-- @since 0.8.0.0
module Language.JavaScript.QQ.Validate
  ( -- * Quasi-quoter
    js,
  )
where

import Control.Exception (evaluate, try, SomeException)
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import qualified Language.Haskell.TH as TH
import qualified Language.JavaScript.Parser.Parser as Parser

-- | Quasi-quoter that validates JavaScript at compile time and returns
-- the source as a 'String'.
--
-- Parses the quoted JavaScript using the project's parser. If parsing
-- fails, a compile-time error is raised with the parse error message
-- and source location. If parsing succeeds, the original source text
-- is embedded as a string literal.
--
-- ==== Usage
--
-- @
-- [js| function add(a, b) { return a + b; } |]
-- @
--
-- @since 0.8.0.0
js :: QuasiQuoter
js =
  QuasiQuoter
    { quoteExp = validateJS,
      quotePat = unsupported "pattern",
      quoteType = unsupported "type",
      quoteDec = unsupported "declaration"
    }
  where
    unsupported ctx _ = fail ("js quasi-quoter cannot be used in a " <> ctx <> " context")

-- | Validate JavaScript source at compile time and embed as string literal.
--
-- Uses 'TH.runIO' to force parsing through compiled code rather than
-- the GHC bytecode interpreter, which is necessary because the flatparse
-- backend uses low-level operations incompatible with bytecode evaluation.
validateJS :: String -> Q Exp
validateJS input = do
  loc <- TH.location
  eitherExc <- TH.runIO (try (evaluate (parseToString input (TH.loc_filename loc))) :: IO (Either SomeException String))
  case eitherExc of
    Left exc -> fail ("js: exception during parse: " <> show exc)
    Right "OK" -> TH.litE (TH.stringL input)
    Right err -> fail (formatError loc err)

-- | Parse JavaScript and return "OK" or an error string.
-- This is a strict helper to avoid lazy evaluation issues in TH.
parseToString :: String -> String -> String
parseToString input srcName =
  case Parser.parse input srcName of
    Left err -> err
    Right _ -> "OK"

-- | Format a parse error with Haskell source location context.
formatError :: TH.Loc -> String -> String
formatError loc err =
  "js: JavaScript parse error in "
    <> TH.loc_filename loc
    <> ": "
    <> err
