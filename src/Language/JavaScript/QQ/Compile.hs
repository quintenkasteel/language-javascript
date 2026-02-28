{-# LANGUAGE TemplateHaskell #-}

-- | Compile-time JavaScript-to-AST quasi-quoter.
--
-- The @jsast@ quasi-quoter parses JavaScript source code at compile time
-- and embeds the resulting 'JSAST' value directly into the Haskell program.
-- Syntax errors are reported as compile-time errors.
--
-- This is useful when you want a pre-parsed AST available at runtime
-- without paying parsing costs, while still catching syntax errors early.
--
-- ==== Examples
--
-- @
-- {-\# LANGUAGE QuasiQuotes \#-}
-- import Language.JavaScript.QQ (jsast)
-- import Language.JavaScript.Parser.AST (JSAST)
--
-- myAst :: JSAST
-- myAst = [jsast| function add(a, b) { return a + b; } |]
-- @
--
-- @since 0.8.0.0
module Language.JavaScript.QQ.Compile
  ( -- * Quasi-quoter
    jsast,
  )
where

import Control.Exception (evaluate)
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (liftData)
import qualified Language.Haskell.TH as TH
import qualified Language.JavaScript.Parser.Parser as Parser

-- | Quasi-quoter that parses JavaScript at compile time and embeds the
-- resulting 'JSAST' value.
--
-- The JavaScript source is parsed using the project's parser at compile
-- time. On failure, a compile-time error is raised. On success, the
-- parsed 'JSAST' is embedded into the Haskell program using 'liftData'
-- (via 'Data' instances), making it available at runtime without any
-- parsing overhead.
--
-- ==== Usage
--
-- @
-- [jsast| var x = 42; |]
-- @
--
-- @since 0.8.0.0
jsast :: QuasiQuoter
jsast =
  QuasiQuoter
    { quoteExp = compileJS,
      quotePat = unsupported "pattern",
      quoteType = unsupported "type",
      quoteDec = unsupported "declaration"
    }
  where
    unsupported ctx _ = fail ("jsast quasi-quoter cannot be used in a " <> ctx <> " context")

-- | Parse JavaScript source at compile time and embed the AST.
--
-- Uses 'TH.runIO' to force parsing through compiled code rather than
-- the GHC bytecode interpreter, which is necessary because the flatparse
-- backend uses low-level operations incompatible with bytecode evaluation.
compileJS :: String -> Q Exp
compileJS input = do
  loc <- TH.location
  result <- TH.runIO (evaluate (Parser.parse input (TH.loc_filename loc)))
  case result of
    Left err -> fail (formatError loc err)
    Right ast -> liftData ast

-- | Format a parse error with Haskell source location context.
formatError :: TH.Loc -> String -> String
formatError loc err =
  "jsast: JavaScript parse error in "
    <> TH.loc_filename loc
    <> ": "
    <> err
