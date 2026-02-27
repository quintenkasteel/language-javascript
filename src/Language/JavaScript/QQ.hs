-- | JavaScript quasi-quoters for embedding JS in Haskell source.
--
-- This module provides three quasi-quoters for working with JavaScript
-- inside Haskell:
--
--   * 'js' — Validates JavaScript at compile time, returns the source as 'String'
--   * 'jsast' — Parses JavaScript at compile time, returns the AST as 'JSAST'
--   * 'jsx' — Parses JavaScript with @${expr}@ antiquotation, returns 'JSAST'
--
-- All three perform compile-time syntax validation, catching JavaScript
-- errors during Haskell compilation rather than at runtime.
--
-- ==== Examples
--
-- Validated string (no runtime parsing cost, source preserved):
--
-- @
-- {-\# LANGUAGE QuasiQuotes \#-}
-- import Language.JavaScript.QQ (js)
--
-- myScript :: String
-- myScript = [js| var x = 42; console.log(x); |]
-- @
--
-- Pre-compiled AST (no runtime parsing cost, AST available):
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
-- Antiquotation (splice Haskell values into JS):
--
-- @
-- {-\# LANGUAGE QuasiQuotes \#-}
-- import Language.JavaScript.QQ (jsx)
-- import Language.JavaScript.Parser.AST (JSAST, JSExpression(..))
--
-- buildGreeting :: JSExpression -> JSAST
-- buildGreeting nameExpr = [jsx| console.log("Hello " + ${nameExpr}); |]
-- @
--
-- @since 0.8.0.0
module Language.JavaScript.QQ
  ( -- * Quasi-quoters
    js,
    jsast,
    jsx,
  )
where

import Language.JavaScript.QQ.Antiquote (jsx)
import Language.JavaScript.QQ.Compile (jsast)
import Language.JavaScript.QQ.Validate (js)
