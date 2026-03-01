{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Smart constructors for building JavaScript ASTs without manual annotation threading.
--
-- These constructors use a default empty annotation ('noAnnot'), making it easy
-- to build ASTs programmatically for testing, code generation, and transformation
-- without needing to specify source locations and comment data at every node.
--
-- ==== Usage
--
-- > import Language.JavaScript.Parser.Build
-- >
-- > -- Build "x + 1"
-- > let expr = mkBinOp (mkIdent "x") (JSBinOpPlus noAnnot) (mkDecimal 1)
-- >
-- > -- Build "var x = 42;"
-- > let stmt = mkVar "x" (mkDecimal 42)
-- >
-- > -- Build "return x;"
-- > let ret = mkReturn (mkIdent "x")
--
-- @since 0.7.1.0
module Language.JavaScript.Parser.Build
  ( -- * Annotation
    noAnnot,
    noSemi,
    autoSemi,

    -- * Identifiers and Literals
    mkIdent,
    mkDecimal,
    mkHexInteger,
    mkString,
    mkBool,
    mkNull,

    -- * Expressions
    mkBinOp,
    mkAssign,
    mkCall,
    mkDot,
    mkIndex,
    mkUnary,

    -- * Statements
    mkVar,
    mkReturn,
    mkExprStmt,
    mkBlock,

    -- * Lists
    mkCommaList,
    toCommaList,
  )
where

import Data.ByteString (ByteString)
import qualified Data.List as List
import Language.JavaScript.Parser.AST
  ( JSAnnot (..),
    JSAssignOp (..),
    JSBinOp,
    JSBlock (..),
    JSCommaList (..),
    JSExpression (..),
    JSSemi (..),
    JSStatement (..),
    JSUnaryOp,
    JSVarInitializer (..),
    fromCommaList,
  )
import Language.JavaScript.Parser.SrcLocation (TokenPosn (..))

-- | Default empty annotation at position zero with no comments.
--
-- Produces a 'JSAnnot' with 'TokenPn 0 0 0' and an empty comment list,
-- suitable for programmatically constructed AST nodes where source
-- location is irrelevant.
--
-- @since 0.7.1.0
noAnnot :: JSAnnot
noAnnot = JSAnnot (TokenPn 0 0 0) []

-- | Automatic semicolon insertion marker.
--
-- Represents an automatically-inserted semicolon, used when no explicit
-- semicolon token is present in the source (e.g., before a closing brace
-- or at end-of-input).
--
-- @since 0.7.1.0
noSemi :: JSSemi
noSemi = JSSemiAuto

-- | Automatic semicolon insertion marker (alias for 'noSemi').
--
-- Synonym provided for readability in contexts where "auto" is clearer
-- than "no" (e.g., @mkExprStmt expr@ uses @autoSemi@ internally).
--
-- @since 0.7.1.0
autoSemi :: JSSemi
autoSemi = JSSemiAuto

-- | Build an identifier expression from a name.
--
-- Wraps the given 'ByteString' in a 'JSIdentifier' with 'noAnnot'.
--
-- >>> mkIdent "foo"
-- JSIdentifier (JSAnnot (TokenPn 0 0 0) []) "foo"
--
-- @since 0.7.1.0
mkIdent :: ByteString -> JSExpression
mkIdent = JSIdentifier noAnnot

-- | Build a decimal numeric literal from a 'Double'.
--
-- Wraps the given value in a 'JSDecimal' with 'noAnnot'.
--
-- >>> mkDecimal 42
-- JSDecimal (JSAnnot (TokenPn 0 0 0) []) 42.0
--
-- @since 0.7.1.0
mkDecimal :: Double -> JSExpression
mkDecimal = JSDecimal noAnnot

-- | Build a hexadecimal integer literal from an 'Integer'.
--
-- Wraps the given value in a 'JSHexInteger' with 'noAnnot'.
--
-- >>> mkHexInteger 255
-- JSHexInteger (JSAnnot (TokenPn 0 0 0) []) 255
--
-- @since 0.7.1.0
mkHexInteger :: Integer -> JSExpression
mkHexInteger = JSHexInteger noAnnot

-- | Build a string literal from a 'ByteString'.
--
-- Wraps the given value in a 'JSStringLiteral' with 'noAnnot'.
-- The string should include surrounding quotes as they appear
-- in JavaScript source (e.g., @"'hello'"@ or @"\"hello\""@).
--
-- >>> mkString "'hello'"
-- JSStringLiteral (JSAnnot (TokenPn 0 0 0) []) "'hello'"
--
-- @since 0.7.1.0
mkString :: ByteString -> JSExpression
mkString = JSStringLiteral noAnnot

-- | Build a boolean literal (@true@ or @false@).
--
-- Produces a 'JSLiteral' containing @"true"@ or @"false"@
-- with 'noAnnot'.
--
-- >>> mkBool True
-- JSLiteral (JSAnnot (TokenPn 0 0 0) []) "true"
--
-- @since 0.7.1.0
mkBool :: Bool -> JSExpression
mkBool b = JSLiteral noAnnot (boolToBS b)
  where
    boolToBS True = "true"
    boolToBS False = "false"

-- | Build a @null@ literal.
--
-- Produces a 'JSLiteral' containing @"null"@ with 'noAnnot'.
--
-- >>> mkNull
-- JSLiteral (JSAnnot (TokenPn 0 0 0) []) "null"
--
-- @since 0.7.1.0
mkNull :: JSExpression
mkNull = JSLiteral noAnnot "null"

-- | Build a binary expression from a left operand, operator, and right operand.
--
-- The caller supplies the 'JSBinOp' (which carries its own annotation).
-- This constructor simply assembles the three parts into a
-- 'JSExpressionBinary' node.
--
-- >>> mkBinOp (mkDecimal 1) (JSBinOpPlus noAnnot) (mkDecimal 2)
-- JSExpressionBinary (JSDecimal ...) (JSBinOpPlus ...) (JSDecimal ...)
--
-- @since 0.7.1.0
mkBinOp :: JSExpression -> JSBinOp -> JSExpression -> JSExpression
mkBinOp = JSExpressionBinary

-- | Build an assignment expression from a target, operator, and value.
--
-- The caller supplies the 'JSAssignOp' (which carries its own annotation).
-- For simple assignment, use @JSAssign noAnnot@ as the operator.
--
-- >>> mkAssign (mkIdent "x") (JSAssign noAnnot) (mkDecimal 42)
-- JSAssignExpression (JSIdentifier ...) (JSAssign ...) (JSDecimal ...)
--
-- @since 0.7.1.0
mkAssign :: JSExpression -> JSAssignOp -> JSExpression -> JSExpression
mkAssign = JSAssignExpression

-- | Build a function call expression from a callee and argument list.
--
-- Wraps the arguments in parentheses annotations using 'noAnnot'.
--
-- >>> mkCall (mkIdent "foo") [mkDecimal 1, mkIdent "x"]
-- JSCallExpression (JSIdentifier ...) ... (JSLCons ...) ...
--
-- @since 0.7.1.0
mkCall :: JSExpression -> [JSExpression] -> JSExpression
mkCall callee args =
  JSCallExpression callee noAnnot (mkCommaList args) noAnnot

-- | Build a dot-access (member) expression.
--
-- Constructs @lhs.rhs@ as a 'JSMemberDot' node with 'noAnnot' for
-- the dot annotation.
--
-- >>> mkDot (mkIdent "obj") (mkIdent "prop")
-- JSMemberDot (JSIdentifier ...) ... (JSIdentifier ...)
--
-- @since 0.7.1.0
mkDot :: JSExpression -> JSExpression -> JSExpression
mkDot lhs rhs = JSMemberDot lhs noAnnot rhs

-- | Build a bracket-access (index) expression.
--
-- Constructs @lhs[idx]@ as a 'JSMemberSquare' node with 'noAnnot'
-- for the bracket annotations.
--
-- >>> mkIndex (mkIdent "arr") (mkDecimal 0)
-- JSMemberSquare (JSIdentifier ...) ... (JSDecimal ...) ...
--
-- @since 0.7.1.0
mkIndex :: JSExpression -> JSExpression -> JSExpression
mkIndex lhs idx = JSMemberSquare lhs noAnnot idx noAnnot

-- | Build a unary expression from an operator and operand.
--
-- The caller supplies the 'JSUnaryOp' (which carries its own annotation).
--
-- >>> mkUnary (JSUnaryOpMinus noAnnot) (mkDecimal 1)
-- JSUnaryExpression (JSUnaryOpMinus ...) (JSDecimal ...)
--
-- @since 0.7.1.0
mkUnary :: JSUnaryOp -> JSExpression -> JSExpression
mkUnary = JSUnaryExpression

-- | Build a @var@ declaration statement with an initializer.
--
-- Constructs @var name = expr;@ as a 'JSVariable' node with a single
-- 'JSVarInitExpression' in the comma list and 'autoSemi'.
--
-- >>> mkVar "x" (mkDecimal 42)
-- JSVariable ... (JSLOne (JSVarInitExpression (JSIdentifier ... "x") (JSVarInit ... (JSDecimal ... 42.0)))) ...
--
-- @since 0.7.1.0
mkVar :: ByteString -> JSExpression -> JSStatement
mkVar name expr =
  JSVariable noAnnot (JSLOne (JSVarInitExpression (mkIdent name) initExpr)) autoSemi
  where
    initExpr = JSVarInit noAnnot expr

-- | Build a @return@ statement with an expression.
--
-- Constructs @return expr;@ as a 'JSReturn' node with 'noAnnot' and
-- 'autoSemi'. The expression is wrapped in 'Just'.
--
-- >>> mkReturn (mkIdent "x")
-- JSReturn ... (Just (JSIdentifier ... "x")) ...
--
-- @since 0.7.1.0
mkReturn :: JSExpression -> JSStatement
mkReturn expr = JSReturn noAnnot (Just expr) autoSemi

-- | Build an expression statement.
--
-- Wraps a 'JSExpression' in a 'JSExpressionStatement' with 'autoSemi',
-- representing an expression used as a statement (e.g., a function call
-- on its own line).
--
-- >>> mkExprStmt (mkCall (mkIdent "foo") [])
-- JSExpressionStatement (JSCallExpression ...) ...
--
-- @since 0.7.1.0
mkExprStmt :: JSExpression -> JSStatement
mkExprStmt expr = JSExpressionStatement expr autoSemi

-- | Build a block statement from a list of statements.
--
-- Wraps the statements in braces using 'noAnnot' for the opening
-- and closing brace annotations.
--
-- >>> mkBlock [mkReturn (mkDecimal 42)]
-- JSBlock ... [JSReturn ...] ...
--
-- @since 0.7.1.0
mkBlock :: [JSStatement] -> JSBlock
mkBlock stmts = JSBlock noAnnot stmts noAnnot

-- | Convert a Haskell list to a 'JSCommaList'.
--
-- Builds a left-nested cons structure with 'noAnnot' comma annotations.
-- An empty list produces 'JSLNil', a singleton produces 'JSLOne', and
-- longer lists produce nested 'JSLCons' nodes.
--
-- >>> mkCommaList []
-- JSLNil
--
-- >>> mkCommaList [mkIdent "x"]
-- JSLOne (JSIdentifier ...)
--
-- >>> mkCommaList [mkIdent "x", mkIdent "y"]
-- JSLCons (JSLOne (JSIdentifier ... "x")) ... (JSIdentifier ... "y")
--
-- @since 0.7.1.0
mkCommaList :: [a] -> JSCommaList a
mkCommaList [] = JSLNil
mkCommaList (x : xs) = List.foldl' (\acc el -> JSLCons acc noAnnot el) (JSLOne x) xs

-- | Convert a 'JSCommaList' back to a Haskell list.
--
-- Extracts elements from the comma-list structure, discarding comma
-- annotations. This is a re-export of 'fromCommaList' from the AST
-- module for convenience.
--
-- >>> toCommaList (JSLOne (mkIdent "x"))
-- [JSIdentifier ...]
--
-- >>> toCommaList JSLNil
-- []
--
-- @since 0.7.1.0
toCommaList :: JSCommaList a -> [a]
toCommaList = fromCommaList
