{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | High-level DSL for constructing JavaScript ASTs programmatically.
--
-- This module provides a clean, readable API for building JavaScript AST nodes
-- without manual annotation threading. Every constructor uses 'JSAnnotSpace'
-- for natural whitespace in formatted output.
--
-- Designed for compiler code generation — particularly ESM module output.
--
-- ==== Example: Building an ESM module
--
-- @
-- import Language.JavaScript.DSL
--
-- myModule :: JSAST
-- myModule = jsModule
--   [ jsImportNamed ["Component"] "\'react\'"
--   , jsExportConst "greeting" (jsStr "\'hello\'")
--   , jsExportFunction "add" ["x", "y"]
--       [jsReturn (jsAdd (jsVar "x") (jsVar "y"))]
--   ]
-- @
--
-- @since 0.8.1.0
module Language.JavaScript.DSL
  ( -- * Module Construction
    jsModule

    -- * Import Declarations
  , jsImportNamed
  , jsImportDefault
  , jsImportBare
  , jsImportNamespace

    -- * Export Declarations
  , jsExportNamed
  , jsExportDefault
  , jsExport
  , jsExportFunction
  , jsExportConst

    -- * Statements
  , jsConst
  , jsLet
  , jsVarDecl
  , jsFunction
  , jsReturn
  , jsThrow
  , jsTry
  , jsIf
  , jsIfElse
  , jsSwitch
  , jsWhile
  , jsDoWhile
  , jsFor
  , jsBreak
  , jsBreakLabel
  , jsContinue
  , jsContinueLabel
  , jsLabelled
  , jsBlock
  , jsExprStmt

    -- * Expressions
  , jsVar
  , jsInt
  , jsNum
  , jsStr
  , jsBool
  , jsNull
  , jsThis
  , jsArray
  , jsObject
  , jsObjectShorthand
  , jsArrow
  , jsArrowBlock
  , jsCall
  , jsDot
  , jsIndex
  , jsAssign
  , jsTernary
  , jsNew
  , jsSpread
  , jsTemplateLiteral
  , jsParen

    -- * Binary Operators
  , jsAdd
  , jsSub
  , jsMul
  , jsDiv
  , jsMod
  , jsStrictEq
  , jsStrictNeq
  , jsLt
  , jsLe
  , jsGt
  , jsGe
  , jsAnd
  , jsOr
  , jsBitAnd
  , jsBitOr
  , jsBitXor
  , jsLShift
  , jsRShift
  , jsURShift
  , jsInstanceOf
  , jsIn
  , jsNullishCoalesce

    -- * Unary Operators
  , jsNot
  , jsNeg
  , jsTypeof
  , jsVoid
  , jsBitNot

    -- * Compound Assignment
  , jsPlusAssign
  , jsMinusAssign
  , jsTimesAssign
  , jsDivAssign

    -- * Annotation Helpers
  , sp
  , noSp
  ) where

import Data.ByteString (ByteString)
import qualified Data.List as List
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.SrcLocation (TokenPosn (..))
import Language.JavaScript.Parser.Token (CommentAnnotation (..))


-- --------------------------------------------------------------------------
-- Annotations
-- --------------------------------------------------------------------------

-- | Annotation with a single leading space — the default for readable output.
--
-- @since 0.8.1.0
sp :: JSAnnot
sp = JSAnnotSpace

-- | No annotation — for tight grouping (parens, brackets).
--
-- @since 0.8.1.0
noSp :: JSAnnot
noSp = JSNoAnnot

-- | Annotation for newline separation between top-level items.
nlAnnot :: JSAnnot
nlAnnot = JSAnnot (TokenPn 0 0 0) [WhiteSpace (TokenPn 0 0 0) "\n"]

-- | Semicolon terminator.
semi :: JSSemi
semi = JSSemi noSp

-- | Auto-semicolon (no visible semicolon in output).
autoSemi :: JSSemi
autoSemi = JSSemiAuto


-- --------------------------------------------------------------------------
-- Module Construction
-- --------------------------------------------------------------------------

-- | Build a complete ESM module from a list of module items.
--
-- >>> renderToString (jsModule [jsExportConst "x" (jsInt 42)])
-- "export const x = 42;\n"
--
-- @since 0.8.1.0
jsModule :: [JSModuleItem] -> JSAST
jsModule items = JSAstModule items nlAnnot


-- --------------------------------------------------------------------------
-- Import Declarations
-- --------------------------------------------------------------------------

-- | Named import: @import { a, b } from \'module\';@
--
-- >>> jsImportNamed ["useState", "useEffect"] "\'react\'"
--
-- @since 0.8.1.0
jsImportNamed :: [ByteString] -> ByteString -> JSModuleItem
jsImportNamed names modPath =
  JSModuleImportDeclaration noSp
    (JSImportDeclaration
      (JSImportClauseNamed
        (JSImportsNamed sp (buildImportSpecList names) sp))
      (JSFromClause sp sp modPath)
      Nothing
      semi)

-- | Default import: @import name from \'module\';@
--
-- @since 0.8.1.0
jsImportDefault :: ByteString -> ByteString -> JSModuleItem
jsImportDefault name modPath =
  JSModuleImportDeclaration noSp
    (JSImportDeclaration
      (JSImportClauseDefault (JSIdentName sp name))
      (JSFromClause sp sp modPath)
      Nothing
      semi)

-- | Bare import (side-effects only): @import \'module\';@
--
-- @since 0.8.1.0
jsImportBare :: ByteString -> JSModuleItem
jsImportBare modPath =
  JSModuleImportDeclaration noSp
    (JSImportDeclarationBare sp modPath Nothing semi)

-- | Namespace import: @import * as name from \'module\';@
--
-- @since 0.8.1.0
jsImportNamespace :: ByteString -> ByteString -> JSModuleItem
jsImportNamespace name modPath =
  JSModuleImportDeclaration noSp
    (JSImportDeclaration
      (JSImportClauseNameSpace
        (JSImportNameSpace (JSBinOpTimes sp) sp (JSIdentName sp name)))
      (JSFromClause sp sp modPath)
      Nothing
      semi)


-- --------------------------------------------------------------------------
-- Export Declarations
-- --------------------------------------------------------------------------

-- | Export named bindings: @export { a, b, c };@
--
-- @since 0.8.1.0
jsExportNamed :: [ByteString] -> JSModuleItem
jsExportNamed names =
  JSModuleExportDeclaration noSp
    (JSExportLocals
      (JSExportClause sp (buildExportSpecList names) sp)
      semi)

-- | Export default expression: @export default expr;@
--
-- @since 0.8.1.0
jsExportDefault :: JSExpression -> JSModuleItem
jsExportDefault expr =
  JSModuleExportDeclaration noSp
    (JSExportDefault sp (JSExpressionStatement expr autoSemi) autoSemi)

-- | Export a statement: @export const x = 1;@ or @export function f() {}@
--
-- @since 0.8.1.0
jsExport :: JSStatement -> JSModuleItem
jsExport stmt =
  JSModuleExportDeclaration noSp
    (JSExport stmt autoSemi)

-- | Export a function declaration.
--
-- @jsExportFunction \"add\" [\"x\", \"y\"] [jsReturn (jsAdd (jsVar \"x\") (jsVar \"y\"))]@
--
-- Produces: @export function add(x, y) { return x + y; }@
--
-- @since 0.8.1.0
jsExportFunction :: ByteString -> [ByteString] -> [JSStatement] -> JSModuleItem
jsExportFunction name params body =
  jsExport (jsFunction name params body)

-- | Export a const declaration.
--
-- @jsExportConst \"PI\" (jsNum 3.14159)@
--
-- Produces: @export const PI = 3.14159;@
--
-- @since 0.8.1.0
jsExportConst :: ByteString -> JSExpression -> JSModuleItem
jsExportConst name expr =
  jsExport (jsConst name expr)


-- --------------------------------------------------------------------------
-- Statements
-- --------------------------------------------------------------------------

-- | Const declaration: @const name = expr;@
--
-- @since 0.8.1.0
jsConst :: ByteString -> JSExpression -> JSStatement
jsConst name expr =
  JSConstant noSp
    (JSLOne (JSVarInitExpression
      (JSIdentifier sp name)
      (JSVarInit sp expr)))
    semi

-- | Let declaration: @let name = expr;@
--
-- @since 0.8.1.0
jsLet :: ByteString -> JSExpression -> JSStatement
jsLet name expr =
  JSLet noSp
    (JSLOne (JSVarInitExpression
      (JSIdentifier sp name)
      (JSVarInit sp expr)))
    semi

-- | Var declaration: @var name = expr;@
--
-- @since 0.8.1.0
jsVarDecl :: ByteString -> JSExpression -> JSStatement
jsVarDecl name expr =
  JSVariable noSp
    (JSLOne (JSVarInitExpression
      (JSIdentifier sp name)
      (JSVarInit sp expr)))
    semi

-- | Function declaration: @function name(params) { body }@
--
-- @since 0.8.1.0
jsFunction :: ByteString -> [ByteString] -> [JSStatement] -> JSStatement
jsFunction name params body =
  JSFunction noSp
    (JSIdentName sp name)
    noSp
    (buildParamList params)
    noSp
    (JSBlock sp body sp)
    autoSemi

-- | Return statement: @return expr;@
--
-- @since 0.8.1.0
jsReturn :: JSExpression -> JSStatement
jsReturn expr = JSReturn noSp (Just expr) semi

-- | Throw statement: @throw expr;@
--
-- @since 0.8.1.0
jsThrow :: JSExpression -> JSStatement
jsThrow expr = JSThrow noSp expr semi

-- | Try-catch statement: @try { ... } catch (name) { ... }@
--
-- @since 0.8.1.0
jsTry :: [JSStatement] -> ByteString -> [JSStatement] -> JSStatement
jsTry tryBody errName catchBody =
  JSTry sp
    (JSBlock sp tryBody sp)
    [JSCatch sp noSp (JSIdentifier noSp errName) noSp (JSBlock sp catchBody sp)]
    JSNoFinally

-- | If statement (no else): @if (cond) { body }@
--
-- @since 0.8.1.0
jsIf :: JSExpression -> [JSStatement] -> JSStatement
jsIf cond body =
  JSIf noSp sp cond sp (JSStatementBlock sp body sp autoSemi)

-- | If-else statement: @if (cond) { then } else { else }@
--
-- @since 0.8.1.0
jsIfElse :: JSExpression -> [JSStatement] -> [JSStatement] -> JSStatement
jsIfElse cond thenBody elseBody =
  JSIfElse noSp sp cond sp
    (JSStatementBlock sp thenBody sp autoSemi) sp
    (JSStatementBlock sp elseBody sp autoSemi)

-- | Switch statement: @switch (expr) { case v1: ...; default: ... }@
--
-- @since 0.8.1.0
jsSwitch :: JSExpression -> [(Maybe JSExpression, [JSStatement])] -> JSStatement
jsSwitch expr cases =
  JSSwitch sp noSp expr noSp sp (map toCasePart cases) sp autoSemi
  where
    toCasePart (Just val, stmts) = JSCase sp val sp stmts
    toCasePart (Nothing, stmts) = JSDefault sp sp stmts

-- | While loop: @while (cond) { body }@
--
-- @since 0.8.1.0
jsWhile :: JSExpression -> [JSStatement] -> JSStatement
jsWhile cond body =
  JSWhile sp sp cond sp (JSStatementBlock sp body sp autoSemi)

-- | Do-while loop: @do { body } while (cond);@
--
-- @since 0.8.1.0
jsDoWhile :: [JSStatement] -> JSExpression -> JSStatement
jsDoWhile body cond =
  JSDoWhile noSp (JSStatementBlock sp body sp autoSemi) sp sp cond sp semi

-- | C-style for loop: @for (init; cond; step) { body }@
--
-- Uses expression statements for init/step.
--
-- @since 0.8.1.0
jsFor :: JSExpression -> JSExpression -> JSExpression -> [JSStatement] -> JSStatement
jsFor initExpr condExpr stepExpr body =
  JSFor sp noSp
    (JSLOne initExpr) noSp
    (JSLOne condExpr) noSp
    (JSLOne stepExpr) noSp
    (JSStatementBlock sp body sp autoSemi)

-- | Break statement: @break;@
--
-- @since 0.8.1.0
jsBreak :: JSStatement
jsBreak = JSBreak noSp JSIdentNone semi

-- | Break with label: @break label;@
--
-- @since 0.8.1.0
jsBreakLabel :: ByteString -> JSStatement
jsBreakLabel label = JSBreak noSp (JSIdentName sp label) semi

-- | Continue statement: @continue;@
--
-- @since 0.8.1.0
jsContinue :: JSStatement
jsContinue = JSContinue noSp JSIdentNone semi

-- | Continue with label: @continue label;@
--
-- @since 0.8.1.0
jsContinueLabel :: ByteString -> JSStatement
jsContinueLabel label = JSContinue noSp (JSIdentName sp label) semi

-- | Labelled statement: @label: stmt@
--
-- @since 0.8.1.0
jsLabelled :: ByteString -> JSStatement -> JSStatement
jsLabelled label stmt =
  JSLabelled (JSIdentName noSp label) noSp stmt

-- | Block statement: @{ stmts }@
--
-- @since 0.8.1.0
jsBlock :: [JSStatement] -> JSStatement
jsBlock stmts = JSStatementBlock sp stmts sp autoSemi

-- | Expression statement: @expr;@
--
-- @since 0.8.1.0
jsExprStmt :: JSExpression -> JSStatement
jsExprStmt expr = JSExpressionStatement expr semi


-- --------------------------------------------------------------------------
-- Expressions
-- --------------------------------------------------------------------------

-- | Variable reference: @name@
--
-- @since 0.8.1.0
jsVar :: ByteString -> JSExpression
jsVar = JSIdentifier sp

-- | Integer literal: @42@
--
-- @since 0.8.1.0
jsInt :: Integer -> JSExpression
jsInt n = JSDecimal sp (fromIntegral n)

-- | Floating point literal: @3.14@
--
-- @since 0.8.1.0
jsNum :: Double -> JSExpression
jsNum = JSDecimal sp

-- | String literal: @\'hello\'@
--
-- The caller must include surrounding quotes in the 'ByteString'.
--
-- @since 0.8.1.0
jsStr :: ByteString -> JSExpression
jsStr = JSStringLiteral sp

-- | Boolean literal: @true@ or @false@
--
-- @since 0.8.1.0
jsBool :: Bool -> JSExpression
jsBool True = JSLiteral sp "true"
jsBool False = JSLiteral sp "false"

-- | Null literal: @null@
--
-- @since 0.8.1.0
jsNull :: JSExpression
jsNull = JSLiteral sp "null"

-- | This expression: @this@
--
-- @since 0.8.1.0
jsThis :: JSExpression
jsThis = JSLiteral sp "this"

-- | Array literal: @[a, b, c]@
--
-- @since 0.8.1.0
jsArray :: [JSExpression] -> JSExpression
jsArray elems =
  JSArrayLiteral noSp (toArrayElements elems) noSp

-- | Object literal: @{ key1: val1, key2: val2 }@
--
-- @since 0.8.1.0
jsObject :: [(ByteString, JSExpression)] -> JSExpression
jsObject fields =
  JSObjectLiteral sp (JSCTLNone (buildPropList fields)) sp

-- | Shorthand object: @{ a, b, c }@ (ES2015 shorthand properties).
--
-- @since 0.8.1.0
jsObjectShorthand :: [ByteString] -> JSExpression
jsObjectShorthand names =
  JSObjectLiteral sp (JSCTLNone (buildShorthandPropList names)) sp

-- | Arrow function with expression body: @(params) => expr@
--
-- @since 0.8.1.0
jsArrow :: [ByteString] -> JSExpression -> JSExpression
jsArrow params expr =
  JSArrowExpression
    (JSParenthesizedArrowParameterList noSp (buildParamList params) noSp)
    sp
    (JSConciseExpressionBody expr)

-- | Arrow function with block body: @(params) => { stmts }@
--
-- @since 0.8.1.0
jsArrowBlock :: [ByteString] -> [JSStatement] -> JSExpression
jsArrowBlock params body =
  JSArrowExpression
    (JSParenthesizedArrowParameterList noSp (buildParamList params) noSp)
    sp
    (JSConciseFunctionBody (JSBlock sp body sp))

-- | Function call: @callee(args)@
--
-- @since 0.8.1.0
jsCall :: JSExpression -> [JSExpression] -> JSExpression
jsCall callee args =
  JSCallExpression callee noSp (buildExprCommaList args) noSp

-- | Member dot access: @obj.prop@
--
-- @since 0.8.1.0
jsDot :: JSExpression -> ByteString -> JSExpression
jsDot obj prop = JSMemberDot obj noSp (JSIdentifier noSp prop)

-- | Bracket index access: @obj[key]@
--
-- @since 0.8.1.0
jsIndex :: JSExpression -> JSExpression -> JSExpression
jsIndex obj key = JSMemberSquare obj noSp key noSp

-- | Assignment expression: @lhs = rhs@
--
-- @since 0.8.1.0
jsAssign :: JSExpression -> JSExpression -> JSExpression
jsAssign lhs rhs = JSAssignExpression lhs (JSAssign sp) rhs

-- | Ternary expression: @cond ? then : else@
--
-- @since 0.8.1.0
jsTernary :: JSExpression -> JSExpression -> JSExpression -> JSExpression
jsTernary cond thenE elseE =
  JSExpressionTernary cond sp thenE sp elseE

-- | New expression: @new Ctor(args)@
--
-- @since 0.8.1.0
jsNew :: JSExpression -> [JSExpression] -> JSExpression
jsNew ctor args =
  JSMemberNew sp ctor noSp (buildExprCommaList args) noSp

-- | Spread expression: @...expr@
--
-- @since 0.8.1.0
jsSpread :: JSExpression -> JSExpression
jsSpread = JSSpreadExpression noSp

-- | Template literal: @\`text\`@ (no interpolation — use the QQ for complex templates).
--
-- @since 0.8.1.0
jsTemplateLiteral :: ByteString -> JSExpression
jsTemplateLiteral text =
  JSTemplateLiteral Nothing sp text []

-- | Parenthesized expression: @(expr)@
--
-- @since 0.8.1.0
jsParen :: JSExpression -> JSExpression
jsParen expr = JSExpressionParen noSp expr noSp


-- --------------------------------------------------------------------------
-- Binary Operators
-- --------------------------------------------------------------------------

-- | @a + b@
jsAdd :: JSExpression -> JSExpression -> JSExpression
jsAdd l r = JSExpressionBinary l (JSBinOpPlus sp) r

-- | @a - b@
jsSub :: JSExpression -> JSExpression -> JSExpression
jsSub l r = JSExpressionBinary l (JSBinOpMinus sp) r

-- | @a * b@
jsMul :: JSExpression -> JSExpression -> JSExpression
jsMul l r = JSExpressionBinary l (JSBinOpTimes sp) r

-- | @a / b@
jsDiv :: JSExpression -> JSExpression -> JSExpression
jsDiv l r = JSExpressionBinary l (JSBinOpDivide sp) r

-- | @a % b@
jsMod :: JSExpression -> JSExpression -> JSExpression
jsMod l r = JSExpressionBinary l (JSBinOpMod sp) r

-- | @a === b@
jsStrictEq :: JSExpression -> JSExpression -> JSExpression
jsStrictEq l r = JSExpressionBinary l (JSBinOpStrictEq sp) r

-- | @a !== b@
jsStrictNeq :: JSExpression -> JSExpression -> JSExpression
jsStrictNeq l r = JSExpressionBinary l (JSBinOpStrictNeq sp) r

-- | @a < b@
jsLt :: JSExpression -> JSExpression -> JSExpression
jsLt l r = JSExpressionBinary l (JSBinOpLt sp) r

-- | @a <= b@
jsLe :: JSExpression -> JSExpression -> JSExpression
jsLe l r = JSExpressionBinary l (JSBinOpLe sp) r

-- | @a > b@
jsGt :: JSExpression -> JSExpression -> JSExpression
jsGt l r = JSExpressionBinary l (JSBinOpGt sp) r

-- | @a >= b@
jsGe :: JSExpression -> JSExpression -> JSExpression
jsGe l r = JSExpressionBinary l (JSBinOpGe sp) r

-- | @a && b@
jsAnd :: JSExpression -> JSExpression -> JSExpression
jsAnd l r = JSExpressionBinary l (JSBinOpAnd sp) r

-- | @a || b@
jsOr :: JSExpression -> JSExpression -> JSExpression
jsOr l r = JSExpressionBinary l (JSBinOpOr sp) r

-- | @a & b@
jsBitAnd :: JSExpression -> JSExpression -> JSExpression
jsBitAnd l r = JSExpressionBinary l (JSBinOpBitAnd sp) r

-- | @a | b@
jsBitOr :: JSExpression -> JSExpression -> JSExpression
jsBitOr l r = JSExpressionBinary l (JSBinOpBitOr sp) r

-- | @a ^ b@
jsBitXor :: JSExpression -> JSExpression -> JSExpression
jsBitXor l r = JSExpressionBinary l (JSBinOpBitXor sp) r

-- | @a << b@
jsLShift :: JSExpression -> JSExpression -> JSExpression
jsLShift l r = JSExpressionBinary l (JSBinOpLsh sp) r

-- | @a >> b@
jsRShift :: JSExpression -> JSExpression -> JSExpression
jsRShift l r = JSExpressionBinary l (JSBinOpRsh sp) r

-- | @a >>> b@
jsURShift :: JSExpression -> JSExpression -> JSExpression
jsURShift l r = JSExpressionBinary l (JSBinOpUrsh sp) r

-- | @a instanceof b@
jsInstanceOf :: JSExpression -> JSExpression -> JSExpression
jsInstanceOf l r = JSExpressionBinary l (JSBinOpInstanceOf sp) r

-- | @a in b@
jsIn :: JSExpression -> JSExpression -> JSExpression
jsIn l r = JSExpressionBinary l (JSBinOpIn sp) r

-- | @a ?? b@
jsNullishCoalesce :: JSExpression -> JSExpression -> JSExpression
jsNullishCoalesce l r = JSExpressionBinary l (JSBinOpNullishCoalescing sp) r


-- --------------------------------------------------------------------------
-- Unary Operators
-- --------------------------------------------------------------------------

-- | @!expr@
jsNot :: JSExpression -> JSExpression
jsNot = JSUnaryExpression (JSUnaryOpNot noSp)

-- | @-expr@
jsNeg :: JSExpression -> JSExpression
jsNeg = JSUnaryExpression (JSUnaryOpMinus noSp)

-- | @typeof expr@
jsTypeof :: JSExpression -> JSExpression
jsTypeof = JSUnaryExpression (JSUnaryOpTypeof sp)

-- | @void expr@
jsVoid :: JSExpression -> JSExpression
jsVoid = JSUnaryExpression (JSUnaryOpVoid sp)

-- | @~expr@
jsBitNot :: JSExpression -> JSExpression
jsBitNot = JSUnaryExpression (JSUnaryOpTilde noSp)


-- --------------------------------------------------------------------------
-- Compound Assignment
-- --------------------------------------------------------------------------

-- | @lhs += rhs@
jsPlusAssign :: JSExpression -> JSExpression -> JSExpression
jsPlusAssign lhs rhs = JSAssignExpression lhs (JSPlusAssign sp) rhs

-- | @lhs -= rhs@
jsMinusAssign :: JSExpression -> JSExpression -> JSExpression
jsMinusAssign lhs rhs = JSAssignExpression lhs (JSMinusAssign sp) rhs

-- | @lhs *= rhs@
jsTimesAssign :: JSExpression -> JSExpression -> JSExpression
jsTimesAssign lhs rhs = JSAssignExpression lhs (JSTimesAssign sp) rhs

-- | @lhs /= rhs@
jsDivAssign :: JSExpression -> JSExpression -> JSExpression
jsDivAssign lhs rhs = JSAssignExpression lhs (JSDivideAssign sp) rhs


-- --------------------------------------------------------------------------
-- Internal Helpers
-- --------------------------------------------------------------------------

-- | Build a comma-separated parameter list from names.
buildParamList :: [ByteString] -> JSCommaList JSExpression
buildParamList [] = JSLNil
buildParamList names =
  List.foldl' (\acc n -> JSLCons acc noSp (JSIdentifier sp n))
    (JSLOne (JSIdentifier noSp (head names)))
    (tail names)

-- | Build array elements with comma separators.
toArrayElements :: [JSExpression] -> [JSArrayElement]
toArrayElements [] = []
toArrayElements [e] = [JSArrayElement e]
toArrayElements (e:es) =
  JSArrayElement e :
    concatMap (\expr -> [JSArrayComma noSp, JSArrayElement expr]) es

-- | Build a comma-separated expression list.
buildExprCommaList :: [JSExpression] -> JSCommaList JSExpression
buildExprCommaList [] = JSLNil
buildExprCommaList [e] = JSLOne e
buildExprCommaList (e:es) =
  List.foldl' (\acc x -> JSLCons acc noSp x) (JSLOne e) es

-- | Build import specifier list from names.
buildImportSpecList :: [ByteString] -> JSCommaList JSImportSpecifier
buildImportSpecList [] = JSLNil
buildImportSpecList names =
  List.foldl' (\acc n -> JSLCons acc noSp (toImportSpec n))
    (JSLOne (toImportSpec (head names)))
    (tail names)
  where
    toImportSpec n = JSImportSpecifier (JSIdentName sp n)

-- | Build export specifier list from names.
buildExportSpecList :: [ByteString] -> JSCommaList JSExportSpecifier
buildExportSpecList [] = JSLNil
buildExportSpecList names =
  List.foldl' (\acc n -> JSLCons acc noSp (toExportSpec n))
    (JSLOne (toExportSpec (head names)))
    (tail names)
  where
    toExportSpec n = JSExportSpecifier (JSIdentName sp n)

-- | Build object property list from key-value pairs.
buildPropList :: [(ByteString, JSExpression)] -> JSCommaList JSObjectProperty
buildPropList [] = JSLNil
buildPropList fields =
  List.foldl' (\acc f -> JSLCons acc noSp (toProp f))
    (JSLOne (toProp (head fields)))
    (tail fields)
  where
    toProp (k, v) = JSPropertyNameandValue (JSPropertyIdent sp k) noSp [v]

-- | Build shorthand property list from names.
buildShorthandPropList :: [ByteString] -> JSCommaList JSObjectProperty
buildShorthandPropList [] = JSLNil
buildShorthandPropList names =
  List.foldl' (\acc n -> JSLCons acc noSp (JSPropertyIdentRef sp n))
    (JSLOne (JSPropertyIdentRef sp (head names)))
    (tail names)
