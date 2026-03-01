{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | JavaScript Abstract Syntax Tree definitions and utilities.
--
-- This module defines the complete AST representation for JavaScript programs,
-- supporting ECMAScript 5 features with ES6+ extensions including:
--
--   * All expression types (literals, binary ops, function calls, etc.)
--   * Statement constructs (control flow, declarations, blocks)
--   * Module import/export declarations
--   * Class definitions and method declarations
--   * Template literals and destructuring patterns
--   * Async/await and generator function support
--   * Modern JavaScript features (BigInt, optional chaining, nullish coalescing)
--
-- The AST preserves source location information and comments through
-- 'JSAnnot' annotations on every node, enabling accurate pretty-printing
-- and source mapping.
--
-- ==== Examples
--
-- Parsing and working with expressions:
--
-- >>> parseExpression "42 + 1"
-- Right (JSExpressionBinary (JSDecimal ...) (JSBinOpPlus ...) (JSDecimal ...))
--
-- >>> showStripped <$> parseExpression "x.foo()"
-- Right "JSCallExpression (JSMemberDot (JSIdentifier 'x',JSIdentifier 'foo'),JSArguments [])"
--
-- Working with statements:
--
-- >>> parseStatement "if (x) return 42;"
-- Right (JSIf ...)
--
-- @since 0.7.1.0
module Language.JavaScript.Parser.AST
  ( JSExpression (..),
    JSAnnot (..),
    JSBinOp (..),
    JSUnaryOp (..),
    JSSemi (..),
    JSAssignOp (..),
    JSTryCatch (..),
    JSTryFinally (..),
    JSStatement (..),
    JSBlock (..),
    JSSwitchParts (..),
    JSAST (..),
    JSObjectProperty (..),
    JSPropertyName (..),
    JSObjectPropertyList,
    JSAccessor (..),
    JSMethodDefinition (..),
    JSIdent (..),
    JSVarInitializer (..),
    JSArrayElement (..),
    JSCommaList (..),
    JSCommaTrailingList (..),
    JSArrowParameterList (..),
    JSConciseBody (..),
    JSTemplatePart (..),
    JSClassHeritage (..),
    JSClassElement (..),
    -- Modules
    JSModuleItem (..),
    JSImportDeclaration (..),
    JSImportClause (..),
    JSFromClause (..),
    JSImportNameSpace (..),
    JSImportsNamed (..),
    JSImportSpecifier (..),
    JSImportAttributes (..),
    JSImportAttribute (..),
    JSExportDeclaration (..),
    JSExportClause (..),
    JSExportSpecifier (..),
    HasAnnot (..),
    binOpEq,
    showStripped,
    fromCommaList,
    showJSDouble,
    showJSHex,
    showJSBinary,
    showJSOctal,
  )
where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Char (intToDigit)
import Data.Data
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)
import Language.JavaScript.Parser.SrcLocation (TokenPosn (..))
import Language.JavaScript.Parser.Token
import Numeric (showHex, showIntAtBase)

-- ---------------------------------------------------------------------

-- | JavaScript AST annotation with optimized memory layout.
--
-- Provides source location and comment information with efficient
-- representation. The 'JSAnnot' constructor is optimized for the
-- common case of position + comments.
data JSAnnot
  = -- | Annotation: position and comment/whitespace information
    JSAnnot {-# UNPACK #-} !TokenPosn ![CommentAnnotation]
  | -- | A single space character
    JSAnnotSpace
  | -- | No annotation
    JSNoAnnot
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | Top-level JavaScript AST with optimized memory layout.
--
-- Represents different kinds of JavaScript source units (programs, modules,
-- individual statements or expressions) with efficient memory usage.
data JSAST
  = -- | Complete program: source elements, trailing whitespace
    JSAstProgram ![JSStatement] !JSAnnot
  | -- | ES6 module: module items, trailing whitespace
    JSAstModule ![JSModuleItem] !JSAnnot
  | -- | Individual statement with annotation
    JSAstStatement !JSStatement !JSAnnot
  | -- | Individual expression with annotation
    JSAstExpression !JSExpression !JSAnnot
  | -- | Individual literal with annotation
    JSAstLiteral !JSExpression !JSAnnot
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- Shift AST
-- https://github.com/shapesecurity/shift-spec/blob/83498b92c436180cc0e2115b225a68c08f43c53e/spec.idl#L229-L234
data JSModuleItem
  = -- | import,decl
    JSModuleImportDeclaration !JSAnnot !JSImportDeclaration
  | -- | export,decl
    JSModuleExportDeclaration !JSAnnot !JSExportDeclaration
  | JSModuleStatementListItem !JSStatement
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSImportDeclaration
  = -- | imports, module, optional attributes, semi
    JSImportDeclaration !JSImportClause !JSFromClause !(Maybe JSImportAttributes) !JSSemi
  | -- | import, module, optional attributes, semi
    JSImportDeclarationBare !JSAnnot !ByteString !(Maybe JSImportAttributes) !JSSemi
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSImportAttributes
  = -- | {, attributes, }
    JSImportAttributes !JSAnnot !(JSCommaList JSImportAttribute) !JSAnnot
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSImportAttribute
  = -- | key, :, value
    JSImportAttribute !JSIdent !JSAnnot !JSExpression
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSImportClause
  = -- | default
    JSImportClauseDefault !JSIdent
  | -- | namespace
    JSImportClauseNameSpace !JSImportNameSpace
  | -- | named imports
    JSImportClauseNamed !JSImportsNamed
  | -- | default, comma, namespace
    JSImportClauseDefaultNameSpace !JSIdent !JSAnnot !JSImportNameSpace
  | -- | default, comma, named imports
    JSImportClauseDefaultNamed !JSIdent !JSAnnot !JSImportsNamed
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSFromClause
  = -- | from, string literal, string literal contents
    JSFromClause !JSAnnot !JSAnnot !ByteString
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | Import namespace, e.g. '* as whatever'
data JSImportNameSpace
  = -- | *, as, ident
    JSImportNameSpace !JSBinOp !JSAnnot !JSIdent
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | Named imports, e.g. '{ foo, bar, baz as quux }'
data JSImportsNamed
  = -- | lb, specifiers, rb
    JSImportsNamed !JSAnnot !(JSCommaList JSImportSpecifier) !JSAnnot
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- |
-- Note that this data type is separate from ExportSpecifier because the
-- grammar is slightly different (e.g. in handling of reserved words).
data JSImportSpecifier
  = -- | ident
    JSImportSpecifier !JSIdent
  | -- | ident, as, ident
    JSImportSpecifierAs !JSIdent !JSAnnot !JSIdent
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSExportDeclaration
  = -- | star, module, semi
    JSExportAllFrom !JSBinOp JSFromClause !JSSemi
  | -- | star, as, ident, module, semi
    JSExportAllAsFrom !JSBinOp !JSAnnot !JSIdent JSFromClause !JSSemi
  | -- | exports, module, semi
    JSExportFrom JSExportClause JSFromClause !JSSemi
  | -- | exports, autosemi
    JSExportLocals JSExportClause !JSSemi
  | -- | default, declaration/expression, autosemi
    JSExportDefault !JSAnnot !JSStatement !JSSemi
  | -- | body, autosemi
    JSExport !JSStatement !JSSemi
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSExportClause
  = -- | lb, specifiers, rb
    JSExportClause !JSAnnot !(JSCommaList JSExportSpecifier) !JSAnnot
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSExportSpecifier
  = -- | ident
    JSExportSpecifier !JSIdent
  | -- | ident1, as, ident2
    JSExportSpecifierAs !JSIdent !JSAnnot !JSIdent
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSStatement
  = -- | lbrace, stmts, rbrace, autosemi
    JSStatementBlock !JSAnnot ![JSStatement] !JSAnnot !JSSemi
  | -- | break,optional identifier, autosemi
    JSBreak !JSAnnot !JSIdent !JSSemi
  | -- | const, decl, autosemi
    JSLet !JSAnnot !(JSCommaList JSExpression) !JSSemi
  | -- | class, name, optional extends clause, lb, body, rb, autosemi
    JSClass !JSAnnot !JSIdent !JSClassHeritage !JSAnnot ![JSClassElement] !JSAnnot !JSSemi
  | -- | const, decl, autosemi
    JSConstant !JSAnnot !(JSCommaList JSExpression) !JSSemi
  | -- | continue, optional identifier,autosemi
    JSContinue !JSAnnot !JSIdent !JSSemi
  | -- | do,stmt,while,lb,expr,rb,autosemi
    JSDoWhile !JSAnnot !JSStatement !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSSemi
  | -- | for,lb,expr,semi,expr,semi,expr,rb.stmt
    JSFor !JSAnnot !JSAnnot !(JSCommaList JSExpression) !JSAnnot !(JSCommaList JSExpression) !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSStatement
  | -- | for,lb,expr,in,expr,rb,stmt
    JSForIn !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement
  | -- | for,lb,var,vardecl,semi,expr,semi,expr,rb,stmt
    JSForVar !JSAnnot !JSAnnot !JSAnnot !(JSCommaList JSExpression) !JSAnnot !(JSCommaList JSExpression) !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSStatement
  | -- | for,lb,var,vardecl,in,expr,rb,stmt
    JSForVarIn !JSAnnot !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement
  | -- | for,lb,var,vardecl,semi,expr,semi,expr,rb,stmt
    JSForLet !JSAnnot !JSAnnot !JSAnnot !(JSCommaList JSExpression) !JSAnnot !(JSCommaList JSExpression) !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSStatement
  | -- | for,lb,var,vardecl,in,expr,rb,stmt
    JSForLetIn !JSAnnot !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement
  | -- | for,lb,var,vardecl,in,expr,rb,stmt
    JSForLetOf !JSAnnot !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement
  | -- | for,lb,var,vardecl,semi,expr,semi,expr,rb,stmt
    JSForConst !JSAnnot !JSAnnot !JSAnnot !(JSCommaList JSExpression) !JSAnnot !(JSCommaList JSExpression) !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSStatement
  | -- | for,lb,var,vardecl,in,expr,rb,stmt
    JSForConstIn !JSAnnot !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement
  | -- | for,lb,var,vardecl,in,expr,rb,stmt
    JSForConstOf !JSAnnot !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement
  | -- | for,lb,expr,in,expr,rb,stmt
    JSForOf !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement
  | -- | for,lb,var,vardecl,in,expr,rb,stmt
    JSForVarOf !JSAnnot !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement
  | -- | fn,name, lb,parameter list,rb,block,autosemi
    JSAsyncFunction !JSAnnot !JSAnnot !JSIdent !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock !JSSemi
  | -- | fn,name, lb,parameter list,rb,block,autosemi
    JSFunction !JSAnnot !JSIdent !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock !JSSemi
  | -- | fn,*,name, lb,parameter list,rb,block,autosemi
    JSGenerator !JSAnnot !JSAnnot !JSIdent !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock !JSSemi
  | -- | if,(,expr,),stmt
    JSIf !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSStatement
  | -- | if,(,expr,),stmt,else,rest
    JSIfElse !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSStatement !JSAnnot !JSStatement
  | -- | identifier,colon,stmt
    JSLabelled !JSIdent !JSAnnot !JSStatement
  | JSEmptyStatement !JSAnnot
  | JSExpressionStatement !JSExpression !JSSemi
  | -- | lhs, assignop, rhs, autosemi
    JSAssignStatement !JSExpression !JSAssignOp !JSExpression !JSSemi
  | JSMethodCall !JSExpression !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSSemi
  | -- | optional expression,autosemi
    JSReturn !JSAnnot !(Maybe JSExpression) !JSSemi
  | -- | switch,lb,expr,rb,caseblock,autosemi
    JSSwitch !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSAnnot ![JSSwitchParts] !JSAnnot !JSSemi
  | -- | throw val autosemi
    JSThrow !JSAnnot !JSExpression !JSSemi
  | -- | try,block,catches,finally
    JSTry !JSAnnot !JSBlock ![JSTryCatch] !JSTryFinally
  | -- | var, decl, autosemi
    JSVariable !JSAnnot !(JSCommaList JSExpression) !JSSemi
  | -- | while,lb,expr,rb,stmt
    JSWhile !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSStatement
  | -- | with,lb,expr,rb,stmt list
    JSWith !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSStatement !JSSemi
  | -- | debugger, autosemi
    JSDebugger !JSAnnot !JSSemi
  | -- | async, function, *, name, lb, params, rb, block, autosemi
    JSAsyncGenerator !JSAnnot !JSAnnot !JSAnnot !JSIdent !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock !JSSemi
  | -- | for, await, lb, expr, of, iter, rb, stmt
    JSForAwaitOf !JSAnnot !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement
  | -- | for, await, lb, var, expr, of, iter, rb, stmt
    JSForAwaitVarOf !JSAnnot !JSAnnot !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement
  | -- | for, await, lb, let, expr, of, iter, rb, stmt
    JSForAwaitLetOf !JSAnnot !JSAnnot !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement
  | -- | for, await, lb, const, expr, of, iter, rb, stmt
    JSForAwaitConstOf !JSAnnot !JSAnnot !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSExpression
  = -- | Terminals
    JSIdentifier !JSAnnot !ByteString
  | -- | Decimal numeric literal (e.g., @42@, @3.14@, @1e5@)
    JSDecimal !JSAnnot !Double
  | JSLiteral !JSAnnot !ByteString
  | -- | Hexadecimal integer literal (e.g., @0xFF@)
    JSHexInteger !JSAnnot !Integer
  | -- | Binary integer literal (e.g., @0b1010@)
    JSBinaryInteger !JSAnnot !Integer
  | -- | Octal integer literal (e.g., @0o77@)
    JSOctal !JSAnnot !Integer
  | -- | BigInt literal (e.g., @42n@, @0xFFn@)
    JSBigIntLiteral !JSAnnot !Integer
  | JSStringLiteral !JSAnnot !ByteString
  | JSRegEx !JSAnnot !ByteString
  | -- | lb, contents, rb
    JSArrayLiteral !JSAnnot ![JSArrayElement] !JSAnnot
  | -- | lhs, assignop, rhs
    JSAssignExpression !JSExpression !JSAssignOp !JSExpression
  | -- | await, expr
    JSAwaitExpression !JSAnnot !JSExpression
  | -- | expr, bl, args, rb
    JSCallExpression !JSExpression !JSAnnot !(JSCommaList JSExpression) !JSAnnot
  | -- | expr, dot, expr
    JSCallExpressionDot !JSExpression !JSAnnot !JSExpression
  | -- | expr, [, expr, ]
    JSCallExpressionSquare !JSExpression !JSAnnot !JSExpression !JSAnnot
  | -- | class, optional identifier, optional extends clause, lb, body, rb
    JSClassExpression !JSAnnot !JSIdent !JSClassHeritage !JSAnnot ![JSClassElement] !JSAnnot
  | -- | expression components
    JSCommaExpression !JSExpression !JSAnnot !JSExpression
  | -- | lhs, op, rhs
    JSExpressionBinary !JSExpression !JSBinOp !JSExpression
  | -- | lb,expression,rb
    JSExpressionParen !JSAnnot !JSExpression !JSAnnot
  | -- | expression, operator
    JSExpressionPostfix !JSExpression !JSUnaryOp
  | -- | cond, ?, trueval, :, falseval
    JSExpressionTernary !JSExpression !JSAnnot !JSExpression !JSAnnot !JSExpression
  | -- | parameter list,arrow,body`
    JSArrowExpression !JSArrowParameterList !JSAnnot !JSConciseBody
  | -- | fn,name,lb, parameter list,rb,block`
    JSFunctionExpression !JSAnnot !JSIdent !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock
  | -- | fn,*,name,lb, parameter list,rb,block`
    JSGeneratorExpression !JSAnnot !JSAnnot !JSIdent !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock
  | -- | async,fn,name,lb, parameter list,rb,block`
    JSAsyncFunctionExpression !JSAnnot !JSAnnot !JSIdent !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock
  | -- | async, parameter list, arrow, body
    JSAsyncArrowExpression !JSAnnot !JSArrowParameterList !JSAnnot !JSConciseBody
  | -- | async, fn, *, name, lb, parameter list, rb, block
    JSAsyncGeneratorExpression !JSAnnot !JSAnnot !JSAnnot !JSIdent !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock
  | -- | firstpart, dot, name
    JSMemberDot !JSExpression !JSAnnot !JSExpression
  | -- | firstpart, dot, hash, private field name (e.g. @obj.#field@)
    JSMemberPrivateDot !JSExpression !JSAnnot !JSAnnot !ByteString
  | JSMemberExpression !JSExpression !JSAnnot !(JSCommaList JSExpression) !JSAnnot -- expr, lb, args, rb
  | -- | new, name, lb, args, rb
    JSMemberNew !JSAnnot !JSExpression !JSAnnot !(JSCommaList JSExpression) !JSAnnot
  | -- | firstpart, lb, expr, rb
    JSMemberSquare !JSExpression !JSAnnot !JSExpression !JSAnnot
  | -- | new, expr
    JSNewExpression !JSAnnot !JSExpression
  | -- | firstpart, ?., name
    JSOptionalMemberDot !JSExpression !JSAnnot !JSExpression
  | -- | firstpart, ?.[, expr, ]
    JSOptionalMemberSquare !JSExpression !JSAnnot !JSExpression !JSAnnot
  | -- | expr, ?.(, args, )
    JSOptionalCallExpression !JSExpression !JSAnnot !(JSCommaList JSExpression) !JSAnnot
  | -- | lbrace contents rbrace
    JSObjectLiteral !JSAnnot !JSObjectPropertyList !JSAnnot
  | JSSpreadExpression !JSAnnot !JSExpression
  | -- | optional tag, lquot, head, parts
    JSTemplateLiteral !(Maybe JSExpression) !JSAnnot !ByteString ![JSTemplatePart]
  | JSUnaryExpression !JSUnaryOp !JSExpression
  | -- | identifier, initializer
    JSVarInitExpression !JSExpression !JSVarInitializer
  | -- | yield, optional expr
    JSYieldExpression !JSAnnot !(Maybe JSExpression)
  | -- | yield, *, expr
    JSYieldFromExpression !JSAnnot !JSAnnot !JSExpression
  | -- | import, .meta
    JSImportMeta !JSAnnot !JSAnnot
  | -- | import, lb, expr, rb
    JSImportCall !JSAnnot !JSAnnot !JSExpression !JSAnnot
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSArrowParameterList
  = JSUnparenthesizedArrowParameter !JSIdent
  | JSParenthesizedArrowParameterList !JSAnnot !(JSCommaList JSExpression) !JSAnnot
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSConciseBody
  = JSConciseFunctionBody !JSBlock
  | JSConciseExpressionBody !JSExpression
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSBinOp
  = JSBinOpAnd !JSAnnot
  | JSBinOpBitAnd !JSAnnot
  | JSBinOpBitOr !JSAnnot
  | JSBinOpBitXor !JSAnnot
  | JSBinOpDivide !JSAnnot
  | JSBinOpEq !JSAnnot
  | JSBinOpExponentiation !JSAnnot
  | JSBinOpGe !JSAnnot
  | JSBinOpGt !JSAnnot
  | JSBinOpIn !JSAnnot
  | JSBinOpInstanceOf !JSAnnot
  | JSBinOpLe !JSAnnot
  | JSBinOpLsh !JSAnnot
  | JSBinOpLt !JSAnnot
  | JSBinOpMinus !JSAnnot
  | JSBinOpMod !JSAnnot
  | JSBinOpNeq !JSAnnot
  | JSBinOpOf !JSAnnot
  | JSBinOpOr !JSAnnot
  | JSBinOpNullishCoalescing !JSAnnot
  | JSBinOpPlus !JSAnnot
  | JSBinOpRsh !JSAnnot
  | JSBinOpStrictEq !JSAnnot
  | JSBinOpStrictNeq !JSAnnot
  | JSBinOpTimes !JSAnnot
  | JSBinOpUrsh !JSAnnot
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSUnaryOp
  = JSUnaryOpDecr !JSAnnot
  | JSUnaryOpDelete !JSAnnot
  | JSUnaryOpIncr !JSAnnot
  | JSUnaryOpMinus !JSAnnot
  | JSUnaryOpNot !JSAnnot
  | JSUnaryOpPlus !JSAnnot
  | JSUnaryOpTilde !JSAnnot
  | JSUnaryOpTypeof !JSAnnot
  | JSUnaryOpVoid !JSAnnot
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSSemi
  = JSSemi !JSAnnot
  | JSSemiAuto
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSAssignOp
  = JSAssign !JSAnnot
  | JSTimesAssign !JSAnnot
  | JSDivideAssign !JSAnnot
  | JSModAssign !JSAnnot
  | JSPlusAssign !JSAnnot
  | JSMinusAssign !JSAnnot
  | JSLshAssign !JSAnnot
  | JSRshAssign !JSAnnot
  | JSUrshAssign !JSAnnot
  | JSBwAndAssign !JSAnnot
  | JSBwXorAssign !JSAnnot
  | JSBwOrAssign !JSAnnot
  | JSLogicalAndAssign !JSAnnot -- &&=
  | JSLogicalOrAssign !JSAnnot
  | -- | |=
    JSNullishAssign !JSAnnot -- ??=
  | JSExponentiationAssign !JSAnnot -- **=
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSTryCatch
  = -- | catch,lb,ident,rb,block
    JSCatch !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSBlock
  | -- | catch,lb,ident,if,expr,rb,block
    JSCatchIf !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSExpression !JSAnnot !JSBlock
  | -- | catch,block (ES2019 optional catch binding)
    JSCatchNoParam !JSAnnot !JSBlock
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSTryFinally
  = -- | finally,block
    JSFinally !JSAnnot !JSBlock
  | JSNoFinally
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSBlock
  = -- | lbrace, stmts, rbrace
    JSBlock !JSAnnot ![JSStatement] !JSAnnot
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSSwitchParts
  = -- | expr,colon,stmtlist
    JSCase !JSAnnot !JSExpression !JSAnnot ![JSStatement]
  | -- | colon,stmtlist
    JSDefault !JSAnnot !JSAnnot ![JSStatement]
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSVarInitializer
  = -- | assignop, initializer
    JSVarInit !JSAnnot !JSExpression
  | JSVarInitNone
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSObjectProperty
  = -- | name, colon, value
    JSPropertyNameandValue !JSPropertyName !JSAnnot ![JSExpression]
  | JSPropertyIdentRef !JSAnnot !ByteString
  | JSObjectMethod !JSMethodDefinition
  | -- | ..., expression
    JSObjectSpread !JSAnnot !JSExpression
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSMethodDefinition
  = JSMethodDefinition !JSPropertyName !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock -- name, lb, params, rb, block
  | -- | *, name, lb, params, rb, block
    JSGeneratorMethodDefinition !JSAnnot !JSPropertyName !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock
  | -- | async, name, lb, params, rb, block
    JSAsyncMethodDefinition !JSAnnot !JSPropertyName !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock
  | -- | get/set, name, lb, params, rb, block
    JSPropertyAccessor !JSAccessor !JSPropertyName !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSPropertyName
  = JSPropertyIdent !JSAnnot !ByteString
  | JSPropertyString !JSAnnot !ByteString
  | JSPropertyNumber !JSAnnot !ByteString
  | -- | lb, expr, rb
    JSPropertyComputed !JSAnnot !JSExpression !JSAnnot
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

type JSObjectPropertyList = JSCommaTrailingList JSObjectProperty

-- | Accessors for JSObjectProperty is either 'get' or 'set'.
data JSAccessor
  = JSAccessorGet !JSAnnot
  | JSAccessorSet !JSAnnot
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSIdent
  = JSIdentName !JSAnnot !ByteString
  | JSIdentNone
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSArrayElement
  = JSArrayElement !JSExpression
  | JSArrayComma !JSAnnot
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSCommaList a
  = -- | head, comma, a
    JSLCons !(JSCommaList a) !JSAnnot !a
  | -- | single element (no comma)
    JSLOne !a
  | JSLNil
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSCommaTrailingList a
  = -- | list, trailing comma
    JSCTLComma !(JSCommaList a) !JSAnnot
  | -- | list
    JSCTLNone !(JSCommaList a)
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSTemplatePart
  = -- | expr, rb, suffix
    JSTemplatePart !JSExpression !JSAnnot !ByteString
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSClassHeritage
  = JSExtends !JSAnnot !JSExpression
  | JSExtendsNone
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSClassElement
  = JSClassInstanceMethod !JSMethodDefinition
  | JSClassStaticMethod !JSAnnot !JSMethodDefinition
  | JSClassSemi !JSAnnot
  | -- | #, name, =, optional initializer, autosemi
    JSPrivateField !JSAnnot !ByteString !JSAnnot !(Maybe JSExpression) !JSSemi
  | -- | #, name, lb, params, rb, block
    JSPrivateMethod !JSAnnot !ByteString !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock
  | -- | get/set, #, name, lb, params, rb, block
    JSPrivateAccessor !JSAccessor !JSAnnot !ByteString !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock
  | -- | name, =, optional initializer, autosemi
    JSClassField !JSPropertyName !JSAnnot !(Maybe JSExpression) !JSSemi
  | -- | static, name, =, optional initializer, autosemi
    JSClassStaticField !JSAnnot !JSPropertyName !JSAnnot !(Maybe JSExpression) !JSSemi
  | -- | static, block
    JSClassStaticBlock !JSAnnot !JSBlock
  | -- | async, *, name, lb, params, rb, block
    JSAsyncGeneratorMethodDefinition !JSAnnot !JSAnnot !JSPropertyName !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- -----------------------------------------------------------------------------
-- HasAnnot typeclass

-- | Typeclass for types containing JSAnnot annotations.
-- Provides direct-dispatch traversal, replacing SYB generic traversals.
class HasAnnot a where
  -- | Apply a function to every JSAnnot in the structure.
  mapAnnot :: (JSAnnot -> JSAnnot) -> a -> a
  -- | Collect values from every JSAnnot in the structure.
  foldAnnot :: (JSAnnot -> [b]) -> a -> [b]

instance HasAnnot JSAnnot where
  mapAnnot f a = f a
  foldAnnot f a = f a

instance HasAnnot JSSemi where
  mapAnnot f (JSSemi a) = JSSemi (f a)
  mapAnnot _ JSSemiAuto = JSSemiAuto
  foldAnnot f (JSSemi a) = f a
  foldAnnot _ JSSemiAuto = []

instance HasAnnot JSBinOp where
  mapAnnot f (JSBinOpAnd a) = JSBinOpAnd (f a)
  mapAnnot f (JSBinOpBitAnd a) = JSBinOpBitAnd (f a)
  mapAnnot f (JSBinOpBitOr a) = JSBinOpBitOr (f a)
  mapAnnot f (JSBinOpBitXor a) = JSBinOpBitXor (f a)
  mapAnnot f (JSBinOpDivide a) = JSBinOpDivide (f a)
  mapAnnot f (JSBinOpEq a) = JSBinOpEq (f a)
  mapAnnot f (JSBinOpExponentiation a) = JSBinOpExponentiation (f a)
  mapAnnot f (JSBinOpGe a) = JSBinOpGe (f a)
  mapAnnot f (JSBinOpGt a) = JSBinOpGt (f a)
  mapAnnot f (JSBinOpIn a) = JSBinOpIn (f a)
  mapAnnot f (JSBinOpInstanceOf a) = JSBinOpInstanceOf (f a)
  mapAnnot f (JSBinOpLe a) = JSBinOpLe (f a)
  mapAnnot f (JSBinOpLsh a) = JSBinOpLsh (f a)
  mapAnnot f (JSBinOpLt a) = JSBinOpLt (f a)
  mapAnnot f (JSBinOpMinus a) = JSBinOpMinus (f a)
  mapAnnot f (JSBinOpMod a) = JSBinOpMod (f a)
  mapAnnot f (JSBinOpNeq a) = JSBinOpNeq (f a)
  mapAnnot f (JSBinOpOf a) = JSBinOpOf (f a)
  mapAnnot f (JSBinOpOr a) = JSBinOpOr (f a)
  mapAnnot f (JSBinOpNullishCoalescing a) = JSBinOpNullishCoalescing (f a)
  mapAnnot f (JSBinOpPlus a) = JSBinOpPlus (f a)
  mapAnnot f (JSBinOpRsh a) = JSBinOpRsh (f a)
  mapAnnot f (JSBinOpStrictEq a) = JSBinOpStrictEq (f a)
  mapAnnot f (JSBinOpStrictNeq a) = JSBinOpStrictNeq (f a)
  mapAnnot f (JSBinOpTimes a) = JSBinOpTimes (f a)
  mapAnnot f (JSBinOpUrsh a) = JSBinOpUrsh (f a)
  foldAnnot f (JSBinOpAnd a) = f a
  foldAnnot f (JSBinOpBitAnd a) = f a
  foldAnnot f (JSBinOpBitOr a) = f a
  foldAnnot f (JSBinOpBitXor a) = f a
  foldAnnot f (JSBinOpDivide a) = f a
  foldAnnot f (JSBinOpEq a) = f a
  foldAnnot f (JSBinOpExponentiation a) = f a
  foldAnnot f (JSBinOpGe a) = f a
  foldAnnot f (JSBinOpGt a) = f a
  foldAnnot f (JSBinOpIn a) = f a
  foldAnnot f (JSBinOpInstanceOf a) = f a
  foldAnnot f (JSBinOpLe a) = f a
  foldAnnot f (JSBinOpLsh a) = f a
  foldAnnot f (JSBinOpLt a) = f a
  foldAnnot f (JSBinOpMinus a) = f a
  foldAnnot f (JSBinOpMod a) = f a
  foldAnnot f (JSBinOpNeq a) = f a
  foldAnnot f (JSBinOpOf a) = f a
  foldAnnot f (JSBinOpOr a) = f a
  foldAnnot f (JSBinOpNullishCoalescing a) = f a
  foldAnnot f (JSBinOpPlus a) = f a
  foldAnnot f (JSBinOpRsh a) = f a
  foldAnnot f (JSBinOpStrictEq a) = f a
  foldAnnot f (JSBinOpStrictNeq a) = f a
  foldAnnot f (JSBinOpTimes a) = f a
  foldAnnot f (JSBinOpUrsh a) = f a

instance HasAnnot JSUnaryOp where
  mapAnnot f (JSUnaryOpDecr a) = JSUnaryOpDecr (f a)
  mapAnnot f (JSUnaryOpDelete a) = JSUnaryOpDelete (f a)
  mapAnnot f (JSUnaryOpIncr a) = JSUnaryOpIncr (f a)
  mapAnnot f (JSUnaryOpMinus a) = JSUnaryOpMinus (f a)
  mapAnnot f (JSUnaryOpNot a) = JSUnaryOpNot (f a)
  mapAnnot f (JSUnaryOpPlus a) = JSUnaryOpPlus (f a)
  mapAnnot f (JSUnaryOpTilde a) = JSUnaryOpTilde (f a)
  mapAnnot f (JSUnaryOpTypeof a) = JSUnaryOpTypeof (f a)
  mapAnnot f (JSUnaryOpVoid a) = JSUnaryOpVoid (f a)
  foldAnnot f (JSUnaryOpDecr a) = f a
  foldAnnot f (JSUnaryOpDelete a) = f a
  foldAnnot f (JSUnaryOpIncr a) = f a
  foldAnnot f (JSUnaryOpMinus a) = f a
  foldAnnot f (JSUnaryOpNot a) = f a
  foldAnnot f (JSUnaryOpPlus a) = f a
  foldAnnot f (JSUnaryOpTilde a) = f a
  foldAnnot f (JSUnaryOpTypeof a) = f a
  foldAnnot f (JSUnaryOpVoid a) = f a

instance HasAnnot JSAccessor where
  mapAnnot f (JSAccessorGet a) = JSAccessorGet (f a)
  mapAnnot f (JSAccessorSet a) = JSAccessorSet (f a)
  foldAnnot f (JSAccessorGet a) = f a
  foldAnnot f (JSAccessorSet a) = f a

instance HasAnnot JSAssignOp where
  mapAnnot f (JSAssign a) = JSAssign (f a)
  mapAnnot f (JSTimesAssign a) = JSTimesAssign (f a)
  mapAnnot f (JSDivideAssign a) = JSDivideAssign (f a)
  mapAnnot f (JSModAssign a) = JSModAssign (f a)
  mapAnnot f (JSPlusAssign a) = JSPlusAssign (f a)
  mapAnnot f (JSMinusAssign a) = JSMinusAssign (f a)
  mapAnnot f (JSLshAssign a) = JSLshAssign (f a)
  mapAnnot f (JSRshAssign a) = JSRshAssign (f a)
  mapAnnot f (JSUrshAssign a) = JSUrshAssign (f a)
  mapAnnot f (JSBwAndAssign a) = JSBwAndAssign (f a)
  mapAnnot f (JSBwXorAssign a) = JSBwXorAssign (f a)
  mapAnnot f (JSBwOrAssign a) = JSBwOrAssign (f a)
  mapAnnot f (JSLogicalAndAssign a) = JSLogicalAndAssign (f a)
  mapAnnot f (JSLogicalOrAssign a) = JSLogicalOrAssign (f a)
  mapAnnot f (JSNullishAssign a) = JSNullishAssign (f a)
  mapAnnot f (JSExponentiationAssign a) = JSExponentiationAssign (f a)
  foldAnnot f (JSAssign a) = f a
  foldAnnot f (JSTimesAssign a) = f a
  foldAnnot f (JSDivideAssign a) = f a
  foldAnnot f (JSModAssign a) = f a
  foldAnnot f (JSPlusAssign a) = f a
  foldAnnot f (JSMinusAssign a) = f a
  foldAnnot f (JSLshAssign a) = f a
  foldAnnot f (JSRshAssign a) = f a
  foldAnnot f (JSUrshAssign a) = f a
  foldAnnot f (JSBwAndAssign a) = f a
  foldAnnot f (JSBwXorAssign a) = f a
  foldAnnot f (JSBwOrAssign a) = f a
  foldAnnot f (JSLogicalAndAssign a) = f a
  foldAnnot f (JSLogicalOrAssign a) = f a
  foldAnnot f (JSNullishAssign a) = f a
  foldAnnot f (JSExponentiationAssign a) = f a

instance HasAnnot JSIdent where
  mapAnnot f (JSIdentName a s) = JSIdentName (f a) s
  mapAnnot _ JSIdentNone = JSIdentNone
  foldAnnot f (JSIdentName a _) = f a
  foldAnnot _ JSIdentNone = []

instance HasAnnot JSVarInitializer where
  mapAnnot f (JSVarInit a e) = JSVarInit (f a) (mapAnnot f e)
  mapAnnot _ JSVarInitNone = JSVarInitNone
  foldAnnot f (JSVarInit a e) = f a ++ foldAnnot f e
  foldAnnot _ JSVarInitNone = []

instance HasAnnot JSClassHeritage where
  mapAnnot f (JSExtends a e) = JSExtends (f a) (mapAnnot f e)
  mapAnnot _ JSExtendsNone = JSExtendsNone
  foldAnnot f (JSExtends a e) = f a ++ foldAnnot f e
  foldAnnot _ JSExtendsNone = []

instance HasAnnot JSTryFinally where
  mapAnnot f (JSFinally a b) = JSFinally (f a) (mapAnnot f b)
  mapAnnot _ JSNoFinally = JSNoFinally
  foldAnnot f (JSFinally a b) = f a ++ foldAnnot f b
  foldAnnot _ JSNoFinally = []

instance HasAnnot JSBlock where
  mapAnnot f (JSBlock a1 stmts a2) = JSBlock (f a1) (map (mapAnnot f) stmts) (f a2)
  foldAnnot f (JSBlock a1 stmts a2) = f a1 ++ concatMap (foldAnnot f) stmts ++ f a2

instance HasAnnot a => HasAnnot (JSCommaList a) where
  mapAnnot f (JSLCons xs a x) = JSLCons (mapAnnot f xs) (f a) (mapAnnot f x)
  mapAnnot f (JSLOne x) = JSLOne (mapAnnot f x)
  mapAnnot _ JSLNil = JSLNil
  foldAnnot f (JSLCons xs a x) = foldAnnot f xs ++ f a ++ foldAnnot f x
  foldAnnot f (JSLOne x) = foldAnnot f x
  foldAnnot _ JSLNil = []

instance HasAnnot a => HasAnnot (JSCommaTrailingList a) where
  mapAnnot f (JSCTLComma xs a) = JSCTLComma (mapAnnot f xs) (f a)
  mapAnnot f (JSCTLNone xs) = JSCTLNone (mapAnnot f xs)
  foldAnnot f (JSCTLComma xs a) = foldAnnot f xs ++ f a
  foldAnnot f (JSCTLNone xs) = foldAnnot f xs

instance HasAnnot JSArrayElement where
  mapAnnot f (JSArrayElement e) = JSArrayElement (mapAnnot f e)
  mapAnnot f (JSArrayComma a) = JSArrayComma (f a)
  foldAnnot f (JSArrayElement e) = foldAnnot f e
  foldAnnot f (JSArrayComma a) = f a

instance HasAnnot JSTemplatePart where
  mapAnnot f (JSTemplatePart e a s) = JSTemplatePart (mapAnnot f e) (f a) s
  foldAnnot f (JSTemplatePart e a _) = foldAnnot f e ++ f a

instance HasAnnot JSSwitchParts where
  mapAnnot f (JSCase a1 e a2 stmts) = JSCase (f a1) (mapAnnot f e) (f a2) (map (mapAnnot f) stmts)
  mapAnnot f (JSDefault a1 a2 stmts) = JSDefault (f a1) (f a2) (map (mapAnnot f) stmts)
  foldAnnot f (JSCase a1 e a2 stmts) = f a1 ++ foldAnnot f e ++ f a2 ++ concatMap (foldAnnot f) stmts
  foldAnnot f (JSDefault a1 a2 stmts) = f a1 ++ f a2 ++ concatMap (foldAnnot f) stmts

instance HasAnnot JSTryCatch where
  mapAnnot f (JSCatch a1 a2 e a3 b) = JSCatch (f a1) (f a2) (mapAnnot f e) (f a3) (mapAnnot f b)
  mapAnnot f (JSCatchIf a1 a2 e1 a3 e2 a4 b) = JSCatchIf (f a1) (f a2) (mapAnnot f e1) (f a3) (mapAnnot f e2) (f a4) (mapAnnot f b)
  mapAnnot f (JSCatchNoParam a1 b) = JSCatchNoParam (f a1) (mapAnnot f b)
  foldAnnot f (JSCatch a1 a2 e a3 b) = f a1 ++ f a2 ++ foldAnnot f e ++ f a3 ++ foldAnnot f b
  foldAnnot f (JSCatchIf a1 a2 e1 a3 e2 a4 b) = f a1 ++ f a2 ++ foldAnnot f e1 ++ f a3 ++ foldAnnot f e2 ++ f a4 ++ foldAnnot f b
  foldAnnot f (JSCatchNoParam a1 b) = f a1 ++ foldAnnot f b

instance HasAnnot JSPropertyName where
  mapAnnot f (JSPropertyIdent a s) = JSPropertyIdent (f a) s
  mapAnnot f (JSPropertyString a s) = JSPropertyString (f a) s
  mapAnnot f (JSPropertyNumber a s) = JSPropertyNumber (f a) s
  mapAnnot f (JSPropertyComputed a1 e a2) = JSPropertyComputed (f a1) (mapAnnot f e) (f a2)
  foldAnnot f (JSPropertyIdent a _) = f a
  foldAnnot f (JSPropertyString a _) = f a
  foldAnnot f (JSPropertyNumber a _) = f a
  foldAnnot f (JSPropertyComputed a1 e a2) = f a1 ++ foldAnnot f e ++ f a2

instance HasAnnot JSObjectProperty where
  mapAnnot f (JSPropertyNameandValue n a es) = JSPropertyNameandValue (mapAnnot f n) (f a) (map (mapAnnot f) es)
  mapAnnot f (JSPropertyIdentRef a s) = JSPropertyIdentRef (f a) s
  mapAnnot f (JSObjectMethod m) = JSObjectMethod (mapAnnot f m)
  mapAnnot f (JSObjectSpread a e) = JSObjectSpread (f a) (mapAnnot f e)
  foldAnnot f (JSPropertyNameandValue n a es) = foldAnnot f n ++ f a ++ concatMap (foldAnnot f) es
  foldAnnot f (JSPropertyIdentRef a _) = f a
  foldAnnot f (JSObjectMethod m) = foldAnnot f m
  foldAnnot f (JSObjectSpread a e) = f a ++ foldAnnot f e

instance HasAnnot JSMethodDefinition where
  mapAnnot f (JSMethodDefinition n a1 ps a2 b) = JSMethodDefinition (mapAnnot f n) (f a1) (mapAnnot f ps) (f a2) (mapAnnot f b)
  mapAnnot f (JSGeneratorMethodDefinition a1 n a2 ps a3 b) = JSGeneratorMethodDefinition (f a1) (mapAnnot f n) (f a2) (mapAnnot f ps) (f a3) (mapAnnot f b)
  mapAnnot f (JSAsyncMethodDefinition a1 n a2 ps a3 b) = JSAsyncMethodDefinition (f a1) (mapAnnot f n) (f a2) (mapAnnot f ps) (f a3) (mapAnnot f b)
  mapAnnot f (JSPropertyAccessor acc n a1 ps a2 b) = JSPropertyAccessor (mapAnnot f acc) (mapAnnot f n) (f a1) (mapAnnot f ps) (f a2) (mapAnnot f b)
  foldAnnot f (JSMethodDefinition n a1 ps a2 b) = foldAnnot f n ++ f a1 ++ foldAnnot f ps ++ f a2 ++ foldAnnot f b
  foldAnnot f (JSGeneratorMethodDefinition a1 n a2 ps a3 b) = f a1 ++ foldAnnot f n ++ f a2 ++ foldAnnot f ps ++ f a3 ++ foldAnnot f b
  foldAnnot f (JSAsyncMethodDefinition a1 n a2 ps a3 b) = f a1 ++ foldAnnot f n ++ f a2 ++ foldAnnot f ps ++ f a3 ++ foldAnnot f b
  foldAnnot f (JSPropertyAccessor acc n a1 ps a2 b) = foldAnnot f acc ++ foldAnnot f n ++ f a1 ++ foldAnnot f ps ++ f a2 ++ foldAnnot f b

instance HasAnnot JSClassElement where
  mapAnnot f (JSClassInstanceMethod m) = JSClassInstanceMethod (mapAnnot f m)
  mapAnnot f (JSClassStaticMethod a m) = JSClassStaticMethod (f a) (mapAnnot f m)
  mapAnnot f (JSClassSemi a) = JSClassSemi (f a)
  mapAnnot f (JSPrivateField a1 s a2 mi semi) = JSPrivateField (f a1) s (f a2) (fmap (mapAnnot f) mi) (mapAnnot f semi)
  mapAnnot f (JSPrivateMethod a1 s a2 ps a3 b) = JSPrivateMethod (f a1) s (f a2) (mapAnnot f ps) (f a3) (mapAnnot f b)
  mapAnnot f (JSPrivateAccessor acc a1 s a2 ps a3 b) = JSPrivateAccessor (mapAnnot f acc) (f a1) s (f a2) (mapAnnot f ps) (f a3) (mapAnnot f b)
  mapAnnot f (JSClassField n a mi semi) = JSClassField (mapAnnot f n) (f a) (fmap (mapAnnot f) mi) (mapAnnot f semi)
  mapAnnot f (JSClassStaticField a1 n a2 mi semi) = JSClassStaticField (f a1) (mapAnnot f n) (f a2) (fmap (mapAnnot f) mi) (mapAnnot f semi)
  mapAnnot f (JSClassStaticBlock a b) = JSClassStaticBlock (f a) (mapAnnot f b)
  mapAnnot f (JSAsyncGeneratorMethodDefinition a1 a2 n a3 ps a4 b) = JSAsyncGeneratorMethodDefinition (f a1) (f a2) (mapAnnot f n) (f a3) (mapAnnot f ps) (f a4) (mapAnnot f b)
  foldAnnot f (JSClassInstanceMethod m) = foldAnnot f m
  foldAnnot f (JSClassStaticMethod a m) = f a ++ foldAnnot f m
  foldAnnot f (JSClassSemi a) = f a
  foldAnnot f (JSPrivateField a1 _ a2 mi semi) = f a1 ++ f a2 ++ maybe [] (foldAnnot f) mi ++ foldAnnot f semi
  foldAnnot f (JSPrivateMethod a1 _ a2 ps a3 b) = f a1 ++ f a2 ++ foldAnnot f ps ++ f a3 ++ foldAnnot f b
  foldAnnot f (JSPrivateAccessor acc a1 _ a2 ps a3 b) = foldAnnot f acc ++ f a1 ++ f a2 ++ foldAnnot f ps ++ f a3 ++ foldAnnot f b
  foldAnnot f (JSClassField n a mi semi) = foldAnnot f n ++ f a ++ maybe [] (foldAnnot f) mi ++ foldAnnot f semi
  foldAnnot f (JSClassStaticField a1 n a2 mi semi) = f a1 ++ foldAnnot f n ++ f a2 ++ maybe [] (foldAnnot f) mi ++ foldAnnot f semi
  foldAnnot f (JSClassStaticBlock a b) = f a ++ foldAnnot f b
  foldAnnot f (JSAsyncGeneratorMethodDefinition a1 a2 n a3 ps a4 b) = f a1 ++ f a2 ++ foldAnnot f n ++ f a3 ++ foldAnnot f ps ++ f a4 ++ foldAnnot f b

instance HasAnnot JSArrowParameterList where
  mapAnnot f (JSUnparenthesizedArrowParameter i) = JSUnparenthesizedArrowParameter (mapAnnot f i)
  mapAnnot f (JSParenthesizedArrowParameterList a1 ps a2) = JSParenthesizedArrowParameterList (f a1) (mapAnnot f ps) (f a2)
  foldAnnot f (JSUnparenthesizedArrowParameter i) = foldAnnot f i
  foldAnnot f (JSParenthesizedArrowParameterList a1 ps a2) = f a1 ++ foldAnnot f ps ++ f a2

instance HasAnnot JSConciseBody where
  mapAnnot f (JSConciseFunctionBody b) = JSConciseFunctionBody (mapAnnot f b)
  mapAnnot f (JSConciseExpressionBody e) = JSConciseExpressionBody (mapAnnot f e)
  foldAnnot f (JSConciseFunctionBody b) = foldAnnot f b
  foldAnnot f (JSConciseExpressionBody e) = foldAnnot f e

instance HasAnnot JSFromClause where
  mapAnnot f (JSFromClause a1 a2 s) = JSFromClause (f a1) (f a2) s
  foldAnnot f (JSFromClause a1 a2 _) = f a1 ++ f a2

instance HasAnnot JSImportNameSpace where
  mapAnnot f (JSImportNameSpace op a i) = JSImportNameSpace (mapAnnot f op) (f a) (mapAnnot f i)
  foldAnnot f (JSImportNameSpace op a i) = foldAnnot f op ++ f a ++ foldAnnot f i

instance HasAnnot JSImportsNamed where
  mapAnnot f (JSImportsNamed a1 specs a2) = JSImportsNamed (f a1) (mapAnnot f specs) (f a2)
  foldAnnot f (JSImportsNamed a1 specs a2) = f a1 ++ foldAnnot f specs ++ f a2

instance HasAnnot JSImportSpecifier where
  mapAnnot f (JSImportSpecifier i) = JSImportSpecifier (mapAnnot f i)
  mapAnnot f (JSImportSpecifierAs i1 a i2) = JSImportSpecifierAs (mapAnnot f i1) (f a) (mapAnnot f i2)
  foldAnnot f (JSImportSpecifier i) = foldAnnot f i
  foldAnnot f (JSImportSpecifierAs i1 a i2) = foldAnnot f i1 ++ f a ++ foldAnnot f i2

instance HasAnnot JSImportAttributes where
  mapAnnot f (JSImportAttributes a1 attrs a2) = JSImportAttributes (f a1) (mapAnnot f attrs) (f a2)
  foldAnnot f (JSImportAttributes a1 attrs a2) = f a1 ++ foldAnnot f attrs ++ f a2

instance HasAnnot JSImportAttribute where
  mapAnnot f (JSImportAttribute key a val) = JSImportAttribute (mapAnnot f key) (f a) (mapAnnot f val)
  foldAnnot f (JSImportAttribute key a val) = foldAnnot f key ++ f a ++ foldAnnot f val

instance HasAnnot JSImportClause where
  mapAnnot f (JSImportClauseDefault i) = JSImportClauseDefault (mapAnnot f i)
  mapAnnot f (JSImportClauseNameSpace ns) = JSImportClauseNameSpace (mapAnnot f ns)
  mapAnnot f (JSImportClauseNamed n) = JSImportClauseNamed (mapAnnot f n)
  mapAnnot f (JSImportClauseDefaultNameSpace i a ns) = JSImportClauseDefaultNameSpace (mapAnnot f i) (f a) (mapAnnot f ns)
  mapAnnot f (JSImportClauseDefaultNamed i a n) = JSImportClauseDefaultNamed (mapAnnot f i) (f a) (mapAnnot f n)
  foldAnnot f (JSImportClauseDefault i) = foldAnnot f i
  foldAnnot f (JSImportClauseNameSpace ns) = foldAnnot f ns
  foldAnnot f (JSImportClauseNamed n) = foldAnnot f n
  foldAnnot f (JSImportClauseDefaultNameSpace i a ns) = foldAnnot f i ++ f a ++ foldAnnot f ns
  foldAnnot f (JSImportClauseDefaultNamed i a n) = foldAnnot f i ++ f a ++ foldAnnot f n

instance HasAnnot JSImportDeclaration where
  mapAnnot f (JSImportDeclaration cl from attrs semi) = JSImportDeclaration (mapAnnot f cl) (mapAnnot f from) (fmap (mapAnnot f) attrs) (mapAnnot f semi)
  mapAnnot f (JSImportDeclarationBare a s attrs semi) = JSImportDeclarationBare (f a) s (fmap (mapAnnot f) attrs) (mapAnnot f semi)
  foldAnnot f (JSImportDeclaration cl from attrs semi) = foldAnnot f cl ++ foldAnnot f from ++ maybe [] (foldAnnot f) attrs ++ foldAnnot f semi
  foldAnnot f (JSImportDeclarationBare a _ attrs semi) = f a ++ maybe [] (foldAnnot f) attrs ++ foldAnnot f semi

instance HasAnnot JSExportSpecifier where
  mapAnnot f (JSExportSpecifier i) = JSExportSpecifier (mapAnnot f i)
  mapAnnot f (JSExportSpecifierAs i1 a i2) = JSExportSpecifierAs (mapAnnot f i1) (f a) (mapAnnot f i2)
  foldAnnot f (JSExportSpecifier i) = foldAnnot f i
  foldAnnot f (JSExportSpecifierAs i1 a i2) = foldAnnot f i1 ++ f a ++ foldAnnot f i2

instance HasAnnot JSExportClause where
  mapAnnot f (JSExportClause a1 specs a2) = JSExportClause (f a1) (mapAnnot f specs) (f a2)
  foldAnnot f (JSExportClause a1 specs a2) = f a1 ++ foldAnnot f specs ++ f a2

instance HasAnnot JSExportDeclaration where
  mapAnnot f (JSExportAllFrom star from semi) = JSExportAllFrom (mapAnnot f star) (mapAnnot f from) (mapAnnot f semi)
  mapAnnot f (JSExportAllAsFrom star a i from semi) = JSExportAllAsFrom (mapAnnot f star) (f a) (mapAnnot f i) (mapAnnot f from) (mapAnnot f semi)
  mapAnnot f (JSExportFrom cl from semi) = JSExportFrom (mapAnnot f cl) (mapAnnot f from) (mapAnnot f semi)
  mapAnnot f (JSExportLocals cl semi) = JSExportLocals (mapAnnot f cl) (mapAnnot f semi)
  mapAnnot f (JSExportDefault a stmt semi) = JSExportDefault (f a) (mapAnnot f stmt) (mapAnnot f semi)
  mapAnnot f (JSExport stmt semi) = JSExport (mapAnnot f stmt) (mapAnnot f semi)
  foldAnnot f (JSExportAllFrom star from semi) = foldAnnot f star ++ foldAnnot f from ++ foldAnnot f semi
  foldAnnot f (JSExportAllAsFrom star a i from semi) = foldAnnot f star ++ f a ++ foldAnnot f i ++ foldAnnot f from ++ foldAnnot f semi
  foldAnnot f (JSExportFrom cl from semi) = foldAnnot f cl ++ foldAnnot f from ++ foldAnnot f semi
  foldAnnot f (JSExportLocals cl semi) = foldAnnot f cl ++ foldAnnot f semi
  foldAnnot f (JSExportDefault a stmt semi) = f a ++ foldAnnot f stmt ++ foldAnnot f semi
  foldAnnot f (JSExport stmt semi) = foldAnnot f stmt ++ foldAnnot f semi

instance HasAnnot JSModuleItem where
  mapAnnot f (JSModuleImportDeclaration a d) = JSModuleImportDeclaration (f a) (mapAnnot f d)
  mapAnnot f (JSModuleExportDeclaration a d) = JSModuleExportDeclaration (f a) (mapAnnot f d)
  mapAnnot f (JSModuleStatementListItem s) = JSModuleStatementListItem (mapAnnot f s)
  foldAnnot f (JSModuleImportDeclaration a d) = f a ++ foldAnnot f d
  foldAnnot f (JSModuleExportDeclaration a d) = f a ++ foldAnnot f d
  foldAnnot f (JSModuleStatementListItem s) = foldAnnot f s

instance HasAnnot JSExpression where
  mapAnnot f (JSIdentifier a s) = JSIdentifier (f a) s
  mapAnnot f (JSDecimal a d) = JSDecimal (f a) d
  mapAnnot f (JSLiteral a s) = JSLiteral (f a) s
  mapAnnot f (JSHexInteger a n) = JSHexInteger (f a) n
  mapAnnot f (JSBinaryInteger a n) = JSBinaryInteger (f a) n
  mapAnnot f (JSOctal a n) = JSOctal (f a) n
  mapAnnot f (JSBigIntLiteral a n) = JSBigIntLiteral (f a) n
  mapAnnot f (JSStringLiteral a s) = JSStringLiteral (f a) s
  mapAnnot f (JSRegEx a s) = JSRegEx (f a) s
  mapAnnot f (JSArrayLiteral a1 es a2) = JSArrayLiteral (f a1) (map (mapAnnot f) es) (f a2)
  mapAnnot f (JSAssignExpression e1 op e2) = JSAssignExpression (mapAnnot f e1) (mapAnnot f op) (mapAnnot f e2)
  mapAnnot f (JSAwaitExpression a e) = JSAwaitExpression (f a) (mapAnnot f e)
  mapAnnot f (JSCallExpression e a1 args a2) = JSCallExpression (mapAnnot f e) (f a1) (mapAnnot f args) (f a2)
  mapAnnot f (JSCallExpressionDot e a x) = JSCallExpressionDot (mapAnnot f e) (f a) (mapAnnot f x)
  mapAnnot f (JSCallExpressionSquare e a1 x a2) = JSCallExpressionSquare (mapAnnot f e) (f a1) (mapAnnot f x) (f a2)
  mapAnnot f (JSClassExpression a1 i h a2 es a3) = JSClassExpression (f a1) (mapAnnot f i) (mapAnnot f h) (f a2) (map (mapAnnot f) es) (f a3)
  mapAnnot f (JSCommaExpression e1 a e2) = JSCommaExpression (mapAnnot f e1) (f a) (mapAnnot f e2)
  mapAnnot f (JSExpressionBinary e1 op e2) = JSExpressionBinary (mapAnnot f e1) (mapAnnot f op) (mapAnnot f e2)
  mapAnnot f (JSExpressionParen a1 e a2) = JSExpressionParen (f a1) (mapAnnot f e) (f a2)
  mapAnnot f (JSExpressionPostfix e op) = JSExpressionPostfix (mapAnnot f e) (mapAnnot f op)
  mapAnnot f (JSExpressionTernary e1 a1 e2 a2 e3) = JSExpressionTernary (mapAnnot f e1) (f a1) (mapAnnot f e2) (f a2) (mapAnnot f e3)
  mapAnnot f (JSArrowExpression ps a b) = JSArrowExpression (mapAnnot f ps) (f a) (mapAnnot f b)
  mapAnnot f (JSFunctionExpression a1 i a2 ps a3 b) = JSFunctionExpression (f a1) (mapAnnot f i) (f a2) (mapAnnot f ps) (f a3) (mapAnnot f b)
  mapAnnot f (JSGeneratorExpression a1 a2 i a3 ps a4 b) = JSGeneratorExpression (f a1) (f a2) (mapAnnot f i) (f a3) (mapAnnot f ps) (f a4) (mapAnnot f b)
  mapAnnot f (JSAsyncFunctionExpression a1 a2 i a3 ps a4 b) = JSAsyncFunctionExpression (f a1) (f a2) (mapAnnot f i) (f a3) (mapAnnot f ps) (f a4) (mapAnnot f b)
  mapAnnot f (JSAsyncArrowExpression a1 ps a2 b) = JSAsyncArrowExpression (f a1) (mapAnnot f ps) (f a2) (mapAnnot f b)
  mapAnnot f (JSAsyncGeneratorExpression a1 a2 a3 i a4 ps a5 b) = JSAsyncGeneratorExpression (f a1) (f a2) (f a3) (mapAnnot f i) (f a4) (mapAnnot f ps) (f a5) (mapAnnot f b)
  mapAnnot f (JSMemberDot e1 a e2) = JSMemberDot (mapAnnot f e1) (f a) (mapAnnot f e2)
  mapAnnot f (JSMemberPrivateDot e1 a1 a2 name) = JSMemberPrivateDot (mapAnnot f e1) (f a1) (f a2) name
  mapAnnot f (JSMemberExpression e a1 args a2) = JSMemberExpression (mapAnnot f e) (f a1) (mapAnnot f args) (f a2)
  mapAnnot f (JSMemberNew a1 e a2 args a3) = JSMemberNew (f a1) (mapAnnot f e) (f a2) (mapAnnot f args) (f a3)
  mapAnnot f (JSMemberSquare e a1 x a2) = JSMemberSquare (mapAnnot f e) (f a1) (mapAnnot f x) (f a2)
  mapAnnot f (JSNewExpression a e) = JSNewExpression (f a) (mapAnnot f e)
  mapAnnot f (JSOptionalMemberDot e1 a e2) = JSOptionalMemberDot (mapAnnot f e1) (f a) (mapAnnot f e2)
  mapAnnot f (JSOptionalMemberSquare e1 a1 e2 a2) = JSOptionalMemberSquare (mapAnnot f e1) (f a1) (mapAnnot f e2) (f a2)
  mapAnnot f (JSOptionalCallExpression e a1 args a2) = JSOptionalCallExpression (mapAnnot f e) (f a1) (mapAnnot f args) (f a2)
  mapAnnot f (JSObjectLiteral a1 props a2) = JSObjectLiteral (f a1) (mapAnnot f props) (f a2)
  mapAnnot f (JSSpreadExpression a e) = JSSpreadExpression (f a) (mapAnnot f e)
  mapAnnot f (JSTemplateLiteral mt a s ps) = JSTemplateLiteral (fmap (mapAnnot f) mt) (f a) s (map (mapAnnot f) ps)
  mapAnnot f (JSUnaryExpression op e) = JSUnaryExpression (mapAnnot f op) (mapAnnot f e)
  mapAnnot f (JSVarInitExpression e vi) = JSVarInitExpression (mapAnnot f e) (mapAnnot f vi)
  mapAnnot f (JSYieldExpression a me) = JSYieldExpression (f a) (fmap (mapAnnot f) me)
  mapAnnot f (JSYieldFromExpression a1 a2 e) = JSYieldFromExpression (f a1) (f a2) (mapAnnot f e)
  mapAnnot f (JSImportMeta a1 a2) = JSImportMeta (f a1) (f a2)
  mapAnnot f (JSImportCall a1 a2 e a3) = JSImportCall (f a1) (f a2) (mapAnnot f e) (f a3)
  foldAnnot f (JSIdentifier a _) = f a
  foldAnnot f (JSDecimal a _) = f a
  foldAnnot f (JSLiteral a _) = f a
  foldAnnot f (JSHexInteger a _) = f a
  foldAnnot f (JSBinaryInteger a _) = f a
  foldAnnot f (JSOctal a _) = f a
  foldAnnot f (JSBigIntLiteral a _) = f a
  foldAnnot f (JSStringLiteral a _) = f a
  foldAnnot f (JSRegEx a _) = f a
  foldAnnot f (JSArrayLiteral a1 es a2) = f a1 ++ concatMap (foldAnnot f) es ++ f a2
  foldAnnot f (JSAssignExpression e1 op e2) = foldAnnot f e1 ++ foldAnnot f op ++ foldAnnot f e2
  foldAnnot f (JSAwaitExpression a e) = f a ++ foldAnnot f e
  foldAnnot f (JSCallExpression e a1 args a2) = foldAnnot f e ++ f a1 ++ foldAnnot f args ++ f a2
  foldAnnot f (JSCallExpressionDot e a x) = foldAnnot f e ++ f a ++ foldAnnot f x
  foldAnnot f (JSCallExpressionSquare e a1 x a2) = foldAnnot f e ++ f a1 ++ foldAnnot f x ++ f a2
  foldAnnot f (JSClassExpression a1 i h a2 es a3) = f a1 ++ foldAnnot f i ++ foldAnnot f h ++ f a2 ++ concatMap (foldAnnot f) es ++ f a3
  foldAnnot f (JSCommaExpression e1 a e2) = foldAnnot f e1 ++ f a ++ foldAnnot f e2
  foldAnnot f (JSExpressionBinary e1 op e2) = foldAnnot f e1 ++ foldAnnot f op ++ foldAnnot f e2
  foldAnnot f (JSExpressionParen a1 e a2) = f a1 ++ foldAnnot f e ++ f a2
  foldAnnot f (JSExpressionPostfix e op) = foldAnnot f e ++ foldAnnot f op
  foldAnnot f (JSExpressionTernary e1 a1 e2 a2 e3) = foldAnnot f e1 ++ f a1 ++ foldAnnot f e2 ++ f a2 ++ foldAnnot f e3
  foldAnnot f (JSArrowExpression ps a b) = foldAnnot f ps ++ f a ++ foldAnnot f b
  foldAnnot f (JSFunctionExpression a1 i a2 ps a3 b) = f a1 ++ foldAnnot f i ++ f a2 ++ foldAnnot f ps ++ f a3 ++ foldAnnot f b
  foldAnnot f (JSGeneratorExpression a1 a2 i a3 ps a4 b) = f a1 ++ f a2 ++ foldAnnot f i ++ f a3 ++ foldAnnot f ps ++ f a4 ++ foldAnnot f b
  foldAnnot f (JSAsyncFunctionExpression a1 a2 i a3 ps a4 b) = f a1 ++ f a2 ++ foldAnnot f i ++ f a3 ++ foldAnnot f ps ++ f a4 ++ foldAnnot f b
  foldAnnot f (JSAsyncArrowExpression a1 ps a2 b) = f a1 ++ foldAnnot f ps ++ f a2 ++ foldAnnot f b
  foldAnnot f (JSAsyncGeneratorExpression a1 a2 a3 i a4 ps a5 b) = f a1 ++ f a2 ++ f a3 ++ foldAnnot f i ++ f a4 ++ foldAnnot f ps ++ f a5 ++ foldAnnot f b
  foldAnnot f (JSMemberDot e1 a e2) = foldAnnot f e1 ++ f a ++ foldAnnot f e2
  foldAnnot f (JSMemberPrivateDot e1 a1 a2 _name) = foldAnnot f e1 ++ f a1 ++ f a2
  foldAnnot f (JSMemberExpression e a1 args a2) = foldAnnot f e ++ f a1 ++ foldAnnot f args ++ f a2
  foldAnnot f (JSMemberNew a1 e a2 args a3) = f a1 ++ foldAnnot f e ++ f a2 ++ foldAnnot f args ++ f a3
  foldAnnot f (JSMemberSquare e a1 x a2) = foldAnnot f e ++ f a1 ++ foldAnnot f x ++ f a2
  foldAnnot f (JSNewExpression a e) = f a ++ foldAnnot f e
  foldAnnot f (JSOptionalMemberDot e1 a e2) = foldAnnot f e1 ++ f a ++ foldAnnot f e2
  foldAnnot f (JSOptionalMemberSquare e1 a1 e2 a2) = foldAnnot f e1 ++ f a1 ++ foldAnnot f e2 ++ f a2
  foldAnnot f (JSOptionalCallExpression e a1 args a2) = foldAnnot f e ++ f a1 ++ foldAnnot f args ++ f a2
  foldAnnot f (JSObjectLiteral a1 props a2) = f a1 ++ foldAnnot f props ++ f a2
  foldAnnot f (JSSpreadExpression a e) = f a ++ foldAnnot f e
  foldAnnot f (JSTemplateLiteral mt a _ ps) = maybe [] (foldAnnot f) mt ++ f a ++ concatMap (foldAnnot f) ps
  foldAnnot f (JSUnaryExpression op e) = foldAnnot f op ++ foldAnnot f e
  foldAnnot f (JSVarInitExpression e vi) = foldAnnot f e ++ foldAnnot f vi
  foldAnnot f (JSYieldExpression a me) = f a ++ maybe [] (foldAnnot f) me
  foldAnnot f (JSYieldFromExpression a1 a2 e) = f a1 ++ f a2 ++ foldAnnot f e
  foldAnnot f (JSImportMeta a1 a2) = f a1 ++ f a2
  foldAnnot f (JSImportCall a1 a2 e a3) = f a1 ++ f a2 ++ foldAnnot f e ++ f a3

instance HasAnnot JSStatement where
  mapAnnot f (JSStatementBlock a1 stmts a2 semi) = JSStatementBlock (f a1) (map (mapAnnot f) stmts) (f a2) (mapAnnot f semi)
  mapAnnot f (JSBreak a i semi) = JSBreak (f a) (mapAnnot f i) (mapAnnot f semi)
  mapAnnot f (JSLet a es semi) = JSLet (f a) (mapAnnot f es) (mapAnnot f semi)
  mapAnnot f (JSClass a1 i h a2 es a3 semi) = JSClass (f a1) (mapAnnot f i) (mapAnnot f h) (f a2) (map (mapAnnot f) es) (f a3) (mapAnnot f semi)
  mapAnnot f (JSConstant a es semi) = JSConstant (f a) (mapAnnot f es) (mapAnnot f semi)
  mapAnnot f (JSContinue a i semi) = JSContinue (f a) (mapAnnot f i) (mapAnnot f semi)
  mapAnnot f (JSDoWhile a1 s a2 a3 e a4 semi) = JSDoWhile (f a1) (mapAnnot f s) (f a2) (f a3) (mapAnnot f e) (f a4) (mapAnnot f semi)
  mapAnnot f (JSFor a1 a2 es1 a3 es2 a4 es3 a5 s) = JSFor (f a1) (f a2) (mapAnnot f es1) (f a3) (mapAnnot f es2) (f a4) (mapAnnot f es3) (f a5) (mapAnnot f s)
  mapAnnot f (JSForIn a1 a2 e1 op e2 a3 s) = JSForIn (f a1) (f a2) (mapAnnot f e1) (mapAnnot f op) (mapAnnot f e2) (f a3) (mapAnnot f s)
  mapAnnot f (JSForVar a1 a2 a3 es1 a4 es2 a5 es3 a6 s) = JSForVar (f a1) (f a2) (f a3) (mapAnnot f es1) (f a4) (mapAnnot f es2) (f a5) (mapAnnot f es3) (f a6) (mapAnnot f s)
  mapAnnot f (JSForVarIn a1 a2 a3 e1 op e2 a4 s) = JSForVarIn (f a1) (f a2) (f a3) (mapAnnot f e1) (mapAnnot f op) (mapAnnot f e2) (f a4) (mapAnnot f s)
  mapAnnot f (JSForLet a1 a2 a3 es1 a4 es2 a5 es3 a6 s) = JSForLet (f a1) (f a2) (f a3) (mapAnnot f es1) (f a4) (mapAnnot f es2) (f a5) (mapAnnot f es3) (f a6) (mapAnnot f s)
  mapAnnot f (JSForLetIn a1 a2 a3 e1 op e2 a4 s) = JSForLetIn (f a1) (f a2) (f a3) (mapAnnot f e1) (mapAnnot f op) (mapAnnot f e2) (f a4) (mapAnnot f s)
  mapAnnot f (JSForLetOf a1 a2 a3 e1 op e2 a4 s) = JSForLetOf (f a1) (f a2) (f a3) (mapAnnot f e1) (mapAnnot f op) (mapAnnot f e2) (f a4) (mapAnnot f s)
  mapAnnot f (JSForConst a1 a2 a3 es1 a4 es2 a5 es3 a6 s) = JSForConst (f a1) (f a2) (f a3) (mapAnnot f es1) (f a4) (mapAnnot f es2) (f a5) (mapAnnot f es3) (f a6) (mapAnnot f s)
  mapAnnot f (JSForConstIn a1 a2 a3 e1 op e2 a4 s) = JSForConstIn (f a1) (f a2) (f a3) (mapAnnot f e1) (mapAnnot f op) (mapAnnot f e2) (f a4) (mapAnnot f s)
  mapAnnot f (JSForConstOf a1 a2 a3 e1 op e2 a4 s) = JSForConstOf (f a1) (f a2) (f a3) (mapAnnot f e1) (mapAnnot f op) (mapAnnot f e2) (f a4) (mapAnnot f s)
  mapAnnot f (JSForOf a1 a2 e1 op e2 a3 s) = JSForOf (f a1) (f a2) (mapAnnot f e1) (mapAnnot f op) (mapAnnot f e2) (f a3) (mapAnnot f s)
  mapAnnot f (JSForVarOf a1 a2 a3 e1 op e2 a4 s) = JSForVarOf (f a1) (f a2) (f a3) (mapAnnot f e1) (mapAnnot f op) (mapAnnot f e2) (f a4) (mapAnnot f s)
  mapAnnot f (JSAsyncFunction a1 a2 i a3 ps a4 b semi) = JSAsyncFunction (f a1) (f a2) (mapAnnot f i) (f a3) (mapAnnot f ps) (f a4) (mapAnnot f b) (mapAnnot f semi)
  mapAnnot f (JSFunction a1 i a2 ps a3 b semi) = JSFunction (f a1) (mapAnnot f i) (f a2) (mapAnnot f ps) (f a3) (mapAnnot f b) (mapAnnot f semi)
  mapAnnot f (JSGenerator a1 a2 i a3 ps a4 b semi) = JSGenerator (f a1) (f a2) (mapAnnot f i) (f a3) (mapAnnot f ps) (f a4) (mapAnnot f b) (mapAnnot f semi)
  mapAnnot f (JSIf a1 a2 e a3 s) = JSIf (f a1) (f a2) (mapAnnot f e) (f a3) (mapAnnot f s)
  mapAnnot f (JSIfElse a1 a2 e a3 s1 a4 s2) = JSIfElse (f a1) (f a2) (mapAnnot f e) (f a3) (mapAnnot f s1) (f a4) (mapAnnot f s2)
  mapAnnot f (JSLabelled i a s) = JSLabelled (mapAnnot f i) (f a) (mapAnnot f s)
  mapAnnot f (JSEmptyStatement a) = JSEmptyStatement (f a)
  mapAnnot f (JSExpressionStatement e semi) = JSExpressionStatement (mapAnnot f e) (mapAnnot f semi)
  mapAnnot f (JSAssignStatement e1 op e2 semi) = JSAssignStatement (mapAnnot f e1) (mapAnnot f op) (mapAnnot f e2) (mapAnnot f semi)
  mapAnnot f (JSMethodCall e a1 args a2 semi) = JSMethodCall (mapAnnot f e) (f a1) (mapAnnot f args) (f a2) (mapAnnot f semi)
  mapAnnot f (JSReturn a me semi) = JSReturn (f a) (fmap (mapAnnot f) me) (mapAnnot f semi)
  mapAnnot f (JSSwitch a1 a2 e a3 a4 parts a5 semi) = JSSwitch (f a1) (f a2) (mapAnnot f e) (f a3) (f a4) (map (mapAnnot f) parts) (f a5) (mapAnnot f semi)
  mapAnnot f (JSThrow a e semi) = JSThrow (f a) (mapAnnot f e) (mapAnnot f semi)
  mapAnnot f (JSTry a b catches fin) = JSTry (f a) (mapAnnot f b) (map (mapAnnot f) catches) (mapAnnot f fin)
  mapAnnot f (JSVariable a es semi) = JSVariable (f a) (mapAnnot f es) (mapAnnot f semi)
  mapAnnot f (JSWhile a1 a2 e a3 s) = JSWhile (f a1) (f a2) (mapAnnot f e) (f a3) (mapAnnot f s)
  mapAnnot f (JSWith a1 a2 e a3 s semi) = JSWith (f a1) (f a2) (mapAnnot f e) (f a3) (mapAnnot f s) (mapAnnot f semi)
  mapAnnot f (JSDebugger a semi) = JSDebugger (f a) (mapAnnot f semi)
  mapAnnot f (JSAsyncGenerator a1 a2 a3 i a4 ps a5 b semi) = JSAsyncGenerator (f a1) (f a2) (f a3) (mapAnnot f i) (f a4) (mapAnnot f ps) (f a5) (mapAnnot f b) (mapAnnot f semi)
  mapAnnot f (JSForAwaitOf a1 a2 a3 e1 op e2 a4 s) = JSForAwaitOf (f a1) (f a2) (f a3) (mapAnnot f e1) (mapAnnot f op) (mapAnnot f e2) (f a4) (mapAnnot f s)
  mapAnnot f (JSForAwaitVarOf a1 a2 a3 a4 e1 op e2 a5 s) = JSForAwaitVarOf (f a1) (f a2) (f a3) (f a4) (mapAnnot f e1) (mapAnnot f op) (mapAnnot f e2) (f a5) (mapAnnot f s)
  mapAnnot f (JSForAwaitLetOf a1 a2 a3 a4 e1 op e2 a5 s) = JSForAwaitLetOf (f a1) (f a2) (f a3) (f a4) (mapAnnot f e1) (mapAnnot f op) (mapAnnot f e2) (f a5) (mapAnnot f s)
  mapAnnot f (JSForAwaitConstOf a1 a2 a3 a4 e1 op e2 a5 s) = JSForAwaitConstOf (f a1) (f a2) (f a3) (f a4) (mapAnnot f e1) (mapAnnot f op) (mapAnnot f e2) (f a5) (mapAnnot f s)
  foldAnnot f (JSStatementBlock a1 stmts a2 semi) = f a1 ++ concatMap (foldAnnot f) stmts ++ f a2 ++ foldAnnot f semi
  foldAnnot f (JSBreak a i semi) = f a ++ foldAnnot f i ++ foldAnnot f semi
  foldAnnot f (JSLet a es semi) = f a ++ foldAnnot f es ++ foldAnnot f semi
  foldAnnot f (JSClass a1 i h a2 es a3 semi) = f a1 ++ foldAnnot f i ++ foldAnnot f h ++ f a2 ++ concatMap (foldAnnot f) es ++ f a3 ++ foldAnnot f semi
  foldAnnot f (JSConstant a es semi) = f a ++ foldAnnot f es ++ foldAnnot f semi
  foldAnnot f (JSContinue a i semi) = f a ++ foldAnnot f i ++ foldAnnot f semi
  foldAnnot f (JSDoWhile a1 s a2 a3 e a4 semi) = f a1 ++ foldAnnot f s ++ f a2 ++ f a3 ++ foldAnnot f e ++ f a4 ++ foldAnnot f semi
  foldAnnot f (JSFor a1 a2 es1 a3 es2 a4 es3 a5 s) = f a1 ++ f a2 ++ foldAnnot f es1 ++ f a3 ++ foldAnnot f es2 ++ f a4 ++ foldAnnot f es3 ++ f a5 ++ foldAnnot f s
  foldAnnot f (JSForIn a1 a2 e1 op e2 a3 s) = f a1 ++ f a2 ++ foldAnnot f e1 ++ foldAnnot f op ++ foldAnnot f e2 ++ f a3 ++ foldAnnot f s
  foldAnnot f (JSForVar a1 a2 a3 es1 a4 es2 a5 es3 a6 s) = f a1 ++ f a2 ++ f a3 ++ foldAnnot f es1 ++ f a4 ++ foldAnnot f es2 ++ f a5 ++ foldAnnot f es3 ++ f a6 ++ foldAnnot f s
  foldAnnot f (JSForVarIn a1 a2 a3 e1 op e2 a4 s) = f a1 ++ f a2 ++ f a3 ++ foldAnnot f e1 ++ foldAnnot f op ++ foldAnnot f e2 ++ f a4 ++ foldAnnot f s
  foldAnnot f (JSForLet a1 a2 a3 es1 a4 es2 a5 es3 a6 s) = f a1 ++ f a2 ++ f a3 ++ foldAnnot f es1 ++ f a4 ++ foldAnnot f es2 ++ f a5 ++ foldAnnot f es3 ++ f a6 ++ foldAnnot f s
  foldAnnot f (JSForLetIn a1 a2 a3 e1 op e2 a4 s) = f a1 ++ f a2 ++ f a3 ++ foldAnnot f e1 ++ foldAnnot f op ++ foldAnnot f e2 ++ f a4 ++ foldAnnot f s
  foldAnnot f (JSForLetOf a1 a2 a3 e1 op e2 a4 s) = f a1 ++ f a2 ++ f a3 ++ foldAnnot f e1 ++ foldAnnot f op ++ foldAnnot f e2 ++ f a4 ++ foldAnnot f s
  foldAnnot f (JSForConst a1 a2 a3 es1 a4 es2 a5 es3 a6 s) = f a1 ++ f a2 ++ f a3 ++ foldAnnot f es1 ++ f a4 ++ foldAnnot f es2 ++ f a5 ++ foldAnnot f es3 ++ f a6 ++ foldAnnot f s
  foldAnnot f (JSForConstIn a1 a2 a3 e1 op e2 a4 s) = f a1 ++ f a2 ++ f a3 ++ foldAnnot f e1 ++ foldAnnot f op ++ foldAnnot f e2 ++ f a4 ++ foldAnnot f s
  foldAnnot f (JSForConstOf a1 a2 a3 e1 op e2 a4 s) = f a1 ++ f a2 ++ f a3 ++ foldAnnot f e1 ++ foldAnnot f op ++ foldAnnot f e2 ++ f a4 ++ foldAnnot f s
  foldAnnot f (JSForOf a1 a2 e1 op e2 a3 s) = f a1 ++ f a2 ++ foldAnnot f e1 ++ foldAnnot f op ++ foldAnnot f e2 ++ f a3 ++ foldAnnot f s
  foldAnnot f (JSForVarOf a1 a2 a3 e1 op e2 a4 s) = f a1 ++ f a2 ++ f a3 ++ foldAnnot f e1 ++ foldAnnot f op ++ foldAnnot f e2 ++ f a4 ++ foldAnnot f s
  foldAnnot f (JSAsyncFunction a1 a2 i a3 ps a4 b semi) = f a1 ++ f a2 ++ foldAnnot f i ++ f a3 ++ foldAnnot f ps ++ f a4 ++ foldAnnot f b ++ foldAnnot f semi
  foldAnnot f (JSFunction a1 i a2 ps a3 b semi) = f a1 ++ foldAnnot f i ++ f a2 ++ foldAnnot f ps ++ f a3 ++ foldAnnot f b ++ foldAnnot f semi
  foldAnnot f (JSGenerator a1 a2 i a3 ps a4 b semi) = f a1 ++ f a2 ++ foldAnnot f i ++ f a3 ++ foldAnnot f ps ++ f a4 ++ foldAnnot f b ++ foldAnnot f semi
  foldAnnot f (JSIf a1 a2 e a3 s) = f a1 ++ f a2 ++ foldAnnot f e ++ f a3 ++ foldAnnot f s
  foldAnnot f (JSIfElse a1 a2 e a3 s1 a4 s2) = f a1 ++ f a2 ++ foldAnnot f e ++ f a3 ++ foldAnnot f s1 ++ f a4 ++ foldAnnot f s2
  foldAnnot f (JSLabelled i a s) = foldAnnot f i ++ f a ++ foldAnnot f s
  foldAnnot f (JSEmptyStatement a) = f a
  foldAnnot f (JSExpressionStatement e semi) = foldAnnot f e ++ foldAnnot f semi
  foldAnnot f (JSAssignStatement e1 op e2 semi) = foldAnnot f e1 ++ foldAnnot f op ++ foldAnnot f e2 ++ foldAnnot f semi
  foldAnnot f (JSMethodCall e a1 args a2 semi) = foldAnnot f e ++ f a1 ++ foldAnnot f args ++ f a2 ++ foldAnnot f semi
  foldAnnot f (JSReturn a me semi) = f a ++ maybe [] (foldAnnot f) me ++ foldAnnot f semi
  foldAnnot f (JSSwitch a1 a2 e a3 a4 parts a5 semi) = f a1 ++ f a2 ++ foldAnnot f e ++ f a3 ++ f a4 ++ concatMap (foldAnnot f) parts ++ f a5 ++ foldAnnot f semi
  foldAnnot f (JSThrow a e semi) = f a ++ foldAnnot f e ++ foldAnnot f semi
  foldAnnot f (JSTry a b catches fin) = f a ++ foldAnnot f b ++ concatMap (foldAnnot f) catches ++ foldAnnot f fin
  foldAnnot f (JSVariable a es semi) = f a ++ foldAnnot f es ++ foldAnnot f semi
  foldAnnot f (JSWhile a1 a2 e a3 s) = f a1 ++ f a2 ++ foldAnnot f e ++ f a3 ++ foldAnnot f s
  foldAnnot f (JSWith a1 a2 e a3 s semi) = f a1 ++ f a2 ++ foldAnnot f e ++ f a3 ++ foldAnnot f s ++ foldAnnot f semi
  foldAnnot f (JSDebugger a semi) = f a ++ foldAnnot f semi
  foldAnnot f (JSAsyncGenerator a1 a2 a3 i a4 ps a5 b semi) = f a1 ++ f a2 ++ f a3 ++ foldAnnot f i ++ f a4 ++ foldAnnot f ps ++ f a5 ++ foldAnnot f b ++ foldAnnot f semi
  foldAnnot f (JSForAwaitOf a1 a2 a3 e1 op e2 a4 s) = f a1 ++ f a2 ++ f a3 ++ foldAnnot f e1 ++ foldAnnot f op ++ foldAnnot f e2 ++ f a4 ++ foldAnnot f s
  foldAnnot f (JSForAwaitVarOf a1 a2 a3 a4 e1 op e2 a5 s) = f a1 ++ f a2 ++ f a3 ++ f a4 ++ foldAnnot f e1 ++ foldAnnot f op ++ foldAnnot f e2 ++ f a5 ++ foldAnnot f s
  foldAnnot f (JSForAwaitLetOf a1 a2 a3 a4 e1 op e2 a5 s) = f a1 ++ f a2 ++ f a3 ++ f a4 ++ foldAnnot f e1 ++ foldAnnot f op ++ foldAnnot f e2 ++ f a5 ++ foldAnnot f s
  foldAnnot f (JSForAwaitConstOf a1 a2 a3 a4 e1 op e2 a5 s) = f a1 ++ f a2 ++ f a3 ++ f a4 ++ foldAnnot f e1 ++ foldAnnot f op ++ foldAnnot f e2 ++ f a5 ++ foldAnnot f s

instance HasAnnot JSAST where
  mapAnnot f (JSAstProgram stmts a) = JSAstProgram (map (mapAnnot f) stmts) (f a)
  mapAnnot f (JSAstModule items a) = JSAstModule (map (mapAnnot f) items) (f a)
  mapAnnot f (JSAstStatement s a) = JSAstStatement (mapAnnot f s) (f a)
  mapAnnot f (JSAstExpression e a) = JSAstExpression (mapAnnot f e) (f a)
  mapAnnot f (JSAstLiteral e a) = JSAstLiteral (mapAnnot f e) (f a)
  foldAnnot f (JSAstProgram stmts a) = concatMap (foldAnnot f) stmts ++ f a
  foldAnnot f (JSAstModule items a) = concatMap (foldAnnot f) items ++ f a
  foldAnnot f (JSAstStatement s a) = foldAnnot f s ++ f a
  foldAnnot f (JSAstExpression e a) = foldAnnot f e ++ f a
  foldAnnot f (JSAstLiteral e a) = foldAnnot f e ++ f a

-- -----------------------------------------------------------------------------

-- | Show the AST elements stripped of their JSAnnot data.

-- Strip out the location info

-- | Convert AST to ByteString representation stripped of position information.
--
-- Removes all 'JSAnnot' location data while preserving the logical structure
-- of the JavaScript AST. Useful for testing and debugging when position
-- information is not relevant. Uses ByteString for optimal performance.
--
-- ==== Examples
--
-- >>> showStripped (JSAstProgram [JSEmptyStatement JSNoAnnot] JSNoAnnot)
-- "JSAstProgram [JSEmptyStatement]"
--
-- @since 0.7.1.0
showStripped :: JSAST -> String
showStripped (JSAstProgram xs _) = "JSAstProgram " <> ss xs
showStripped (JSAstModule xs _) = "JSAstModule " <> ss xs
showStripped (JSAstStatement s _) = "JSAstStatement (" <> ss s <> ")"
showStripped (JSAstExpression e _) = "JSAstExpression (" <> ss e <> ")"
showStripped (JSAstLiteral e _) = "JSAstLiteral (" <> ss e <> ")"

class ShowStripped a where
  ss :: a -> String

instance ShowStripped JSStatement where
  ss (JSStatementBlock _ xs _ _) = "JSStatementBlock " <> ss xs
  ss (JSBreak _ JSIdentNone s) = "JSBreak" <> commaIf (ss s)
  ss (JSBreak _ (JSIdentName _ n) s) = "JSBreak " <> singleQuote (bsToStr n) <> commaIf (ss s)
  ss (JSClass _ n h _lb xs _rb _) = "JSClass " <> ssid n <> " (" <> ss h <> ") " <> ss xs
  ss (JSContinue _ JSIdentNone s) = "JSContinue" <> commaIf (ss s)
  ss (JSContinue _ (JSIdentName _ n) s) = "JSContinue " <> singleQuote (bsToStr n) <> commaIf (ss s)
  ss (JSConstant _ xs _as) = "JSConstant " <> ss xs
  ss (JSDoWhile _d x1 _w _lb x2 _rb x3) = "JSDoWhile (" <> ss x1 <> ") (" <> ss x2 <> ") (" <> ss x3 <> ")"
  ss (JSFor _ _lb x1s _s1 x2s _s2 x3s _rb x4) = "JSFor " <> ss x1s <> " " <> ss x2s <> " " <> ss x3s <> " (" <> ss x4 <> ")"
  ss (JSForIn _ _lb x1s _i x2 _rb x3) = "JSForIn " <> ss x1s <> " (" <> ss x2 <> ") (" <> ss x3 <> ")"
  ss (JSForVar _ _lb _v x1s _s1 x2s _s2 x3s _rb x4) = "JSForVar " <> ss x1s <> " " <> ss x2s <> " " <> ss x3s <> " (" <> ss x4 <> ")"
  ss (JSForVarIn _ _lb _v x1 _i x2 _rb x3) = "JSForVarIn (" <> ss x1 <> ") (" <> ss x2 <> ") (" <> ss x3 <> ")"
  ss (JSForLet _ _lb _v x1s _s1 x2s _s2 x3s _rb x4) = "JSForLet " <> ss x1s <> " " <> ss x2s <> " " <> ss x3s <> " (" <> ss x4 <> ")"
  ss (JSForLetIn _ _lb _v x1 _i x2 _rb x3) = "JSForLetIn (" <> ss x1 <> ") (" <> ss x2 <> ") (" <> ss x3 <> ")"
  ss (JSForLetOf _ _lb _v x1 _i x2 _rb x3) = "JSForLetOf (" <> ss x1 <> ") (" <> ss x2 <> ") (" <> ss x3 <> ")"
  ss (JSForConst _ _lb _v x1s _s1 x2s _s2 x3s _rb x4) = "JSForConst " <> ss x1s <> " " <> ss x2s <> " " <> ss x3s <> " (" <> ss x4 <> ")"
  ss (JSForConstIn _ _lb _v x1 _i x2 _rb x3) = "JSForConstIn (" <> ss x1 <> ") (" <> ss x2 <> ") (" <> ss x3 <> ")"
  ss (JSForConstOf _ _lb _v x1 _i x2 _rb x3) = "JSForConstOf (" <> ss x1 <> ") (" <> ss x2 <> ") (" <> ss x3 <> ")"
  ss (JSForOf _ _lb x1s _i x2 _rb x3) = "JSForOf " <> ss x1s <> " (" <> ss x2 <> ") (" <> ss x3 <> ")"
  ss (JSForVarOf _ _lb _v x1 _i x2 _rb x3) = "JSForVarOf (" <> ss x1 <> ") (" <> ss x2 <> ") (" <> ss x3 <> ")"
  ss (JSFunction _ n _lb pl _rb x3 _) = "JSFunction " <> ssid n <> " " <> ss pl <> " (" <> ss x3 <> ")"
  ss (JSAsyncFunction _ _ n _lb pl _rb x3 _) = "JSAsyncFunction " <> ssid n <> " " <> ss pl <> " (" <> ss x3 <> ")"
  ss (JSGenerator _ _ n _lb pl _rb x3 _) = "JSGenerator " <> ssid n <> " " <> ss pl <> " (" <> ss x3 <> ")"
  ss (JSIf _ _lb x1 _rb x2) = "JSIf (" <> ss x1 <> ") (" <> ss x2 <> ")"
  ss (JSIfElse _ _lb x1 _rb x2 _e x3) = "JSIfElse (" <> ss x1 <> ") (" <> ss x2 <> ") (" <> ss x3 <> ")"
  ss (JSLabelled x1 _c x2) = "JSLabelled (" <> ss x1 <> ") (" <> ss x2 <> ")"
  ss (JSLet _ xs _as) = "JSLet " <> ss xs
  ss (JSEmptyStatement _) = "JSEmptyStatement"
  ss (JSExpressionStatement l s) = ss l <> (let x = ss s in if not (null x) then "," <> x else "")
  ss (JSAssignStatement lhs op rhs s) = "JSOpAssign (" <> ss op <> "," <> ss lhs <> "," <> ss rhs <> (let x = ss s in if not (null x) then ")," <> x else ")")
  ss (JSMethodCall e _ a _ s) = "JSMethodCall (" <> ss e <> ",JSArguments " <> ss a <> (let x = ss s in if not (null x) then ")," <> x else ")")
  ss (JSReturn _ (Just me) s) = "JSReturn " <> ss me <> " " <> ss s
  ss (JSReturn _ Nothing s) = "JSReturn " <> ss s
  ss (JSSwitch _ _lp x _rp _lb x2 _rb _) = "JSSwitch (" <> ss x <> ") " <> ss x2
  ss (JSThrow _ x _) = "JSThrow (" <> ss x <> ")"
  ss (JSTry _ xt1 xtc xtf) = "JSTry (" <> ss xt1 <> "," <> ss xtc <> "," <> ss xtf <> ")"
  ss (JSVariable _ xs _as) = "JSVariable " <> ss xs
  ss (JSWhile _ _lb x1 _rb x2) = "JSWhile (" <> ss x1 <> ") (" <> ss x2 <> ")"
  ss (JSWith _ _lb x1 _rb x _) = "JSWith (" <> ss x1 <> ") (" <> ss x <> ")"
  ss (JSDebugger _ _) = "JSDebugger"
  ss (JSAsyncGenerator _ _ _ n _lb pl _rb x3 _) = "JSAsyncGenerator " <> ssid n <> " " <> ss pl <> " (" <> ss x3 <> ")"
  ss (JSForAwaitOf _ _ _lb x1 _i x2 _rb x3) = "JSForAwaitOf " <> ss x1 <> " (" <> ss x2 <> ") (" <> ss x3 <> ")"
  ss (JSForAwaitVarOf _ _ _lb _v x1 _i x2 _rb x3) = "JSForAwaitVarOf (" <> ss x1 <> ") (" <> ss x2 <> ") (" <> ss x3 <> ")"
  ss (JSForAwaitLetOf _ _ _lb _v x1 _i x2 _rb x3) = "JSForAwaitLetOf (" <> ss x1 <> ") (" <> ss x2 <> ") (" <> ss x3 <> ")"
  ss (JSForAwaitConstOf _ _ _lb _v x1 _i x2 _rb x3) = "JSForAwaitConstOf (" <> ss x1 <> ") (" <> ss x2 <> ") (" <> ss x3 <> ")"

instance ShowStripped JSExpression where
  ss (JSArrayLiteral _lb xs _rb) = "JSArrayLiteral " <> ss xs
  ss (JSAssignExpression lhs op rhs) = "JSOpAssign (" <> ss op <> "," <> ss lhs <> "," <> ss rhs <> ")"
  ss (JSAwaitExpression _ e) = "JSAwaitExpresson " <> ss e
  ss (JSCallExpression ex _ xs _) = "JSCallExpression (" <> ss ex <> ",JSArguments " <> ss xs <> ")"
  ss (JSCallExpressionDot ex _os xs) = "JSCallExpressionDot (" <> ss ex <> "," <> ss xs <> ")"
  ss (JSCallExpressionSquare ex _os xs _cs) = "JSCallExpressionSquare (" <> ss ex <> "," <> ss xs <> ")"
  ss (JSClassExpression _ n h _lb xs _rb) = "JSClassExpression " <> ssid n <> " (" <> ss h <> ") " <> ss xs
  ss (JSDecimal _ d) = "JSDecimal " <> singleQuote (showJSDouble d)
  ss (JSCommaExpression l _ r) = "JSExpression [" <> ss l <> "," <> ss r <> "]"
  ss (JSExpressionBinary x2 op x3) = "JSExpressionBinary (" <> ss op <> "," <> ss x2 <> "," <> ss x3 <> ")"
  ss (JSExpressionParen _lp x _rp) = "JSExpressionParen (" <> ss x <> ")"
  ss (JSExpressionPostfix xs op) = "JSExpressionPostfix (" <> ss op <> "," <> ss xs <> ")"
  ss (JSExpressionTernary x1 _q x2 _c x3) = "JSExpressionTernary (" <> ss x1 <> "," <> ss x2 <> "," <> ss x3 <> ")"
  ss (JSArrowExpression ps _ body) = "JSArrowExpression (" <> ss ps <> ") => " <> ss body
  ss (JSFunctionExpression _ n _lb pl _rb x3) = "JSFunctionExpression " <> ssid n <> " " <> ss pl <> " (" <> ss x3 <> ")"
  ss (JSGeneratorExpression _ _ n _lb pl _rb x3) = "JSGeneratorExpression " <> ssid n <> " " <> ss pl <> " (" <> ss x3 <> ")"
  ss (JSAsyncFunctionExpression _ _ n _lb pl _rb x3) = "JSAsyncFunctionExpression " <> ssid n <> " " <> ss pl <> " (" <> ss x3 <> ")"
  ss (JSAsyncArrowExpression _ ps _ body) = "JSAsyncArrowExpression (" <> ss ps <> ") => " <> ss body
  ss (JSAsyncGeneratorExpression _ _ _ n _lb pl _rb x3) = "JSAsyncGeneratorExpression " <> ssid n <> " " <> ss pl <> " (" <> ss x3 <> ")"
  ss (JSHexInteger _ n) = "JSHexInteger " <> singleQuote (showJSHex n)
  ss (JSBinaryInteger _ n) = "JSBinaryInteger " <> singleQuote (showJSBinary n)
  ss (JSOctal _ n) = "JSOctal " <> singleQuote (showJSOctal n)
  ss (JSBigIntLiteral _ n) = "JSBigIntLiteral " <> singleQuote (show n <> "n")
  ss (JSIdentifier _ s) = "JSIdentifier " <> singleQuote (bsToStr s)
  ss (JSLiteral _ s) | BS8.null s = "JSLiteral ''"
  ss (JSLiteral _ s) = "JSLiteral " <> singleQuote (bsToStr s)
  ss (JSMemberDot x1s _d x2) = "JSMemberDot (" <> ss x1s <> "," <> ss x2 <> ")"
  ss (JSMemberPrivateDot x1s _d _h name) = "JSMemberPrivateDot (" <> ss x1s <> ",#" <> bsToStr name <> ")"
  ss (JSMemberExpression e _ a _) = "JSMemberExpression (" <> ss e <> ",JSArguments " <> ss a <> ")"
  ss (JSMemberNew _a n _ s _) = "JSMemberNew (" <> ss n <> ",JSArguments " <> ss s <> ")"
  ss (JSMemberSquare x1s _lb x2 _rb) = "JSMemberSquare (" <> ss x1s <> "," <> ss x2 <> ")"
  ss (JSNewExpression _n e) = "JSNewExpression " <> ss e
  ss (JSOptionalMemberDot x1s _d x2) = "JSOptionalMemberDot (" <> ss x1s <> "," <> ss x2 <> ")"
  ss (JSOptionalMemberSquare x1s _lb x2 _rb) = "JSOptionalMemberSquare (" <> ss x1s <> "," <> ss x2 <> ")"
  ss (JSOptionalCallExpression ex _ xs _) = "JSOptionalCallExpression (" <> ss ex <> ",JSArguments " <> ss xs <> ")"
  ss (JSObjectLiteral _lb xs _rb) = "JSObjectLiteral " <> ss xs
  ss (JSRegEx _ s) = "JSRegEx " <> singleQuote (bsToStr s)
  ss (JSStringLiteral _ s) = "JSStringLiteral " <> bsToStr s
  ss (JSUnaryExpression op x) = "JSUnaryExpression (" <> ss op <> "," <> ss x <> ")"
  ss (JSVarInitExpression x1 x2) = "JSVarInitExpression (" <> ss x1 <> ") " <> ss x2
  ss (JSYieldExpression _ Nothing) = "JSYieldExpression ()"
  ss (JSYieldExpression _ (Just x)) = "JSYieldExpression (" <> ss x <> ")"
  ss (JSYieldFromExpression _ _ x) = "JSYieldFromExpression (" <> ss x <> ")"
  ss (JSImportMeta _ _) = "JSImportMeta"
  ss (JSImportCall _ _ expr _) = "JSImportCall (" <> ss expr <> ")"
  ss (JSSpreadExpression _ x1) = "JSSpreadExpression (" <> ss x1 <> ")"
  ss (JSTemplateLiteral Nothing _ s ps) = "JSTemplateLiteral (()," <> singleQuote (bsToStr s) <> "," <> ss ps <> ")"
  ss (JSTemplateLiteral (Just t) _ s ps) = "JSTemplateLiteral ((" <> ss t <> ")," <> singleQuote (bsToStr s) <> "," <> ss ps <> ")"

instance ShowStripped JSArrowParameterList where
  ss (JSUnparenthesizedArrowParameter x) = ss x
  ss (JSParenthesizedArrowParameterList _ xs _) = ss xs

instance ShowStripped JSConciseBody where
  ss (JSConciseFunctionBody block) = "JSConciseFunctionBody (" <> ss block <> ")"
  ss (JSConciseExpressionBody expr) = "JSConciseExpressionBody (" <> ss expr <> ")"

instance ShowStripped JSModuleItem where
  ss (JSModuleExportDeclaration _ x1) = "JSModuleExportDeclaration (" <> ss x1 <> ")"
  ss (JSModuleImportDeclaration _ x1) = "JSModuleImportDeclaration (" <> ss x1 <> ")"
  ss (JSModuleStatementListItem x1) = "JSModuleStatementListItem (" <> ss x1 <> ")"

instance ShowStripped JSImportDeclaration where
  ss (JSImportDeclaration imp from attrs _) = "JSImportDeclaration (" <> ss imp <> "," <> ss from <> maybe "" (\a -> "," <> ss a) attrs <> ")"
  ss (JSImportDeclarationBare _ m attrs _) = "JSImportDeclarationBare (" <> singleQuote (bsToStr m) <> maybe "" (\a -> "," <> ss a) attrs <> ")"

instance ShowStripped JSImportClause where
  ss (JSImportClauseDefault x) = "JSImportClauseDefault (" <> ss x <> ")"
  ss (JSImportClauseNameSpace x) = "JSImportClauseNameSpace (" <> ss x <> ")"
  ss (JSImportClauseNamed x) = "JSImportClauseNameSpace (" <> ss x <> ")"
  ss (JSImportClauseDefaultNameSpace x1 _ x2) = "JSImportClauseDefaultNameSpace (" <> ss x1 <> "," <> ss x2 <> ")"
  ss (JSImportClauseDefaultNamed x1 _ x2) = "JSImportClauseDefaultNamed (" <> ss x1 <> "," <> ss x2 <> ")"

instance ShowStripped JSFromClause where
  ss (JSFromClause _ _ m) = "JSFromClause " <> singleQuote (bsToStr m)

instance ShowStripped JSImportNameSpace where
  ss (JSImportNameSpace _ _ x) = "JSImportNameSpace (" <> ss x <> ")"

instance ShowStripped JSImportsNamed where
  ss (JSImportsNamed _ xs _) = "JSImportsNamed (" <> ss xs <> ")"

instance ShowStripped JSImportSpecifier where
  ss (JSImportSpecifier x1) = "JSImportSpecifier (" <> ss x1 <> ")"
  ss (JSImportSpecifierAs x1 _ x2) = "JSImportSpecifierAs (" <> ss x1 <> "," <> ss x2 <> ")"

instance ShowStripped JSImportAttributes where
  ss (JSImportAttributes _ attrs _) = "JSImportAttributes (" <> ss attrs <> ")"

instance ShowStripped JSImportAttribute where
  ss (JSImportAttribute key _ value) = "JSImportAttribute (" <> ss key <> "," <> ss value <> ")"

instance ShowStripped JSExportDeclaration where
  ss (JSExportAllFrom star from _) = "JSExportAllFrom (" <> ss star <> "," <> ss from <> ")"
  ss (JSExportAllAsFrom star _ ident from _) = "JSExportAllAsFrom (" <> ss star <> "," <> ss ident <> "," <> ss from <> ")"
  ss (JSExportFrom xs from _) = "JSExportFrom (" <> ss xs <> "," <> ss from <> ")"
  ss (JSExportLocals xs _) = "JSExportLocals (" <> ss xs <> ")"
  ss (JSExportDefault _ stmt _) = "JSExportDefault (" <> ss stmt <> ")"
  ss (JSExport x1 _) = "JSExport (" <> ss x1 <> ")"

instance ShowStripped JSExportClause where
  ss (JSExportClause _ xs _) = "JSExportClause (" <> ss xs <> ")"

instance ShowStripped JSExportSpecifier where
  ss (JSExportSpecifier x1) = "JSExportSpecifier (" <> ss x1 <> ")"
  ss (JSExportSpecifierAs x1 _ x2) = "JSExportSpecifierAs (" <> ss x1 <> "," <> ss x2 <> ")"

instance ShowStripped JSTryCatch where
  ss (JSCatch _ _lb x1 _rb x3) = "JSCatch (" <> ss x1 <> "," <> ss x3 <> ")"
  ss (JSCatchIf _ _lb x1 _ ex _rb x3) = "JSCatch (" <> ss x1 <> ") if " <> ss ex <> " (" <> ss x3 <> ")"
  ss (JSCatchNoParam _ x1) = "JSCatchNoParam (" <> ss x1 <> ")"

instance ShowStripped JSTryFinally where
  ss (JSFinally _ x) = "JSFinally (" <> ss x <> ")"
  ss JSNoFinally = "JSFinally ()"

instance ShowStripped JSIdent where
  ss (JSIdentName _ s) = "JSIdentifier " <> singleQuote (bsToStr s)
  ss JSIdentNone = "JSIdentNone"

instance ShowStripped JSObjectProperty where
  ss (JSPropertyNameandValue x1 _colon x2s) = "JSPropertyNameandValue (" <> ss x1 <> ") " <> ss x2s
  ss (JSPropertyIdentRef _ s) = "JSPropertyIdentRef " <> singleQuote (bsToStr s)
  ss (JSObjectMethod m) = ss m
  ss (JSObjectSpread _ expr) = "JSObjectSpread (" <> ss expr <> ")"

instance ShowStripped JSMethodDefinition where
  ss (JSMethodDefinition x1 _lb1 x2s _rb1 x3) = "JSMethodDefinition (" <> ss x1 <> ") " <> ss x2s <> " (" <> ss x3 <> ")"
  ss (JSPropertyAccessor s x1 _lb1 x2s _rb1 x3) = "JSPropertyAccessor " <> ss s <> " (" <> ss x1 <> ") " <> ss x2s <> " (" <> ss x3 <> ")"
  ss (JSGeneratorMethodDefinition _ x1 _lb1 x2s _rb1 x3) = "JSGeneratorMethodDefinition (" <> ss x1 <> ") " <> ss x2s <> " (" <> ss x3 <> ")"
  ss (JSAsyncMethodDefinition _ x1 _lb1 x2s _rb1 x3) = "JSAsyncMethodDefinition (" <> ss x1 <> ") " <> ss x2s <> " (" <> ss x3 <> ")"

instance ShowStripped JSPropertyName where
  ss (JSPropertyIdent _ s) = "JSIdentifier " <> singleQuote (bsToStr s)
  ss (JSPropertyString _ s) = "JSIdentifier " <> singleQuote (bsToStr s)
  ss (JSPropertyNumber _ s) = "JSIdentifier " <> singleQuote (bsToStr s)
  ss (JSPropertyComputed _ x _) = "JSPropertyComputed (" <> ss x <> ")"

instance ShowStripped JSAccessor where
  ss (JSAccessorGet _) = "JSAccessorGet"
  ss (JSAccessorSet _) = "JSAccessorSet"

instance ShowStripped JSBlock where
  ss (JSBlock _ xs _) = "JSBlock " <> ss xs

instance ShowStripped JSSwitchParts where
  ss (JSCase _ x1 _c x2s) = "JSCase (" <> ss x1 <> ") (" <> ss x2s <> ")"
  ss (JSDefault _ _c xs) = "JSDefault (" <> ss xs <> ")"

instance ShowStripped JSBinOp where
  ss (JSBinOpAnd _) = "'&&'"
  ss (JSBinOpBitAnd _) = "'&'"
  ss (JSBinOpBitOr _) = "'|'"
  ss (JSBinOpBitXor _) = "'^'"
  ss (JSBinOpDivide _) = "'/'"
  ss (JSBinOpEq _) = "'=='"
  ss (JSBinOpExponentiation _) = "'**'"
  ss (JSBinOpGe _) = "'>='"
  ss (JSBinOpGt _) = "'>'"
  ss (JSBinOpIn _) = "'in'"
  ss (JSBinOpInstanceOf _) = "'instanceof'"
  ss (JSBinOpLe _) = "'<='"
  ss (JSBinOpLsh _) = "'<<'"
  ss (JSBinOpLt _) = "'<'"
  ss (JSBinOpMinus _) = "'-'"
  ss (JSBinOpMod _) = "'%'"
  ss (JSBinOpNeq _) = "'!='"
  ss (JSBinOpOf _) = "'of'"
  ss (JSBinOpOr _) = "'||'"
  ss (JSBinOpNullishCoalescing _) = "'??'"
  ss (JSBinOpPlus _) = "'+'"
  ss (JSBinOpRsh _) = "'>>'"
  ss (JSBinOpStrictEq _) = "'==='"
  ss (JSBinOpStrictNeq _) = "'!=='"
  ss (JSBinOpTimes _) = "'*'"
  ss (JSBinOpUrsh _) = "'>>>'"

instance ShowStripped JSUnaryOp where
  ss (JSUnaryOpDecr _) = "'--'"
  ss (JSUnaryOpDelete _) = "'delete'"
  ss (JSUnaryOpIncr _) = "'++'"
  ss (JSUnaryOpMinus _) = "'-'"
  ss (JSUnaryOpNot _) = "'!'"
  ss (JSUnaryOpPlus _) = "'+'"
  ss (JSUnaryOpTilde _) = "'~'"
  ss (JSUnaryOpTypeof _) = "'typeof'"
  ss (JSUnaryOpVoid _) = "'void'"

instance ShowStripped JSAssignOp where
  ss (JSAssign _) = "'='"
  ss (JSTimesAssign _) = "'*='"
  ss (JSDivideAssign _) = "'/='"
  ss (JSModAssign _) = "'%='"
  ss (JSPlusAssign _) = "'+='"
  ss (JSMinusAssign _) = "'-='"
  ss (JSLshAssign _) = "'<<='"
  ss (JSRshAssign _) = "'>>='"
  ss (JSUrshAssign _) = "'>>>='"
  ss (JSBwAndAssign _) = "'&='"
  ss (JSBwXorAssign _) = "'^='"
  ss (JSBwOrAssign _) = "'|='"
  ss (JSLogicalAndAssign _) = "'&&='"
  ss (JSLogicalOrAssign _) = "'||='"
  ss (JSNullishAssign _) = "'??='"
  ss (JSExponentiationAssign _) = "'**='"

instance ShowStripped JSVarInitializer where
  ss (JSVarInit _ n) = "[" <> ss n <> "]"
  ss JSVarInitNone = ""

instance ShowStripped JSSemi where
  ss (JSSemi _) = "JSSemicolon"
  ss JSSemiAuto = ""

instance ShowStripped JSArrayElement where
  ss (JSArrayElement e) = ss e
  ss (JSArrayComma _) = "JSComma"

instance ShowStripped JSTemplatePart where
  ss (JSTemplatePart e _ s) = "(" <> ss e <> "," <> singleQuote (bsToStr s) <> ")"

instance ShowStripped JSClassHeritage where
  ss JSExtendsNone = ""
  ss (JSExtends _ x) = ss x

instance ShowStripped JSClassElement where
  ss (JSClassInstanceMethod m) = ss m
  ss (JSClassStaticMethod _ m) = "JSClassStaticMethod (" <> ss m <> ")"
  ss (JSClassSemi _) = "JSClassSemi"
  ss (JSPrivateField _ name _ Nothing _) = "JSPrivateField " <> singleQuote ("#" <> bsToStr name)
  ss (JSPrivateField _ name _ (Just initializer) _) = "JSPrivateField " <> singleQuote ("#" <> bsToStr name) <> " (" <> ss initializer <> ")"
  ss (JSPrivateMethod _ name _ params _ block) = "JSPrivateMethod " <> singleQuote ("#" <> bsToStr name) <> " " <> ss params <> " (" <> ss block <> ")"
  ss (JSPrivateAccessor accessor _ name _ params _ block) = "JSPrivateAccessor " <> ss accessor <> " " <> singleQuote ("#" <> bsToStr name) <> " " <> ss params <> " (" <> ss block <> ")"
  ss (JSClassField name _ Nothing _) = "JSClassField (" <> ss name <> ")"
  ss (JSClassField name _ (Just initializer) _) = "JSClassField (" <> ss name <> ") (" <> ss initializer <> ")"
  ss (JSClassStaticField _ name _ Nothing _) = "JSClassStaticField (" <> ss name <> ")"
  ss (JSClassStaticField _ name _ (Just initializer) _) = "JSClassStaticField (" <> ss name <> ") (" <> ss initializer <> ")"
  ss (JSClassStaticBlock _ block) = "JSClassStaticBlock (" <> ss block <> ")"
  ss (JSAsyncGeneratorMethodDefinition _ _ x1 _lb x2s _rb x3) = "JSAsyncGeneratorMethodDefinition (" <> ss x1 <> ") " <> ss x2s <> " (" <> ss x3 <> ")"

instance ShowStripped a => ShowStripped (JSCommaList a) where
  ss xs = "(" <> commaJoin (map ss $ fromCommaList xs) <> ")"

instance ShowStripped a => ShowStripped (JSCommaTrailingList a) where
  ss (JSCTLComma xs _) = "[" <> commaJoin (map ss $ fromCommaList xs) <> ",JSComma]"
  ss (JSCTLNone xs) = "[" <> commaJoin (map ss $ fromCommaList xs) <> "]"

instance ShowStripped a => ShowStripped [a] where
  ss xs = "[" <> commaJoin (map ss xs) <> "]"

-- -----------------------------------------------------------------------------
-- Helpers.

-- | Decode a UTF-8 ByteString to String for display purposes.
-- Used in ShowStripped instances where AST ByteString fields
-- need to be rendered as human-readable text.
bsToStr :: ByteString -> String
bsToStr = Text.unpack . Text.decodeUtf8

-- | Render a Double as a compact JavaScript numeric string.
-- Whole numbers render without decimal point (e.g., @42.0@ becomes @"42"@).
-- Fractional values use Haskell's 'show' (e.g., @3.14@ stays @"3.14"@).
showJSDouble :: Double -> String
showJSDouble d
  | d == fromIntegral n && abs d < 1e15 = show n
  | otherwise = show d
  where
    n = round d :: Integer

-- | Render an Integer as a hexadecimal string with @0x@ prefix.
showJSHex :: Integer -> String
showJSHex n = "0x" <> showHex n ""

-- | Render an Integer as a binary string with @0b@ prefix.
showJSBinary :: Integer -> String
showJSBinary n = "0b" <> showIntAtBase 2 intToDigit n ""

-- | Render an Integer as an octal string with @0o@ prefix.
showJSOctal :: Integer -> String
showJSOctal n = "0o" <> showOct' n ""
  where
    showOct' = showIntAtBase 8 intToDigit

-- | Join ByteStrings with commas, filtering out empty ByteStrings.
--
-- Utility function for generating comma-separated lists in pretty printing,
-- automatically removing empty ByteStrings to avoid extra commas.
--
-- ==== Examples
--
-- >>> commaJoin ["foo", "", "bar"]
-- "foo,bar"
--
-- >>> commaJoin ["single"]
-- "single"
--
-- @since 0.7.1.0
commaJoin :: [String] -> String
commaJoin s = List.intercalate "," $ List.filter (not . null) s

-- | Convert comma-separated list AST to regular Haskell list.
--
-- Extracts the elements from a 'JSCommaList' structure, which represents
-- comma-separated sequences in JavaScript syntax (function parameters,
-- array elements, etc.).
--
-- ==== Examples
--
-- >>> fromCommaList (JSLOne element)
-- [element]
--
-- >>> fromCommaList (JSLCons (JSLOne a) comma b)
-- [a, b]
--
-- @since 0.7.1.0
fromCommaList :: JSCommaList a -> [a]
fromCommaList (JSLCons l _ i) = fromCommaList l <> [i]
fromCommaList (JSLOne i) = [i]
fromCommaList JSLNil = []

-- | Wrap ByteString in single quotes.
--
-- Utility function for pretty printing JavaScript string literals
-- and identifiers that need to be quoted.
--
-- @since 0.7.1.0
singleQuote :: String -> String
singleQuote s = "'" <> s <> "'"

-- | Extract String from JavaScript identifier with quotes.
--
-- Converts 'JSIdent' to its quoted String representation for pretty printing.
-- Returns empty quotes for 'JSIdentNone'.
--
-- @since 0.7.1.0
ssid :: JSIdent -> String
ssid (JSIdentName _ s) = singleQuote (bsToStr s)
ssid JSIdentNone = "''"

-- | Add comma prefix to non-empty Strings.
--
-- Utility for conditional comma insertion in pretty printing.
-- Returns empty String for empty input, comma-prefixed String otherwise.
--
-- @since 0.7.1.0
commaIf :: String -> String
commaIf s
  | null s = ""
  | otherwise = "," <> s

-- | Remove annotation from binary operator.
--
-- Strips position information from 'JSBinOp' by replacing all annotations
-- with 'JSNoAnnot'. Used in testing and comparison operations where
-- position information should be ignored.
--
-- @since 0.7.1.0
deAnnot :: JSBinOp -> JSBinOp
deAnnot (JSBinOpAnd _) = JSBinOpAnd JSNoAnnot
deAnnot (JSBinOpBitAnd _) = JSBinOpBitAnd JSNoAnnot
deAnnot (JSBinOpBitOr _) = JSBinOpBitOr JSNoAnnot
deAnnot (JSBinOpBitXor _) = JSBinOpBitXor JSNoAnnot
deAnnot (JSBinOpDivide _) = JSBinOpDivide JSNoAnnot
deAnnot (JSBinOpEq _) = JSBinOpEq JSNoAnnot
deAnnot (JSBinOpExponentiation _) = JSBinOpExponentiation JSNoAnnot
deAnnot (JSBinOpGe _) = JSBinOpGe JSNoAnnot
deAnnot (JSBinOpGt _) = JSBinOpGt JSNoAnnot
deAnnot (JSBinOpIn _) = JSBinOpIn JSNoAnnot
deAnnot (JSBinOpInstanceOf _) = JSBinOpInstanceOf JSNoAnnot
deAnnot (JSBinOpLe _) = JSBinOpLe JSNoAnnot
deAnnot (JSBinOpLsh _) = JSBinOpLsh JSNoAnnot
deAnnot (JSBinOpLt _) = JSBinOpLt JSNoAnnot
deAnnot (JSBinOpMinus _) = JSBinOpMinus JSNoAnnot
deAnnot (JSBinOpMod _) = JSBinOpMod JSNoAnnot
deAnnot (JSBinOpNeq _) = JSBinOpNeq JSNoAnnot
deAnnot (JSBinOpOf _) = JSBinOpOf JSNoAnnot
deAnnot (JSBinOpOr _) = JSBinOpOr JSNoAnnot
deAnnot (JSBinOpNullishCoalescing _) = JSBinOpNullishCoalescing JSNoAnnot
deAnnot (JSBinOpPlus _) = JSBinOpPlus JSNoAnnot
deAnnot (JSBinOpRsh _) = JSBinOpRsh JSNoAnnot
deAnnot (JSBinOpStrictEq _) = JSBinOpStrictEq JSNoAnnot
deAnnot (JSBinOpStrictNeq _) = JSBinOpStrictNeq JSNoAnnot
deAnnot (JSBinOpTimes _) = JSBinOpTimes JSNoAnnot
deAnnot (JSBinOpUrsh _) = JSBinOpUrsh JSNoAnnot

-- | Compare binary operators ignoring annotations.
--
-- Tests equality of two 'JSBinOp' values while ignoring position information.
-- Useful for testing and AST comparison where location data is irrelevant.
--
-- ==== Examples
--
-- >>> binOpEq (JSBinOpPlus pos1) (JSBinOpPlus pos2)
-- True  -- Same operator, different positions
--
-- >>> binOpEq (JSBinOpPlus pos1) (JSBinOpMinus pos2)
-- False -- Different operators
--
-- @since 0.7.1.0
binOpEq :: JSBinOp -> JSBinOp -> Bool
binOpEq a b = deAnnot a == deAnnot b
