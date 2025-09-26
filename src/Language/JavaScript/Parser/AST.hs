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
    binOpEq,
    showStripped,

    -- * JSDoc Integration
    extractJSDoc,
    extractJSDocFromStatement,
    extractJSDocFromExpression,
    hasJSDoc,
    getJSDocParams,
    getJSDocReturnType,
    validateJSDocParameters,
    hasJSDocTag,
    getJSDocTagsByName,
  )
where

import Control.DeepSeq (NFData)
import Data.Data
import qualified Data.List as List
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Language.JavaScript.Parser.SrcLocation (TokenPosn (..))
import Language.JavaScript.Parser.Token

-- ---------------------------------------------------------------------

data JSAnnot
  = -- | Annotation: position and comment/whitespace information
    JSAnnot !TokenPosn ![CommentAnnotation]
  | -- | A single space character
    JSAnnotSpace
  | -- | No annotation
    JSNoAnnot
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSAST
  = -- | source elements, trailing whitespace
    JSAstProgram ![JSStatement] !JSAnnot
  | JSAstModule ![JSModuleItem] !JSAnnot
  | JSAstStatement !JSStatement !JSAnnot
  | JSAstExpression !JSExpression !JSAnnot
  | JSAstLiteral !JSExpression !JSAnnot
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
    JSImportDeclarationBare !JSAnnot !String !(Maybe JSImportAttributes) !JSSemi
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
    JSFromClause !JSAnnot !JSAnnot !String
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
  | -- | body, autosemi
    JSExport !JSStatement !JSSemi
  | -- | default, expression/declaration, semi
    JSExportDefault !JSAnnot !JSStatement !JSSemi
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
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSExpression
  = -- | Terminals
    JSIdentifier !JSAnnot !String
  | JSDecimal !JSAnnot !String
  | JSLiteral !JSAnnot !String
  | JSHexInteger !JSAnnot !String
  | JSBinaryInteger !JSAnnot !String
  | JSOctal !JSAnnot !String
  | JSBigIntLiteral !JSAnnot !String
  | JSStringLiteral !JSAnnot !String
  | JSRegEx !JSAnnot !String
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
  | -- | firstpart, dot, name
    JSMemberDot !JSExpression !JSAnnot !JSExpression
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
    JSTemplateLiteral !(Maybe JSExpression) !JSAnnot !String ![JSTemplatePart]
  | JSUnaryExpression !JSUnaryOp !JSExpression
  | -- | identifier, initializer
    JSVarInitExpression !JSExpression !JSVarInitializer
  | -- | parameter with optional default value
    JSParameterExpression !JSExpression !JSVarInitializer
  | -- | yield, optional expr
    JSYieldExpression !JSAnnot !(Maybe JSExpression)
  | -- | yield, *, expr
    JSYieldFromExpression !JSAnnot !JSAnnot !JSExpression
  | -- | import, .meta
    JSImportMeta !JSAnnot !JSAnnot
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
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

data JSTryCatch
  = -- | catch,lb,ident,rb,block
    JSCatch !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSBlock
  | -- | catch,lb,ident,if,expr,rb,block
    JSCatchIf !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSExpression !JSAnnot !JSBlock
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
  | JSPropertyIdentRef !JSAnnot !String
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
  = JSPropertyIdent !JSAnnot !String
  | JSPropertyString !JSAnnot !String
  | JSPropertyNumber !JSAnnot !String
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
  = JSIdentName !JSAnnot !String
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
    JSTemplatePart !JSExpression !JSAnnot !String
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
    JSPrivateField !JSAnnot !String !JSAnnot !(Maybe JSExpression) !JSSemi
  | -- | #, name, lb, params, rb, block
    JSPrivateMethod !JSAnnot !String !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock
  | -- | get/set, #, name, lb, params, rb, block
    JSPrivateAccessor !JSAccessor !JSAnnot !String !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

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
showStripped (JSAstStatement s _) = "JSAstStatement (" <> (ss s <> ")")
showStripped (JSAstExpression e _) = "JSAstExpression (" <> (ss e <> ")")
showStripped (JSAstLiteral e _) = "JSAstLiteral (" <> (ss e <> ")")

class ShowStripped a where
  ss :: a -> String

instance ShowStripped JSStatement where
  ss (JSStatementBlock _ xs _ _) = "JSStatementBlock " <> ss xs
  ss (JSBreak _ JSIdentNone s) = "JSBreak" <> commaIf (ss s)
  ss (JSBreak _ (JSIdentName _ n) s) = "JSBreak " <> (singleQuote n <> commaIf (ss s))
  ss (JSClass _ n h _lb xs _rb _) = "JSClass " <> (ssid n <> (" (" <> (ss h <> (") " <> ss xs))))
  ss (JSContinue _ JSIdentNone s) = "JSContinue" <> commaIf (ss s)
  ss (JSContinue _ (JSIdentName _ n) s) = "JSContinue " <> (singleQuote n <> commaIf (ss s))
  ss (JSConstant _ xs _as) = "JSConstant " <> ss xs
  ss (JSDoWhile _d x1 _w _lb x2 _rb x3) = "JSDoWhile (" <> (ss x1 <> (") (" <> (ss x2 <> (") (" <> (ss x3 <> ")")))))
  ss (JSFor _ _lb x1s _s1 x2s _s2 x3s _rb x4) = "JSFor " <> (ss x1s <> (" " <> (ss x2s <> (" " <> (ss x3s <> (" (" ++ ss x4 ++ ")"))))))
  ss (JSForIn _ _lb x1s _i x2 _rb x3) = "JSForIn " <> (ss x1s <> (" (" <> (ss x2 <> (") (" <> (ss x3 <> ")")))))
  ss (JSForVar _ _lb _v x1s _s1 x2s _s2 x3s _rb x4) = "JSForVar " <> (ss x1s <> (" " <> (ss x2s <> (" " <> (ss x3s <> (" (" ++ ss x4 ++ ")"))))))
  ss (JSForVarIn _ _lb _v x1 _i x2 _rb x3) = "JSForVarIn (" <> (ss x1 <> (") (" <> (ss x2 <> (") (" <> (ss x3 <> ")")))))
  ss (JSForLet _ _lb _v x1s _s1 x2s _s2 x3s _rb x4) = "JSForLet " <> (ss x1s <> (" " <> (ss x2s <> (" " <> (ss x3s <> (" (" ++ ss x4 ++ ")"))))))
  ss (JSForLetIn _ _lb _v x1 _i x2 _rb x3) = "JSForLetIn (" <> (ss x1 <> (") (" <> (ss x2 <> (") (" <> (ss x3 <> ")")))))
  ss (JSForLetOf _ _lb _v x1 _i x2 _rb x3) = "JSForLetOf (" <> (ss x1 <> (") (" <> (ss x2 <> (") (" <> (ss x3 <> ")")))))
  ss (JSForConst _ _lb _v x1s _s1 x2s _s2 x3s _rb x4) = "JSForConst " <> (ss x1s <> (" " <> (ss x2s <> (" " <> (ss x3s <> (" (" ++ ss x4 ++ ")"))))))
  ss (JSForConstIn _ _lb _v x1 _i x2 _rb x3) = "JSForConstIn (" <> (ss x1 <> (") (" <> (ss x2 <> (") (" <> (ss x3 <> ")")))))
  ss (JSForConstOf _ _lb _v x1 _i x2 _rb x3) = "JSForConstOf (" <> (ss x1 <> (") (" <> (ss x2 <> (") (" <> (ss x3 <> ")")))))
  ss (JSForOf _ _lb x1s _i x2 _rb x3) = "JSForOf " <> (ss x1s <> (" (" <> (ss x2 <> (") (" <> (ss x3 <> ")")))))
  ss (JSForVarOf _ _lb _v x1 _i x2 _rb x3) = "JSForVarOf (" <> (ss x1 <> (") (" <> (ss x2 <> (") (" <> (ss x3 <> ")")))))
  ss (JSFunction _ n _lb pl _rb x3 _) = "JSFunction " <> (ssid n <> (" " <> (ss pl <> (" (" <> (ss x3 <> ")")))))
  ss (JSAsyncFunction _ _ n _lb pl _rb x3 _) = "JSAsyncFunction " <> (ssid n <> (" " <> (ss pl <> (" (" <> (ss x3 <> ")")))))
  ss (JSGenerator _ _ n _lb pl _rb x3 _) = "JSGenerator " <> (ssid n <> (" " <> (ss pl <> (" (" <> (ss x3 <> ")")))))
  ss (JSIf _ _lb x1 _rb x2) = "JSIf (" <> (ss x1 <> (") (" <> (ss x2 <> ")")))
  ss (JSIfElse _ _lb x1 _rb x2 _e x3) = "JSIfElse (" <> (ss x1 <> (") (" <> (ss x2 <> (") (" <> (ss x3 <> ")")))))
  ss (JSLabelled x1 _c x2) = "JSLabelled (" <> (ss x1 <> (") (" <> (ss x2 <> ")")))
  ss (JSLet _ xs _as) = "JSLet " <> ss xs
  ss (JSEmptyStatement _) = "JSEmptyStatement"
  ss (JSExpressionStatement l s) = ss l <> (let x = ss s in if not (null x) then "," <> x else "")
  ss (JSAssignStatement lhs op rhs s) = "JSOpAssign (" <> (ss op <> ("," <> (ss lhs <> ("," <> (ss rhs <> (let x = ss s in if not (null x) then ")," ++ x else ")"))))))
  ss (JSMethodCall e _ a _ s) = "JSMethodCall (" <> (ss e <> (",JSArguments " <> (ss a <> (let x = ss s in if not (null x) then ")," <> x else ")"))))
  ss (JSReturn _ (Just me) s) = "JSReturn " <> (ss me <> (" " <> ss s))
  ss (JSReturn _ Nothing s) = "JSReturn " <> ss s
  ss (JSSwitch _ _lp x _rp _lb x2 _rb _) = "JSSwitch (" <> (ss x <> (") " <> ss x2))
  ss (JSThrow _ x _) = "JSThrow (" <> (ss x <> ")")
  ss (JSTry _ xt1 xtc xtf) = "JSTry (" <> (ss xt1 <> ("," <> (ss xtc <> ("," <> (ss xtf <> ")")))))
  ss (JSVariable _ xs _as) = "JSVariable " <> ss xs
  ss (JSWhile _ _lb x1 _rb x2) = "JSWhile (" <> (ss x1 <> (") (" <> (ss x2 <> ")")))
  ss (JSWith _ _lb x1 _rb x _) = "JSWith (" <> (ss x1 <> (") (" <> (ss x <> ")")))

instance ShowStripped JSExpression where
  ss (JSArrayLiteral _lb xs _rb) = "JSArrayLiteral " <> ss xs
  ss (JSAssignExpression lhs op rhs) = "JSOpAssign (" <> (ss op <> ("," <> (ss lhs <> ("," <> (ss rhs <> ")")))))
  ss (JSAwaitExpression _ e) = "JSAwaitExpresson " <> ss e
  ss (JSCallExpression ex _ xs _) = "JSCallExpression (" <> (ss ex <> (",JSArguments " <> (ss xs <> ")")))
  ss (JSCallExpressionDot ex _os xs) = "JSCallExpressionDot (" <> (ss ex <> ("," <> (ss xs <> ")")))
  ss (JSCallExpressionSquare ex _os xs _cs) = "JSCallExpressionSquare (" <> (ss ex <> ("," <> (ss xs <> ")")))
  ss (JSClassExpression _ n h _lb xs _rb) = "JSClassExpression " <> (ssid n <> (" (" <> (ss h <> (") " <> ss xs))))
  ss (JSDecimal _ s) = "JSDecimal " <> singleQuote (s)
  ss (JSCommaExpression l _ r) = "JSExpression [" <> (ss l <> ("," <> (ss r <> "]")))
  ss (JSExpressionBinary x2 op x3) = "JSExpressionBinary (" <> (ss op <> ("," <> (ss x2 <> ("," <> (ss x3 <> ")")))))
  ss (JSExpressionParen _lp x _rp) = "JSExpressionParen (" <> (ss x <> ")")
  ss (JSExpressionPostfix xs op) = "JSExpressionPostfix (" <> (ss op <> ("," <> (ss xs <> ")")))
  ss (JSExpressionTernary x1 _q x2 _c x3) = "JSExpressionTernary (" <> (ss x1 <> ("," <> (ss x2 <> ("," <> (ss x3 <> ")")))))
  ss (JSArrowExpression ps _ body) = "JSArrowExpression (" <> (ss ps <> (") => " <> ss body))
  ss (JSFunctionExpression _ n _lb pl _rb x3) = "JSFunctionExpression " <> (ssid n <> (" " <> (ss pl <> (" (" <> (ss x3 <> ")")))))
  ss (JSGeneratorExpression _ _ n _lb pl _rb x3) = "JSGeneratorExpression " <> (ssid n <> (" " <> (ss pl <> (" (" <> (ss x3 <> ")")))))
  ss (JSAsyncFunctionExpression _ _ n _lb pl _rb x3) = "JSAsyncFunctionExpression " <> (ssid n <> (" " <> (ss pl <> (" (" <> (ss x3 <> ")")))))
  ss (JSHexInteger _ s) = "JSHexInteger " <> singleQuote (s)
  ss (JSBinaryInteger _ s) = "JSBinaryInteger " <> singleQuote (s)
  ss (JSOctal _ s) = "JSOctal " <> singleQuote (s)
  ss (JSBigIntLiteral _ s) = "JSBigIntLiteral " <> singleQuote (s)
  ss (JSIdentifier _ s) = "JSIdentifier " <> singleQuote (s)
  ss (JSLiteral _ s) | null s = "JSLiteral ''"
  ss (JSLiteral _ s) = "JSLiteral " <> singleQuote (s)
  ss (JSMemberDot x1s _d x2) = "JSMemberDot (" <> (ss x1s <> ("," <> (ss x2 <> ")")))
  ss (JSMemberExpression e _ a _) = "JSMemberExpression (" <> (ss e <> (",JSArguments " <> (ss a <> ")")))
  ss (JSMemberNew _a n _ s _) = "JSMemberNew (" <> (ss n <> (",JSArguments " <> (ss s <> ")")))
  ss (JSMemberSquare x1s _lb x2 _rb) = "JSMemberSquare (" <> (ss x1s <> ("," <> (ss x2 <> ")")))
  ss (JSNewExpression _n e) = "JSNewExpression " <> ss e
  ss (JSOptionalMemberDot x1s _d x2) = "JSOptionalMemberDot (" <> (ss x1s <> ("," <> (ss x2 <> ")")))
  ss (JSOptionalMemberSquare x1s _lb x2 _rb) = "JSOptionalMemberSquare (" <> (ss x1s <> ("," <> (ss x2 <> ")")))
  ss (JSOptionalCallExpression ex _ xs _) = "JSOptionalCallExpression (" <> (ss ex <> (",JSArguments " <> (ss xs <> ")")))
  ss (JSObjectLiteral _lb xs _rb) = "JSObjectLiteral " <> ss xs
  ss (JSRegEx _ s) = "JSRegEx " <> singleQuote (s)
  ss (JSStringLiteral _ s) = "JSStringLiteral " <> s
  ss (JSUnaryExpression op x) = "JSUnaryExpression (" <> (ss op <> ("," <> (ss x <> ")")))
  ss (JSVarInitExpression x1 x2) = "JSVarInitExpression (" <> (ss x1 <> (") " <> ss x2))
  ss (JSParameterExpression x1 x2) = "JSParameterExpression (" <> (ss x1 <> (") " <> ss x2))
  ss (JSYieldExpression _ Nothing) = "JSYieldExpression ()"
  ss (JSYieldExpression _ (Just x)) = "JSYieldExpression (" <> (ss x <> ")")
  ss (JSYieldFromExpression _ _ x) = "JSYieldFromExpression (" <> (ss x <> ")")
  ss (JSImportMeta _ _) = "JSImportMeta"
  ss (JSSpreadExpression _ x1) = "JSSpreadExpression (" <> (ss x1 <> ")")
  ss (JSTemplateLiteral Nothing _ s ps) = "JSTemplateLiteral (()," <> (singleQuote (s) <> ("," <> (ss ps <> ")")))
  ss (JSTemplateLiteral (Just t) _ s ps) = "JSTemplateLiteral ((" <> (ss t <> (")," <> (singleQuote (s) <> ("," <> (ss ps <> ")")))))

instance ShowStripped JSArrowParameterList where
  ss (JSUnparenthesizedArrowParameter x) = ss x
  ss (JSParenthesizedArrowParameterList _ xs _) = ss xs

instance ShowStripped JSConciseBody where
  ss (JSConciseFunctionBody block) = "JSConciseFunctionBody (" <> (ss block <> ")")
  ss (JSConciseExpressionBody expr) = "JSConciseExpressionBody (" <> (ss expr <> ")")

instance ShowStripped JSModuleItem where
  ss (JSModuleExportDeclaration _ x1) = "JSModuleExportDeclaration (" <> (ss x1 <> ")")
  ss (JSModuleImportDeclaration _ x1) = "JSModuleImportDeclaration (" <> (ss x1 <> ")")
  ss (JSModuleStatementListItem x1) = "JSModuleStatementListItem (" <> (ss x1 <> ")")

instance ShowStripped JSImportDeclaration where
  ss (JSImportDeclaration imp from attrs _) = "JSImportDeclaration (" <> (ss imp <> ("," <> (ss from <> (maybe "" (\a -> "," <> ss a) attrs <> ")"))))
  ss (JSImportDeclarationBare _ m attrs _) = "JSImportDeclarationBare (" <> (singleQuote (m) <> (maybe "" (\a -> "," <> ss a) attrs <> ")"))

instance ShowStripped JSImportClause where
  ss (JSImportClauseDefault x) = "JSImportClauseDefault (" <> (ss x <> ")")
  ss (JSImportClauseNameSpace x) = "JSImportClauseNameSpace (" <> (ss x <> ")")
  ss (JSImportClauseNamed x) = "JSImportClauseNameSpace (" <> (ss x <> ")")
  ss (JSImportClauseDefaultNameSpace x1 _ x2) = "JSImportClauseDefaultNameSpace (" <> (ss x1 <> ("," <> (ss x2 <> ")")))
  ss (JSImportClauseDefaultNamed x1 _ x2) = "JSImportClauseDefaultNamed (" <> (ss x1 <> ("," <> (ss x2 <> ")")))

instance ShowStripped JSFromClause where
  ss (JSFromClause _ _ m) = "JSFromClause " <> singleQuote (m)

instance ShowStripped JSImportNameSpace where
  ss (JSImportNameSpace _ _ x) = "JSImportNameSpace (" <> (ss x <> ")")

instance ShowStripped JSImportsNamed where
  ss (JSImportsNamed _ xs _) = "JSImportsNamed (" <> (ss xs <> ")")

instance ShowStripped JSImportSpecifier where
  ss (JSImportSpecifier x1) = "JSImportSpecifier (" <> (ss x1 <> ")")
  ss (JSImportSpecifierAs x1 _ x2) = "JSImportSpecifierAs (" <> (ss x1 <> ("," <> (ss x2 <> ")")))

instance ShowStripped JSImportAttributes where
  ss (JSImportAttributes _ attrs _) = "JSImportAttributes (" <> (ss attrs <> ")")

instance ShowStripped JSImportAttribute where
  ss (JSImportAttribute key _ value) = "JSImportAttribute (" <> (ss key <> ("," <> (ss value <> ")")))

instance ShowStripped JSExportDeclaration where
  ss (JSExportAllFrom star from _) = "JSExportAllFrom (" <> (ss star <> ("," <> (ss from <> ")")))
  ss (JSExportAllAsFrom star _ ident from _) = "JSExportAllAsFrom (" <> (ss star <> ("," <> (ss ident <> ("," <> (ss from <> ")")))))
  ss (JSExportFrom xs from _) = "JSExportFrom (" <> (ss xs <> ("," <> (ss from <> ")")))
  ss (JSExportLocals xs _) = "JSExportLocals (" <> (ss xs <> ")")
  ss (JSExport x1 _) = "JSExport (" <> (ss x1 <> ")")
  ss (JSExportDefault _ x1 _) = "JSExportDefault (" <> (ss x1 <> ")")

instance ShowStripped JSExportClause where
  ss (JSExportClause _ xs _) = "JSExportClause (" <> (ss xs <> ")")

instance ShowStripped JSExportSpecifier where
  ss (JSExportSpecifier x1) = "JSExportSpecifier (" <> (ss x1 <> ")")
  ss (JSExportSpecifierAs x1 _ x2) = "JSExportSpecifierAs (" <> (ss x1 <> ("," <> (ss x2 <> ")")))

instance ShowStripped JSTryCatch where
  ss (JSCatch _ _lb x1 _rb x3) = "JSCatch (" <> (ss x1 <> ("," <> (ss x3 <> ")")))
  ss (JSCatchIf _ _lb x1 _ ex _rb x3) = "JSCatch (" <> (ss x1 <> (") if " <> (ss ex <> (" (" <> (ss x3 <> ")")))))

instance ShowStripped JSTryFinally where
  ss (JSFinally _ x) = "JSFinally (" <> (ss x <> ")")
  ss JSNoFinally = "JSFinally ()"

instance ShowStripped JSIdent where
  ss (JSIdentName _ s) = "JSIdentifier " <> singleQuote (s)
  ss JSIdentNone = "JSIdentNone"

instance ShowStripped JSObjectProperty where
  ss (JSPropertyNameandValue x1 _colon x2s) = "JSPropertyNameandValue (" <> (ss x1 <> (") " <> ss x2s))
  ss (JSPropertyIdentRef _ s) = "JSPropertyIdentRef " <> singleQuote (s)
  ss (JSObjectMethod m) = ss m
  ss (JSObjectSpread _ expr) = "JSObjectSpread (" <> (ss expr <> ")")

instance ShowStripped JSMethodDefinition where
  ss (JSMethodDefinition x1 _lb1 x2s _rb1 x3) = "JSMethodDefinition (" <> (ss x1 <> (") " <> (ss x2s <> (" (" <> (ss x3 <> ")")))))
  ss (JSAsyncMethodDefinition _ x1 _lb1 x2s _rb1 x3) = "JSAsyncMethodDefinition (" <> (ss x1 <> (") " <> (ss x2s <> (" (" <> (ss x3 <> ")")))))
  ss (JSPropertyAccessor s x1 _lb1 x2s _rb1 x3) = "JSPropertyAccessor " <> (ss s <> (" (" <> (ss x1 <> (") " <> (ss x2s <> (" (" ++ ss x3 ++ ")"))))))
  ss (JSGeneratorMethodDefinition _ x1 _lb1 x2s _rb1 x3) = "JSGeneratorMethodDefinition (" <> (ss x1 <> (") " <> (ss x2s <> (" (" <> (ss x3 <> ")")))))

instance ShowStripped JSPropertyName where
  ss (JSPropertyIdent _ s) = "JSIdentifier " <> singleQuote (s)
  ss (JSPropertyString _ s) = "JSIdentifier " <> singleQuote (s)
  ss (JSPropertyNumber _ s) = "JSIdentifier " <> singleQuote (s)
  ss (JSPropertyComputed _ x _) = "JSPropertyComputed (" <> (ss x <> ")")

instance ShowStripped JSAccessor where
  ss (JSAccessorGet _) = "JSAccessorGet"
  ss (JSAccessorSet _) = "JSAccessorSet"

instance ShowStripped JSBlock where
  ss (JSBlock _ xs _) = "JSBlock " <> ss xs

instance ShowStripped JSSwitchParts where
  ss (JSCase _ x1 _c x2s) = "JSCase (" <> (ss x1 <> (") (" <> (ss x2s <> ")")))
  ss (JSDefault _ _c xs) = "JSDefault (" <> (ss xs <> ")")

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

instance ShowStripped JSVarInitializer where
  ss (JSVarInit _ n) = "[" <> (ss n <> "]")
  ss JSVarInitNone = ""

instance ShowStripped JSSemi where
  ss (JSSemi _) = "JSSemicolon"
  ss JSSemiAuto = ""

instance ShowStripped JSArrayElement where
  ss (JSArrayElement e) = ss e
  ss (JSArrayComma _) = "JSComma"

instance ShowStripped JSTemplatePart where
  ss (JSTemplatePart e _ s) = "(" <> (ss e <> ("," <> (singleQuote (s) <> ")")))

instance ShowStripped JSClassHeritage where
  ss JSExtendsNone = ""
  ss (JSExtends _ x) = ss x

instance ShowStripped JSClassElement where
  ss (JSClassInstanceMethod m) = ss m
  ss (JSClassStaticMethod _ m) = "JSClassStaticMethod (" <> (ss m <> ")")
  ss (JSClassSemi _) = "JSClassSemi"
  ss (JSPrivateField _ name _ Nothing _) = "JSPrivateField " <> singleQuote ("#" <> name)
  ss (JSPrivateField _ name _ (Just initializer) _) = "JSPrivateField " <> (singleQuote ("#" <> name) <> (" (" <> (ss initializer <> ")")))
  ss (JSPrivateMethod _ name _ params _ block) = "JSPrivateMethod " <> (singleQuote ("#" <> name) <> (" " <> (ss params <> (" (" <> (ss block <> ")")))))
  ss (JSPrivateAccessor accessor _ name _ params _ block) = "JSPrivateAccessor " <> (ss accessor <> (" " <> (singleQuote ("#" <> name) <> (" " <> (ss params <> (" (" ++ ss block ++ ")"))))))

instance ShowStripped a => ShowStripped (JSCommaList a) where
  ss xs = "(" <> (commaJoin (ss <$> fromCommaList xs) <> ")")

instance ShowStripped a => ShowStripped (JSCommaTrailingList a) where
  ss (JSCTLComma xs _) = "[" <> (commaJoin (ss <$> fromCommaList xs) <> ",JSComma]")
  ss (JSCTLNone xs) = "[" <> (commaJoin (ss <$> fromCommaList xs) <> "]")

instance ShowStripped a => ShowStripped [a] where
  ss xs = "[" <> (commaJoin (fmap ss xs) <> "]")

-- -----------------------------------------------------------------------------
-- Helpers.

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
singleQuote s = "'" <> (s <> "'")

-- | Extract String from JavaScript identifier with quotes.
--
-- Converts 'JSIdent' to its quoted String representation for pretty printing.
-- Returns empty quotes for 'JSIdentNone'.
--
-- @since 0.7.1.0
ssid :: JSIdent -> String
ssid (JSIdentName _ s) = singleQuote s
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

-- | JSDoc utility functions for AST integration

-- | Extract JSDoc comment from JSAnnot annotation
--
-- Searches through the comment annotations in a JSAnnot to find
-- JSDoc documentation. Returns the first JSDoc comment found.
--
-- ==== Examples
--
-- >>> extractJSDoc (JSAnnot pos [JSDocA pos jsDoc, CommentA pos "regular"])
-- Just jsDoc
--
-- >>> extractJSDoc (JSAnnot pos [CommentA pos "regular"])
-- Nothing
--
-- @since 0.8.0.0
extractJSDoc :: JSAnnot -> Maybe JSDocComment
extractJSDoc (JSAnnot _ comments) = findJSDoc comments
  where
    findJSDoc [] = Nothing
    findJSDoc (JSDocA _ jsDoc : _) = Just jsDoc
    findJSDoc (_ : rest) = findJSDoc rest
extractJSDoc _ = Nothing

-- | Extract JSDoc from any JavaScript statement that might have documentation
--
-- Looks for JSDoc comments in the leading annotation of various statement types.
-- Useful for extracting function, class, or variable documentation.
extractJSDocFromStatement :: JSStatement -> Maybe JSDocComment
extractJSDocFromStatement stmt = case stmt of
  JSFunction annot _ _ _ _ _ _ -> extractJSDoc annot
  JSAsyncFunction annot _ _ _ _ _ _ _ -> extractJSDoc annot
  JSGenerator annot _ _ _ _ _ _ _ -> extractJSDoc annot
  JSClass annot _ _ _ _ _ _ -> extractJSDoc annot
  JSVariable annot _ _ -> extractJSDoc annot
  JSConstant annot _ _ -> extractJSDoc annot
  JSLet annot _ _ -> extractJSDoc annot
  _ -> Nothing

-- | Extract JSDoc from JavaScript expressions that might have documentation
--
-- Looks for JSDoc comments in function expressions and class expressions.
extractJSDocFromExpression :: JSExpression -> Maybe JSDocComment
extractJSDocFromExpression expr = case expr of
  JSFunctionExpression annot _ _ _ _ _ -> extractJSDoc annot
  JSAsyncFunctionExpression annot _ _ _ _ _ _ -> extractJSDoc annot
  JSGeneratorExpression annot _ _ _ _ _ _ -> extractJSDoc annot
  JSClassExpression annot _ _ _ _ _ -> extractJSDoc annot
  _ -> Nothing

-- | Check if a JavaScript statement has JSDoc documentation
hasJSDoc :: JSStatement -> Bool
hasJSDoc = isJust . extractJSDocFromStatement
  where
    isJust Nothing = False
    isJust (Just _) = True

-- | Get function parameters from JSDoc @param tags
--
-- Extracts parameter names from JSDoc @param tags for validation
-- against actual function parameters.
getJSDocParams :: JSDocComment -> [Text.Text]
getJSDocParams jsDoc =
  [name | JSDocTag "param" _ (Just name) _ _ _ <- jsDocTags jsDoc]

-- | Get return type from JSDoc @returns/@return tag
--
-- Extracts the return type from JSDoc documentation if present.
getJSDocReturnType :: JSDocComment -> Maybe JSDocType
getJSDocReturnType jsDoc =
  case [jsDocType | JSDocTag tagName jsDocType _ _ _ _ <- jsDocTags jsDoc,
                   tagName `elem` ["returns", "return"],
                   isJust jsDocType] of
    (Just returnType : _) -> Just returnType
    _ -> Nothing
  where
    isJust Nothing = False
    isJust (Just _) = True

-- | Validate JSDoc parameter consistency with function signature
--
-- Compares JSDoc @param tags with actual function parameters to detect
-- missing documentation or extra documented parameters.
validateJSDocParameters :: JSDocComment -> [String] -> [String]
validateJSDocParameters jsDoc functionParams =
  let jsDocParams = map Text.unpack (getJSDocParams jsDoc)
      missingDocs = filter (`notElem` jsDocParams) functionParams
      extraDocs = filter (`notElem` functionParams) jsDocParams
  in map ("Missing JSDoc for parameter: " ++) missingDocs ++
     map ("Extra JSDoc parameter: " ++) extraDocs

-- | Check if JSDoc comment has a specific tag
hasJSDocTag :: Text.Text -> JSDocComment -> Bool
hasJSDocTag tagName jsDoc =
  any (\tag -> jsDocTagName tag == tagName) (jsDocTags jsDoc)

-- | Get all JSDoc tags of a specific type
getJSDocTagsByName :: Text.Text -> JSDocComment -> [JSDocTag]
getJSDocTagsByName tagName jsDoc =
  filter (\tag -> jsDocTagName tag == tagName) (jsDocTags jsDoc)
