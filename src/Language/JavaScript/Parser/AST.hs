{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -O2 #-}

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
    ShowStripped (..),
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
-- Every variant carries a trailing 'JSAnnot' for end-of-input whitespace
-- and comments.
data JSAST
  = -- | A complete JavaScript program (script mode).
    --
    -- Contains a list of top-level statements and trailing annotation.
    --
    -- JavaScript: @var x = 1; function foo() {}@
    JSAstProgram ![JSStatement] !JSAnnot
  | -- | An ES6 module containing import\/export declarations and statements.
    --
    -- Contains a list of module items (imports, exports, statements) and
    -- trailing annotation.
    --
    -- JavaScript: @import { foo } from 'bar'; export default 42;@
    JSAstModule ![JSModuleItem] !JSAnnot
  | -- | A single JavaScript statement wrapped as a top-level AST node.
    --
    -- Used when parsing a standalone statement rather than a full program.
    --
    -- JavaScript: @if (x) return 42;@
    JSAstStatement !JSStatement !JSAnnot
  | -- | A single JavaScript expression wrapped as a top-level AST node.
    --
    -- Used when parsing a standalone expression rather than a full program.
    --
    -- JavaScript: @x + y * z@
    JSAstExpression !JSExpression !JSAnnot
  | -- | A single JavaScript literal wrapped as a top-level AST node.
    --
    -- Used when parsing a standalone literal value.
    --
    -- JavaScript: @42@, @"hello"@, @true@
    JSAstLiteral !JSExpression !JSAnnot
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | An item within an ES6 module body.
--
-- A module body consists of import declarations, export declarations,
-- and regular statements. Based on the
-- <https://github.com/shapesecurity/shift-spec Shift AST specification>.
data JSModuleItem
  = -- | An import declaration within a module.
    --
    -- Fields: @import@ keyword annotation, import declaration body.
    --
    -- JavaScript: @import { foo } from 'module';@
    JSModuleImportDeclaration !JSAnnot !JSImportDeclaration
  | -- | An export declaration within a module.
    --
    -- Fields: @export@ keyword annotation, export declaration body.
    --
    -- JavaScript: @export function bar() {}@
    JSModuleExportDeclaration !JSAnnot !JSExportDeclaration
  | -- | A regular statement within a module body.
    --
    -- Any statement that is not an import or export declaration.
    --
    -- JavaScript: @const x = 42;@ (inside a module)
    JSModuleStatementListItem !JSStatement
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | An ES6 import declaration specifying what to import and from where.
data JSImportDeclaration
  = -- | Import with bindings from a module specifier.
    --
    -- Fields: import clause, from clause, optional import attributes, semicolon.
    --
    -- JavaScript: @import { foo, bar } from 'module';@
    JSImportDeclaration !JSImportClause !JSFromClause !(Maybe JSImportAttributes) !JSSemi
  | -- | Bare import for side effects only (no bindings).
    --
    -- Fields: @import@ annotation, module specifier string, optional import
    -- attributes, semicolon.
    --
    -- JavaScript: @import 'module';@ (executes module for side effects)
    JSImportDeclarationBare !JSAnnot !ByteString !(Maybe JSImportAttributes) !JSSemi
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | Import attributes (TC39 proposal), providing metadata for module loading.
--
-- JavaScript: @import json from './data.json' with { type: 'json' };@
data JSImportAttributes
  = -- | Braced list of key-value import attributes.
    --
    -- Fields: opening brace annotation, comma-separated attributes, closing
    -- brace annotation.
    --
    -- JavaScript: @with { type: 'json' }@
    JSImportAttributes !JSAnnot !(JSCommaList JSImportAttribute) !JSAnnot
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | A single key-value pair in an import attributes block.
data JSImportAttribute
  = -- | A key-colon-value attribute pair.
    --
    -- Fields: attribute key identifier, colon annotation, attribute value
    -- expression (typically a string literal).
    --
    -- JavaScript: @type: 'json'@ (within @with { ... }@)
    JSImportAttribute !JSIdent !JSAnnot !JSExpression
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | The import clause specifying which bindings to import.
--
-- ES6 imports support several forms: default imports, namespace imports,
-- named imports, and combinations of default with namespace or named.
data JSImportClause
  = -- | Default import only.
    --
    -- Fields: local binding name for the default export.
    --
    -- JavaScript: @import foo from 'module';@
    JSImportClauseDefault !JSIdent
  | -- | Namespace import (import everything as a single object).
    --
    -- Fields: namespace import specifier.
    --
    -- JavaScript: @import * as ns from 'module';@
    JSImportClauseNameSpace !JSImportNameSpace
  | -- | Named imports (destructured from module exports).
    --
    -- Fields: named imports specifier list.
    --
    -- JavaScript: @import { foo, bar } from 'module';@
    JSImportClauseNamed !JSImportsNamed
  | -- | Default import combined with a namespace import.
    --
    -- Fields: default binding name, comma annotation, namespace import.
    --
    -- JavaScript: @import foo, * as ns from 'module';@
    JSImportClauseDefaultNameSpace !JSIdent !JSAnnot !JSImportNameSpace
  | -- | Default import combined with named imports.
    --
    -- Fields: default binding name, comma annotation, named imports.
    --
    -- JavaScript: @import foo, { bar, baz } from 'module';@
    JSImportClauseDefaultNamed !JSIdent !JSAnnot !JSImportsNamed
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | The @from@ clause in an import or export declaration, specifying the
-- module specifier.
data JSFromClause
  = -- | A @from 'module-specifier'@ clause.
    --
    -- Fields: @from@ keyword annotation, string literal annotation,
    -- module specifier string (the raw content between quotes).
    --
    -- JavaScript: @from 'lodash'@, @from './utils.js'@
    JSFromClause !JSAnnot !JSAnnot !ByteString
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | Import namespace binding, importing all exports as a single object.
data JSImportNameSpace
  = -- | Namespace import: @* as localName@.
    --
    -- Fields: star operator (@*@), @as@ keyword annotation, local binding
    -- name.
    --
    -- JavaScript: @* as utils@ (within @import * as utils from 'module';@)
    JSImportNameSpace !JSBinOp !JSAnnot !JSIdent
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | Named imports enclosed in braces, selecting specific exports by name.
data JSImportsNamed
  = -- | Braced list of named import specifiers.
    --
    -- Fields: opening brace annotation, comma-separated import specifiers,
    -- closing brace annotation.
    --
    -- JavaScript: @{ foo, bar as baz }@ (within @import { ... } from 'module';@)
    JSImportsNamed !JSAnnot !(JSCommaList JSImportSpecifier) !JSAnnot
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | A single named import specifier within a braced import list.
--
-- Note: this data type is separate from 'JSExportSpecifier' because the
-- grammar is slightly different (e.g. in handling of reserved words).
data JSImportSpecifier
  = -- | Import a binding using its original exported name.
    --
    -- Fields: exported name (used as local binding).
    --
    -- JavaScript: @foo@ (within @import { foo } from 'module';@)
    JSImportSpecifier !JSIdent
  | -- | Import a binding with a local alias.
    --
    -- Fields: exported name, @as@ keyword annotation, local alias name.
    --
    -- JavaScript: @foo as bar@ (within @import { foo as bar } from 'module';@)
    JSImportSpecifierAs !JSIdent !JSAnnot !JSIdent
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | The body of an ES6 export declaration, specifying what is exported.
data JSExportDeclaration
  = -- | Re-export all exports from another module.
    --
    -- Fields: star operator (@*@), from clause, semicolon.
    --
    -- JavaScript: @export * from 'module';@
    JSExportAllFrom !JSBinOp JSFromClause !JSSemi
  | -- | Re-export all exports from another module under a namespace.
    --
    -- Fields: star operator (@*@), @as@ annotation, namespace name, from
    -- clause, semicolon.
    --
    -- JavaScript: @export * as ns from 'module';@
    JSExportAllAsFrom !JSBinOp !JSAnnot !JSIdent JSFromClause !JSSemi
  | -- | Re-export specific named bindings from another module.
    --
    -- Fields: export clause with specifiers, from clause, semicolon.
    --
    -- JavaScript: @export { foo, bar } from 'module';@
    JSExportFrom JSExportClause JSFromClause !JSSemi
  | -- | Export specific local bindings by name.
    --
    -- Fields: export clause with specifiers, semicolon.
    --
    -- JavaScript: @export { foo, bar };@
    JSExportLocals JSExportClause !JSSemi
  | -- | Export a default value (expression or declaration).
    --
    -- Fields: @default@ keyword annotation, exported statement\/expression,
    -- semicolon.
    --
    -- JavaScript: @export default function() {}@, @export default 42;@
    JSExportDefault !JSAnnot !JSStatement !JSSemi
  | -- | Export a declaration directly (non-default).
    --
    -- Fields: exported declaration statement, semicolon.
    --
    -- JavaScript: @export function foo() {}@, @export const x = 1;@
    JSExport !JSStatement !JSSemi
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | Braced list of export specifiers naming which bindings to export.
data JSExportClause
  = -- | Braced list of named export specifiers.
    --
    -- Fields: opening brace annotation, comma-separated export specifiers,
    -- closing brace annotation.
    --
    -- JavaScript: @{ foo, bar as baz }@ (within @export { ... };@)
    JSExportClause !JSAnnot !(JSCommaList JSExportSpecifier) !JSAnnot
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | A single named export specifier within a braced export list.
data JSExportSpecifier
  = -- | Export a binding using its original local name.
    --
    -- Fields: local binding name.
    --
    -- JavaScript: @foo@ (within @export { foo };@)
    JSExportSpecifier !JSIdent
  | -- | Export a binding with a different public name.
    --
    -- Fields: local name, @as@ keyword annotation, exported name.
    --
    -- JavaScript: @foo as bar@ (within @export { foo as bar };@)
    JSExportSpecifierAs !JSIdent !JSAnnot !JSIdent
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | JavaScript statement AST nodes.
--
-- Covers all JavaScript statement types: declarations, control flow,
-- iteration, and expression statements.
data JSStatement
  = -- | A block statement enclosed in braces.
    --
    -- Fields: opening brace, statements, closing brace, auto-semicolon.
    --
    -- JavaScript: @{ stmt1; stmt2; }@
    JSStatementBlock !JSAnnot ![JSStatement] !JSAnnot !JSSemi
  | -- | A @break@ statement, optionally targeting a label.
    --
    -- Fields: @break@ annotation, optional label identifier, auto-semicolon.
    --
    -- JavaScript: @break;@, @break myLabel;@
    JSBreak !JSAnnot !JSIdent !JSSemi
  | -- | A @let@ variable declaration.
    --
    -- Fields: @let@ annotation, comma-separated declarators, auto-semicolon.
    --
    -- JavaScript: @let x = 1, y = 2;@
    JSLet !JSAnnot !(JSCommaList JSExpression) !JSSemi
  | -- | A @class@ declaration statement.
    --
    -- Fields: @class@ annotation, class name, optional extends clause,
    -- opening brace, class body elements, closing brace, auto-semicolon.
    --
    -- JavaScript: @class Foo extends Bar { method() {} }@
    JSClass !JSAnnot !JSIdent !JSClassHeritage !JSAnnot ![JSClassElement] !JSAnnot !JSSemi
  | -- | A @const@ variable declaration.
    --
    -- Fields: @const@ annotation, comma-separated declarators, auto-semicolon.
    --
    -- JavaScript: @const PI = 3.14, E = 2.718;@
    JSConstant !JSAnnot !(JSCommaList JSExpression) !JSSemi
  | -- | A @continue@ statement, optionally targeting a label.
    --
    -- Fields: @continue@ annotation, optional label identifier, auto-semicolon.
    --
    -- JavaScript: @continue;@, @continue outerLoop;@
    JSContinue !JSAnnot !JSIdent !JSSemi
  | -- | A @do...while@ loop statement.
    --
    -- Fields: @do@ annotation, body statement, @while@ annotation, opening
    -- paren, condition expression, closing paren, auto-semicolon.
    --
    -- JavaScript: @do { x++; } while (x \< 10);@
    JSDoWhile !JSAnnot !JSStatement !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSSemi
  | -- | A C-style @for@ loop with three clauses.
    --
    -- Fields: @for@ annotation, opening paren, init expressions, first
    -- semicolon, condition expressions, second semicolon, update expressions,
    -- closing paren, body statement.
    --
    -- JavaScript: @for (i = 0; i \< 10; i++) stmt@
    JSFor !JSAnnot !JSAnnot !(JSCommaList JSExpression) !JSAnnot !(JSCommaList JSExpression) !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSStatement
  | -- | A @for...in@ loop iterating over object properties.
    --
    -- Fields: @for@ annotation, opening paren, loop variable expression,
    -- @in@ operator, object expression, closing paren, body statement.
    --
    -- JavaScript: @for (key in obj) stmt@
    JSForIn !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement
  | -- | A @for (var ...; ...; ...)@ loop with @var@ declarations.
    --
    -- Fields: @for@ annotation, opening paren, @var@ annotation, declarators,
    -- first semicolon, condition, second semicolon, update, closing paren,
    -- body statement.
    --
    -- JavaScript: @for (var i = 0; i \< 10; i++) stmt@
    JSForVar !JSAnnot !JSAnnot !JSAnnot !(JSCommaList JSExpression) !JSAnnot !(JSCommaList JSExpression) !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSStatement
  | -- | A @for (var ... in ...)@ loop with @var@ declaration.
    --
    -- Fields: @for@ annotation, opening paren, @var@ annotation, variable
    -- declarator, @in@ operator, object expression, closing paren, body.
    --
    -- JavaScript: @for (var key in obj) stmt@
    JSForVarIn !JSAnnot !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement
  | -- | A @for (let ...; ...; ...)@ loop with @let@ declarations.
    --
    -- Fields: @for@ annotation, opening paren, @let@ annotation, declarators,
    -- first semicolon, condition, second semicolon, update, closing paren,
    -- body statement.
    --
    -- JavaScript: @for (let i = 0; i \< 10; i++) stmt@
    JSForLet !JSAnnot !JSAnnot !JSAnnot !(JSCommaList JSExpression) !JSAnnot !(JSCommaList JSExpression) !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSStatement
  | -- | A @for (let ... in ...)@ loop with @let@ declaration.
    --
    -- Fields: @for@ annotation, opening paren, @let@ annotation, variable
    -- declarator, @in@ operator, object expression, closing paren, body.
    --
    -- JavaScript: @for (let key in obj) stmt@
    JSForLetIn !JSAnnot !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement
  | -- | A @for (let ... of ...)@ loop with @let@ declaration.
    --
    -- Fields: @for@ annotation, opening paren, @let@ annotation, variable
    -- declarator, @of@ operator, iterable expression, closing paren, body.
    --
    -- JavaScript: @for (let item of array) stmt@
    JSForLetOf !JSAnnot !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement
  | -- | A @for (const ...; ...; ...)@ loop with @const@ declarations.
    --
    -- Fields: @for@ annotation, opening paren, @const@ annotation, declarators,
    -- first semicolon, condition, second semicolon, update, closing paren,
    -- body statement.
    --
    -- JavaScript: @for (const i = 0; i \< 10; i++) stmt@
    JSForConst !JSAnnot !JSAnnot !JSAnnot !(JSCommaList JSExpression) !JSAnnot !(JSCommaList JSExpression) !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSStatement
  | -- | A @for (const ... in ...)@ loop with @const@ declaration.
    --
    -- Fields: @for@ annotation, opening paren, @const@ annotation, variable
    -- declarator, @in@ operator, object expression, closing paren, body.
    --
    -- JavaScript: @for (const key in obj) stmt@
    JSForConstIn !JSAnnot !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement
  | -- | A @for (const ... of ...)@ loop with @const@ declaration.
    --
    -- Fields: @for@ annotation, opening paren, @const@ annotation, variable
    -- declarator, @of@ operator, iterable expression, closing paren, body.
    --
    -- JavaScript: @for (const item of array) stmt@
    JSForConstOf !JSAnnot !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement
  | -- | A @for...of@ loop without a variable declaration keyword.
    --
    -- Fields: @for@ annotation, opening paren, loop variable expression,
    -- @of@ operator, iterable expression, closing paren, body statement.
    --
    -- JavaScript: @for (x of iterable) stmt@
    JSForOf !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement
  | -- | A @for (var ... of ...)@ loop with @var@ declaration.
    --
    -- Fields: @for@ annotation, opening paren, @var@ annotation, variable
    -- declarator, @of@ operator, iterable expression, closing paren, body.
    --
    -- JavaScript: @for (var item of array) stmt@
    JSForVarOf !JSAnnot !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement
  | -- | An @async function@ declaration.
    --
    -- Fields: @async@ annotation, @function@ annotation, function name,
    -- opening paren, parameter list, closing paren, function body block,
    -- auto-semicolon.
    --
    -- JavaScript: @async function fetchData(url) { ... }@
    JSAsyncFunction !JSAnnot !JSAnnot !JSIdent !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock !JSSemi
  | -- | A @function@ declaration.
    --
    -- Fields: @function@ annotation, function name, opening paren, parameter
    -- list, closing paren, function body block, auto-semicolon.
    --
    -- JavaScript: @function add(a, b) { return a + b; }@
    JSFunction !JSAnnot !JSIdent !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock !JSSemi
  | -- | A generator @function*@ declaration.
    --
    -- Fields: @function@ annotation, star annotation, generator name,
    -- opening paren, parameter list, closing paren, function body block,
    -- auto-semicolon.
    --
    -- JavaScript: @function* range(start, end) { ... }@
    JSGenerator !JSAnnot !JSAnnot !JSIdent !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock !JSSemi
  | -- | An @if@ statement without an @else@ branch.
    --
    -- Fields: @if@ annotation, opening paren, condition expression, closing
    -- paren, consequent statement.
    --
    -- JavaScript: @if (x > 0) doSomething();@
    JSIf !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSStatement
  | -- | An @if...else@ statement with both branches.
    --
    -- Fields: @if@ annotation, opening paren, condition expression, closing
    -- paren, consequent statement, @else@ annotation, alternate statement.
    --
    -- JavaScript: @if (x > 0) doA(); else doB();@
    JSIfElse !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSStatement !JSAnnot !JSStatement
  | -- | A labelled statement.
    --
    -- Fields: label identifier, colon annotation, labelled statement.
    --
    -- JavaScript: @outer: for (;;) { ... }@
    JSLabelled !JSIdent !JSAnnot !JSStatement
  | -- | An empty statement (bare semicolon).
    --
    -- Fields: semicolon annotation.
    --
    -- JavaScript: @;@
    JSEmptyStatement !JSAnnot
  | -- | An expression used as a statement.
    --
    -- Fields: expression, auto-semicolon.
    --
    -- JavaScript: @foo();@, @x + 1;@
    JSExpressionStatement !JSExpression !JSSemi
  | -- | An assignment statement (shorthand for expression statement with assignment).
    --
    -- Fields: left-hand side, assignment operator, right-hand side, auto-semicolon.
    --
    -- JavaScript: @x = 42;@, @arr[0] += 1;@
    JSAssignStatement !JSExpression !JSAssignOp !JSExpression !JSSemi
  | -- | A method call statement (expression followed by arguments).
    --
    -- Fields: callee expression, opening paren, arguments, closing paren,
    -- auto-semicolon.
    --
    -- JavaScript: @console.log("hello");@
    JSMethodCall !JSExpression !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSSemi
  | -- | A @return@ statement with an optional return value.
    --
    -- Fields: @return@ annotation, optional return expression, auto-semicolon.
    --
    -- JavaScript: @return;@, @return x + 1;@
    JSReturn !JSAnnot !(Maybe JSExpression) !JSSemi
  | -- | A @switch@ statement with case\/default blocks.
    --
    -- Fields: @switch@ annotation, opening paren, discriminant expression,
    -- closing paren, opening brace, switch parts (cases\/defaults), closing
    -- brace, auto-semicolon.
    --
    -- JavaScript: @switch (x) { case 1: break; default: break; }@
    JSSwitch !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSAnnot ![JSSwitchParts] !JSAnnot !JSSemi
  | -- | A @throw@ statement.
    --
    -- Fields: @throw@ annotation, thrown expression, auto-semicolon.
    --
    -- JavaScript: @throw new Error("oops");@
    JSThrow !JSAnnot !JSExpression !JSSemi
  | -- | A @try@ statement with optional catch and finally blocks.
    --
    -- Fields: @try@ annotation, try block, catch clauses, finally clause.
    --
    -- JavaScript: @try { ... } catch (e) { ... } finally { ... }@
    JSTry !JSAnnot !JSBlock ![JSTryCatch] !JSTryFinally
  | -- | A @var@ variable declaration.
    --
    -- Fields: @var@ annotation, comma-separated declarators, auto-semicolon.
    --
    -- JavaScript: @var x = 1, y = 2;@
    JSVariable !JSAnnot !(JSCommaList JSExpression) !JSSemi
  | -- | A @while@ loop statement.
    --
    -- Fields: @while@ annotation, opening paren, condition expression,
    -- closing paren, body statement.
    --
    -- JavaScript: @while (x \< 10) x++;@
    JSWhile !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSStatement
  | -- | A @with@ statement (deprecated in strict mode).
    --
    -- Fields: @with@ annotation, opening paren, object expression, closing
    -- paren, body statement, auto-semicolon.
    --
    -- JavaScript: @with (Math) { log(PI); }@
    JSWith !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSStatement !JSSemi
  | -- | A @debugger@ statement.
    --
    -- Fields: @debugger@ annotation, auto-semicolon.
    --
    -- JavaScript: @debugger;@
    JSDebugger !JSAnnot !JSSemi
  | -- | An @async function*@ (async generator) declaration.
    --
    -- Fields: @async@ annotation, @function@ annotation, star annotation,
    -- generator name, opening paren, parameter list, closing paren,
    -- function body block, auto-semicolon.
    --
    -- JavaScript: @async function* stream() { yield await fetch(url); }@
    JSAsyncGenerator !JSAnnot !JSAnnot !JSAnnot !JSIdent !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock !JSSemi
  | -- | A @for await...of@ loop without a declaration keyword.
    --
    -- Fields: @for@ annotation, @await@ annotation, opening paren, loop
    -- variable expression, @of@ operator, iterable expression, closing
    -- paren, body statement.
    --
    -- JavaScript: @for await (chunk of stream) stmt@
    JSForAwaitOf !JSAnnot !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement
  | -- | A @for await (var ... of ...)@ loop.
    --
    -- Fields: @for@ annotation, @await@ annotation, opening paren, @var@
    -- annotation, variable declarator, @of@ operator, iterable expression,
    -- closing paren, body statement.
    --
    -- JavaScript: @for await (var chunk of stream) stmt@
    JSForAwaitVarOf !JSAnnot !JSAnnot !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement
  | -- | A @for await (let ... of ...)@ loop.
    --
    -- Fields: @for@ annotation, @await@ annotation, opening paren, @let@
    -- annotation, variable declarator, @of@ operator, iterable expression,
    -- closing paren, body statement.
    --
    -- JavaScript: @for await (let chunk of stream) stmt@
    JSForAwaitLetOf !JSAnnot !JSAnnot !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement
  | -- | A @for await (const ... of ...)@ loop.
    --
    -- Fields: @for@ annotation, @await@ annotation, opening paren, @const@
    -- annotation, variable declarator, @of@ operator, iterable expression,
    -- closing paren, body statement.
    --
    -- JavaScript: @for await (const chunk of stream) stmt@
    JSForAwaitConstOf !JSAnnot !JSAnnot !JSAnnot !JSAnnot !JSExpression !JSBinOp !JSExpression !JSAnnot !JSStatement
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | JavaScript expression AST nodes.
--
-- Covers all JavaScript expression types: literals, operators, function
-- expressions, member access, and modern ES6+ syntax like arrow functions,
-- template literals, and optional chaining.
data JSExpression
  = -- | An identifier reference (variable name, function name, etc.).
    --
    -- Fields: annotation, identifier name as raw bytes.
    --
    -- JavaScript: @foo@, @myVariable@, @$element@
    JSIdentifier !JSAnnot !ByteString
  | -- | A decimal numeric literal.
    --
    -- Fields: annotation, numeric value as Double.
    --
    -- JavaScript: @42@, @3.14@, @1e5@
    JSDecimal !JSAnnot !Double
  | -- | A keyword literal (@true@, @false@, @null@, @this@).
    --
    -- Fields: annotation, literal text as raw bytes.
    --
    -- JavaScript: @true@, @false@, @null@, @this@
    JSLiteral !JSAnnot !ByteString
  | -- | A hexadecimal integer literal.
    --
    -- Fields: annotation, integer value.
    --
    -- JavaScript: @0xFF@, @0x1A3F@
    JSHexInteger !JSAnnot !Integer
  | -- | A binary integer literal (ES2015).
    --
    -- Fields: annotation, integer value.
    --
    -- JavaScript: @0b1010@, @0b11111111@
    JSBinaryInteger !JSAnnot !Integer
  | -- | An octal integer literal (ES2015 @0o@ prefix form).
    --
    -- Fields: annotation, integer value.
    --
    -- JavaScript: @0o77@, @0o755@
    JSOctal !JSAnnot !Integer
  | -- | A BigInt literal (ES2020).
    --
    -- Fields: annotation, integer value (without the trailing @n@).
    --
    -- JavaScript: @42n@, @0xFFn@, @9007199254740991n@
    JSBigIntLiteral !JSAnnot !Integer
  | -- | A string literal (single or double quoted).
    --
    -- Fields: annotation, string content as raw bytes (including quotes).
    --
    -- JavaScript: @"hello"@, @'world'@
    JSStringLiteral !JSAnnot !ByteString
  | -- | A regular expression literal.
    --
    -- Fields: annotation, regex pattern and flags as raw bytes.
    --
    -- JavaScript: @\/pattern\/gi@
    JSRegEx !JSAnnot !ByteString
  | -- | An array literal expression.
    --
    -- Fields: opening bracket annotation, array elements (including elisions),
    -- closing bracket annotation.
    --
    -- JavaScript: @[1, 2, 3]@, @[, , x]@
    JSArrayLiteral !JSAnnot ![JSArrayElement] !JSAnnot
  | -- | An assignment expression.
    --
    -- Fields: left-hand side, assignment operator, right-hand side.
    --
    -- JavaScript: @x = 42@, @obj.prop += 1@
    JSAssignExpression !JSExpression !JSAssignOp !JSExpression
  | -- | An @await@ expression (inside async functions).
    --
    -- Fields: @await@ annotation, awaited expression.
    --
    -- JavaScript: @await fetchData()@
    JSAwaitExpression !JSAnnot !JSExpression
  | -- | A function call expression with parenthesized arguments.
    --
    -- Fields: callee expression, opening paren, arguments, closing paren.
    --
    -- JavaScript: @foo(1, 2)@, @obj.method(arg)@
    JSCallExpression !JSExpression !JSAnnot !(JSCommaList JSExpression) !JSAnnot
  | -- | A dot-access on a call expression result.
    --
    -- Fields: call expression, dot annotation, property name expression.
    --
    -- JavaScript: @foo().bar@ (the @.bar@ part after a call)
    JSCallExpressionDot !JSExpression !JSAnnot !JSExpression
  | -- | A bracket-access on a call expression result.
    --
    -- Fields: call expression, opening bracket, index expression, closing
    -- bracket.
    --
    -- JavaScript: @foo()[0]@ (the @[0]@ part after a call)
    JSCallExpressionSquare !JSExpression !JSAnnot !JSExpression !JSAnnot
  | -- | A class expression (anonymous or named).
    --
    -- Fields: @class@ annotation, optional class name, optional extends
    -- clause, opening brace, class body elements, closing brace.
    --
    -- JavaScript: @class {}@, @class Foo extends Bar { method() {} }@
    JSClassExpression !JSAnnot !JSIdent !JSClassHeritage !JSAnnot ![JSClassElement] !JSAnnot
  | -- | A comma expression (sequence of two expressions).
    --
    -- Fields: left expression, comma annotation, right expression.
    --
    -- JavaScript: @a, b@ (evaluates both, returns last)
    JSCommaExpression !JSExpression !JSAnnot !JSExpression
  | -- | A binary operator expression.
    --
    -- Fields: left operand, binary operator, right operand.
    --
    -- JavaScript: @x + y@, @a && b@, @i \< 10@
    JSExpressionBinary !JSExpression !JSBinOp !JSExpression
  | -- | A parenthesized expression.
    --
    -- Fields: opening paren, inner expression, closing paren.
    --
    -- JavaScript: @(x + y)@
    JSExpressionParen !JSAnnot !JSExpression !JSAnnot
  | -- | A postfix unary expression.
    --
    -- Fields: operand expression, postfix operator.
    --
    -- JavaScript: @x++@, @y--@
    JSExpressionPostfix !JSExpression !JSUnaryOp
  | -- | A ternary (conditional) expression.
    --
    -- Fields: condition, question mark annotation, true branch, colon
    -- annotation, false branch.
    --
    -- JavaScript: @cond ? trueVal : falseVal@
    JSExpressionTernary !JSExpression !JSAnnot !JSExpression !JSAnnot !JSExpression
  | -- | An arrow function expression.
    --
    -- Fields: parameter list, arrow (@=>@) annotation, concise body.
    --
    -- JavaScript: @x => x + 1@, @(a, b) => { return a + b; }@
    JSArrowExpression !JSArrowParameterList !JSAnnot !JSConciseBody
  | -- | A function expression (anonymous or named).
    --
    -- Fields: @function@ annotation, optional function name, opening paren,
    -- parameter list, closing paren, function body block.
    --
    -- JavaScript: @function() {}@, @function add(a, b) { return a + b; }@
    JSFunctionExpression !JSAnnot !JSIdent !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock
  | -- | A generator function expression.
    --
    -- Fields: @function@ annotation, star annotation, optional name, opening
    -- paren, parameter list, closing paren, function body block.
    --
    -- JavaScript: @function*() { yield 1; }@
    JSGeneratorExpression !JSAnnot !JSAnnot !JSIdent !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock
  | -- | An async function expression.
    --
    -- Fields: @async@ annotation, @function@ annotation, optional name,
    -- opening paren, parameter list, closing paren, function body block.
    --
    -- JavaScript: @async function() { await fetch(url); }@
    JSAsyncFunctionExpression !JSAnnot !JSAnnot !JSIdent !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock
  | -- | An async arrow function expression.
    --
    -- Fields: @async@ annotation, parameter list, arrow (@=>@) annotation,
    -- concise body.
    --
    -- JavaScript: @async x => await x@, @async (a, b) => { ... }@
    JSAsyncArrowExpression !JSAnnot !JSArrowParameterList !JSAnnot !JSConciseBody
  | -- | An async generator function expression.
    --
    -- Fields: @async@ annotation, @function@ annotation, star annotation,
    -- optional name, opening paren, parameter list, closing paren, body block.
    --
    -- JavaScript: @async function*() { yield await fetch(url); }@
    JSAsyncGeneratorExpression !JSAnnot !JSAnnot !JSAnnot !JSIdent !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock
  | -- | Dot-notation member access.
    --
    -- Fields: object expression, dot annotation, property name expression.
    --
    -- JavaScript: @obj.prop@, @arr.length@
    JSMemberDot !JSExpression !JSAnnot !JSExpression
  | -- | Private field dot-notation member access (ES2022).
    --
    -- Fields: object expression, dot annotation, hash annotation, private
    -- field name (without the @#@ prefix).
    --
    -- JavaScript: @obj.#field@
    JSMemberPrivateDot !JSExpression !JSAnnot !JSAnnot !ByteString
  | -- | Function call via member expression (method invocation).
    --
    -- Fields: callee expression, opening paren, arguments, closing paren.
    --
    -- JavaScript: @obj.method(arg1, arg2)@
    JSMemberExpression !JSExpression !JSAnnot !(JSCommaList JSExpression) !JSAnnot
  | -- | A @new@ expression with arguments.
    --
    -- Fields: @new@ annotation, constructor expression, opening paren,
    -- arguments, closing paren.
    --
    -- JavaScript: @new Foo(1, 2)@
    JSMemberNew !JSAnnot !JSExpression !JSAnnot !(JSCommaList JSExpression) !JSAnnot
  | -- | Bracket-notation (computed) member access.
    --
    -- Fields: object expression, opening bracket, index expression, closing
    -- bracket.
    --
    -- JavaScript: @obj["prop"]@, @arr[0]@
    JSMemberSquare !JSExpression !JSAnnot !JSExpression !JSAnnot
  | -- | A @new@ expression without arguments.
    --
    -- Fields: @new@ annotation, constructor expression.
    --
    -- JavaScript: @new Foo@
    JSNewExpression !JSAnnot !JSExpression
  | -- | Optional chaining dot-notation member access (ES2020).
    --
    -- Fields: object expression, @?.@ annotation, property name expression.
    --
    -- JavaScript: @obj?.prop@
    JSOptionalMemberDot !JSExpression !JSAnnot !JSExpression
  | -- | Optional chaining bracket-notation member access (ES2020).
    --
    -- Fields: object expression, @?.[@ annotation, index expression,
    -- closing bracket.
    --
    -- JavaScript: @obj?.[key]@
    JSOptionalMemberSquare !JSExpression !JSAnnot !JSExpression !JSAnnot
  | -- | Optional chaining function call (ES2020).
    --
    -- Fields: callee expression, @?.(@ annotation, arguments, closing paren.
    --
    -- JavaScript: @obj?.method(arg)@
    JSOptionalCallExpression !JSExpression !JSAnnot !(JSCommaList JSExpression) !JSAnnot
  | -- | An object literal expression.
    --
    -- Fields: opening brace annotation, property list, closing brace annotation.
    --
    -- JavaScript: @{ key: value, method() {} }@
    JSObjectLiteral !JSAnnot !JSObjectPropertyList !JSAnnot
  | -- | A spread expression (ES2015).
    --
    -- Fields: @...@ annotation, spread operand expression.
    --
    -- JavaScript: @...arr@, @...obj@ (in array literals, function calls, etc.)
    JSSpreadExpression !JSAnnot !JSExpression
  | -- | A template literal, optionally tagged.
    --
    -- Fields: optional tag expression, opening backtick annotation, head
    -- string (before first @${@), template parts (expression + suffix pairs).
    --
    -- JavaScript: @\`hello ${name}\`@, @html\`\<div>${content}\<\/div>\`@
    JSTemplateLiteral !(Maybe JSExpression) !JSAnnot !ByteString ![JSTemplatePart]
  | -- | A prefix unary expression.
    --
    -- Fields: unary operator, operand expression.
    --
    -- JavaScript: @!x@, @-y@, @typeof z@, @++i@
    JSUnaryExpression !JSUnaryOp !JSExpression
  | -- | A variable declarator with optional initializer.
    --
    -- Fields: variable name expression (may be a pattern), initializer.
    -- Used within @var@\/@let@\/@const@ declarations.
    --
    -- JavaScript: @x = 42@ (within @var x = 42;@)
    JSVarInitExpression !JSExpression !JSVarInitializer
  | -- | A @yield@ expression (inside generator functions).
    --
    -- Fields: @yield@ annotation, optional yielded expression.
    --
    -- JavaScript: @yield@, @yield value@
    JSYieldExpression !JSAnnot !(Maybe JSExpression)
  | -- | A @yield*@ (delegating yield) expression.
    --
    -- Fields: @yield@ annotation, star annotation, delegated iterable
    -- expression.
    --
    -- JavaScript: @yield* otherGenerator()@
    JSYieldFromExpression !JSAnnot !JSAnnot !JSExpression
  | -- | The @import.meta@ meta-property (ES2020).
    --
    -- Fields: @import@ annotation, @.meta@ annotation.
    --
    -- JavaScript: @import.meta@, @import.meta.url@
    JSImportMeta !JSAnnot !JSAnnot
  | -- | A dynamic @import()@ call expression (ES2020).
    --
    -- Fields: @import@ annotation, opening paren, module specifier expression,
    -- closing paren.
    --
    -- JavaScript: @import('./module.js')@
    JSImportCall !JSAnnot !JSAnnot !JSExpression !JSAnnot
  | -- | A private identifier (ES2022), used for private-in-object checks.
    --
    -- Fields: hash annotation, private name (without the @#@ prefix).
    --
    -- JavaScript: @#x@ (in @#x in obj@)
    JSPrivateIdentifier !JSAnnot !ByteString
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | Parameter list for an arrow function expression.
--
-- Arrow functions accept either a single unparenthesized identifier or a
-- parenthesized comma-separated parameter list.
data JSArrowParameterList
  = -- | A single identifier parameter without parentheses.
    --
    -- Fields: parameter identifier.
    --
    -- JavaScript: @x@ (in @x => x + 1@)
    JSUnparenthesizedArrowParameter !JSIdent
  | -- | A parenthesized parameter list (zero or more parameters).
    --
    -- Fields: opening paren, comma-separated parameter expressions, closing
    -- paren.
    --
    -- JavaScript: @(a, b)@ (in @(a, b) => a + b@), @()@ (in @() => 42@)
    JSParenthesizedArrowParameterList !JSAnnot !(JSCommaList JSExpression) !JSAnnot
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | The body of an arrow function, either a block or a concise expression.
data JSConciseBody
  = -- | A block body enclosed in braces (requires explicit @return@).
    --
    -- Fields: function body block.
    --
    -- JavaScript: @{ return a + b; }@ (in @(a, b) => { return a + b; }@)
    JSConciseFunctionBody !JSBlock
  | -- | A concise expression body (implicit return of the expression value).
    --
    -- Fields: body expression.
    --
    -- JavaScript: @a + b@ (in @(a, b) => a + b@)
    JSConciseExpressionBody !JSExpression
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | JavaScript binary operators.
--
-- Each constructor carries a 'JSAnnot' for the operator token's position
-- and surrounding whitespace\/comments.
data JSBinOp
  = -- | Logical AND operator (@&&@). Short-circuits: right operand not evaluated if left is falsy.
    JSBinOpAnd !JSAnnot
  | -- | Bitwise AND operator (@&@). Performs bitwise AND on integer operands.
    JSBinOpBitAnd !JSAnnot
  | -- | Bitwise OR operator (@|@). Performs bitwise OR on integer operands.
    JSBinOpBitOr !JSAnnot
  | -- | Bitwise XOR operator (@^@). Performs bitwise exclusive OR on integer operands.
    JSBinOpBitXor !JSAnnot
  | -- | Division operator (@\/@).
    JSBinOpDivide !JSAnnot
  | -- | Loose equality operator (@==@). Performs type coercion before comparison.
    JSBinOpEq !JSAnnot
  | -- | Exponentiation operator (@**@, ES2016). Equivalent to @Math.pow@.
    JSBinOpExponentiation !JSAnnot
  | -- | Greater-than-or-equal operator (@>=@).
    JSBinOpGe !JSAnnot
  | -- | Greater-than operator (@>@).
    JSBinOpGt !JSAnnot
  | -- | The @in@ operator. Tests whether a property exists in an object.
    JSBinOpIn !JSAnnot
  | -- | The @instanceof@ operator. Tests prototype chain membership.
    JSBinOpInstanceOf !JSAnnot
  | -- | Less-than-or-equal operator (@\<=@).
    JSBinOpLe !JSAnnot
  | -- | Left shift operator (@\<\<@). Shifts bits left, filling with zeros.
    JSBinOpLsh !JSAnnot
  | -- | Less-than operator (@\<@).
    JSBinOpLt !JSAnnot
  | -- | Subtraction operator (@-@).
    JSBinOpMinus !JSAnnot
  | -- | Remainder (modulo) operator (@%@).
    JSBinOpMod !JSAnnot
  | -- | Loose inequality operator (@!=@). Performs type coercion before comparison.
    JSBinOpNeq !JSAnnot
  | -- | The @of@ keyword used as an operator in @for...of@ loops.
    JSBinOpOf !JSAnnot
  | -- | Logical OR operator (@||@). Short-circuits: right operand not evaluated if left is truthy.
    JSBinOpOr !JSAnnot
  | -- | Nullish coalescing operator (@??@, ES2020). Returns right operand when left is @null@ or @undefined@.
    JSBinOpNullishCoalescing !JSAnnot
  | -- | Addition operator (@+@). Also performs string concatenation.
    JSBinOpPlus !JSAnnot
  | -- | Signed right shift operator (@>>@). Shifts bits right, preserving sign.
    JSBinOpRsh !JSAnnot
  | -- | Strict equality operator (@===@). No type coercion.
    JSBinOpStrictEq !JSAnnot
  | -- | Strict inequality operator (@!==@). No type coercion.
    JSBinOpStrictNeq !JSAnnot
  | -- | Multiplication operator (@*@).
    JSBinOpTimes !JSAnnot
  | -- | Unsigned right shift operator (@>>>@). Shifts bits right, filling with zeros.
    JSBinOpUrsh !JSAnnot
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | JavaScript unary operators (prefix and postfix).
--
-- Used in both prefix (@++x@, @!x@) and postfix (@x++@, @x--@) positions.
-- Each constructor carries a 'JSAnnot' for the operator token's position.
data JSUnaryOp
  = -- | Decrement operator (@--@). Used as prefix (@--x@) or postfix (@x--@).
    JSUnaryOpDecr !JSAnnot
  | -- | The @delete@ operator. Removes a property from an object.
    JSUnaryOpDelete !JSAnnot
  | -- | Increment operator (@++@). Used as prefix (@++x@) or postfix (@x++@).
    JSUnaryOpIncr !JSAnnot
  | -- | Unary negation operator (@-@). Negates its numeric operand.
    JSUnaryOpMinus !JSAnnot
  | -- | Logical NOT operator (@!@). Returns @true@ if operand is falsy.
    JSUnaryOpNot !JSAnnot
  | -- | Unary plus operator (@+@). Attempts to convert operand to a number.
    JSUnaryOpPlus !JSAnnot
  | -- | Bitwise NOT operator (@~@). Inverts all bits of its operand.
    JSUnaryOpTilde !JSAnnot
  | -- | The @typeof@ operator. Returns a string indicating the type of its operand.
    JSUnaryOpTypeof !JSAnnot
  | -- | The @void@ operator. Evaluates expression and returns @undefined@.
    JSUnaryOpVoid !JSAnnot
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | Semicolon representation, distinguishing explicit from automatically
-- inserted semicolons (ASI).
data JSSemi
  = -- | An explicit semicolon token present in the source.
    --
    -- Fields: semicolon token annotation.
    --
    -- JavaScript: the @;@ in @var x = 1;@
    JSSemi !JSAnnot
  | -- | An automatically inserted semicolon (ASI).
    --
    -- Represents a semicolon that was not present in the source but was
    -- inferred by JavaScript's automatic semicolon insertion rules.
    --
    -- JavaScript: the implicit @;@ after @return x@ when followed by a newline
    JSSemiAuto
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | JavaScript assignment operators.
--
-- Includes simple assignment and all compound assignment operators.
-- Each constructor carries a 'JSAnnot' for the operator token's position.
data JSAssignOp
  = -- | Simple assignment operator (@=@).
    JSAssign !JSAnnot
  | -- | Multiplication assignment operator (@*=@).
    JSTimesAssign !JSAnnot
  | -- | Division assignment operator (@\/=@).
    JSDivideAssign !JSAnnot
  | -- | Remainder assignment operator (@%=@).
    JSModAssign !JSAnnot
  | -- | Addition assignment operator (@+=@).
    JSPlusAssign !JSAnnot
  | -- | Subtraction assignment operator (@-=@).
    JSMinusAssign !JSAnnot
  | -- | Left shift assignment operator (@\<\<=@).
    JSLshAssign !JSAnnot
  | -- | Signed right shift assignment operator (@>>=@).
    JSRshAssign !JSAnnot
  | -- | Unsigned right shift assignment operator (@>>>=@).
    JSUrshAssign !JSAnnot
  | -- | Bitwise AND assignment operator (@&=@).
    JSBwAndAssign !JSAnnot
  | -- | Bitwise XOR assignment operator (@^=@).
    JSBwXorAssign !JSAnnot
  | -- | Bitwise OR assignment operator (@|=@).
    JSBwOrAssign !JSAnnot
  | -- | Logical AND assignment operator (@&&=@, ES2021). Assigns only if left operand is truthy.
    JSLogicalAndAssign !JSAnnot
  | -- | Logical OR assignment operator (@||=@, ES2021). Assigns only if left operand is falsy.
    JSLogicalOrAssign !JSAnnot
  | -- | Nullish coalescing assignment operator (@??=@, ES2021). Assigns only if left is @null@\/@undefined@.
    JSNullishAssign !JSAnnot
  | -- | Exponentiation assignment operator (@**=@, ES2016).
    JSExponentiationAssign !JSAnnot
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | A @catch@ clause in a @try@ statement.
data JSTryCatch
  = -- | A standard @catch@ clause with a parameter binding.
    --
    -- Fields: @catch@ annotation, opening paren, catch parameter expression,
    -- closing paren, catch body block.
    --
    -- JavaScript: @catch (e) { handleError(e); }@
    JSCatch !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSBlock
  | -- | A conditional @catch@ clause (Mozilla extension, non-standard).
    --
    -- Fields: @catch@ annotation, opening paren, catch parameter, @if@
    -- annotation, guard expression, closing paren, catch body block.
    --
    -- JavaScript: @catch (e if e instanceof TypeError) { ... }@
    JSCatchIf !JSAnnot !JSAnnot !JSExpression !JSAnnot !JSExpression !JSAnnot !JSBlock
  | -- | A @catch@ clause without a parameter binding (ES2019 optional catch binding).
    --
    -- Fields: @catch@ annotation, catch body block.
    --
    -- JavaScript: @catch { handleError(); }@
    JSCatchNoParam !JSAnnot !JSBlock
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | An optional @finally@ clause in a @try@ statement.
data JSTryFinally
  = -- | A @finally@ clause that always executes after try\/catch.
    --
    -- Fields: @finally@ annotation, finally body block.
    --
    -- JavaScript: @finally { cleanup(); }@
    JSFinally !JSAnnot !JSBlock
  | -- | No @finally@ clause present.
    JSNoFinally
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | A brace-delimited block of statements.
--
-- Used for function bodies, control flow bodies, and standalone blocks.
data JSBlock
  = -- | A block statement enclosed in braces.
    --
    -- Fields: opening brace annotation, list of statements, closing brace
    -- annotation.
    --
    -- JavaScript: @{ stmt1; stmt2; }@
    JSBlock !JSAnnot ![JSStatement] !JSAnnot
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | A @case@ or @default@ clause within a @switch@ statement body.
data JSSwitchParts
  = -- | A @case@ clause matching a specific value.
    --
    -- Fields: @case@ annotation, match expression, colon annotation,
    -- consequent statements.
    --
    -- JavaScript: @case 42: doSomething(); break;@
    JSCase !JSAnnot !JSExpression !JSAnnot ![JSStatement]
  | -- | A @default@ clause (fallback when no case matches).
    --
    -- Fields: @default@ annotation, colon annotation, consequent statements.
    --
    -- JavaScript: @default: handleDefault(); break;@
    JSDefault !JSAnnot !JSAnnot ![JSStatement]
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | An optional variable initializer (the @= value@ part of a declaration).
data JSVarInitializer
  = -- | An initializer with an equals sign and expression.
    --
    -- Fields: equals sign annotation, initializer expression.
    --
    -- JavaScript: @= 42@ (in @var x = 42;@)
    JSVarInit !JSAnnot !JSExpression
  | -- | No initializer present (variable declared without assignment).
    --
    -- JavaScript: @var x;@ (the @x@ has no initializer)
    JSVarInitNone
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | A property definition within an object literal.
data JSObjectProperty
  = -- | A key-value property with explicit colon syntax.
    --
    -- Fields: property name, colon annotation, value expressions.
    --
    -- JavaScript: @key: value@ (in @{ key: value }@)
    JSPropertyNameandValue !JSPropertyName !JSAnnot ![JSExpression]
  | -- | A shorthand property using an identifier reference (ES2015).
    --
    -- Fields: annotation, identifier name. The property name and value
    -- are both the same identifier.
    --
    -- JavaScript: @x@ (in @{ x }@, equivalent to @{ x: x }@)
    JSPropertyIdentRef !JSAnnot !ByteString
  | -- | A method definition within an object literal.
    --
    -- JavaScript: @method() {}@ (in @{ method() {} }@)
    JSObjectMethod !JSMethodDefinition
  | -- | A spread property (ES2018 object spread).
    --
    -- Fields: @...@ annotation, spread operand expression.
    --
    -- JavaScript: @...other@ (in @{ ...other, x: 1 }@)
    JSObjectSpread !JSAnnot !JSExpression
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | A method definition, used in object literals and class bodies.
data JSMethodDefinition
  = -- | A regular method definition.
    --
    -- Fields: method name, opening paren, parameter list, closing paren,
    -- method body block.
    --
    -- JavaScript: @foo(a, b) { return a + b; }@
    JSMethodDefinition !JSPropertyName !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock
  | -- | A generator method definition.
    --
    -- Fields: star annotation, method name, opening paren, parameter list,
    -- closing paren, method body block.
    --
    -- JavaScript: @*items() { yield 1; yield 2; }@
    JSGeneratorMethodDefinition !JSAnnot !JSPropertyName !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock
  | -- | An async method definition.
    --
    -- Fields: @async@ annotation, method name, opening paren, parameter list,
    -- closing paren, method body block.
    --
    -- JavaScript: @async fetch(url) { return await fetch(url); }@
    JSAsyncMethodDefinition !JSAnnot !JSPropertyName !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock
  | -- | A getter or setter accessor method.
    --
    -- Fields: accessor type (get\/set), property name, opening paren,
    -- parameter list, closing paren, method body block.
    --
    -- JavaScript: @get name() { return this._name; }@, @set name(v) { this._name = v; }@
    JSPropertyAccessor !JSAccessor !JSPropertyName !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | The name of a property in an object literal, class method, or similar context.
data JSPropertyName
  = -- | An identifier property name.
    --
    -- Fields: annotation, identifier name.
    --
    -- JavaScript: @foo@ (in @{ foo: 1 }@)
    JSPropertyIdent !JSAnnot !ByteString
  | -- | A string literal property name.
    --
    -- Fields: annotation, string content (including quotes).
    --
    -- JavaScript: @"foo"@ (in @{ "foo": 1 }@)
    JSPropertyString !JSAnnot !ByteString
  | -- | A numeric literal property name.
    --
    -- Fields: annotation, numeric literal text.
    --
    -- JavaScript: @42@ (in @{ 42: "answer" }@)
    JSPropertyNumber !JSAnnot !ByteString
  | -- | A computed property name (ES2015).
    --
    -- Fields: opening bracket annotation, name expression, closing bracket
    -- annotation.
    --
    -- JavaScript: @[expr]@ (in @{ [Symbol.iterator]() {} }@)
    JSPropertyComputed !JSAnnot !JSExpression !JSAnnot
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

type JSObjectPropertyList = JSCommaTrailingList JSObjectProperty

-- | Accessor type for getter and setter property definitions.
data JSAccessor
  = -- | A getter accessor (@get@). Defines a property that is read by calling a function.
    --
    -- JavaScript: @get@ (in @get name() { return this._name; }@)
    JSAccessorGet !JSAnnot
  | -- | A setter accessor (@set@). Defines a property that is written by calling a function.
    --
    -- JavaScript: @set@ (in @set name(v) { this._name = v; }@)
    JSAccessorSet !JSAnnot
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | An optional identifier, used where a name may or may not be present
-- (e.g. function names, break\/continue labels, class names).
data JSIdent
  = -- | A named identifier.
    --
    -- Fields: annotation, identifier name as raw bytes.
    --
    -- JavaScript: @foo@ (in @function foo() {}@)
    JSIdentName !JSAnnot !ByteString
  | -- | No identifier present (anonymous function, unlabelled break, etc.).
    --
    -- JavaScript: the absent name in @function() {}@ or @break;@
    JSIdentNone
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | An element within an array literal, which may be an expression or
-- an elision (hole).
data JSArrayElement
  = -- | An array element containing an expression.
    --
    -- Fields: element expression.
    --
    -- JavaScript: @42@ (in @[42, "hello"]@)
    JSArrayElement !JSExpression
  | -- | An elision (hole) represented by a comma without a preceding expression.
    --
    -- Fields: comma annotation.
    --
    -- JavaScript: the missing element in @[1, , 3]@ (the gap between commas)
    JSArrayComma !JSAnnot
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | A comma-separated list that preserves comma annotations.
--
-- Represents zero or more elements separated by commas, as found in
-- function parameter lists, argument lists, variable declarations, etc.
-- Use 'fromCommaList' to convert to a regular Haskell list.
data JSCommaList a
  = -- | A list with at least two elements: a head list, a comma, and a
    -- tail element.
    --
    -- Fields: preceding comma list, comma annotation, last element.
    --
    -- JavaScript: @a, b, c@ is represented as @JSLCons (JSLCons (JSLOne a) comma b) comma c@
    JSLCons !(JSCommaList a) !JSAnnot !a
  | -- | A single-element list (no comma).
    --
    -- Fields: the single element.
    --
    -- JavaScript: @x@ (a list with exactly one item)
    JSLOne !a
  | -- | An empty list (no elements).
    --
    -- JavaScript: the empty parameter list in @function() {}@
    JSLNil
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | A comma-separated list that may have a trailing comma.
--
-- Used for object properties and other contexts where JavaScript allows
-- an optional trailing comma after the last element.
data JSCommaTrailingList a
  = -- | A list with a trailing comma after the last element.
    --
    -- Fields: comma list of elements, trailing comma annotation.
    --
    -- JavaScript: @a, b, c,@ (note the trailing comma)
    JSCTLComma !(JSCommaList a) !JSAnnot
  | -- | A list without a trailing comma.
    --
    -- Fields: comma list of elements.
    --
    -- JavaScript: @a, b, c@ (no trailing comma)
    JSCTLNone !(JSCommaList a)
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | A part of a template literal containing an interpolated expression
-- followed by a string suffix.
data JSTemplatePart
  = -- | An interpolated expression within a template literal.
    --
    -- Fields: interpolated expression (between @${@ and @}@), closing brace
    -- annotation, string suffix (text after @}@ until the next @${@ or
    -- closing backtick).
    --
    -- JavaScript: @${name} world@ (in @\`hello ${name} world\`@)
    JSTemplatePart !JSExpression !JSAnnot !ByteString
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | An optional class heritage (extends) clause.
data JSClassHeritage
  = -- | An @extends@ clause specifying a superclass.
    --
    -- Fields: @extends@ annotation, superclass expression.
    --
    -- JavaScript: @extends BaseClass@ (in @class Foo extends BaseClass {}@)
    JSExtends !JSAnnot !JSExpression
  | -- | No @extends@ clause (class has no explicit superclass).
    --
    -- JavaScript: @class Foo {}@ (no extends)
    JSExtendsNone
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | An element within a class body (method, field, or static block).
data JSClassElement
  = -- | An instance method definition.
    --
    -- JavaScript: @method() {}@ (in a class body)
    JSClassInstanceMethod !JSMethodDefinition
  | -- | A static method definition.
    --
    -- Fields: @static@ annotation, method definition.
    --
    -- JavaScript: @static create() { return new this(); }@
    JSClassStaticMethod !JSAnnot !JSMethodDefinition
  | -- | An empty class element (bare semicolon in class body).
    --
    -- Fields: semicolon annotation.
    --
    -- JavaScript: @;@ (in a class body)
    JSClassSemi !JSAnnot
  | -- | A private instance field (ES2022).
    --
    -- Fields: hash annotation, field name (without @#@), equals annotation,
    -- optional initializer expression, auto-semicolon.
    --
    -- JavaScript: @#count = 0;@
    JSPrivateField !JSAnnot !ByteString !JSAnnot !(Maybe JSExpression) !JSSemi
  | -- | A private instance method (ES2022).
    --
    -- Fields: hash annotation, method name (without @#@), opening paren,
    -- parameter list, closing paren, method body block.
    --
    -- JavaScript: @#validate(input) { ... }@
    JSPrivateMethod !JSAnnot !ByteString !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock
  | -- | A private getter or setter accessor (ES2022).
    --
    -- Fields: accessor type (get\/set), hash annotation, field name
    -- (without @#@), opening paren, parameter list, closing paren, body block.
    --
    -- JavaScript: @get #value() { return this.#_value; }@
    JSPrivateAccessor !JSAccessor !JSAnnot !ByteString !JSAnnot !(JSCommaList JSExpression) !JSAnnot !JSBlock
  | -- | A public instance field (ES2022).
    --
    -- Fields: field name, equals annotation, optional initializer expression,
    -- auto-semicolon.
    --
    -- JavaScript: @count = 0;@ (in a class body)
    JSClassField !JSPropertyName !JSAnnot !(Maybe JSExpression) !JSSemi
  | -- | A static field (ES2022).
    --
    -- Fields: @static@ annotation, field name, equals annotation, optional
    -- initializer expression, auto-semicolon.
    --
    -- JavaScript: @static defaultValue = 42;@
    JSClassStaticField !JSAnnot !JSPropertyName !JSAnnot !(Maybe JSExpression) !JSSemi
  | -- | A static initialization block (ES2022).
    --
    -- Fields: @static@ annotation, block body.
    --
    -- JavaScript: @static { this.initialize(); }@
    JSClassStaticBlock !JSAnnot !JSBlock
  | -- | An async generator method definition in a class body.
    --
    -- Fields: @async@ annotation, star annotation, method name, opening paren,
    -- parameter list, closing paren, method body block.
    --
    -- JavaScript: @async *stream() { yield await fetch(url); }@
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
  -- Uses difference lists internally for O(n) instead of O(n²) concatenation.
  foldAnnot :: (JSAnnot -> [b]) -> a -> [b]
  foldAnnot f x = foldAnnotDL f x []
  -- | Difference list version of 'foldAnnot' for O(n) accumulation.
  foldAnnotDL :: (JSAnnot -> [b]) -> a -> [b] -> [b]
  foldAnnotDL f x rest = foldAnnot f x ++ rest
  {-# MINIMAL mapAnnot, (foldAnnot | foldAnnotDL) #-}

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
  foldAnnotDL f (JSVarInit a e) rest = f a ++ foldAnnotDL f e rest
  foldAnnotDL _ JSVarInitNone rest = rest

instance HasAnnot JSClassHeritage where
  mapAnnot f (JSExtends a e) = JSExtends (f a) (mapAnnot f e)
  mapAnnot _ JSExtendsNone = JSExtendsNone
  foldAnnotDL f (JSExtends a e) rest = f a ++ foldAnnotDL f e rest
  foldAnnotDL _ JSExtendsNone rest = rest

instance HasAnnot JSTryFinally where
  mapAnnot f (JSFinally a b) = JSFinally (f a) (mapAnnot f b)
  mapAnnot _ JSNoFinally = JSNoFinally
  foldAnnotDL f (JSFinally a b) rest = f a ++ foldAnnotDL f b rest
  foldAnnotDL _ JSNoFinally rest = rest

instance HasAnnot JSBlock where
  mapAnnot f (JSBlock a1 stmts a2) = JSBlock (f a1) (map (mapAnnot f) stmts) (f a2)
  foldAnnotDL f (JSBlock a1 stmts a2) rest = f a1 ++ foldr (foldAnnotDL f) (f a2 ++ rest) stmts

instance HasAnnot a => HasAnnot (JSCommaList a) where
  mapAnnot f (JSLCons xs a x) = JSLCons (mapAnnot f xs) (f a) (mapAnnot f x)
  mapAnnot f (JSLOne x) = JSLOne (mapAnnot f x)
  mapAnnot _ JSLNil = JSLNil
  foldAnnotDL f (JSLCons xs a x) rest = foldAnnotDL f xs (f a ++ foldAnnotDL f x rest)
  foldAnnotDL f (JSLOne x) rest = foldAnnotDL f x rest
  foldAnnotDL _ JSLNil rest = rest

instance HasAnnot a => HasAnnot (JSCommaTrailingList a) where
  mapAnnot f (JSCTLComma xs a) = JSCTLComma (mapAnnot f xs) (f a)
  mapAnnot f (JSCTLNone xs) = JSCTLNone (mapAnnot f xs)
  foldAnnotDL f (JSCTLComma xs a) rest = foldAnnotDL f xs (f a ++ rest)
  foldAnnotDL f (JSCTLNone xs) rest = foldAnnotDL f xs rest

instance HasAnnot JSArrayElement where
  mapAnnot f (JSArrayElement e) = JSArrayElement (mapAnnot f e)
  mapAnnot f (JSArrayComma a) = JSArrayComma (f a)
  foldAnnotDL f (JSArrayElement e) rest = foldAnnotDL f e rest
  foldAnnotDL f (JSArrayComma a) rest = f a ++ rest

instance HasAnnot JSTemplatePart where
  mapAnnot f (JSTemplatePart e a s) = JSTemplatePart (mapAnnot f e) (f a) s
  foldAnnotDL f (JSTemplatePart e a _) rest = foldAnnotDL f e (f a ++ rest)

instance HasAnnot JSSwitchParts where
  mapAnnot f (JSCase a1 e a2 stmts) = JSCase (f a1) (mapAnnot f e) (f a2) (map (mapAnnot f) stmts)
  mapAnnot f (JSDefault a1 a2 stmts) = JSDefault (f a1) (f a2) (map (mapAnnot f) stmts)
  foldAnnotDL f (JSCase a1 e a2 stmts) rest = f a1 ++ foldAnnotDL f e (f a2 ++ foldr (foldAnnotDL f) rest stmts)
  foldAnnotDL f (JSDefault a1 a2 stmts) rest = f a1 ++ f a2 ++ foldr (foldAnnotDL f) rest stmts

instance HasAnnot JSTryCatch where
  mapAnnot f (JSCatch a1 a2 e a3 b) = JSCatch (f a1) (f a2) (mapAnnot f e) (f a3) (mapAnnot f b)
  mapAnnot f (JSCatchIf a1 a2 e1 a3 e2 a4 b) = JSCatchIf (f a1) (f a2) (mapAnnot f e1) (f a3) (mapAnnot f e2) (f a4) (mapAnnot f b)
  mapAnnot f (JSCatchNoParam a1 b) = JSCatchNoParam (f a1) (mapAnnot f b)
  foldAnnotDL f (JSCatch a1 a2 e a3 b) rest = f a1 ++ f a2 ++ foldAnnotDL f e (f a3 ++ foldAnnotDL f b rest)
  foldAnnotDL f (JSCatchIf a1 a2 e1 a3 e2 a4 b) rest = f a1 ++ f a2 ++ foldAnnotDL f e1 (f a3 ++ foldAnnotDL f e2 (f a4 ++ foldAnnotDL f b rest))
  foldAnnotDL f (JSCatchNoParam a1 b) rest = f a1 ++ foldAnnotDL f b rest

instance HasAnnot JSPropertyName where
  mapAnnot f (JSPropertyIdent a s) = JSPropertyIdent (f a) s
  mapAnnot f (JSPropertyString a s) = JSPropertyString (f a) s
  mapAnnot f (JSPropertyNumber a s) = JSPropertyNumber (f a) s
  mapAnnot f (JSPropertyComputed a1 e a2) = JSPropertyComputed (f a1) (mapAnnot f e) (f a2)
  foldAnnotDL f (JSPropertyIdent a _) rest = f a ++ rest
  foldAnnotDL f (JSPropertyString a _) rest = f a ++ rest
  foldAnnotDL f (JSPropertyNumber a _) rest = f a ++ rest
  foldAnnotDL f (JSPropertyComputed a1 e a2) rest = f a1 ++ foldAnnotDL f e (f a2 ++ rest)

instance HasAnnot JSObjectProperty where
  mapAnnot f (JSPropertyNameandValue n a es) = JSPropertyNameandValue (mapAnnot f n) (f a) (map (mapAnnot f) es)
  mapAnnot f (JSPropertyIdentRef a s) = JSPropertyIdentRef (f a) s
  mapAnnot f (JSObjectMethod m) = JSObjectMethod (mapAnnot f m)
  mapAnnot f (JSObjectSpread a e) = JSObjectSpread (f a) (mapAnnot f e)
  foldAnnotDL f (JSPropertyNameandValue n a es) rest = foldAnnotDL f n (f a ++ foldr (foldAnnotDL f) rest es)
  foldAnnotDL f (JSPropertyIdentRef a _) rest = f a ++ rest
  foldAnnotDL f (JSObjectMethod m) rest = foldAnnotDL f m rest
  foldAnnotDL f (JSObjectSpread a e) rest = f a ++ foldAnnotDL f e rest

instance HasAnnot JSMethodDefinition where
  mapAnnot f (JSMethodDefinition n a1 ps a2 b) = JSMethodDefinition (mapAnnot f n) (f a1) (mapAnnot f ps) (f a2) (mapAnnot f b)
  mapAnnot f (JSGeneratorMethodDefinition a1 n a2 ps a3 b) = JSGeneratorMethodDefinition (f a1) (mapAnnot f n) (f a2) (mapAnnot f ps) (f a3) (mapAnnot f b)
  mapAnnot f (JSAsyncMethodDefinition a1 n a2 ps a3 b) = JSAsyncMethodDefinition (f a1) (mapAnnot f n) (f a2) (mapAnnot f ps) (f a3) (mapAnnot f b)
  mapAnnot f (JSPropertyAccessor acc n a1 ps a2 b) = JSPropertyAccessor (mapAnnot f acc) (mapAnnot f n) (f a1) (mapAnnot f ps) (f a2) (mapAnnot f b)
  foldAnnotDL f (JSMethodDefinition n a1 ps a2 b) rest = foldAnnotDL f n (f a1 ++ foldAnnotDL f ps (f a2 ++ foldAnnotDL f b rest))
  foldAnnotDL f (JSGeneratorMethodDefinition a1 n a2 ps a3 b) rest = f a1 ++ foldAnnotDL f n (f a2 ++ foldAnnotDL f ps (f a3 ++ foldAnnotDL f b rest))
  foldAnnotDL f (JSAsyncMethodDefinition a1 n a2 ps a3 b) rest = f a1 ++ foldAnnotDL f n (f a2 ++ foldAnnotDL f ps (f a3 ++ foldAnnotDL f b rest))
  foldAnnotDL f (JSPropertyAccessor acc n a1 ps a2 b) rest = foldAnnotDL f acc (foldAnnotDL f n (f a1 ++ foldAnnotDL f ps (f a2 ++ foldAnnotDL f b rest)))

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
  foldAnnotDL f (JSClassInstanceMethod m) rest = foldAnnotDL f m rest
  foldAnnotDL f (JSClassStaticMethod a m) rest = f a ++ foldAnnotDL f m rest
  foldAnnotDL f (JSClassSemi a) rest = f a ++ rest
  foldAnnotDL f (JSPrivateField a1 _ a2 mi semi) rest = f a1 ++ f a2 ++ maybe id (\x -> foldAnnotDL f x) mi (foldAnnotDL f semi rest)
  foldAnnotDL f (JSPrivateMethod a1 _ a2 ps a3 b) rest = f a1 ++ f a2 ++ foldAnnotDL f ps (f a3 ++ foldAnnotDL f b rest)
  foldAnnotDL f (JSPrivateAccessor acc a1 _ a2 ps a3 b) rest = foldAnnotDL f acc (f a1 ++ f a2 ++ foldAnnotDL f ps (f a3 ++ foldAnnotDL f b rest))
  foldAnnotDL f (JSClassField n a mi semi) rest = foldAnnotDL f n (f a ++ maybe id (\x -> foldAnnotDL f x) mi (foldAnnotDL f semi rest))
  foldAnnotDL f (JSClassStaticField a1 n a2 mi semi) rest = f a1 ++ foldAnnotDL f n (f a2 ++ maybe id (\x -> foldAnnotDL f x) mi (foldAnnotDL f semi rest))
  foldAnnotDL f (JSClassStaticBlock a b) rest = f a ++ foldAnnotDL f b rest
  foldAnnotDL f (JSAsyncGeneratorMethodDefinition a1 a2 n a3 ps a4 b) rest = f a1 ++ f a2 ++ foldAnnotDL f n (f a3 ++ foldAnnotDL f ps (f a4 ++ foldAnnotDL f b rest))

instance HasAnnot JSArrowParameterList where
  mapAnnot f (JSUnparenthesizedArrowParameter i) = JSUnparenthesizedArrowParameter (mapAnnot f i)
  mapAnnot f (JSParenthesizedArrowParameterList a1 ps a2) = JSParenthesizedArrowParameterList (f a1) (mapAnnot f ps) (f a2)
  foldAnnotDL f (JSUnparenthesizedArrowParameter i) rest = foldAnnotDL f i rest
  foldAnnotDL f (JSParenthesizedArrowParameterList a1 ps a2) rest = f a1 ++ foldAnnotDL f ps (f a2 ++ rest)

instance HasAnnot JSConciseBody where
  mapAnnot f (JSConciseFunctionBody b) = JSConciseFunctionBody (mapAnnot f b)
  mapAnnot f (JSConciseExpressionBody e) = JSConciseExpressionBody (mapAnnot f e)
  foldAnnotDL f (JSConciseFunctionBody b) rest = foldAnnotDL f b rest
  foldAnnotDL f (JSConciseExpressionBody e) rest = foldAnnotDL f e rest

instance HasAnnot JSFromClause where
  mapAnnot f (JSFromClause a1 a2 s) = JSFromClause (f a1) (f a2) s
  foldAnnotDL f (JSFromClause a1 a2 _) rest = f a1 ++ f a2 ++ rest

instance HasAnnot JSImportNameSpace where
  mapAnnot f (JSImportNameSpace op a i) = JSImportNameSpace (mapAnnot f op) (f a) (mapAnnot f i)
  foldAnnotDL f (JSImportNameSpace op a i) rest = foldAnnotDL f op (f a ++ foldAnnotDL f i rest)

instance HasAnnot JSImportsNamed where
  mapAnnot f (JSImportsNamed a1 specs a2) = JSImportsNamed (f a1) (mapAnnot f specs) (f a2)
  foldAnnotDL f (JSImportsNamed a1 specs a2) rest = f a1 ++ foldAnnotDL f specs (f a2 ++ rest)

instance HasAnnot JSImportSpecifier where
  mapAnnot f (JSImportSpecifier i) = JSImportSpecifier (mapAnnot f i)
  mapAnnot f (JSImportSpecifierAs i1 a i2) = JSImportSpecifierAs (mapAnnot f i1) (f a) (mapAnnot f i2)
  foldAnnotDL f (JSImportSpecifier i) rest = foldAnnotDL f i rest
  foldAnnotDL f (JSImportSpecifierAs i1 a i2) rest = foldAnnotDL f i1 (f a ++ foldAnnotDL f i2 rest)

instance HasAnnot JSImportAttributes where
  mapAnnot f (JSImportAttributes a1 attrs a2) = JSImportAttributes (f a1) (mapAnnot f attrs) (f a2)
  foldAnnotDL f (JSImportAttributes a1 attrs a2) rest = f a1 ++ foldAnnotDL f attrs (f a2 ++ rest)

instance HasAnnot JSImportAttribute where
  mapAnnot f (JSImportAttribute key a val) = JSImportAttribute (mapAnnot f key) (f a) (mapAnnot f val)
  foldAnnotDL f (JSImportAttribute key a val) rest = foldAnnotDL f key (f a ++ foldAnnotDL f val rest)

instance HasAnnot JSImportClause where
  mapAnnot f (JSImportClauseDefault i) = JSImportClauseDefault (mapAnnot f i)
  mapAnnot f (JSImportClauseNameSpace ns) = JSImportClauseNameSpace (mapAnnot f ns)
  mapAnnot f (JSImportClauseNamed n) = JSImportClauseNamed (mapAnnot f n)
  mapAnnot f (JSImportClauseDefaultNameSpace i a ns) = JSImportClauseDefaultNameSpace (mapAnnot f i) (f a) (mapAnnot f ns)
  mapAnnot f (JSImportClauseDefaultNamed i a n) = JSImportClauseDefaultNamed (mapAnnot f i) (f a) (mapAnnot f n)
  foldAnnotDL f (JSImportClauseDefault i) rest = foldAnnotDL f i rest
  foldAnnotDL f (JSImportClauseNameSpace ns) rest = foldAnnotDL f ns rest
  foldAnnotDL f (JSImportClauseNamed n) rest = foldAnnotDL f n rest
  foldAnnotDL f (JSImportClauseDefaultNameSpace i a ns) rest = foldAnnotDL f i (f a ++ foldAnnotDL f ns rest)
  foldAnnotDL f (JSImportClauseDefaultNamed i a n) rest = foldAnnotDL f i (f a ++ foldAnnotDL f n rest)

instance HasAnnot JSImportDeclaration where
  mapAnnot f (JSImportDeclaration cl from attrs semi) = JSImportDeclaration (mapAnnot f cl) (mapAnnot f from) (fmap (mapAnnot f) attrs) (mapAnnot f semi)
  mapAnnot f (JSImportDeclarationBare a s attrs semi) = JSImportDeclarationBare (f a) s (fmap (mapAnnot f) attrs) (mapAnnot f semi)
  foldAnnotDL f (JSImportDeclaration cl from attrs semi) rest = foldAnnotDL f cl (foldAnnotDL f from (maybe id (\x -> foldAnnotDL f x) attrs (foldAnnotDL f semi rest)))
  foldAnnotDL f (JSImportDeclarationBare a _ attrs semi) rest = f a ++ maybe id (\x -> foldAnnotDL f x) attrs (foldAnnotDL f semi rest)

instance HasAnnot JSExportSpecifier where
  mapAnnot f (JSExportSpecifier i) = JSExportSpecifier (mapAnnot f i)
  mapAnnot f (JSExportSpecifierAs i1 a i2) = JSExportSpecifierAs (mapAnnot f i1) (f a) (mapAnnot f i2)
  foldAnnotDL f (JSExportSpecifier i) rest = foldAnnotDL f i rest
  foldAnnotDL f (JSExportSpecifierAs i1 a i2) rest = foldAnnotDL f i1 (f a ++ foldAnnotDL f i2 rest)

instance HasAnnot JSExportClause where
  mapAnnot f (JSExportClause a1 specs a2) = JSExportClause (f a1) (mapAnnot f specs) (f a2)
  foldAnnotDL f (JSExportClause a1 specs a2) rest = f a1 ++ foldAnnotDL f specs (f a2 ++ rest)

instance HasAnnot JSExportDeclaration where
  mapAnnot f (JSExportAllFrom star from semi) = JSExportAllFrom (mapAnnot f star) (mapAnnot f from) (mapAnnot f semi)
  mapAnnot f (JSExportAllAsFrom star a i from semi) = JSExportAllAsFrom (mapAnnot f star) (f a) (mapAnnot f i) (mapAnnot f from) (mapAnnot f semi)
  mapAnnot f (JSExportFrom cl from semi) = JSExportFrom (mapAnnot f cl) (mapAnnot f from) (mapAnnot f semi)
  mapAnnot f (JSExportLocals cl semi) = JSExportLocals (mapAnnot f cl) (mapAnnot f semi)
  mapAnnot f (JSExportDefault a stmt semi) = JSExportDefault (f a) (mapAnnot f stmt) (mapAnnot f semi)
  mapAnnot f (JSExport stmt semi) = JSExport (mapAnnot f stmt) (mapAnnot f semi)
  foldAnnotDL f (JSExportAllFrom star from semi) rest = foldAnnotDL f star (foldAnnotDL f from (foldAnnotDL f semi rest))
  foldAnnotDL f (JSExportAllAsFrom star a i from semi) rest = foldAnnotDL f star (f a ++ foldAnnotDL f i (foldAnnotDL f from (foldAnnotDL f semi rest)))
  foldAnnotDL f (JSExportFrom cl from semi) rest = foldAnnotDL f cl (foldAnnotDL f from (foldAnnotDL f semi rest))
  foldAnnotDL f (JSExportLocals cl semi) rest = foldAnnotDL f cl (foldAnnotDL f semi rest)
  foldAnnotDL f (JSExportDefault a stmt semi) rest = f a ++ foldAnnotDL f stmt (foldAnnotDL f semi rest)
  foldAnnotDL f (JSExport stmt semi) rest = foldAnnotDL f stmt (foldAnnotDL f semi rest)

instance HasAnnot JSModuleItem where
  mapAnnot f (JSModuleImportDeclaration a d) = JSModuleImportDeclaration (f a) (mapAnnot f d)
  mapAnnot f (JSModuleExportDeclaration a d) = JSModuleExportDeclaration (f a) (mapAnnot f d)
  mapAnnot f (JSModuleStatementListItem s) = JSModuleStatementListItem (mapAnnot f s)
  foldAnnotDL f (JSModuleImportDeclaration a d) rest = f a ++ foldAnnotDL f d rest
  foldAnnotDL f (JSModuleExportDeclaration a d) rest = f a ++ foldAnnotDL f d rest
  foldAnnotDL f (JSModuleStatementListItem s) rest = foldAnnotDL f s rest

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
  mapAnnot f (JSPrivateIdentifier a name) = JSPrivateIdentifier (f a) name
  foldAnnotDL f (JSIdentifier a _) rest = f a ++ rest
  foldAnnotDL f (JSDecimal a _) rest = f a ++ rest
  foldAnnotDL f (JSLiteral a _) rest = f a ++ rest
  foldAnnotDL f (JSHexInteger a _) rest = f a ++ rest
  foldAnnotDL f (JSBinaryInteger a _) rest = f a ++ rest
  foldAnnotDL f (JSOctal a _) rest = f a ++ rest
  foldAnnotDL f (JSBigIntLiteral a _) rest = f a ++ rest
  foldAnnotDL f (JSStringLiteral a _) rest = f a ++ rest
  foldAnnotDL f (JSRegEx a _) rest = f a ++ rest
  foldAnnotDL f (JSArrayLiteral a1 es a2) rest = f a1 ++ foldr (foldAnnotDL f) (f a2 ++ rest) es
  foldAnnotDL f (JSAssignExpression e1 op e2) rest = foldAnnotDL f e1 (foldAnnotDL f op (foldAnnotDL f e2 rest))
  foldAnnotDL f (JSAwaitExpression a e) rest = f a ++ foldAnnotDL f e rest
  foldAnnotDL f (JSCallExpression e a1 args a2) rest = foldAnnotDL f e (f a1 ++ foldAnnotDL f args (f a2 ++ rest))
  foldAnnotDL f (JSCallExpressionDot e a x) rest = foldAnnotDL f e (f a ++ foldAnnotDL f x rest)
  foldAnnotDL f (JSCallExpressionSquare e a1 x a2) rest = foldAnnotDL f e (f a1 ++ foldAnnotDL f x (f a2 ++ rest))
  foldAnnotDL f (JSClassExpression a1 i h a2 es a3) rest = f a1 ++ foldAnnotDL f i (foldAnnotDL f h (f a2 ++ foldr (foldAnnotDL f) (f a3 ++ rest) es))
  foldAnnotDL f (JSCommaExpression e1 a e2) rest = foldAnnotDL f e1 (f a ++ foldAnnotDL f e2 rest)
  foldAnnotDL f (JSExpressionBinary e1 op e2) rest = foldAnnotDL f e1 (foldAnnotDL f op (foldAnnotDL f e2 rest))
  foldAnnotDL f (JSExpressionParen a1 e a2) rest = f a1 ++ foldAnnotDL f e (f a2 ++ rest)
  foldAnnotDL f (JSExpressionPostfix e op) rest = foldAnnotDL f e (foldAnnotDL f op rest)
  foldAnnotDL f (JSExpressionTernary e1 a1 e2 a2 e3) rest = foldAnnotDL f e1 (f a1 ++ foldAnnotDL f e2 (f a2 ++ foldAnnotDL f e3 rest))
  foldAnnotDL f (JSArrowExpression ps a b) rest = foldAnnotDL f ps (f a ++ foldAnnotDL f b rest)
  foldAnnotDL f (JSFunctionExpression a1 i a2 ps a3 b) rest = f a1 ++ foldAnnotDL f i (f a2 ++ foldAnnotDL f ps (f a3 ++ foldAnnotDL f b rest))
  foldAnnotDL f (JSGeneratorExpression a1 a2 i a3 ps a4 b) rest = f a1 ++ f a2 ++ foldAnnotDL f i (f a3 ++ foldAnnotDL f ps (f a4 ++ foldAnnotDL f b rest))
  foldAnnotDL f (JSAsyncFunctionExpression a1 a2 i a3 ps a4 b) rest = f a1 ++ f a2 ++ foldAnnotDL f i (f a3 ++ foldAnnotDL f ps (f a4 ++ foldAnnotDL f b rest))
  foldAnnotDL f (JSAsyncArrowExpression a1 ps a2 b) rest = f a1 ++ foldAnnotDL f ps (f a2 ++ foldAnnotDL f b rest)
  foldAnnotDL f (JSAsyncGeneratorExpression a1 a2 a3 i a4 ps a5 b) rest = f a1 ++ f a2 ++ f a3 ++ foldAnnotDL f i (f a4 ++ foldAnnotDL f ps (f a5 ++ foldAnnotDL f b rest))
  foldAnnotDL f (JSMemberDot e1 a e2) rest = foldAnnotDL f e1 (f a ++ foldAnnotDL f e2 rest)
  foldAnnotDL f (JSMemberPrivateDot e1 a1 a2 _name) rest = foldAnnotDL f e1 (f a1 ++ f a2 ++ rest)
  foldAnnotDL f (JSMemberExpression e a1 args a2) rest = foldAnnotDL f e (f a1 ++ foldAnnotDL f args (f a2 ++ rest))
  foldAnnotDL f (JSMemberNew a1 e a2 args a3) rest = f a1 ++ foldAnnotDL f e (f a2 ++ foldAnnotDL f args (f a3 ++ rest))
  foldAnnotDL f (JSMemberSquare e a1 x a2) rest = foldAnnotDL f e (f a1 ++ foldAnnotDL f x (f a2 ++ rest))
  foldAnnotDL f (JSNewExpression a e) rest = f a ++ foldAnnotDL f e rest
  foldAnnotDL f (JSOptionalMemberDot e1 a e2) rest = foldAnnotDL f e1 (f a ++ foldAnnotDL f e2 rest)
  foldAnnotDL f (JSOptionalMemberSquare e1 a1 e2 a2) rest = foldAnnotDL f e1 (f a1 ++ foldAnnotDL f e2 (f a2 ++ rest))
  foldAnnotDL f (JSOptionalCallExpression e a1 args a2) rest = foldAnnotDL f e (f a1 ++ foldAnnotDL f args (f a2 ++ rest))
  foldAnnotDL f (JSObjectLiteral a1 props a2) rest = f a1 ++ foldAnnotDL f props (f a2 ++ rest)
  foldAnnotDL f (JSSpreadExpression a e) rest = f a ++ foldAnnotDL f e rest
  foldAnnotDL f (JSTemplateLiteral mt a _ ps) rest = maybe id (\x -> foldAnnotDL f x) mt (f a ++ foldr (foldAnnotDL f) rest ps)
  foldAnnotDL f (JSUnaryExpression op e) rest = foldAnnotDL f op (foldAnnotDL f e rest)
  foldAnnotDL f (JSVarInitExpression e vi) rest = foldAnnotDL f e (foldAnnotDL f vi rest)
  foldAnnotDL f (JSYieldExpression a me) rest = f a ++ maybe id (\x -> foldAnnotDL f x) me rest
  foldAnnotDL f (JSYieldFromExpression a1 a2 e) rest = f a1 ++ f a2 ++ foldAnnotDL f e rest
  foldAnnotDL f (JSImportMeta a1 a2) rest = f a1 ++ f a2 ++ rest
  foldAnnotDL f (JSImportCall a1 a2 e a3) rest = f a1 ++ f a2 ++ foldAnnotDL f e (f a3 ++ rest)
  foldAnnotDL f (JSPrivateIdentifier a _name) rest = f a ++ rest

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
  foldAnnotDL f (JSStatementBlock a1 stmts a2 semi) rest = f a1 ++ foldr (foldAnnotDL f) (f a2 ++ foldAnnotDL f semi rest) stmts
  foldAnnotDL f (JSBreak a i semi) rest = f a ++ foldAnnotDL f i (foldAnnotDL f semi rest)
  foldAnnotDL f (JSLet a es semi) rest = f a ++ foldAnnotDL f es (foldAnnotDL f semi rest)
  foldAnnotDL f (JSClass a1 i h a2 es a3 semi) rest = f a1 ++ foldAnnotDL f i (foldAnnotDL f h (f a2 ++ foldr (foldAnnotDL f) (f a3 ++ foldAnnotDL f semi rest) es))
  foldAnnotDL f (JSConstant a es semi) rest = f a ++ foldAnnotDL f es (foldAnnotDL f semi rest)
  foldAnnotDL f (JSContinue a i semi) rest = f a ++ foldAnnotDL f i (foldAnnotDL f semi rest)
  foldAnnotDL f (JSDoWhile a1 s a2 a3 e a4 semi) rest = f a1 ++ foldAnnotDL f s (f a2 ++ f a3 ++ foldAnnotDL f e (f a4 ++ foldAnnotDL f semi rest))
  foldAnnotDL f (JSFor a1 a2 es1 a3 es2 a4 es3 a5 s) rest = f a1 ++ f a2 ++ foldAnnotDL f es1 (f a3 ++ foldAnnotDL f es2 (f a4 ++ foldAnnotDL f es3 (f a5 ++ foldAnnotDL f s rest)))
  foldAnnotDL f (JSForIn a1 a2 e1 op e2 a3 s) rest = f a1 ++ f a2 ++ foldAnnotDL f e1 (foldAnnotDL f op (foldAnnotDL f e2 (f a3 ++ foldAnnotDL f s rest)))
  foldAnnotDL f (JSForVar a1 a2 a3 es1 a4 es2 a5 es3 a6 s) rest = f a1 ++ f a2 ++ f a3 ++ foldAnnotDL f es1 (f a4 ++ foldAnnotDL f es2 (f a5 ++ foldAnnotDL f es3 (f a6 ++ foldAnnotDL f s rest)))
  foldAnnotDL f (JSForVarIn a1 a2 a3 e1 op e2 a4 s) rest = f a1 ++ f a2 ++ f a3 ++ foldAnnotDL f e1 (foldAnnotDL f op (foldAnnotDL f e2 (f a4 ++ foldAnnotDL f s rest)))
  foldAnnotDL f (JSForLet a1 a2 a3 es1 a4 es2 a5 es3 a6 s) rest = f a1 ++ f a2 ++ f a3 ++ foldAnnotDL f es1 (f a4 ++ foldAnnotDL f es2 (f a5 ++ foldAnnotDL f es3 (f a6 ++ foldAnnotDL f s rest)))
  foldAnnotDL f (JSForLetIn a1 a2 a3 e1 op e2 a4 s) rest = f a1 ++ f a2 ++ f a3 ++ foldAnnotDL f e1 (foldAnnotDL f op (foldAnnotDL f e2 (f a4 ++ foldAnnotDL f s rest)))
  foldAnnotDL f (JSForLetOf a1 a2 a3 e1 op e2 a4 s) rest = f a1 ++ f a2 ++ f a3 ++ foldAnnotDL f e1 (foldAnnotDL f op (foldAnnotDL f e2 (f a4 ++ foldAnnotDL f s rest)))
  foldAnnotDL f (JSForConst a1 a2 a3 es1 a4 es2 a5 es3 a6 s) rest = f a1 ++ f a2 ++ f a3 ++ foldAnnotDL f es1 (f a4 ++ foldAnnotDL f es2 (f a5 ++ foldAnnotDL f es3 (f a6 ++ foldAnnotDL f s rest)))
  foldAnnotDL f (JSForConstIn a1 a2 a3 e1 op e2 a4 s) rest = f a1 ++ f a2 ++ f a3 ++ foldAnnotDL f e1 (foldAnnotDL f op (foldAnnotDL f e2 (f a4 ++ foldAnnotDL f s rest)))
  foldAnnotDL f (JSForConstOf a1 a2 a3 e1 op e2 a4 s) rest = f a1 ++ f a2 ++ f a3 ++ foldAnnotDL f e1 (foldAnnotDL f op (foldAnnotDL f e2 (f a4 ++ foldAnnotDL f s rest)))
  foldAnnotDL f (JSForOf a1 a2 e1 op e2 a3 s) rest = f a1 ++ f a2 ++ foldAnnotDL f e1 (foldAnnotDL f op (foldAnnotDL f e2 (f a3 ++ foldAnnotDL f s rest)))
  foldAnnotDL f (JSForVarOf a1 a2 a3 e1 op e2 a4 s) rest = f a1 ++ f a2 ++ f a3 ++ foldAnnotDL f e1 (foldAnnotDL f op (foldAnnotDL f e2 (f a4 ++ foldAnnotDL f s rest)))
  foldAnnotDL f (JSAsyncFunction a1 a2 i a3 ps a4 b semi) rest = f a1 ++ f a2 ++ foldAnnotDL f i (f a3 ++ foldAnnotDL f ps (f a4 ++ foldAnnotDL f b (foldAnnotDL f semi rest)))
  foldAnnotDL f (JSFunction a1 i a2 ps a3 b semi) rest = f a1 ++ foldAnnotDL f i (f a2 ++ foldAnnotDL f ps (f a3 ++ foldAnnotDL f b (foldAnnotDL f semi rest)))
  foldAnnotDL f (JSGenerator a1 a2 i a3 ps a4 b semi) rest = f a1 ++ f a2 ++ foldAnnotDL f i (f a3 ++ foldAnnotDL f ps (f a4 ++ foldAnnotDL f b (foldAnnotDL f semi rest)))
  foldAnnotDL f (JSIf a1 a2 e a3 s) rest = f a1 ++ f a2 ++ foldAnnotDL f e (f a3 ++ foldAnnotDL f s rest)
  foldAnnotDL f (JSIfElse a1 a2 e a3 s1 a4 s2) rest = f a1 ++ f a2 ++ foldAnnotDL f e (f a3 ++ foldAnnotDL f s1 (f a4 ++ foldAnnotDL f s2 rest))
  foldAnnotDL f (JSLabelled i a s) rest = foldAnnotDL f i (f a ++ foldAnnotDL f s rest)
  foldAnnotDL f (JSEmptyStatement a) rest = f a ++ rest
  foldAnnotDL f (JSExpressionStatement e semi) rest = foldAnnotDL f e (foldAnnotDL f semi rest)
  foldAnnotDL f (JSAssignStatement e1 op e2 semi) rest = foldAnnotDL f e1 (foldAnnotDL f op (foldAnnotDL f e2 (foldAnnotDL f semi rest)))
  foldAnnotDL f (JSMethodCall e a1 args a2 semi) rest = foldAnnotDL f e (f a1 ++ foldAnnotDL f args (f a2 ++ foldAnnotDL f semi rest))
  foldAnnotDL f (JSReturn a me semi) rest = f a ++ maybe id (\x -> foldAnnotDL f x) me (foldAnnotDL f semi rest)
  foldAnnotDL f (JSSwitch a1 a2 e a3 a4 parts a5 semi) rest = f a1 ++ f a2 ++ foldAnnotDL f e (f a3 ++ f a4 ++ foldr (foldAnnotDL f) (f a5 ++ foldAnnotDL f semi rest) parts)
  foldAnnotDL f (JSThrow a e semi) rest = f a ++ foldAnnotDL f e (foldAnnotDL f semi rest)
  foldAnnotDL f (JSTry a b catches fin) rest = f a ++ foldAnnotDL f b (foldr (foldAnnotDL f) (foldAnnotDL f fin rest) catches)
  foldAnnotDL f (JSVariable a es semi) rest = f a ++ foldAnnotDL f es (foldAnnotDL f semi rest)
  foldAnnotDL f (JSWhile a1 a2 e a3 s) rest = f a1 ++ f a2 ++ foldAnnotDL f e (f a3 ++ foldAnnotDL f s rest)
  foldAnnotDL f (JSWith a1 a2 e a3 s semi) rest = f a1 ++ f a2 ++ foldAnnotDL f e (f a3 ++ foldAnnotDL f s (foldAnnotDL f semi rest))
  foldAnnotDL f (JSDebugger a semi) rest = f a ++ foldAnnotDL f semi rest
  foldAnnotDL f (JSAsyncGenerator a1 a2 a3 i a4 ps a5 b semi) rest = f a1 ++ f a2 ++ f a3 ++ foldAnnotDL f i (f a4 ++ foldAnnotDL f ps (f a5 ++ foldAnnotDL f b (foldAnnotDL f semi rest)))
  foldAnnotDL f (JSForAwaitOf a1 a2 a3 e1 op e2 a4 s) rest = f a1 ++ f a2 ++ f a3 ++ foldAnnotDL f e1 (foldAnnotDL f op (foldAnnotDL f e2 (f a4 ++ foldAnnotDL f s rest)))
  foldAnnotDL f (JSForAwaitVarOf a1 a2 a3 a4 e1 op e2 a5 s) rest = f a1 ++ f a2 ++ f a3 ++ f a4 ++ foldAnnotDL f e1 (foldAnnotDL f op (foldAnnotDL f e2 (f a5 ++ foldAnnotDL f s rest)))
  foldAnnotDL f (JSForAwaitLetOf a1 a2 a3 a4 e1 op e2 a5 s) rest = f a1 ++ f a2 ++ f a3 ++ f a4 ++ foldAnnotDL f e1 (foldAnnotDL f op (foldAnnotDL f e2 (f a5 ++ foldAnnotDL f s rest)))
  foldAnnotDL f (JSForAwaitConstOf a1 a2 a3 a4 e1 op e2 a5 s) rest = f a1 ++ f a2 ++ f a3 ++ f a4 ++ foldAnnotDL f e1 (foldAnnotDL f op (foldAnnotDL f e2 (f a5 ++ foldAnnotDL f s rest)))

instance HasAnnot JSAST where
  mapAnnot f (JSAstProgram stmts a) = JSAstProgram (map (mapAnnot f) stmts) (f a)
  mapAnnot f (JSAstModule items a) = JSAstModule (map (mapAnnot f) items) (f a)
  mapAnnot f (JSAstStatement s a) = JSAstStatement (mapAnnot f s) (f a)
  mapAnnot f (JSAstExpression e a) = JSAstExpression (mapAnnot f e) (f a)
  mapAnnot f (JSAstLiteral e a) = JSAstLiteral (mapAnnot f e) (f a)
  foldAnnotDL f (JSAstProgram stmts a) rest = foldr (foldAnnotDL f) (f a ++ rest) stmts
  foldAnnotDL f (JSAstModule items a) rest = foldr (foldAnnotDL f) (f a ++ rest) items
  foldAnnotDL f (JSAstStatement s a) rest = foldAnnotDL f s (f a ++ rest)
  foldAnnotDL f (JSAstExpression e a) rest = foldAnnotDL f e (f a ++ rest)
  foldAnnotDL f (JSAstLiteral e a) rest = foldAnnotDL f e (f a ++ rest)

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
  ss (JSAwaitExpression _ e) = "JSAwaitExpression " <> ss e
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
  ss (JSPrivateIdentifier _ name) = "JSPrivateIdentifier " <> singleQuote ("#" <> bsToStr name)
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
