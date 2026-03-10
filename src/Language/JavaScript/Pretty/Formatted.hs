{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -O2 #-}

-- | Indentation-aware JavaScript pretty printer.
--
-- Unlike 'Language.JavaScript.Pretty.Printer' which preserves source
-- annotations, this printer produces cleanly formatted, human-readable
-- JavaScript output with configurable indentation.
--
-- Designed for compiler code generation where readable output matters
-- more than preserving original formatting.
--
-- ==== Usage
--
-- @
-- import Language.JavaScript.DSL
-- import Language.JavaScript.Pretty.Formatted
--
-- main :: IO ()
-- main = putStrLn (formatJS defaultStyle myModule)
-- @
--
-- ==== Output Style
--
-- @
-- import { useState } from \'react\';
--
-- export function Counter(initial) {
--     const [count, setCount] = useState(initial);
--     return count;
-- }
-- @
--
-- @since 0.8.1.0
module Language.JavaScript.Pretty.Formatted
  ( -- * Formatting
    formatJS
  , formatToText
  , formatToBuilder

    -- * Style Configuration
  , FormatStyle (..)
  , defaultStyle
  , compactStyle
  , twoSpaceStyle
  ) where

import Blaze.ByteString.Builder (Builder, fromByteString, toLazyByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Blaze.ByteString.Builder.Char.Utf8 as BU
import Data.Char (intToDigit)
import Data.List (intersperse)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as LT
import Language.JavaScript.Parser.AST
import Numeric (showHex, showIntAtBase)


-- --------------------------------------------------------------------------
-- Style Configuration
-- --------------------------------------------------------------------------

-- | Formatting style configuration.
--
-- @since 0.8.1.0
data FormatStyle = FormatStyle
  { _styleIndent :: !Int
  , _styleSemicolons :: !Bool
  , _styleNewlineBetweenItems :: !Bool
  } deriving (Eq, Show)

-- | Default style: 4-space indentation, semicolons, newlines between items.
--
-- @since 0.8.1.0
defaultStyle :: FormatStyle
defaultStyle = FormatStyle
  { _styleIndent = 4
  , _styleSemicolons = True
  , _styleNewlineBetweenItems = True
  }

-- | Compact style: 2-space indentation, semicolons, no extra newlines.
--
-- @since 0.8.1.0
compactStyle :: FormatStyle
compactStyle = FormatStyle
  { _styleIndent = 2
  , _styleSemicolons = True
  , _styleNewlineBetweenItems = False
  }

-- | Two-space indentation style (popular in JS community).
--
-- @since 0.8.1.0
twoSpaceStyle :: FormatStyle
twoSpaceStyle = defaultStyle { _styleIndent = 2 }


-- --------------------------------------------------------------------------
-- Formatting Context
-- --------------------------------------------------------------------------

data FmtCtx = FmtCtx
  { _ctxStyle :: !FormatStyle
  , _ctxIndent :: !Int
  }

indent :: FmtCtx -> FmtCtx
indent ctx = ctx { _ctxIndent = _ctxIndent ctx + _styleIndent (_ctxStyle ctx) }

indentStr :: FmtCtx -> Builder
indentStr ctx = BU.fromString (replicate (_ctxIndent ctx) ' ')


-- --------------------------------------------------------------------------
-- Public API
-- --------------------------------------------------------------------------

-- | Format a JavaScript AST to a 'String' with the given style.
--
-- @since 0.8.1.0
formatJS :: FormatStyle -> JSAST -> String
formatJS style ast =
  BS8.unpack (BS8.concat (LBS.toChunks (toLazyByteString (formatToBuilder style ast))))

-- | Format a JavaScript AST to lazy 'Text'.
--
-- @since 0.8.1.0
formatToText :: FormatStyle -> JSAST -> Text
formatToText style = LT.decodeUtf8 . toLazyByteString . formatToBuilder style

-- | Format a JavaScript AST to a 'Builder'.
--
-- @since 0.8.1.0
formatToBuilder :: FormatStyle -> JSAST -> Builder
formatToBuilder style ast =
  fmtAST ctx ast
  where
    ctx = FmtCtx style 0


-- --------------------------------------------------------------------------
-- AST Formatting
-- --------------------------------------------------------------------------

fmtAST :: FmtCtx -> JSAST -> Builder
fmtAST ctx (JSAstProgram stmts _) =
  mconcat (intersperse (BU.fromChar '\n') (map (fmtStmt ctx) stmts))
fmtAST ctx (JSAstModule items _) =
  fmtModuleItems ctx items
fmtAST ctx (JSAstStatement stmt _) = fmtStmt ctx stmt
fmtAST ctx (JSAstExpression expr _) = fmtExpr ctx expr
fmtAST ctx (JSAstLiteral expr _) = fmtExpr ctx expr


-- --------------------------------------------------------------------------
-- Module Items
-- --------------------------------------------------------------------------

fmtModuleItems :: FmtCtx -> [JSModuleItem] -> Builder
fmtModuleItems ctx items =
  mconcat (intersperse sep (map (fmtModuleItem ctx) items))
  where
    sep = if _styleNewlineBetweenItems (_ctxStyle ctx)
            then BU.fromString "\n\n"
            else BU.fromChar '\n'

fmtModuleItem :: FmtCtx -> JSModuleItem -> Builder
fmtModuleItem ctx (JSModuleImportDeclaration _ decl) =
  fromByteString "import" <> fmtImportDecl ctx decl
fmtModuleItem ctx (JSModuleExportDeclaration _ decl) =
  fromByteString "export" <> fmtExportDecl ctx decl
fmtModuleItem ctx (JSModuleStatementListItem stmt) =
  fmtStmt ctx stmt


-- --------------------------------------------------------------------------
-- Import Declarations
-- --------------------------------------------------------------------------

fmtImportDecl :: FmtCtx -> JSImportDeclaration -> Builder
fmtImportDecl ctx (JSImportDeclaration clause from attrs _semi) =
  fmtImportClause ctx clause <> fmtFromClause from <> fmtMaybeAttrs attrs <> sc ctx
fmtImportDecl _ (JSImportDeclarationBare _ modPath attrs _semi) =
  BU.fromChar ' ' <> fromByteString modPath <> fmtMaybeAttrs attrs <> fromByteString ";\n"

fmtImportClause :: FmtCtx -> JSImportClause -> Builder
fmtImportClause _ (JSImportClauseDefault ident) =
  BU.fromChar ' ' <> fmtJSIdent ident
fmtImportClause _ (JSImportClauseNameSpace ns) =
  BU.fromChar ' ' <> fmtNameSpace ns
fmtImportClause _ (JSImportClauseNamed named) =
  BU.fromChar ' ' <> fmtImportsNamed named
fmtImportClause _ (JSImportClauseDefaultNameSpace def _ ns) =
  BU.fromChar ' ' <> fmtJSIdent def <> fromByteString ", " <> fmtNameSpace ns
fmtImportClause _ (JSImportClauseDefaultNamed def _ named) =
  BU.fromChar ' ' <> fmtJSIdent def <> fromByteString ", " <> fmtImportsNamed named

fmtNameSpace :: JSImportNameSpace -> Builder
fmtNameSpace (JSImportNameSpace _ _ ident) =
  fromByteString "* as " <> fmtJSIdent ident

fmtImportsNamed :: JSImportsNamed -> Builder
fmtImportsNamed (JSImportsNamed _ specs _) =
  fromByteString "{ " <> fmtCommaList fmtImportSpec specs <> fromByteString " }"

fmtImportSpec :: JSImportSpecifier -> Builder
fmtImportSpec (JSImportSpecifier ident) = fmtJSIdent ident
fmtImportSpec (JSImportSpecifierAs ident _ alias) =
  fmtJSIdent ident <> fromByteString " as " <> fmtJSIdent alias

fmtFromClause :: JSFromClause -> Builder
fmtFromClause (JSFromClause _ _ modPath) =
  fromByteString " from " <> fromByteString modPath

fmtMaybeAttrs :: Maybe JSImportAttributes -> Builder
fmtMaybeAttrs Nothing = mempty
fmtMaybeAttrs (Just (JSImportAttributes _ attrs _)) =
  fromByteString " with { " <> fmtCommaList fmtImportAttr attrs <> fromByteString " }"

fmtImportAttr :: JSImportAttribute -> Builder
fmtImportAttr (JSImportAttribute key _ val) =
  fmtJSIdent key <> fromByteString ": " <> fmtExprRaw val


-- --------------------------------------------------------------------------
-- Export Declarations
-- --------------------------------------------------------------------------

fmtExportDecl :: FmtCtx -> JSExportDeclaration -> Builder
fmtExportDecl ctx (JSExport stmt _) =
  BU.fromChar ' ' <> fmtStmt ctx stmt
fmtExportDecl ctx (JSExportDefault _ stmt _) =
  fromByteString " default " <> fmtStmtBody ctx stmt
fmtExportDecl _ (JSExportLocals clause _) =
  BU.fromChar ' ' <> fmtExportClause clause <> fromByteString ";\n"
fmtExportDecl _ (JSExportFrom clause from _) =
  BU.fromChar ' ' <> fmtExportClause clause <> fmtFromClause from <> fromByteString ";\n"
fmtExportDecl _ (JSExportAllFrom _ from _) =
  fromByteString " *" <> fmtFromClause from <> fromByteString ";\n"
fmtExportDecl _ (JSExportAllAsFrom _ _ ident from _) =
  fromByteString " * as " <> fmtJSIdent ident <> fmtFromClause from <> fromByteString ";\n"

fmtExportClause :: JSExportClause -> Builder
fmtExportClause (JSExportClause _ specs _) =
  fromByteString "{ " <> fmtCommaList fmtExportSpec specs <> fromByteString " }"

fmtExportSpec :: JSExportSpecifier -> Builder
fmtExportSpec (JSExportSpecifier ident) = fmtJSIdent ident
fmtExportSpec (JSExportSpecifierAs ident _ alias) =
  fmtJSIdent ident <> fromByteString " as " <> fmtJSIdent alias


-- --------------------------------------------------------------------------
-- Statements
-- --------------------------------------------------------------------------

fmtStmt :: FmtCtx -> JSStatement -> Builder
fmtStmt ctx stmt = indentStr ctx <> fmtStmtBody ctx stmt

fmtStmtBody :: FmtCtx -> JSStatement -> Builder
fmtStmtBody _ (JSEmptyStatement _) = fromByteString ";\n"

fmtStmtBody ctx (JSStatementBlock _ stmts _ _) =
  fromByteString "{\n" <> mconcat (map (fmtStmt inner) stmts) <> indentStr ctx <> fromByteString "}\n"
  where inner = indent ctx

fmtStmtBody ctx (JSVariable _ decls _) =
  fromByteString "var " <> fmtCommaList (fmtVarInit ctx) decls <> sc ctx

fmtStmtBody ctx (JSLet _ decls _) =
  fromByteString "let " <> fmtCommaList (fmtVarInit ctx) decls <> sc ctx

fmtStmtBody ctx (JSConstant _ decls _) =
  fromByteString "const " <> fmtCommaList (fmtVarInit ctx) decls <> sc ctx

fmtStmtBody ctx (JSReturn _ mexpr _) =
  fromByteString "return" <> fmtMaybeExpr ctx mexpr <> sc ctx

fmtStmtBody ctx (JSThrow _ expr _) =
  fromByteString "throw " <> fmtExprRaw expr <> sc ctx

fmtStmtBody _ (JSBreak _ JSIdentNone _) =
  fromByteString "break;\n"
fmtStmtBody _ (JSBreak _ ident _) =
  fromByteString "break " <> fmtJSIdent ident <> fromByteString ";\n"

fmtStmtBody _ (JSContinue _ JSIdentNone _) =
  fromByteString "continue;\n"
fmtStmtBody _ (JSContinue _ ident _) =
  fromByteString "continue " <> fmtJSIdent ident <> fromByteString ";\n"

fmtStmtBody ctx (JSLabelled ident _ s) =
  fmtJSIdent ident <> fromByteString ":\n" <> fmtStmt ctx s

fmtStmtBody ctx (JSIf _ _ cond _ body) =
  fromByteString "if (" <> fmtExprRaw cond <> fromByteString ") " <> fmtStmtInline ctx body

fmtStmtBody ctx (JSIfElse _ _ cond _ thenS _ elseS) =
  fromByteString "if (" <> fmtExprRaw cond <> fromByteString ") "
    <> fmtBlockNoNewline ctx thenS
    <> fromByteString " else "
    <> fmtStmtInline ctx elseS

fmtStmtBody ctx (JSWhile _ _ cond _ body) =
  fromByteString "while (" <> fmtExprRaw cond <> fromByteString ") " <> fmtStmtInline ctx body

fmtStmtBody ctx (JSDoWhile _ body _ _ cond _ _) =
  fromByteString "do " <> fmtBlockNoNewline ctx body
    <> fromByteString " while (" <> fmtExprRaw cond <> fromByteString ");\n"

fmtStmtBody ctx (JSFor _ _ inits _ conds _ steps _ body) =
  fromByteString "for ("
    <> fmtCommaList fmtExprRaw inits
    <> fromByteString "; "
    <> fmtCommaList fmtExprRaw conds
    <> fromByteString "; "
    <> fmtCommaList fmtExprRaw steps
    <> fromByteString ") "
    <> fmtStmtInline ctx body

fmtStmtBody ctx (JSSwitch _ _ expr _ _ cases _ _) =
  fromByteString "switch (" <> fmtExprRaw expr <> fromByteString ") {\n"
    <> mconcat (map (fmtSwitchPart inner) cases)
    <> indentStr ctx <> fromByteString "}\n"
  where inner = indent ctx

fmtStmtBody ctx (JSTry _ block catches finally) =
  fromByteString "try " <> fmtBlock ctx block
    <> mconcat (map (fmtCatch ctx) catches)
    <> fmtFinally ctx finally

fmtStmtBody ctx (JSFunction _ ident _ params _ block _) =
  fromByteString "function " <> fmtJSIdent ident
    <> fromByteString "(" <> fmtCommaList fmtExprRaw params <> fromByteString ") "
    <> fmtBlock ctx block <> BU.fromChar '\n'

fmtStmtBody ctx (JSExpressionStatement expr _) =
  fmtExprRaw expr <> sc ctx

fmtStmtBody ctx (JSAssignStatement lhs op rhs _) =
  fmtExprRaw lhs <> fmtAssignOp op <> fmtExprRaw rhs <> sc ctx

fmtStmtBody ctx (JSMethodCall expr _ args _ _) =
  fmtExprRaw expr <> fromByteString "(" <> fmtCommaList fmtExprRaw args <> fromByteString ")" <> sc ctx

fmtStmtBody ctx (JSClass _ ident heritage _ elems _ _) =
  fromByteString "class " <> fmtJSIdent ident <> fmtHeritage heritage
    <> fromByteString " {\n"
    <> mconcat (map (fmtClassElem inner) elems)
    <> indentStr ctx <> fromByteString "}\n"
  where inner = indent ctx

fmtStmtBody _ (JSDebugger _ _) = fromByteString "debugger;\n"

-- Catch-all for less common statements
fmtStmtBody ctx (JSWith _ _ expr _ body _) =
  fromByteString "with (" <> fmtExprRaw expr <> fromByteString ") " <> fmtStmtInline ctx body

fmtStmtBody ctx (JSAsyncFunction _ _ ident _ params _ block _) =
  fromByteString "async function " <> fmtJSIdent ident
    <> fromByteString "(" <> fmtCommaList fmtExprRaw params <> fromByteString ") "
    <> fmtBlock ctx block <> BU.fromChar '\n'

fmtStmtBody ctx (JSGenerator _ _ ident _ params _ block _) =
  fromByteString "function* " <> fmtJSIdent ident
    <> fromByteString "(" <> fmtCommaList fmtExprRaw params <> fromByteString ") "
    <> fmtBlock ctx block <> BU.fromChar '\n'

-- For loop variants
fmtStmtBody ctx (JSForIn _ _ expr _ iterExpr _ body) =
  fromByteString "for (" <> fmtExprRaw expr <> fromByteString " in " <> fmtExprRaw iterExpr
    <> fromByteString ") " <> fmtStmtInline ctx body

fmtStmtBody ctx (JSForOf _ _ expr _ iterExpr _ body) =
  fromByteString "for (" <> fmtExprRaw expr <> fromByteString " of " <> fmtExprRaw iterExpr
    <> fromByteString ") " <> fmtStmtInline ctx body

fmtStmtBody ctx (JSForVar _ _ _ decls _ conds _ steps _ body) =
  fromByteString "for (var " <> fmtCommaList (fmtVarInit ctx) decls
    <> fromByteString "; " <> fmtCommaList fmtExprRaw conds
    <> fromByteString "; " <> fmtCommaList fmtExprRaw steps
    <> fromByteString ") " <> fmtStmtInline ctx body

fmtStmtBody ctx (JSForLet _ _ _ decls _ conds _ steps _ body) =
  fromByteString "for (let " <> fmtCommaList (fmtVarInit ctx) decls
    <> fromByteString "; " <> fmtCommaList fmtExprRaw conds
    <> fromByteString "; " <> fmtCommaList fmtExprRaw steps
    <> fromByteString ") " <> fmtStmtInline ctx body

fmtStmtBody ctx (JSForConst _ _ _ decls _ conds _ steps _ body) =
  fromByteString "for (const " <> fmtCommaList (fmtVarInit ctx) decls
    <> fromByteString "; " <> fmtCommaList fmtExprRaw conds
    <> fromByteString "; " <> fmtCommaList fmtExprRaw steps
    <> fromByteString ") " <> fmtStmtInline ctx body

fmtStmtBody ctx (JSForVarIn _ _ _ decl _ iterExpr _ body) =
  fromByteString "for (var " <> fmtExprRaw decl <> fromByteString " in " <> fmtExprRaw iterExpr
    <> fromByteString ") " <> fmtStmtInline ctx body

fmtStmtBody ctx (JSForLetIn _ _ _ decl _ iterExpr _ body) =
  fromByteString "for (let " <> fmtExprRaw decl <> fromByteString " in " <> fmtExprRaw iterExpr
    <> fromByteString ") " <> fmtStmtInline ctx body

fmtStmtBody ctx (JSForConstIn _ _ _ decl _ iterExpr _ body) =
  fromByteString "for (const " <> fmtExprRaw decl <> fromByteString " in " <> fmtExprRaw iterExpr
    <> fromByteString ") " <> fmtStmtInline ctx body

fmtStmtBody ctx (JSForLetOf _ _ _ decl _ iterExpr _ body) =
  fromByteString "for (let " <> fmtExprRaw decl <> fromByteString " of " <> fmtExprRaw iterExpr
    <> fromByteString ") " <> fmtStmtInline ctx body

fmtStmtBody ctx (JSForConstOf _ _ _ decl _ iterExpr _ body) =
  fromByteString "for (const " <> fmtExprRaw decl <> fromByteString " of " <> fmtExprRaw iterExpr
    <> fromByteString ") " <> fmtStmtInline ctx body

fmtStmtBody ctx (JSForVarOf _ _ _ decl _ iterExpr _ body) =
  fromByteString "for (var " <> fmtExprRaw decl <> fromByteString " of " <> fmtExprRaw iterExpr
    <> fromByteString ") " <> fmtStmtInline ctx body

fmtStmtBody ctx (JSForAwaitOf _ _ _ expr _ iterExpr _ body) =
  fromByteString "for await (" <> fmtExprRaw expr <> fromByteString " of " <> fmtExprRaw iterExpr
    <> fromByteString ") " <> fmtStmtInline ctx body

fmtStmtBody ctx (JSForAwaitVarOf _ _ _ _ decl _ iterExpr _ body) =
  fromByteString "for await (var " <> fmtExprRaw decl <> fromByteString " of " <> fmtExprRaw iterExpr
    <> fromByteString ") " <> fmtStmtInline ctx body

fmtStmtBody ctx (JSForAwaitLetOf _ _ _ _ decl _ iterExpr _ body) =
  fromByteString "for await (let " <> fmtExprRaw decl <> fromByteString " of " <> fmtExprRaw iterExpr
    <> fromByteString ") " <> fmtStmtInline ctx body

fmtStmtBody ctx (JSForAwaitConstOf _ _ _ _ decl _ iterExpr _ body) =
  fromByteString "for await (const " <> fmtExprRaw decl <> fromByteString " of " <> fmtExprRaw iterExpr
    <> fromByteString ") " <> fmtStmtInline ctx body

fmtStmtBody ctx (JSAsyncGenerator _ _ _ ident _ params _ block _) =
  fromByteString "async function* " <> fmtJSIdent ident
    <> fromByteString "(" <> fmtCommaList fmtExprRaw params <> fromByteString ") "
    <> fmtBlock ctx block <> BU.fromChar '\n'


-- --------------------------------------------------------------------------
-- Statement Helpers
-- --------------------------------------------------------------------------

-- | Format a statement inline (for if/while/for bodies).
fmtStmtInline :: FmtCtx -> JSStatement -> Builder
fmtStmtInline ctx (JSStatementBlock _ stmts _ _) =
  fromByteString "{\n" <> mconcat (map (fmtStmt inner) stmts) <> indentStr ctx <> fromByteString "}\n"
  where inner = indent ctx
fmtStmtInline ctx s = BU.fromChar '\n' <> fmtStmt (indent ctx) s

-- | Format a block without trailing newline (for if-else chaining).
fmtBlockNoNewline :: FmtCtx -> JSStatement -> Builder
fmtBlockNoNewline ctx (JSStatementBlock _ stmts _ _) =
  fromByteString "{\n" <> mconcat (map (fmtStmt inner) stmts) <> indentStr ctx <> fromByteString "}"
  where inner = indent ctx
fmtBlockNoNewline ctx s = BU.fromChar '\n' <> fmtStmt (indent ctx) s

-- | Format a JSBlock.
fmtBlock :: FmtCtx -> JSBlock -> Builder
fmtBlock ctx (JSBlock _ stmts _) =
  fromByteString "{\n" <> mconcat (map (fmtStmt inner) stmts) <> indentStr ctx <> fromByteString "}"
  where inner = indent ctx

-- | Semicolon + newline.
sc :: FmtCtx -> Builder
sc _ = fromByteString ";\n"

fmtMaybeExpr :: FmtCtx -> Maybe JSExpression -> Builder
fmtMaybeExpr _ Nothing = mempty
fmtMaybeExpr _ (Just expr) = BU.fromChar ' ' <> fmtExprRaw expr

fmtVarInit :: FmtCtx -> JSExpression -> Builder
fmtVarInit _ (JSVarInitExpression ident (JSVarInit _ expr)) =
  fmtIdent ident <> fromByteString " = " <> fmtExprRaw expr
fmtVarInit _ (JSVarInitExpression ident JSVarInitNone) =
  fmtIdent ident
fmtVarInit _ expr = fmtExprRaw expr

fmtSwitchPart :: FmtCtx -> JSSwitchParts -> Builder
fmtSwitchPart ctx (JSCase _ expr _ stmts) =
  indentStr ctx <> fromByteString "case " <> fmtExprRaw expr <> fromByteString ":\n"
    <> mconcat (map (fmtStmt inner) stmts)
  where inner = indent ctx
fmtSwitchPart ctx (JSDefault _ _ stmts) =
  indentStr ctx <> fromByteString "default:\n"
    <> mconcat (map (fmtStmt inner) stmts)
  where inner = indent ctx

fmtCatch :: FmtCtx -> JSTryCatch -> Builder
fmtCatch ctx (JSCatch _ _ expr _ block) =
  fromByteString " catch (" <> fmtExprRaw expr <> fromByteString ") " <> fmtBlock ctx block
fmtCatch _ (JSCatchIf _ _ expr _ cond _ block) =
  fromByteString " catch (" <> fmtExprRaw expr <> fromByteString " if " <> fmtExprRaw cond <> fromByteString ") " <> fmtBlockRaw block
fmtCatch ctx (JSCatchNoParam _ block) =
  fromByteString " catch " <> fmtBlock ctx block

fmtFinally :: FmtCtx -> JSTryFinally -> Builder
fmtFinally ctx (JSFinally _ block) = fromByteString " finally " <> fmtBlock ctx block <> BU.fromChar '\n'
fmtFinally _ JSNoFinally = BU.fromChar '\n'

fmtHeritage :: JSClassHeritage -> Builder
fmtHeritage JSExtendsNone = mempty
fmtHeritage (JSExtends _ expr) = fromByteString " extends " <> fmtExprRaw expr

fmtClassElem :: FmtCtx -> JSClassElement -> Builder
fmtClassElem ctx (JSClassInstanceMethod m) = indentStr ctx <> fmtMethodDef m <> BU.fromChar '\n'
fmtClassElem ctx (JSClassStaticMethod _ m) = indentStr ctx <> fromByteString "static " <> fmtMethodDef m <> BU.fromChar '\n'
fmtClassElem ctx (JSClassSemi _) = indentStr ctx <> fromByteString ";\n"
fmtClassElem ctx (JSClassField name _ minit _) =
  indentStr ctx <> fmtPropName name <> fmtMaybeInit minit <> fromByteString ";\n"
fmtClassElem ctx (JSClassStaticField _ name _ minit _) =
  indentStr ctx <> fromByteString "static " <> fmtPropName name <> fmtMaybeInit minit <> fromByteString ";\n"
fmtClassElem ctx (JSClassStaticBlock _ block) =
  indentStr ctx <> fromByteString "static " <> fmtBlockRaw block <> BU.fromChar '\n'
fmtClassElem ctx (JSPrivateField _ name _ minit _) =
  indentStr ctx <> fromByteString "#" <> fromByteString name <> fmtMaybeInit minit <> fromByteString ";\n"
fmtClassElem ctx (JSPrivateMethod _ name _ params _ block) =
  indentStr ctx <> fromByteString "#" <> fromByteString name
    <> fromByteString "(" <> fmtCommaList fmtExprRaw params <> fromByteString ") "
    <> fmtBlockRaw block <> BU.fromChar '\n'
fmtClassElem ctx (JSPrivateAccessor accessor _ name _ params _ block) =
  indentStr ctx <> fmtAccessor accessor <> fromByteString " #" <> fromByteString name
    <> fromByteString "(" <> fmtCommaList fmtExprRaw params <> fromByteString ") "
    <> fmtBlockRaw block <> BU.fromChar '\n'
fmtClassElem ctx (JSAsyncGeneratorMethodDefinition _ _ name _ params _ block) =
  indentStr ctx <> fromByteString "async *" <> fmtPropName name
    <> fromByteString "(" <> fmtCommaList fmtExprRaw params <> fromByteString ") "
    <> fmtBlockRaw block <> BU.fromChar '\n'

fmtMaybeInit :: Maybe JSExpression -> Builder
fmtMaybeInit Nothing = mempty
fmtMaybeInit (Just expr) = fromByteString " = " <> fmtExprRaw expr

fmtMethodDef :: JSMethodDefinition -> Builder
fmtMethodDef (JSMethodDefinition name _ params _ block) =
  fmtPropName name <> fromByteString "(" <> fmtCommaList fmtExprRaw params <> fromByteString ") " <> fmtBlockRaw block
fmtMethodDef (JSGeneratorMethodDefinition _ name _ params _ block) =
  fromByteString "*" <> fmtPropName name <> fromByteString "(" <> fmtCommaList fmtExprRaw params <> fromByteString ") " <> fmtBlockRaw block
fmtMethodDef (JSAsyncMethodDefinition _ name _ params _ block) =
  fromByteString "async " <> fmtPropName name <> fromByteString "(" <> fmtCommaList fmtExprRaw params <> fromByteString ") " <> fmtBlockRaw block
fmtMethodDef (JSPropertyAccessor accessor name _ params _ block) =
  fmtAccessor accessor <> BU.fromChar ' ' <> fmtPropName name <> fromByteString "(" <> fmtCommaList fmtExprRaw params <> fromByteString ") " <> fmtBlockRaw block

fmtAccessor :: JSAccessor -> Builder
fmtAccessor (JSAccessorGet _) = fromByteString "get"
fmtAccessor (JSAccessorSet _) = fromByteString "set"

fmtBlockRaw :: JSBlock -> Builder
fmtBlockRaw (JSBlock _ stmts _) =
  fromByteString "{ " <> mconcat (map fmtStmtRaw stmts) <> fromByteString "}"

fmtStmtRaw :: JSStatement -> Builder
fmtStmtRaw = fmtStmtBody (FmtCtx defaultStyle 0)


-- --------------------------------------------------------------------------
-- Expressions
-- --------------------------------------------------------------------------

-- | Format expression, stripping leading annotation whitespace.
fmtExprRaw :: JSExpression -> Builder
fmtExprRaw = fmtExpr (FmtCtx defaultStyle 0)

fmtExpr :: FmtCtx -> JSExpression -> Builder
fmtExpr _ (JSIdentifier _ name) = fromByteString name
fmtExpr _ (JSDecimal _ d) = BU.fromString (renderDouble d)
fmtExpr _ (JSLiteral _ lit) = fromByteString lit
fmtExpr _ (JSHexInteger _ n) = BU.fromString ("0x" <> showHex n "")
fmtExpr _ (JSBinaryInteger _ n) = BU.fromString ("0b" <> showIntAtBase 2 intToDigit n "")
fmtExpr _ (JSOctal _ n) = BU.fromString ("0o" <> showIntAtBase 8 intToDigit n "")
fmtExpr _ (JSStringLiteral _ s) = fromByteString s
fmtExpr _ (JSRegEx _ s) = fromByteString s
fmtExpr _ (JSBigIntLiteral _ n) = BU.fromString (show n <> "n")

fmtExpr ctx (JSArrayLiteral _ elems _) =
  fromByteString "[" <> fmtArrayElems ctx elems <> fromByteString "]"

fmtExpr _ (JSObjectLiteral _ props _) =
  fromByteString "{ " <> fmtCommaTrailingList fmtObjProp props <> fromByteString " }"

fmtExpr ctx (JSExpressionBinary l op r) =
  fmtExpr ctx l <> fmtBinOp op <> fmtExpr ctx r

fmtExpr ctx (JSUnaryExpression op e) =
  fmtUnaryOp op <> fmtExpr ctx e

fmtExpr ctx (JSExpressionPostfix e op) =
  fmtExpr ctx e <> fmtPostfixOp op

fmtExpr ctx (JSExpressionTernary cond _ thenE _ elseE) =
  fmtExpr ctx cond <> fromByteString " ? " <> fmtExpr ctx thenE <> fromByteString " : " <> fmtExpr ctx elseE

fmtExpr ctx (JSAssignExpression lhs op rhs) =
  fmtExpr ctx lhs <> fmtAssignOp op <> fmtExpr ctx rhs

fmtExpr ctx (JSCallExpression callee _ args _) =
  fmtExpr ctx callee <> fromByteString "(" <> fmtCommaList fmtExprRaw args <> fromByteString ")"

fmtExpr ctx (JSMemberExpression callee _ args _) =
  fmtExpr ctx callee <> fromByteString "(" <> fmtCommaList fmtExprRaw args <> fromByteString ")"

fmtExpr ctx (JSMemberDot obj _ prop) =
  fmtExpr ctx obj <> fromByteString "." <> fmtExprRaw prop

fmtExpr ctx (JSCallExpressionDot obj _ prop) =
  fmtExpr ctx obj <> fromByteString "." <> fmtExprRaw prop

fmtExpr ctx (JSMemberSquare obj _ key _) =
  fmtExpr ctx obj <> fromByteString "[" <> fmtExprRaw key <> fromByteString "]"

fmtExpr ctx (JSCallExpressionSquare obj _ key _) =
  fmtExpr ctx obj <> fromByteString "[" <> fmtExprRaw key <> fromByteString "]"

fmtExpr ctx (JSMemberPrivateDot obj _ _ name) =
  fmtExpr ctx obj <> fromByteString ".#" <> fromByteString name

fmtExpr ctx (JSOptionalMemberDot obj _ prop) =
  fmtExpr ctx obj <> fromByteString "?." <> fmtExprRaw prop

fmtExpr ctx (JSOptionalMemberSquare obj _ key _) =
  fmtExpr ctx obj <> fromByteString "?.[" <> fmtExprRaw key <> fromByteString "]"

fmtExpr ctx (JSOptionalCallExpression callee _ args _) =
  fmtExpr ctx callee <> fromByteString "?.(" <> fmtCommaList fmtExprRaw args <> fromByteString ")"

fmtExpr ctx (JSExpressionParen _ e _) =
  fromByteString "(" <> fmtExpr ctx e <> fromByteString ")"

fmtExpr ctx (JSCommaExpression l _ r) =
  fmtExpr ctx l <> fromByteString ", " <> fmtExpr ctx r

fmtExpr ctx (JSFunctionExpression _ name _ params _ block) =
  fromByteString "function" <> fmtJSIdentMaybe name
    <> fromByteString "(" <> fmtCommaList fmtExprRaw params <> fromByteString ") "
    <> fmtBlock ctx block

fmtExpr ctx (JSAsyncFunctionExpression _ _ name _ params _ block) =
  fromByteString "async function" <> fmtJSIdentMaybe name
    <> fromByteString "(" <> fmtCommaList fmtExprRaw params <> fromByteString ") "
    <> fmtBlock ctx block

fmtExpr ctx (JSGeneratorExpression _ _ name _ params _ block) =
  fromByteString "function*" <> fmtJSIdentMaybe name
    <> fromByteString "(" <> fmtCommaList fmtExprRaw params <> fromByteString ") "
    <> fmtBlock ctx block

fmtExpr ctx (JSAsyncGeneratorExpression _ _ _ name _ params _ block) =
  fromByteString "async function*" <> fmtJSIdentMaybe name
    <> fromByteString "(" <> fmtCommaList fmtExprRaw params <> fromByteString ") "
    <> fmtBlock ctx block

fmtExpr ctx (JSArrowExpression params _ body) =
  fmtArrowParams params <> fromByteString " => " <> fmtConciseBody ctx body

fmtExpr ctx (JSAsyncArrowExpression _ params _ body) =
  fromByteString "async " <> fmtArrowParams params <> fromByteString " => " <> fmtConciseBody ctx body

fmtExpr _ (JSVarInitExpression ident initr) =
  fmtExprRaw ident <> fmtVarInitr initr

fmtExpr ctx (JSSpreadExpression _ e) =
  fromByteString "..." <> fmtExpr ctx e

fmtExpr ctx (JSNewExpression _ e) =
  fromByteString "new " <> fmtExpr ctx e

fmtExpr ctx (JSMemberNew _ ctor _ args _) =
  fromByteString "new " <> fmtExpr ctx ctor <> fromByteString "(" <> fmtCommaList fmtExprRaw args <> fromByteString ")"

fmtExpr _ (JSYieldExpression _ me) =
  fromByteString "yield" <> maybe mempty (\e -> BU.fromChar ' ' <> fmtExprRaw e) me

fmtExpr _ (JSYieldFromExpression _ _ e) =
  fromByteString "yield* " <> fmtExprRaw e

fmtExpr _ (JSAwaitExpression _ e) =
  fromByteString "await " <> fmtExprRaw e

fmtExpr _ (JSImportMeta _ _) = fromByteString "import.meta"

fmtExpr _ (JSImportCall _ _ e _) =
  fromByteString "import(" <> fmtExprRaw e <> fromByteString ")"

fmtExpr _ (JSPrivateIdentifier _ name) =
  fromByteString "#" <> fromByteString name

fmtExpr _ (JSTemplateLiteral _ _ h parts) =
  fromByteString "`" <> fromByteString h <> mconcat (map fmtTplPart parts) <> fromByteString "`"

fmtExpr _ (JSClassExpression _ name heritage _ elems _) =
  fromByteString "class" <> fmtJSIdentMaybe name <> fmtHeritage heritage
    <> fromByteString " { " <> mconcat (map fmtClassElemInline elems) <> fromByteString "}"


-- --------------------------------------------------------------------------
-- Expression Helpers
-- --------------------------------------------------------------------------

fmtArrowParams :: JSArrowParameterList -> Builder
fmtArrowParams (JSUnparenthesizedArrowParameter ident) = fmtJSIdent ident
fmtArrowParams (JSParenthesizedArrowParameterList _ params _) =
  fromByteString "(" <> fmtCommaList fmtExprRaw params <> fromByteString ")"

fmtConciseBody :: FmtCtx -> JSConciseBody -> Builder
fmtConciseBody _ (JSConciseExpressionBody e) = fmtExprRaw e
fmtConciseBody ctx (JSConciseFunctionBody block) = fmtBlock ctx block

fmtIdent :: JSExpression -> Builder
fmtIdent (JSIdentifier _ name) = fromByteString name
fmtIdent expr = fmtExprRaw expr

fmtJSIdent :: JSIdent -> Builder
fmtJSIdent (JSIdentName _ name) = fromByteString name
fmtJSIdent JSIdentNone = mempty

fmtJSIdentMaybe :: JSIdent -> Builder
fmtJSIdentMaybe (JSIdentName _ name) = BU.fromChar ' ' <> fromByteString name
fmtJSIdentMaybe JSIdentNone = mempty

fmtPropName :: JSPropertyName -> Builder
fmtPropName (JSPropertyIdent _ s) = fromByteString s
fmtPropName (JSPropertyString _ s) = fromByteString s
fmtPropName (JSPropertyNumber _ s) = fromByteString s
fmtPropName (JSPropertyComputed _ e _) = fromByteString "[" <> fmtExprRaw e <> fromByteString "]"

fmtVarInitr :: JSVarInitializer -> Builder
fmtVarInitr (JSVarInit _ e) = fromByteString " = " <> fmtExprRaw e
fmtVarInitr JSVarInitNone = mempty

fmtObjProp :: JSObjectProperty -> Builder
fmtObjProp (JSPropertyNameandValue name _ vals) =
  fmtPropName name <> fromByteString ": " <> mconcat (intersperse (fromByteString ", ") (map fmtExprRaw vals))
fmtObjProp (JSPropertyIdentRef _ name) = fromByteString name
fmtObjProp (JSObjectMethod m) = fmtMethodDef m
fmtObjProp (JSObjectSpread _ e) = fromByteString "..." <> fmtExprRaw e

fmtArrayElems :: FmtCtx -> [JSArrayElement] -> Builder
fmtArrayElems _ elems =
  mconcat (intersperse (fromByteString ", ") (concatMap extractElem elems))
  where
    extractElem (JSArrayElement e) = [fmtExprRaw e]
    extractElem (JSArrayComma _) = []

fmtTplPart :: JSTemplatePart -> Builder
fmtTplPart (JSTemplatePart expr _ suffix) =
  fromByteString "${" <> fmtExprRaw expr <> fromByteString "}" <> fromByteString suffix

fmtClassElemInline :: JSClassElement -> Builder
fmtClassElemInline (JSClassSemi _) = fromByteString "; "
fmtClassElemInline (JSClassInstanceMethod m) = fmtMethodDef m <> BU.fromChar ' '
fmtClassElemInline (JSClassStaticMethod _ m) = fromByteString "static " <> fmtMethodDef m <> BU.fromChar ' '
fmtClassElemInline _ = mempty


-- --------------------------------------------------------------------------
-- Operators
-- --------------------------------------------------------------------------

fmtBinOp :: JSBinOp -> Builder
fmtBinOp (JSBinOpPlus _) = fromByteString " + "
fmtBinOp (JSBinOpMinus _) = fromByteString " - "
fmtBinOp (JSBinOpTimes _) = fromByteString " * "
fmtBinOp (JSBinOpDivide _) = fromByteString " / "
fmtBinOp (JSBinOpMod _) = fromByteString " % "
fmtBinOp (JSBinOpEq _) = fromByteString " == "
fmtBinOp (JSBinOpStrictEq _) = fromByteString " === "
fmtBinOp (JSBinOpNeq _) = fromByteString " != "
fmtBinOp (JSBinOpStrictNeq _) = fromByteString " !== "
fmtBinOp (JSBinOpLt _) = fromByteString " < "
fmtBinOp (JSBinOpLe _) = fromByteString " <= "
fmtBinOp (JSBinOpGt _) = fromByteString " > "
fmtBinOp (JSBinOpGe _) = fromByteString " >= "
fmtBinOp (JSBinOpAnd _) = fromByteString " && "
fmtBinOp (JSBinOpOr _) = fromByteString " || "
fmtBinOp (JSBinOpBitAnd _) = fromByteString " & "
fmtBinOp (JSBinOpBitOr _) = fromByteString " | "
fmtBinOp (JSBinOpBitXor _) = fromByteString " ^ "
fmtBinOp (JSBinOpLsh _) = fromByteString " << "
fmtBinOp (JSBinOpRsh _) = fromByteString " >> "
fmtBinOp (JSBinOpUrsh _) = fromByteString " >>> "
fmtBinOp (JSBinOpIn _) = fromByteString " in "
fmtBinOp (JSBinOpInstanceOf _) = fromByteString " instanceof "
fmtBinOp (JSBinOpOf _) = fromByteString " of "
fmtBinOp (JSBinOpExponentiation _) = fromByteString " ** "
fmtBinOp (JSBinOpNullishCoalescing _) = fromByteString " ?? "

fmtUnaryOp :: JSUnaryOp -> Builder
fmtUnaryOp (JSUnaryOpNot _) = fromByteString "!"
fmtUnaryOp (JSUnaryOpMinus _) = fromByteString "-"
fmtUnaryOp (JSUnaryOpPlus _) = fromByteString "+"
fmtUnaryOp (JSUnaryOpTilde _) = fromByteString "~"
fmtUnaryOp (JSUnaryOpTypeof _) = fromByteString "typeof "
fmtUnaryOp (JSUnaryOpVoid _) = fromByteString "void "
fmtUnaryOp (JSUnaryOpDelete _) = fromByteString "delete "
fmtUnaryOp (JSUnaryOpIncr _) = fromByteString "++"
fmtUnaryOp (JSUnaryOpDecr _) = fromByteString "--"

fmtPostfixOp :: JSUnaryOp -> Builder
fmtPostfixOp (JSUnaryOpIncr _) = fromByteString "++"
fmtPostfixOp (JSUnaryOpDecr _) = fromByteString "--"
fmtPostfixOp op = fmtUnaryOp op

fmtAssignOp :: JSAssignOp -> Builder
fmtAssignOp (JSAssign _) = fromByteString " = "
fmtAssignOp (JSPlusAssign _) = fromByteString " += "
fmtAssignOp (JSMinusAssign _) = fromByteString " -= "
fmtAssignOp (JSTimesAssign _) = fromByteString " *= "
fmtAssignOp (JSDivideAssign _) = fromByteString " /= "
fmtAssignOp (JSModAssign _) = fromByteString " %= "
fmtAssignOp (JSLshAssign _) = fromByteString " <<= "
fmtAssignOp (JSRshAssign _) = fromByteString " >>= "
fmtAssignOp (JSUrshAssign _) = fromByteString " >>>= "
fmtAssignOp (JSBwAndAssign _) = fromByteString " &= "
fmtAssignOp (JSBwXorAssign _) = fromByteString " ^= "
fmtAssignOp (JSBwOrAssign _) = fromByteString " |= "
fmtAssignOp (JSLogicalAndAssign _) = fromByteString " &&= "
fmtAssignOp (JSLogicalOrAssign _) = fromByteString " ||= "
fmtAssignOp (JSNullishAssign _) = fromByteString " ??= "
fmtAssignOp (JSExponentiationAssign _) = fromByteString " **= "


-- --------------------------------------------------------------------------
-- Comma List Helpers
-- --------------------------------------------------------------------------

fmtCommaList :: (a -> Builder) -> JSCommaList a -> Builder
fmtCommaList _ JSLNil = mempty
fmtCommaList f (JSLOne x) = f x
fmtCommaList f (JSLCons xs _ x) = fmtCommaList f xs <> fromByteString ", " <> f x

fmtCommaTrailingList :: (a -> Builder) -> JSCommaTrailingList a -> Builder
fmtCommaTrailingList f (JSCTLComma xs _) = fmtCommaList f xs <> fromByteString ","
fmtCommaTrailingList f (JSCTLNone xs) = fmtCommaList f xs


-- --------------------------------------------------------------------------
-- Numeric Rendering
-- --------------------------------------------------------------------------

renderDouble :: Double -> String
renderDouble d
  | d == fromIntegral n && abs d < 1e15 = show n
  | otherwise = show d
  where
    n = round d :: Integer


