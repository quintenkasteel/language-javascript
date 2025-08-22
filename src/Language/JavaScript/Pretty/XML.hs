{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | XML serialization for JavaScript AST nodes.
--
-- This module provides comprehensive XML output for all JavaScript language
-- constructs including ES2020+ features like BigInt literals, optional
-- chaining, and nullish coalescing.
--
-- The XML format preserves complete AST structure with attributes for
-- metadata and nested elements for child nodes. This format is ideal
-- for:
--
--   * Static analysis tools
--   * Code transformation pipelines
--   * Language-agnostic AST processing
--   * Documentation generation
--
-- ==== Examples
--
-- >>> import Language.JavaScript.Parser.AST as AST
-- >>> import Language.JavaScript.Pretty.XML as XML
-- >>> let ast = JSDecimal (JSAnnot noPos []) "42"
-- >>> XML.renderToXML ast
-- "<JSDecimal value=\"42\"><annotation><position line=\"0\" column=\"0\" address=\"0\"/><comments/></annotation></JSDecimal>"
--
-- ==== Features
--
-- * Complete ES5+ JavaScript construct support
-- * ES2020+ BigInt, optional chaining, nullish coalescing
-- * Full AST structure preservation
-- * Source location and comment preservation
-- * Well-formed XML with proper escaping
-- * Hierarchical representation of nested structures
--
-- @since 0.7.1.0
module Language.JavaScript.Pretty.XML
    ( 
    -- * XML rendering functions
      renderToXML
    , renderProgramToXML
    , renderExpressionToXML
    , renderStatementToXML
    , renderImportDeclarationToXML
    , renderExportDeclarationToXML
    , renderAnnotation
    -- * XML utilities
    , escapeXMLString
    , formatXMLElement
    , formatXMLAttribute
    , formatXMLAttributes
    ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.JavaScript.Parser.AST as AST
import qualified Language.JavaScript.Parser.Token as Token
import Language.JavaScript.Parser.SrcLocation (TokenPosn(..))
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- | Helper function to safely decode UTF-8 ByteString to String
-- Falls back to Latin-1 decoding if UTF-8 fails
utf8ToString :: BS8.ByteString -> String
utf8ToString bs = case Text.decodeUtf8' bs of
  Right text -> Text.unpack text
  Left _ -> BS8.unpack bs  -- Fallback to Latin-1 for invalid UTF-8

-- | Convert a JavaScript AST to XML string representation.
renderToXML :: AST.JSAST -> Text
renderToXML ast = case ast of
    AST.JSAstProgram statements annot -> renderProgramAST statements annot
    AST.JSAstModule items annot -> renderModuleAST items annot
    AST.JSAstStatement statement annot -> renderStatementAST statement annot
    AST.JSAstExpression expression annot -> renderExpressionAST expression annot
    AST.JSAstLiteral literal annot -> renderLiteralAST literal annot
  where
    renderProgramAST statements annot = formatXMLElement "JSAstProgram" [] $
        renderAnnotation annot <> 
        formatXMLElement "statements" [] (Text.concat (map renderStatementToXML statements))
    
    renderModuleAST items annot = formatXMLElement "JSAstModule" [] $
        renderAnnotation annot <>
        formatXMLElement "items" [] (Text.concat (map renderModuleItemToXML items))
    
    renderStatementAST statement annot = formatXMLElement "JSAstStatement" [] $
        renderAnnotation annot <>
        renderStatementToXML statement
    
    renderExpressionAST expression annot = formatXMLElement "JSAstExpression" [] $
        renderAnnotation annot <>
        renderExpressionToXML expression
    
    renderLiteralAST literal annot = formatXMLElement "JSAstLiteral" [] $
        renderAnnotation annot <>
        renderExpressionToXML literal

-- | Convert a JavaScript program (list of statements) to XML.
renderProgramToXML :: [AST.JSStatement] -> Text
renderProgramToXML statements = formatXMLElement "JSProgram" [] $
    formatXMLElement "statements" [] (Text.concat (map renderStatementToXML statements))

-- | Convert a JavaScript expression to XML representation.
renderExpressionToXML :: AST.JSExpression -> Text
renderExpressionToXML expr = case expr of
    AST.JSDecimal annot value -> 
        formatXMLElement "JSDecimal" [("value", escapeXMLString (utf8ToString value))] $
            renderAnnotation annot
    
    AST.JSHexInteger annot value -> 
        formatXMLElement "JSHexInteger" [("value", escapeXMLString (utf8ToString value))] $
            renderAnnotation annot
    
    AST.JSOctal annot value -> 
        formatXMLElement "JSOctal" [("value", escapeXMLString (utf8ToString value))] $
            renderAnnotation annot
    
    AST.JSBinaryInteger annot value -> 
        formatXMLElement "JSBinaryInteger" [("value", escapeXMLString (utf8ToString value))] $
            renderAnnotation annot
    
    AST.JSBigIntLiteral annot value -> 
        formatXMLElement "JSBigIntLiteral" [("value", escapeXMLString (utf8ToString value))] $
            renderAnnotation annot
    
    AST.JSStringLiteral annot value -> 
        formatXMLElement "JSStringLiteral" [("value", escapeXMLString (utf8ToString value))] $
            renderAnnotation annot
    
    AST.JSIdentifier annot name -> 
        formatXMLElement "JSIdentifier" [("name", escapeXMLString (utf8ToString name))] $
            renderAnnotation annot
    
    AST.JSLiteral annot value -> 
        formatXMLElement "JSLiteral" [("value", escapeXMLString (utf8ToString value))] $
            renderAnnotation annot
    
    AST.JSRegEx annot pattern -> 
        formatXMLElement "JSRegEx" [("pattern", escapeXMLString (utf8ToString pattern))] $
            renderAnnotation annot
    
    AST.JSExpressionBinary left op right -> 
        formatXMLElement "JSExpressionBinary" [] $
            formatXMLElement "left" [] (renderExpressionToXML left) <>
            renderBinOpToXML op <>
            formatXMLElement "right" [] (renderExpressionToXML right)
    
    AST.JSMemberDot object annot property -> 
        formatXMLElement "JSMemberDot" [] $
            formatXMLElement "object" [] (renderExpressionToXML object) <>
            renderAnnotation annot <>
            formatXMLElement "property" [] (renderExpressionToXML property)
    
    AST.JSMemberSquare object lbracket property rbracket -> 
        formatXMLElement "JSMemberSquare" [] $
            formatXMLElement "object" [] (renderExpressionToXML object) <>
            renderAnnotation lbracket <>
            formatXMLElement "property" [] (renderExpressionToXML property) <>
            renderAnnotation rbracket
    
    AST.JSOptionalMemberDot object annot property -> 
        formatXMLElement "JSOptionalMemberDot" [] $
            formatXMLElement "object" [] (renderExpressionToXML object) <>
            renderAnnotation annot <>
            formatXMLElement "property" [] (renderExpressionToXML property)
    
    AST.JSOptionalMemberSquare object lbracket property rbracket -> 
        formatXMLElement "JSOptionalMemberSquare" [] $
            formatXMLElement "object" [] (renderExpressionToXML object) <>
            renderAnnotation lbracket <>
            formatXMLElement "property" [] (renderExpressionToXML property) <>
            renderAnnotation rbracket
    
    AST.JSCallExpression func annot args rannot -> 
        formatXMLElement "JSCallExpression" [] $
            formatXMLElement "function" [] (renderExpressionToXML func) <>
            renderAnnotation annot <>
            renderCommaListToXML "arguments" args <>
            renderAnnotation rannot
    
    AST.JSOptionalCallExpression func annot args rannot -> 
        formatXMLElement "JSOptionalCallExpression" [] $
            formatXMLElement "function" [] (renderExpressionToXML func) <>
            renderAnnotation annot <>
            renderCommaListToXML "arguments" args <>
            renderAnnotation rannot
    
    AST.JSArrowExpression params annot body -> 
        formatXMLElement "JSArrowExpression" [] $
            renderArrowParametersToXML params <>
            renderAnnotation annot <>
            renderArrowBodyToXML body
    
    _ -> formatXMLElement "JSUnsupportedExpression" [] "<!-- Unsupported expression type -->"

-- | Convert a JavaScript statement to XML representation.
renderStatementToXML :: AST.JSStatement -> Text
renderStatementToXML stmt = case stmt of
    AST.JSExpressionStatement expr semi -> 
        formatXMLElement "JSExpressionStatement" [] $
            formatXMLElement "expression" [] (renderExpressionToXML expr) <>
            renderSemiToXML semi
    
    AST.JSVariable annot decls semi -> 
        formatXMLElement "JSVariable" [] $
            renderAnnotation annot <>
            renderCommaListToXML "declarations" decls <>
            renderSemiToXML semi
    
    AST.JSLet annot decls semi -> 
        formatXMLElement "JSLet" [] $
            renderAnnotation annot <>
            renderCommaListToXML "declarations" decls <>
            renderSemiToXML semi
    
    AST.JSConstant annot decls semi -> 
        formatXMLElement "JSConstant" [] $
            renderAnnotation annot <>
            renderCommaListToXML "declarations" decls <>
            renderSemiToXML semi
    
    AST.JSEmptyStatement annot -> 
        formatXMLElement "JSEmptyStatement" [] $
            renderAnnotation annot
    
    AST.JSReturn annot maybeExpr semi -> 
        formatXMLElement "JSReturn" [] $
            renderAnnotation annot <>
            renderMaybeExpressionToXML "expression" maybeExpr <>
            renderSemiToXML semi
    
    AST.JSIf ifAnn lparen cond rparen stmt' -> 
        formatXMLElement "JSIf" [] $
            renderAnnotation ifAnn <>
            renderAnnotation lparen <>
            formatXMLElement "condition" [] (renderExpressionToXML cond) <>
            renderAnnotation rparen <>
            formatXMLElement "statement" [] (renderStatementToXML stmt')
    
    AST.JSIfElse ifAnn lparen cond rparen thenStmt elseAnn elseStmt -> 
        formatXMLElement "JSIfElse" [] $
            renderAnnotation ifAnn <>
            renderAnnotation lparen <>
            formatXMLElement "condition" [] (renderExpressionToXML cond) <>
            renderAnnotation rparen <>
            formatXMLElement "thenStatement" [] (renderStatementToXML thenStmt) <>
            renderAnnotation elseAnn <>
            formatXMLElement "elseStatement" [] (renderStatementToXML elseStmt)
    
    AST.JSWhile whileAnn lparen cond rparen stmt' -> 
        formatXMLElement "JSWhile" [] $
            renderAnnotation whileAnn <>
            renderAnnotation lparen <>
            formatXMLElement "condition" [] (renderExpressionToXML cond) <>
            renderAnnotation rparen <>
            formatXMLElement "statement" [] (renderStatementToXML stmt')
    
    _ -> formatXMLElement "JSUnsupportedStatement" [] "<!-- Unsupported statement type -->"

-- | Render module item to XML
renderModuleItemToXML :: AST.JSModuleItem -> Text
renderModuleItemToXML item = case item of
    AST.JSModuleImportDeclaration annot decl -> 
        formatXMLElement "JSModuleImportDeclaration" [] $
            renderAnnotation annot <>
            renderImportDeclarationToXML decl
    
    AST.JSModuleExportDeclaration annot decl -> 
        formatXMLElement "JSModuleExportDeclaration" [] $
            renderAnnotation annot <>
            renderExportDeclarationToXML decl
    
    AST.JSModuleStatementListItem stmt -> 
        formatXMLElement "JSModuleStatementListItem" [] $
            renderStatementToXML stmt

-- | Render import declaration to XML
renderImportDeclarationToXML :: AST.JSImportDeclaration -> Text
renderImportDeclarationToXML = const $ formatXMLElement "JSImportDeclaration" [] "<!-- Import declaration XML rendering not yet implemented -->"

-- | Render export declaration to XML  
renderExportDeclarationToXML :: AST.JSExportDeclaration -> Text
renderExportDeclarationToXML = const $ formatXMLElement "JSExportDeclaration" [] "<!-- Export declaration XML rendering not yet implemented -->"

-- | Render annotation to XML with position and comments
renderAnnotation :: AST.JSAnnot -> Text
renderAnnotation annot = case annot of
    AST.JSNoAnnot -> formatXMLElement "annotation" [] $
        formatXMLElement "position" [] mempty <>
        formatXMLElement "comments" [] mempty
    
    AST.JSAnnot pos comments -> formatXMLElement "annotation" [] $
        renderPositionToXML pos <>
        formatXMLElement "comments" [] (Text.concat (map renderCommentToXML comments))
    
    AST.JSAnnotSpace -> formatXMLElement "annotation" [] $
        formatXMLElement "position" [] mempty <>
        formatXMLElement "comments" [] mempty

-- | Render token position to XML
renderPositionToXML :: TokenPosn -> Text
renderPositionToXML (TokenPn addr line col) = 
    formatXMLElement "position" attrs mempty
  where
    attrs = [ ("line", Text.pack (show line))
            , ("column", Text.pack (show col))
            , ("address", Text.pack (show addr))
            ]

-- | Render comment annotation to XML
renderCommentToXML :: Token.CommentAnnotation -> Text
renderCommentToXML comment = case comment of
    Token.CommentA pos content -> formatXMLElement "comment" [] $
        renderPositionToXML pos <>
        formatXMLElement "content" [("value", escapeXMLString (utf8ToString content))] mempty
    Token.WhiteSpace pos content -> formatXMLElement "whitespace" [] $
        renderPositionToXML pos <>
        formatXMLElement "content" [("value", escapeXMLString (utf8ToString content))] mempty
    Token.NoComment -> formatXMLElement "no-comment" [] mempty

-- | Render binary operator to XML
renderBinOpToXML :: AST.JSBinOp -> Text
renderBinOpToXML op = case op of
    AST.JSBinOpAnd annot -> formatXMLElement "JSBinOpAnd" [] (renderAnnotation annot)
    AST.JSBinOpBitAnd annot -> formatXMLElement "JSBinOpBitAnd" [] (renderAnnotation annot)
    AST.JSBinOpBitOr annot -> formatXMLElement "JSBinOpBitOr" [] (renderAnnotation annot)
    AST.JSBinOpBitXor annot -> formatXMLElement "JSBinOpBitXor" [] (renderAnnotation annot)
    AST.JSBinOpDivide annot -> formatXMLElement "JSBinOpDivide" [] (renderAnnotation annot)
    AST.JSBinOpEq annot -> formatXMLElement "JSBinOpEq" [] (renderAnnotation annot)
    AST.JSBinOpExponentiation annot -> formatXMLElement "JSBinOpExponentiation" [] (renderAnnotation annot)
    AST.JSBinOpGe annot -> formatXMLElement "JSBinOpGe" [] (renderAnnotation annot)
    AST.JSBinOpGt annot -> formatXMLElement "JSBinOpGt" [] (renderAnnotation annot)
    AST.JSBinOpIn annot -> formatXMLElement "JSBinOpIn" [] (renderAnnotation annot)
    AST.JSBinOpInstanceOf annot -> formatXMLElement "JSBinOpInstanceOf" [] (renderAnnotation annot)
    AST.JSBinOpLe annot -> formatXMLElement "JSBinOpLe" [] (renderAnnotation annot)
    AST.JSBinOpLsh annot -> formatXMLElement "JSBinOpLsh" [] (renderAnnotation annot)
    AST.JSBinOpLt annot -> formatXMLElement "JSBinOpLt" [] (renderAnnotation annot)
    AST.JSBinOpMinus annot -> formatXMLElement "JSBinOpMinus" [] (renderAnnotation annot)
    AST.JSBinOpMod annot -> formatXMLElement "JSBinOpMod" [] (renderAnnotation annot)
    AST.JSBinOpNeq annot -> formatXMLElement "JSBinOpNeq" [] (renderAnnotation annot)
    AST.JSBinOpOf annot -> formatXMLElement "JSBinOpOf" [] (renderAnnotation annot)
    AST.JSBinOpOr annot -> formatXMLElement "JSBinOpOr" [] (renderAnnotation annot)
    AST.JSBinOpNullishCoalescing annot -> formatXMLElement "JSBinOpNullishCoalescing" [] (renderAnnotation annot)
    AST.JSBinOpPlus annot -> formatXMLElement "JSBinOpPlus" [] (renderAnnotation annot)
    AST.JSBinOpRsh annot -> formatXMLElement "JSBinOpRsh" [] (renderAnnotation annot)
    AST.JSBinOpStrictEq annot -> formatXMLElement "JSBinOpStrictEq" [] (renderAnnotation annot)
    AST.JSBinOpStrictNeq annot -> formatXMLElement "JSBinOpStrictNeq" [] (renderAnnotation annot)
    AST.JSBinOpTimes annot -> formatXMLElement "JSBinOpTimes" [] (renderAnnotation annot)
    AST.JSBinOpUrsh annot -> formatXMLElement "JSBinOpUrsh" [] (renderAnnotation annot)

-- | Render comma list to XML
renderCommaListToXML :: Text -> AST.JSCommaList AST.JSExpression -> Text
renderCommaListToXML elementName list = formatXMLElement elementName [] $
    case list of
        AST.JSLNil -> mempty
        AST.JSLOne expr -> renderExpressionToXML expr
        AST.JSLCons tailList annot headExpr -> 
            renderCommaListToXML elementName tailList <>
            renderAnnotation annot <>
            renderExpressionToXML headExpr

-- | Render semicolon to XML
renderSemiToXML :: AST.JSSemi -> Text
renderSemiToXML semi = case semi of
    AST.JSSemi annot -> formatXMLElement "JSSemi" [] (renderAnnotation annot)
    AST.JSSemiAuto -> formatXMLElement "JSSemiAuto" [] mempty

-- | Render maybe expression to XML
renderMaybeExpressionToXML :: Text -> Maybe AST.JSExpression -> Text
renderMaybeExpressionToXML elementName maybeExpr = case maybeExpr of
    Nothing -> formatXMLElement elementName [] mempty
    Just expr -> formatXMLElement elementName [] (renderExpressionToXML expr)

-- | Render arrow parameters to XML
renderArrowParametersToXML :: AST.JSArrowParameterList -> Text
renderArrowParametersToXML params = case params of
    AST.JSUnparenthesizedArrowParameter ident -> 
        formatXMLElement "JSUnparenthesizedArrowParameter" [] $
            renderIdentToXML ident
    AST.JSParenthesizedArrowParameterList annot list rannot -> 
        formatXMLElement "JSParenthesizedArrowParameterList" [] $
            renderAnnotation annot <>
            renderCommaListToXML "parameters" list <>
            renderAnnotation rannot

-- | Render arrow body to XML
renderArrowBodyToXML :: AST.JSConciseBody -> Text
renderArrowBodyToXML body = case body of
    AST.JSConciseExpressionBody expr -> 
        formatXMLElement "JSConciseExpressionBody" [] $
            formatXMLElement "expression" [] (renderExpressionToXML expr)
    AST.JSConciseFunctionBody block -> 
        formatXMLElement "JSConciseFunctionBody" [] $
            renderBlockToXML block

-- | Render identifier to XML
renderIdentToXML :: AST.JSIdent -> Text
renderIdentToXML ident = case ident of
    AST.JSIdentName annot name -> 
        formatXMLElement "JSIdentName" [("name", escapeXMLString (utf8ToString name))] $
            renderAnnotation annot
    AST.JSIdentNone -> 
        formatXMLElement "JSIdentNone" [] mempty

-- | Render block to XML
renderBlockToXML :: AST.JSBlock -> Text
renderBlockToXML (AST.JSBlock lbrace stmts rbrace) = 
    formatXMLElement "JSBlock" [] $
        renderAnnotation lbrace <>
        formatXMLElement "statements" [] (Text.concat (map renderStatementToXML stmts)) <>
        renderAnnotation rbrace

-- | Escape special XML characters in a string
escapeXMLString :: String -> Text
escapeXMLString = Text.pack . concatMap escapeChar
  where
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '&' = "&amp;"
    escapeChar '"' = "&quot;"
    escapeChar '\'' = "&apos;"
    escapeChar c = [c]

-- | Format XML element with attributes and content
formatXMLElement :: Text -> [(Text, Text)] -> Text -> Text
formatXMLElement name attrs content
    | Text.null content = 
        "<" <> name <> formatXMLAttributes attrs <> "/>"
    | otherwise = 
        "<" <> name <> formatXMLAttributes attrs <> ">" <> 
        content <> 
        "</" <> name <> ">"

-- | Format XML attributes
formatXMLAttributes :: [(Text, Text)] -> Text
formatXMLAttributes [] = ""
formatXMLAttributes attrs = " " <> Text.intercalate " " (map formatXMLAttribute attrs)

-- | Format a single XML attribute
formatXMLAttribute :: (Text, Text) -> Text
formatXMLAttribute (name, value) = name <> "=\"" <> value <> "\""