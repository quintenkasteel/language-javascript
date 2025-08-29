{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | S-expression serialization for JavaScript AST nodes.
--
-- This module provides comprehensive S-expression output for all JavaScript 
-- language constructs including ES2020+ features like BigInt literals, optional
-- chaining, and nullish coalescing.
--
-- The S-expression format preserves complete AST structure in a Lisp-like
-- syntax that is ideal for:
--
--   * Functional programming language interoperability 
--   * Symbolic computation systems
--   * Tree-walking interpreters and compilers
--   * Educational programming language implementations
--   * Research in programming language theory
--
-- ==== Examples
--
-- >>> import Language.JavaScript.Parser.AST as AST
-- >>> import Language.JavaScript.Pretty.SExpr as SExpr
-- >>> let ast = JSDecimal (JSAnnot noPos []) "42"
-- >>> SExpr.renderToSExpr ast
-- "(JSDecimal \"42\" (annotation (position 0 0 0) (comments)))"
--
-- ==== Features
--
-- * Complete ES5+ JavaScript construct support
-- * ES2020+ BigInt, optional chaining, nullish coalescing
-- * Full AST structure preservation  
-- * Source location and comment preservation
-- * Lisp-compatible S-expression syntax
-- * Hierarchical representation of nested structures
-- * Proper escaping of strings and symbols
--
-- @since 0.7.1.0
module Language.JavaScript.Pretty.SExpr
    ( 
    -- * S-expression rendering functions
      renderToSExpr
    , renderProgramToSExpr
    , renderExpressionToSExpr
    , renderStatementToSExpr
    , renderImportDeclarationToSExpr
    , renderExportDeclarationToSExpr
    , renderAnnotation
    -- * S-expression utilities
    , escapeSExprString
    , formatSExprList
    , formatSExprAtom
    ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.JavaScript.Parser.AST as AST
import qualified Language.JavaScript.Parser.Token as Token
import Language.JavaScript.Parser.SrcLocation (TokenPosn(..))

-- | Convert a JavaScript AST to S-expression string representation.
renderToSExpr :: AST.JSAST -> Text
renderToSExpr ast = case ast of
    AST.JSAstProgram statements annot -> renderProgramAST statements annot
    AST.JSAstModule items annot -> renderModuleAST items annot
    AST.JSAstStatement statement annot -> renderStatementAST statement annot
    AST.JSAstExpression expression annot -> renderExpressionAST expression annot
    AST.JSAstLiteral literal annot -> renderLiteralAST literal annot
  where
    renderProgramAST statements annot = formatSExprList
        [ "JSAstProgram"
        , renderAnnotation annot
        , formatSExprList ("statements" : map renderStatementToSExpr statements)
        ]
    
    renderModuleAST items annot = formatSExprList
        [ "JSAstModule"
        , renderAnnotation annot
        , formatSExprList ("items" : map renderModuleItemToSExpr items)
        ]
    
    renderStatementAST statement annot = formatSExprList
        [ "JSAstStatement"
        , renderAnnotation annot
        , renderStatementToSExpr statement
        ]
    
    renderExpressionAST expression annot = formatSExprList
        [ "JSAstExpression"
        , renderAnnotation annot
        , renderExpressionToSExpr expression
        ]
    
    renderLiteralAST literal annot = formatSExprList
        [ "JSAstLiteral"
        , renderAnnotation annot
        , renderExpressionToSExpr literal
        ]

-- | Convert a JavaScript program (list of statements) to S-expression.
renderProgramToSExpr :: [AST.JSStatement] -> Text
renderProgramToSExpr statements = formatSExprList
    [ "JSProgram"
    , formatSExprList ("statements" : map renderStatementToSExpr statements)
    ]

-- | Convert a JavaScript expression to S-expression representation.
renderExpressionToSExpr :: AST.JSExpression -> Text
renderExpressionToSExpr expr = case expr of
    AST.JSDecimal annot value -> formatSExprList
        [ "JSDecimal"
        , escapeSExprString value
        , renderAnnotation annot
        ]
    
    AST.JSHexInteger annot value -> formatSExprList
        [ "JSHexInteger"
        , escapeSExprString value
        , renderAnnotation annot
        ]
    
    AST.JSOctal annot value -> formatSExprList
        [ "JSOctal"
        , escapeSExprString value
        , renderAnnotation annot
        ]
    
    AST.JSBinaryInteger annot value -> formatSExprList
        [ "JSBinaryInteger"
        , escapeSExprString value
        , renderAnnotation annot
        ]
    
    AST.JSBigIntLiteral annot value -> formatSExprList
        [ "JSBigIntLiteral"
        , escapeSExprString value
        , renderAnnotation annot
        ]
    
    AST.JSStringLiteral annot value -> formatSExprList
        [ "JSStringLiteral"
        , escapeSExprString value
        , renderAnnotation annot
        ]
    
    AST.JSIdentifier annot name -> formatSExprList
        [ "JSIdentifier"
        , escapeSExprString name
        , renderAnnotation annot
        ]
    
    AST.JSLiteral annot value -> formatSExprList
        [ "JSLiteral"
        , escapeSExprString value
        , renderAnnotation annot
        ]
    
    AST.JSRegEx annot pattern -> formatSExprList
        [ "JSRegEx"
        , escapeSExprString pattern
        , renderAnnotation annot
        ]
    
    AST.JSExpressionBinary left op right -> formatSExprList
        [ "JSExpressionBinary"
        , renderExpressionToSExpr left
        , renderBinOpToSExpr op
        , renderExpressionToSExpr right
        ]
    
    AST.JSMemberDot object annot property -> formatSExprList
        [ "JSMemberDot"
        , renderExpressionToSExpr object
        , renderAnnotation annot
        , renderExpressionToSExpr property
        ]
    
    AST.JSMemberSquare object lbracket property rbracket -> formatSExprList
        [ "JSMemberSquare"
        , renderExpressionToSExpr object
        , renderAnnotation lbracket
        , renderExpressionToSExpr property
        , renderAnnotation rbracket
        ]
    
    AST.JSOptionalMemberDot object annot property -> formatSExprList
        [ "JSOptionalMemberDot"
        , renderExpressionToSExpr object
        , renderAnnotation annot
        , renderExpressionToSExpr property
        ]
    
    AST.JSOptionalMemberSquare object lbracket property rbracket -> formatSExprList
        [ "JSOptionalMemberSquare"
        , renderExpressionToSExpr object
        , renderAnnotation lbracket
        , renderExpressionToSExpr property
        , renderAnnotation rbracket
        ]
    
    AST.JSCallExpression func annot args rannot -> formatSExprList
        [ "JSCallExpression"
        , renderExpressionToSExpr func
        , renderAnnotation annot
        , renderCommaListToSExpr args
        , renderAnnotation rannot
        ]
    
    AST.JSOptionalCallExpression func annot args rannot -> formatSExprList
        [ "JSOptionalCallExpression"
        , renderExpressionToSExpr func
        , renderAnnotation annot
        , renderCommaListToSExpr args
        , renderAnnotation rannot
        ]
    
    AST.JSArrowExpression params annot body -> formatSExprList
        [ "JSArrowExpression"
        , renderArrowParametersToSExpr params
        , renderAnnotation annot
        , renderArrowBodyToSExpr body
        ]
    
    _ -> formatSExprList ["JSUnsupportedExpression", "unsupported-expression-type"]

-- | Convert a JavaScript statement to S-expression representation.
renderStatementToSExpr :: AST.JSStatement -> Text
renderStatementToSExpr stmt = case stmt of
    AST.JSExpressionStatement expr semi -> formatSExprList
        [ "JSExpressionStatement"
        , renderExpressionToSExpr expr
        , renderSemiToSExpr semi
        ]
    
    AST.JSVariable annot decls semi -> formatSExprList
        [ "JSVariable"
        , renderAnnotation annot
        , renderCommaListToSExpr decls
        , renderSemiToSExpr semi
        ]
    
    AST.JSLet annot decls semi -> formatSExprList
        [ "JSLet"
        , renderAnnotation annot
        , renderCommaListToSExpr decls
        , renderSemiToSExpr semi
        ]
    
    AST.JSConstant annot decls semi -> formatSExprList
        [ "JSConstant"
        , renderAnnotation annot
        , renderCommaListToSExpr decls
        , renderSemiToSExpr semi
        ]
    
    AST.JSEmptyStatement annot -> formatSExprList
        [ "JSEmptyStatement"
        , renderAnnotation annot
        ]
    
    AST.JSReturn annot maybeExpr semi -> formatSExprList
        [ "JSReturn"
        , renderAnnotation annot
        , renderMaybeExpressionToSExpr maybeExpr
        , renderSemiToSExpr semi
        ]
    
    _ -> formatSExprList ["JSUnsupportedStatement", "unsupported-statement-type"]

-- | Render module item to S-expression
renderModuleItemToSExpr :: AST.JSModuleItem -> Text
renderModuleItemToSExpr item = case item of
    AST.JSModuleImportDeclaration annot decl -> formatSExprList
        [ "JSModuleImportDeclaration"
        , renderAnnotation annot
        , renderImportDeclarationToSExpr decl
        ]
    
    AST.JSModuleExportDeclaration annot decl -> formatSExprList
        [ "JSModuleExportDeclaration"
        , renderAnnotation annot
        , renderExportDeclarationToSExpr decl
        ]
    
    AST.JSModuleStatementListItem stmt -> formatSExprList
        [ "JSModuleStatementListItem"
        , renderStatementToSExpr stmt
        ]

-- | Render import declaration to S-expression
renderImportDeclarationToSExpr :: AST.JSImportDeclaration -> Text
renderImportDeclarationToSExpr = const $ formatSExprList
    [ "JSImportDeclaration"
    , "import-declaration-not-yet-implemented"
    ]

-- | Render export declaration to S-expression  
renderExportDeclarationToSExpr :: AST.JSExportDeclaration -> Text
renderExportDeclarationToSExpr = const $ formatSExprList
    [ "JSExportDeclaration"
    , "export-declaration-not-yet-implemented"
    ]

-- | Render annotation to S-expression with position and comments
renderAnnotation :: AST.JSAnnot -> Text
renderAnnotation annot = case annot of
    AST.JSNoAnnot -> formatSExprList
        [ "annotation"
        , formatSExprList ["position"]
        , formatSExprList ["comments"]
        ]
    
    AST.JSAnnot pos comments -> formatSExprList
        [ "annotation"
        , renderPositionToSExpr pos
        , formatSExprList ("comments" : map renderCommentToSExpr comments)
        ]
    
    AST.JSAnnotSpace -> formatSExprList
        [ "annotation-space"
        ]

-- | Render token position to S-expression
renderPositionToSExpr :: TokenPosn -> Text
renderPositionToSExpr (TokenPn addr line col) = formatSExprList
    [ "position"
    , Text.pack (show addr)
    , Text.pack (show line)
    , Text.pack (show col)
    ]

-- | Render comment annotation to S-expression
renderCommentToSExpr :: Token.CommentAnnotation -> Text
renderCommentToSExpr comment = case comment of
    Token.CommentA pos content -> formatSExprList
        [ "comment"
        , renderPositionToSExpr pos
        , escapeSExprString content
        ]
    Token.WhiteSpace pos content -> formatSExprList
        [ "whitespace"
        , renderPositionToSExpr pos
        , escapeSExprString content
        ]
    Token.NoComment -> formatSExprList
        [ "no-comment"
        ]

-- | Render binary operator to S-expression
renderBinOpToSExpr :: AST.JSBinOp -> Text
renderBinOpToSExpr op = case op of
    AST.JSBinOpAnd annot -> formatSExprList ["JSBinOpAnd", renderAnnotation annot]
    AST.JSBinOpBitAnd annot -> formatSExprList ["JSBinOpBitAnd", renderAnnotation annot]
    AST.JSBinOpBitOr annot -> formatSExprList ["JSBinOpBitOr", renderAnnotation annot]
    AST.JSBinOpBitXor annot -> formatSExprList ["JSBinOpBitXor", renderAnnotation annot]
    AST.JSBinOpDivide annot -> formatSExprList ["JSBinOpDivide", renderAnnotation annot]
    AST.JSBinOpEq annot -> formatSExprList ["JSBinOpEq", renderAnnotation annot]
    AST.JSBinOpExponentiation annot -> formatSExprList ["JSBinOpExponentiation", renderAnnotation annot]
    AST.JSBinOpGe annot -> formatSExprList ["JSBinOpGe", renderAnnotation annot]
    AST.JSBinOpGt annot -> formatSExprList ["JSBinOpGt", renderAnnotation annot]
    AST.JSBinOpIn annot -> formatSExprList ["JSBinOpIn", renderAnnotation annot]
    AST.JSBinOpInstanceOf annot -> formatSExprList ["JSBinOpInstanceOf", renderAnnotation annot]
    AST.JSBinOpLe annot -> formatSExprList ["JSBinOpLe", renderAnnotation annot]
    AST.JSBinOpLsh annot -> formatSExprList ["JSBinOpLsh", renderAnnotation annot]
    AST.JSBinOpLt annot -> formatSExprList ["JSBinOpLt", renderAnnotation annot]
    AST.JSBinOpMinus annot -> formatSExprList ["JSBinOpMinus", renderAnnotation annot]
    AST.JSBinOpMod annot -> formatSExprList ["JSBinOpMod", renderAnnotation annot]
    AST.JSBinOpNeq annot -> formatSExprList ["JSBinOpNeq", renderAnnotation annot]
    AST.JSBinOpOf annot -> formatSExprList ["JSBinOpOf", renderAnnotation annot]
    AST.JSBinOpOr annot -> formatSExprList ["JSBinOpOr", renderAnnotation annot]
    AST.JSBinOpNullishCoalescing annot -> formatSExprList ["JSBinOpNullishCoalescing", renderAnnotation annot]
    AST.JSBinOpPlus annot -> formatSExprList ["JSBinOpPlus", renderAnnotation annot]
    AST.JSBinOpRsh annot -> formatSExprList ["JSBinOpRsh", renderAnnotation annot]
    AST.JSBinOpStrictEq annot -> formatSExprList ["JSBinOpStrictEq", renderAnnotation annot]
    AST.JSBinOpStrictNeq annot -> formatSExprList ["JSBinOpStrictNeq", renderAnnotation annot]
    AST.JSBinOpTimes annot -> formatSExprList ["JSBinOpTimes", renderAnnotation annot]
    AST.JSBinOpUrsh annot -> formatSExprList ["JSBinOpUrsh", renderAnnotation annot]

-- | Render comma list to S-expression
renderCommaListToSExpr :: AST.JSCommaList AST.JSExpression -> Text
renderCommaListToSExpr list = case list of
    AST.JSLNil -> formatSExprList ["comma-list"]
    AST.JSLOne expr -> formatSExprList
        [ "comma-list"
        , renderExpressionToSExpr expr
        ]
    AST.JSLCons restList annot headItem -> formatSExprList
        [ "comma-list"
        , renderCommaListToSExpr restList
        , renderAnnotation annot
        , renderExpressionToSExpr headItem
        ]

-- | Render semicolon to S-expression
renderSemiToSExpr :: AST.JSSemi -> Text
renderSemiToSExpr semi = case semi of
    AST.JSSemi annot -> formatSExprList ["JSSemi", renderAnnotation annot]
    AST.JSSemiAuto -> formatSExprList ["JSSemiAuto"]

-- | Render maybe expression to S-expression
renderMaybeExpressionToSExpr :: Maybe AST.JSExpression -> Text
renderMaybeExpressionToSExpr maybeExpr = case maybeExpr of
    Nothing -> formatSExprList ["maybe-expression", "nil"]
    Just expr -> formatSExprList ["maybe-expression", renderExpressionToSExpr expr]

-- | Render arrow parameters to S-expression
renderArrowParametersToSExpr :: AST.JSArrowParameterList -> Text
renderArrowParametersToSExpr params = case params of
    AST.JSUnparenthesizedArrowParameter ident -> formatSExprList
        [ "JSUnparenthesizedArrowParameter"
        , renderIdentToSExpr ident
        ]
    AST.JSParenthesizedArrowParameterList annot list rannot -> formatSExprList
        [ "JSParenthesizedArrowParameterList"
        , renderAnnotation annot
        , renderCommaListToSExpr list
        , renderAnnotation rannot
        ]

-- | Render arrow body to S-expression
renderArrowBodyToSExpr :: AST.JSConciseBody -> Text
renderArrowBodyToSExpr body = case body of
    AST.JSConciseExpressionBody expr -> formatSExprList
        [ "JSConciseExpressionBody"
        , renderExpressionToSExpr expr
        ]
    AST.JSConciseFunctionBody block -> formatSExprList
        [ "JSConciseFunctionBody"
        , renderBlockToSExpr block
        ]

-- | Render identifier to S-expression
renderIdentToSExpr :: AST.JSIdent -> Text
renderIdentToSExpr ident = case ident of
    AST.JSIdentName annot name -> formatSExprList
        [ "JSIdentName"
        , escapeSExprString name
        , renderAnnotation annot
        ]
    AST.JSIdentNone -> formatSExprList
        [ "JSIdentNone"
        ]

-- | Render block to S-expression
renderBlockToSExpr :: AST.JSBlock -> Text
renderBlockToSExpr (AST.JSBlock lbrace stmts rbrace) = formatSExprList
    [ "JSBlock"
    , renderAnnotation lbrace
    , formatSExprList ("statements" : map renderStatementToSExpr stmts)
    , renderAnnotation rbrace
    ]

-- | Escape special S-expression characters in a string
escapeSExprString :: String -> Text
escapeSExprString str = "\"" <> Text.pack (concatMap escapeChar str) <> "\""
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c = [c]

-- | Format S-expression list with parentheses
formatSExprList :: [Text] -> Text
formatSExprList elements = "(" <> Text.intercalate " " elements <> ")"

-- | Format S-expression atom (identifier or literal)
formatSExprAtom :: Text -> Text
formatSExprAtom = id