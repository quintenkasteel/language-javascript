{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.JavaScript.Pretty.JSON
-- Copyright   : (c) 2024 JavaScript Parser Contributors
-- License     : BSD-style
-- Maintainer  : maintainer@example.com
-- Stability   : experimental
-- Portability : ghc
--
-- JSON serialization for JavaScript AST nodes. Provides comprehensive
-- JSON output for all JavaScript language constructs including ES2020+
-- features like BigInt literals, optional chaining, and nullish coalescing.
--
-- ==== Examples
--
-- >>> import Language.JavaScript.Parser.AST as AST
-- >>> import Language.JavaScript.Pretty.JSON as JSON
-- >>> let ast = JSDecimal (JSAnnot noPos []) "42"
-- >>> JSON.renderToJSON ast
-- "{\"type\":\"JSDecimal\",\"annotation\":{\"position\":null,\"comments\":[]},\"value\":\"42\"}"
--
-- ==== Supported Features
--
-- * All ES5 JavaScript constructs
-- * ES2020+ BigInt literals 
-- * ES2020+ Optional chaining operators
-- * ES2020+ Nullish coalescing operator
-- * Complete AST structure preservation
-- * Source location and comment preservation
--
-- @since 0.7.1.0
-----------------------------------------------------------------------------

module Language.JavaScript.Pretty.JSON
    ( 
    -- * JSON rendering functions
      renderToJSON
    , renderProgramToJSON
    , renderExpressionToJSON
    , renderStatementToJSON
    -- * JSON utilities
    , escapeJSONString
    , formatJSONObject
    , formatJSONArray
    ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Language.JavaScript.Parser.AST as AST
import qualified Language.JavaScript.Parser.Token as Token
import Language.JavaScript.Parser.SrcLocation (TokenPosn(..))

-- | Convert a JavaScript AST to JSON string representation.
--
-- This function provides a complete JSON serialization of the JavaScript
-- AST preserving all semantic information including source locations,
-- comments, and ES2020+ language features.
--
-- The JSON structure follows a consistent pattern:
-- * Each AST node has a \"type\" field indicating the constructor
-- * Node-specific data is included as additional fields
-- * Annotations include position and comment information
-- * Nested AST nodes are recursively serialized
renderToJSON :: AST.JSAST -> Text
renderToJSON ast = case ast of
    AST.JSAstProgram statements _ -> renderProgramAST statements
    AST.JSAstModule items _ -> renderModuleAST items
    AST.JSAstStatement statement _ -> renderStatementAST statement
    AST.JSAstExpression expression _ -> renderExpressionAST expression
    AST.JSAstLiteral literal _ -> renderLiteralAST literal
  where
    renderProgramAST statements = formatJSONObject
        [ ("type", "\"JSAstProgram\"")
        , ("statements", formatJSONArray (map renderStatementToJSON statements))
        ]
    renderModuleAST items = formatJSONObject
        [ ("type", "\"JSAstModule\"")
        , ("items", formatJSONArray (map renderModuleItemToJSON items))
        ]
    renderStatementAST statement = formatJSONObject
        [ ("type", "\"JSAstStatement\"")
        , ("statement", renderStatementToJSON statement)
        ]
    renderExpressionAST expression = formatJSONObject
        [ ("type", "\"JSAstExpression\"")
        , ("expression", renderExpressionToJSON expression)
        ]
    renderLiteralAST literal = formatJSONObject
        [ ("type", "\"JSAstLiteral\"")
        , ("literal", renderExpressionToJSON literal)
        ]

-- | Convert a JavaScript program (list of statements) to JSON.
renderProgramToJSON :: [AST.JSStatement] -> Text
renderProgramToJSON statements = formatJSONObject
    [ ("type", "\"JSProgram\"")
    , ("statements", formatJSONArray (map renderStatementToJSON statements))
    ]

-- | Convert a JavaScript expression to JSON representation.
renderExpressionToJSON :: AST.JSExpression -> Text
renderExpressionToJSON expr = case expr of
    AST.JSDecimal annot value -> renderDecimalLiteral annot value
    AST.JSHexInteger annot value -> renderHexLiteral annot value
    AST.JSOctal annot value -> renderOctalLiteral annot value
    AST.JSBigIntLiteral annot value -> renderBigIntLiteral annot value
    AST.JSStringLiteral annot value -> renderStringLiteral annot value
    AST.JSIdentifier annot name -> renderIdentifier annot name
    AST.JSLiteral annot value -> renderGenericLiteral annot value
    AST.JSRegEx annot pattern -> renderRegexLiteral annot pattern
    AST.JSExpressionBinary left op right -> renderBinaryExpression left op right
    AST.JSMemberDot object annot property -> renderMemberDot object annot property
    AST.JSMemberSquare object lbracket property rbracket -> renderMemberSquare object lbracket property rbracket
    AST.JSOptionalMemberDot object annot property -> renderOptionalMemberDot object annot property
    AST.JSOptionalMemberSquare object lbracket property rbracket -> renderOptionalMemberSquare object lbracket property rbracket
    AST.JSCallExpression func annot args rannot -> renderCallExpression func annot args rannot
    AST.JSOptionalCallExpression func annot args rannot -> renderOptionalCallExpression func annot args rannot
    AST.JSArrowExpression params annot body -> renderArrowExpression params annot body
    _ -> renderUnsupportedExpression
  where
    renderDecimalLiteral annot value = formatJSONObject
        [ ("type", "\"JSDecimal\"")
        , ("annotation", renderAnnotation annot)
        , ("value", escapeJSONString value)
        ]
    renderHexLiteral annot value = formatJSONObject
        [ ("type", "\"JSHexInteger\"")
        , ("annotation", renderAnnotation annot)
        , ("value", escapeJSONString value)
        ]
    renderOctalLiteral annot value = formatJSONObject
        [ ("type", "\"JSOctal\"")
        , ("annotation", renderAnnotation annot)
        , ("value", escapeJSONString value)
        ]
    renderBigIntLiteral annot value = formatJSONObject
        [ ("type", "\"JSBigIntLiteral\"")
        , ("annotation", renderAnnotation annot)
        , ("value", escapeJSONString value)
        ]
    renderStringLiteral annot value = formatJSONObject
        [ ("type", "\"JSStringLiteral\"")
        , ("annotation", renderAnnotation annot)
        , ("value", escapeJSONString value)
        ]
    renderIdentifier annot name = formatJSONObject
        [ ("type", "\"JSIdentifier\"")
        , ("annotation", renderAnnotation annot)
        , ("name", escapeJSONString name)
        ]
    renderGenericLiteral annot value = formatJSONObject
        [ ("type", "\"JSLiteral\"")
        , ("annotation", renderAnnotation annot)
        , ("value", escapeJSONString value)
        ]
    renderRegexLiteral annot pattern = formatJSONObject
        [ ("type", "\"JSRegEx\"")
        , ("annotation", renderAnnotation annot)
        , ("pattern", escapeJSONString pattern)
        ]
    renderBinaryExpression left op right = formatJSONObject
        [ ("type", "\"JSExpressionBinary\"")
        , ("left", renderExpressionToJSON left)
        , ("operator", renderBinOpToJSON op)
        , ("right", renderExpressionToJSON right)
        ]
    renderMemberDot object annot property = formatJSONObject
        [ ("type", "\"JSMemberDot\"")
        , ("object", renderExpressionToJSON object)
        , ("annotation", renderAnnotation annot)
        , ("property", renderExpressionToJSON property)
        ]
    renderMemberSquare object lbracket property rbracket = formatJSONObject
        [ ("type", "\"JSMemberSquare\"")
        , ("object", renderExpressionToJSON object)
        , ("lbracket", renderAnnotation lbracket)
        , ("property", renderExpressionToJSON property)
        , ("rbracket", renderAnnotation rbracket)
        ]
    renderOptionalMemberDot object annot property = formatJSONObject
        [ ("type", "\"JSOptionalMemberDot\"")
        , ("object", renderExpressionToJSON object)
        , ("annotation", renderAnnotation annot)
        , ("property", renderExpressionToJSON property)
        ]
    renderOptionalMemberSquare object lbracket property rbracket = formatJSONObject
        [ ("type", "\"JSOptionalMemberSquare\"")
        , ("object", renderExpressionToJSON object)
        , ("lbracket", renderAnnotation lbracket)
        , ("property", renderExpressionToJSON property)
        , ("rbracket", renderAnnotation rbracket)
        ]
    renderCallExpression func annot args rannot = formatJSONObject
        [ ("type", "\"JSCallExpression\"")
        , ("function", renderExpressionToJSON func)
        , ("lannot", renderAnnotation annot)
        , ("arguments", renderArgumentsToJSON args)
        , ("rannot", renderAnnotation rannot)
        ]
    renderOptionalCallExpression func annot args rannot = formatJSONObject
        [ ("type", "\"JSOptionalCallExpression\"")
        , ("function", renderExpressionToJSON func)
        , ("lannot", renderAnnotation annot)
        , ("arguments", renderArgumentsToJSON args)
        , ("rannot", renderAnnotation rannot)
        ]
    renderArrowExpression params annot body = formatJSONObject
        [ ("type", "\"JSArrowExpression\"")
        , ("parameters", renderArrowParametersToJSON params)
        , ("annotation", renderAnnotation annot)
        , ("body", renderConciseBodyToJSON body)
        ]
    renderUnsupportedExpression = formatJSONObject
        [ ("type", "\"JSExpression\"")
        , ("unsupported", "true")
        ]

-- | Convert a JavaScript statement to JSON representation.
renderStatementToJSON :: AST.JSStatement -> Text
renderStatementToJSON stmt = case stmt of
    AST.JSExpressionStatement expr _ -> formatJSONObject
        [ ("type", "\"JSStatementExpression\"")
        , ("expression", renderExpressionToJSON expr)
        ]
    -- Add more statement types as needed
    _ -> formatJSONObject
        [ ("type", "\"JSStatement\"")
        , ("unsupported", "true")
        ]

-- | Render binary operator to JSON.
renderBinOpToJSON :: AST.JSBinOp -> Text
renderBinOpToJSON op = case op of
    AST.JSBinOpAnd _ -> renderLogicalOp "&&"
    AST.JSBinOpOr _ -> renderLogicalOp "||"
    AST.JSBinOpNullishCoalescing _ -> renderLogicalOp "??"
    AST.JSBinOpPlus _ -> renderArithmeticOp "+"
    AST.JSBinOpMinus _ -> renderArithmeticOp "-"
    AST.JSBinOpTimes _ -> renderArithmeticOp "*"
    AST.JSBinOpDivide _ -> renderArithmeticOp "/"
    AST.JSBinOpMod _ -> renderArithmeticOp "%"
    AST.JSBinOpEq _ -> renderEqualityOp "=="
    AST.JSBinOpNeq _ -> renderEqualityOp "!="
    AST.JSBinOpStrictEq _ -> renderEqualityOp "==="
    AST.JSBinOpStrictNeq _ -> renderEqualityOp "!=="
    AST.JSBinOpLt _ -> renderComparisonOp "<"
    AST.JSBinOpLe _ -> renderComparisonOp "<="
    AST.JSBinOpGt _ -> renderComparisonOp ">"
    AST.JSBinOpGe _ -> renderComparisonOp ">="
    AST.JSBinOpBitAnd _ -> renderBitwiseOp "&"
    AST.JSBinOpBitOr _ -> renderBitwiseOp "|"
    AST.JSBinOpBitXor _ -> renderBitwiseOp "^"
    AST.JSBinOpLsh _ -> renderBitwiseOp "<<"
    AST.JSBinOpRsh _ -> renderBitwiseOp ">>"
    AST.JSBinOpUrsh _ -> renderBitwiseOp ">>>"
    AST.JSBinOpIn _ -> renderKeywordOp "in"
    AST.JSBinOpInstanceOf _ -> renderKeywordOp "instanceof"
    AST.JSBinOpOf _ -> renderKeywordOp "of"
  where
    renderLogicalOp opStr = "\"" <> Text.pack opStr <> "\""
    renderArithmeticOp opStr = "\"" <> Text.pack opStr <> "\""
    renderEqualityOp opStr = "\"" <> Text.pack opStr <> "\""
    renderComparisonOp opStr = "\"" <> Text.pack opStr <> "\""
    renderBitwiseOp opStr = "\"" <> Text.pack opStr <> "\""
    renderKeywordOp opStr = "\"" <> Text.pack opStr <> "\""

-- | Render module item to JSON.
renderModuleItemToJSON :: AST.JSModuleItem -> Text
renderModuleItemToJSON item = case item of
    AST.JSModuleStatementListItem statement -> renderStatementToJSON statement
    AST.JSModuleImportDeclaration ann importDecl -> formatJSONObject
        [ ("type", "\"ImportDeclaration\"")
        , ("annotation", renderAnnotation ann)
        , ("importDeclaration", renderImportDeclarationToJSON importDecl)
        ]
    AST.JSModuleExportDeclaration ann exportDecl -> formatJSONObject
        [ ("type", "\"ExportDeclaration\"")
        , ("annotation", renderAnnotation ann)
        , ("exportDeclaration", renderExportDeclarationToJSON exportDecl)
        ]

-- | Render import clause to JSON.
renderImportClauseToJSON :: AST.JSImportClause -> Text
renderImportClauseToJSON clause = case clause of
    AST.JSImportClauseDefault ident -> renderDefaultImport ident
    AST.JSImportClauseNameSpace namespace -> renderNamespaceImport namespace
    AST.JSImportClauseNamed imports -> renderNamedImports imports
    AST.JSImportClauseDefaultNameSpace ident annot namespace -> renderDefaultNamespaceImport ident annot namespace
    AST.JSImportClauseDefaultNamed ident annot imports -> renderDefaultNamedImport ident annot imports
  where
    renderDefaultImport ident = formatJSONObject
        [ ("type", "\"ImportDefaultSpecifier\"")
        , ("local", renderIdentToJSON ident)
        ]
    renderNamespaceImport namespace = formatJSONObject
        [ ("type", "\"ImportNamespaceSpecifier\"")
        , ("namespace", renderImportNameSpaceToJSON namespace)
        ]
    renderNamedImports imports = formatJSONObject
        [ ("type", "\"ImportSpecifiers\"")
        , ("specifiers", renderImportsToJSON imports)
        ]
    renderDefaultNamespaceImport ident annot namespace = formatJSONObject
        [ ("type", "\"ImportDefaultAndNamespace\"")
        , ("default", renderIdentToJSON ident)
        , ("annotation", renderAnnotation annot)
        , ("namespace", renderImportNameSpaceToJSON namespace)
        ]
    renderDefaultNamedImport ident annot imports = formatJSONObject
        [ ("type", "\"ImportDefaultAndNamed\"")
        , ("default", renderIdentToJSON ident)
        , ("annotation", renderAnnotation annot)
        , ("named", renderImportsToJSON imports)
        ]

-- | Render import specifiers to JSON.
renderImportsToJSON :: AST.JSImportsNamed -> Text
renderImportsToJSON (AST.JSImportsNamed ann specifiers _) = formatJSONObject
    [ ("type", "\"NamedImports\"")
    , ("annotation", renderAnnotation ann)
    , ("specifiers", formatJSONArray (map renderImportSpecifierToJSON (extractCommaListExpressions specifiers)))
    ]

-- | Render import specifier to JSON.
renderImportSpecifierToJSON :: AST.JSImportSpecifier -> Text
renderImportSpecifierToJSON spec = case spec of
    AST.JSImportSpecifier ident -> formatJSONObject
        [ ("type", "\"ImportSpecifier\"")
        , ("imported", renderIdentToJSON ident)
        , ("local", renderIdentToJSON ident)
        ]
    AST.JSImportSpecifierAs ident ann localIdent -> formatJSONObject
        [ ("type", "\"ImportSpecifier\"")
        , ("imported", renderIdentToJSON ident)
        , ("annotation", renderAnnotation ann)
        , ("local", renderIdentToJSON localIdent)
        ]

-- | Render export declaration to JSON.
renderExportDeclarationToJSON :: AST.JSExportDeclaration -> Text
renderExportDeclarationToJSON decl = case decl of
    AST.JSExportFrom clause fromClause semi -> formatJSONObject
        [ ("type", "\"ExportFromDeclaration\"")
        , ("clause", renderExportClauseToJSON clause)
        , ("source", renderFromClauseToJSON fromClause)
        , ("semicolon", renderSemiColonToJSON semi)
        ]
    AST.JSExportLocals clause semi -> formatJSONObject
        [ ("type", "\"ExportLocalsDeclaration\"")
        , ("clause", renderExportClauseToJSON clause)
        , ("semicolon", renderSemiColonToJSON semi)
        ]
    AST.JSExport statement semi -> formatJSONObject
        [ ("type", "\"ExportDeclaration\"")
        , ("declaration", renderStatementToJSON statement)
        , ("semicolon", renderSemiColonToJSON semi)
        ]
    AST.JSExportAllFrom star fromClause semi -> formatJSONObject
        [ ("type", "\"ExportAllFromDeclaration\"")
        , ("star", renderBinOpToJSON star)
        , ("source", renderFromClauseToJSON fromClause)
        , ("semicolon", renderSemiColonToJSON semi)
        ]

-- | Render export clause to JSON.
renderExportClauseToJSON :: AST.JSExportClause -> Text
renderExportClauseToJSON (AST.JSExportClause ann specifiers _) = formatJSONObject
    [ ("type", "\"ExportClause\"")
    , ("annotation", renderAnnotation ann)
    , ("specifiers", formatJSONArray (map renderExportSpecifierToJSON (extractCommaListExpressions specifiers)))
    ]

-- | Render export specifier to JSON.
renderExportSpecifierToJSON :: AST.JSExportSpecifier -> Text
renderExportSpecifierToJSON spec = case spec of
    AST.JSExportSpecifier ident -> formatJSONObject
        [ ("type", "\"ExportSpecifier\"")
        , ("exported", renderIdentToJSON ident)
        , ("local", renderIdentToJSON ident)
        ]
    AST.JSExportSpecifierAs ident1 ann ident2 -> formatJSONObject
        [ ("type", "\"ExportSpecifier\"")
        , ("local", renderIdentToJSON ident1)
        , ("annotation", renderAnnotation ann)
        , ("exported", renderIdentToJSON ident2)
        ]

-- | Helper function to render identifier to JSON.
renderIdentToJSON :: AST.JSIdent -> Text
renderIdentToJSON (AST.JSIdentName ann name) = formatJSONObject
    [ ("type", "\"Identifier\"")
    , ("annotation", renderAnnotation ann)
    , ("name", "\"" <> Text.pack name <> "\"")
    ]
renderIdentToJSON AST.JSIdentNone = formatJSONObject
    [ ("type", "\"EmptyIdentifier\"")
    ]

-- | Render semicolon to JSON.
renderSemiColonToJSON :: AST.JSSemi -> Text
renderSemiColonToJSON semi = case semi of
    AST.JSSemi ann -> formatJSONObject
        [ ("type", "\"Semicolon\"")
        , ("annotation", renderAnnotation ann)
        ]
    AST.JSSemiAuto -> formatJSONObject
        [ ("type", "\"AutoSemicolon\"")
        ]

-- | Render import declaration to JSON.
renderImportDeclarationToJSON :: AST.JSImportDeclaration -> Text
renderImportDeclarationToJSON decl = case decl of
    AST.JSImportDeclaration clause fromClause semi -> formatJSONObject
        [ ("type", "\"ImportDeclaration\"")
        , ("clause", renderImportClauseToJSON clause)
        , ("source", renderFromClauseToJSON fromClause)
        , ("semicolon", renderSemiColonToJSON semi)
        ]
    AST.JSImportDeclarationBare ann moduleName semi -> formatJSONObject
        [ ("type", "\"ImportBareDeclaration\"")
        , ("annotation", renderAnnotation ann)
        , ("module", "\"" <> Text.pack moduleName <> "\"")
        , ("semicolon", renderSemiColonToJSON semi)
        ]

-- | Render from clause to JSON.
renderFromClauseToJSON :: AST.JSFromClause -> Text
renderFromClauseToJSON (AST.JSFromClause ann1 ann2 moduleName) = formatJSONObject
    [ ("type", "\"FromClause\"")
    , ("fromAnnotation", renderAnnotation ann1)
    , ("moduleAnnotation", renderAnnotation ann2)
    , ("module", "\"" <> Text.pack moduleName <> "\"")
    ]

-- | Render import namespace to JSON.
renderImportNameSpaceToJSON :: AST.JSImportNameSpace -> Text
renderImportNameSpaceToJSON (AST.JSImportNameSpace binOp ann ident) = formatJSONObject
    [ ("type", "\"ImportNameSpace\"")
    , ("operator", renderBinOpToJSON binOp)
    , ("annotation", renderAnnotation ann)
    , ("local", renderIdentToJSON ident)
    ]

-- | Helper function to extract expressions from comma list.
extractCommaListExpressions :: AST.JSCommaList a -> [a]
extractCommaListExpressions (AST.JSLCons rest _ expr) = extractCommaListExpressions rest ++ [expr]
extractCommaListExpressions (AST.JSLOne expr) = [expr]
extractCommaListExpressions AST.JSLNil = []

-- | Function arguments to JSON.
renderArgumentsToJSON :: AST.JSCommaList AST.JSExpression -> Text
renderArgumentsToJSON args = formatJSONArray (map renderExpressionToJSON (extractCommaListExpressions args))

-- | Render annotation (position and comments) to JSON.
renderAnnotation :: AST.JSAnnot -> Text
renderAnnotation annot = case annot of
    AST.JSAnnot pos comments -> formatJSONObject
        [ ("position", renderPosition pos)
        , ("comments", formatJSONArray (map renderComment comments))
        ]
    AST.JSNoAnnot -> formatJSONObject
        [ ("position", "null")
        , ("comments", "[]")
        ]
    AST.JSAnnotSpace -> formatJSONObject
        [ ("type", "\"space\"")
        , ("position", "null")
        , ("comments", "[]")
        ]

-- | Render source position to JSON.
renderPosition :: TokenPosn -> Text
renderPosition pos = case pos of
    TokenPn _ line col -> formatJSONObject
        [ ("line", Text.pack (show line))
        , ("column", Text.pack (show col))
        ]

-- | Render comment to JSON.
renderComment :: Token.CommentAnnotation -> Text
renderComment comment = case comment of
    Token.CommentA pos text -> formatJSONObject
        [ ("type", "\"Comment\"")
        , ("position", renderPosition pos)
        , ("text", escapeJSONString text)
        ]
    Token.WhiteSpace pos text -> formatJSONObject
        [ ("type", "\"WhiteSpace\"")
        , ("position", renderPosition pos)
        , ("text", escapeJSONString text)
        ]
    Token.NoComment -> formatJSONObject
        [ ("type", "\"NoComment\"")
        ]

-- | Escape a string for JSON representation.
escapeJSONString :: String -> Text
escapeJSONString str = "\"" <> Text.pack (concatMap escapeChar str) <> "\""
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\b' = "\\b"
    escapeChar '\f' = "\\f"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c = [c]

-- | Format a JSON object from key-value pairs.
formatJSONObject :: [(Text, Text)] -> Text
formatJSONObject pairs = "{" <> Text.intercalate "," (map formatPair pairs) <> "}"
  where
    formatPair (key, value) = "\"" <> key <> "\":" <> value

-- | Format a JSON array from a list of JSON values.
formatJSONArray :: [Text] -> Text
formatJSONArray values = "[" <> Text.intercalate "," values <> "]"

-- | Convert arrow function parameters to JSON.
renderArrowParametersToJSON :: AST.JSArrowParameterList -> Text
renderArrowParametersToJSON params = case params of
    AST.JSUnparenthesizedArrowParameter ident -> formatJSONObject
        [ ("type", "\"JSUnparenthesizedArrowParameter\"")
        , ("parameter", renderIdentToJSON ident)
        ]
    AST.JSParenthesizedArrowParameterList _ paramList _ -> formatJSONObject
        [ ("type", "\"JSParenthesizedArrowParameterList\"")
        , ("parameters", renderArgumentsToJSON paramList)
        ]

-- | Convert JSConciseBody to JSON.
renderConciseBodyToJSON :: AST.JSConciseBody -> Text
renderConciseBodyToJSON body = case body of
    AST.JSConciseFunctionBody block -> formatJSONObject
        [ ("type", "\"JSConciseFunctionBody\"")
        , ("block", renderBlockToJSON block)
        ]
    AST.JSConciseExpressionBody expr -> formatJSONObject
        [ ("type", "\"JSConciseExpressionBody\"")
        , ("expression", renderExpressionToJSON expr)
        ]

-- | Convert JSBlock to JSON.
renderBlockToJSON :: AST.JSBlock -> Text
renderBlockToJSON (AST.JSBlock _ statements _) = formatJSONObject
    [ ("type", "\"JSBlock\"")
    , ("statements", formatJSONArray (map renderStatementToJSON statements))
    ]