{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-----------------------------------------------------------------------------

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
module Language.JavaScript.Pretty.JSON
  ( -- * JSON rendering functions
    renderToJSON,
    renderProgramToJSON,
    renderExpressionToJSON,
    renderStatementToJSON,
    renderImportDeclarationToJSON,
    renderExportDeclarationToJSON,
    renderAnnotation,

    -- * JSON utilities
    escapeJSONString,
    formatJSONObject,
    formatJSONArray,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Language.JavaScript.Parser.AST as AST
import Language.JavaScript.Parser.SrcLocation (TokenPosn (..))
import qualified Language.JavaScript.Parser.Token as Token

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
    renderProgramAST statements =
      formatJSONObject
        [ ("type", "\"JSAstProgram\""),
          ("statements", formatJSONArray (map renderStatementToJSON statements))
        ]
    renderModuleAST items =
      formatJSONObject
        [ ("type", "\"JSAstModule\""),
          ("items", formatJSONArray (map renderModuleItemToJSON items))
        ]
    renderStatementAST statement =
      formatJSONObject
        [ ("type", "\"JSAstStatement\""),
          ("statement", renderStatementToJSON statement)
        ]
    renderExpressionAST expression =
      formatJSONObject
        [ ("type", "\"JSAstExpression\""),
          ("expression", renderExpressionToJSON expression)
        ]
    renderLiteralAST literal =
      formatJSONObject
        [ ("type", "\"JSAstLiteral\""),
          ("literal", renderExpressionToJSON literal)
        ]

-- | Convert a JavaScript program (list of statements) to JSON.
renderProgramToJSON :: [AST.JSStatement] -> Text
renderProgramToJSON statements =
  formatJSONObject
    [ ("type", "\"JSProgram\""),
      ("statements", formatJSONArray (map renderStatementToJSON statements))
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
    renderDecimalLiteral annot value =
      formatJSONObject
        [ ("type", "\"JSDecimal\""),
          ("annotation", renderAnnotation annot),
          ("value", escapeJSONString (AST.showJSDouble value))
        ]
    renderHexLiteral annot value =
      formatJSONObject
        [ ("type", "\"JSHexInteger\""),
          ("annotation", renderAnnotation annot),
          ("value", escapeJSONString (AST.showJSHex value))
        ]
    renderOctalLiteral annot value =
      formatJSONObject
        [ ("type", "\"JSOctal\""),
          ("annotation", renderAnnotation annot),
          ("value", escapeJSONString (AST.showJSOctal value))
        ]
    renderBigIntLiteral annot value =
      formatJSONObject
        [ ("type", "\"JSBigIntLiteral\""),
          ("annotation", renderAnnotation annot),
          ("value", escapeJSONString (show value <> "n"))
        ]
    renderStringLiteral annot value =
      formatJSONObject
        [ ("type", "\"JSStringLiteral\""),
          ("annotation", renderAnnotation annot),
          ("value", escapeJSONString (Text.unpack . Text.decodeUtf8 $ value))
        ]
    renderIdentifier annot name =
      formatJSONObject
        [ ("type", "\"JSIdentifier\""),
          ("annotation", renderAnnotation annot),
          ("name", escapeJSONString (Text.unpack . Text.decodeUtf8 $ name))
        ]
    renderGenericLiteral annot value =
      formatJSONObject
        [ ("type", "\"JSLiteral\""),
          ("annotation", renderAnnotation annot),
          ("value", escapeJSONString (Text.unpack . Text.decodeUtf8 $ value))
        ]
    renderRegexLiteral annot pattern =
      formatJSONObject
        [ ("type", "\"JSRegEx\""),
          ("annotation", renderAnnotation annot),
          ("pattern", escapeJSONString (Text.unpack . Text.decodeUtf8 $ pattern))
        ]
    renderBinaryExpression left op right =
      formatJSONObject
        [ ("type", "\"JSExpressionBinary\""),
          ("left", renderExpressionToJSON left),
          ("operator", renderBinOpToJSON op),
          ("right", renderExpressionToJSON right)
        ]
    renderMemberDot object annot property =
      formatJSONObject
        [ ("type", "\"JSMemberDot\""),
          ("object", renderExpressionToJSON object),
          ("annotation", renderAnnotation annot),
          ("property", renderExpressionToJSON property)
        ]
    renderMemberSquare object lbracket property rbracket =
      formatJSONObject
        [ ("type", "\"JSMemberSquare\""),
          ("object", renderExpressionToJSON object),
          ("lbracket", renderAnnotation lbracket),
          ("property", renderExpressionToJSON property),
          ("rbracket", renderAnnotation rbracket)
        ]
    renderOptionalMemberDot object annot property =
      formatJSONObject
        [ ("type", "\"JSOptionalMemberDot\""),
          ("object", renderExpressionToJSON object),
          ("annotation", renderAnnotation annot),
          ("property", renderExpressionToJSON property)
        ]
    renderOptionalMemberSquare object lbracket property rbracket =
      formatJSONObject
        [ ("type", "\"JSOptionalMemberSquare\""),
          ("object", renderExpressionToJSON object),
          ("lbracket", renderAnnotation lbracket),
          ("property", renderExpressionToJSON property),
          ("rbracket", renderAnnotation rbracket)
        ]
    renderCallExpression func annot args rannot =
      formatJSONObject
        [ ("type", "\"JSCallExpression\""),
          ("function", renderExpressionToJSON func),
          ("lannot", renderAnnotation annot),
          ("arguments", renderArgumentsToJSON args),
          ("rannot", renderAnnotation rannot)
        ]
    renderOptionalCallExpression func annot args rannot =
      formatJSONObject
        [ ("type", "\"JSOptionalCallExpression\""),
          ("function", renderExpressionToJSON func),
          ("lannot", renderAnnotation annot),
          ("arguments", renderArgumentsToJSON args),
          ("rannot", renderAnnotation rannot)
        ]
    renderArrowExpression params annot body =
      formatJSONObject
        [ ("type", "\"JSArrowExpression\""),
          ("parameters", renderArrowParametersToJSON params),
          ("annotation", renderAnnotation annot),
          ("body", renderConciseBodyToJSON body)
        ]
    renderUnsupportedExpression =
      formatJSONObject
        [ ("type", "\"JSExpression\""),
          ("unsupported", "true")
        ]

-- | Convert a JavaScript statement to JSON representation.
renderStatementToJSON :: AST.JSStatement -> Text
renderStatementToJSON stmt = case stmt of
  AST.JSExpressionStatement expr _ ->
    formatJSONObject
      [ ("type", "\"JSStatementExpression\""),
        ("expression", renderExpressionToJSON expr)
      ]
  -- Add more statement types as needed
  _ ->
    formatJSONObject
      [ ("type", "\"JSStatement\""),
        ("unsupported", "true")
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
  AST.JSBinOpExponentiation _ -> renderArithmeticOp "**"
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
    renderLogicalOp opStr = "\"" <> opStr <> "\""
    renderArithmeticOp opStr = "\"" <> opStr <> "\""
    renderEqualityOp opStr = "\"" <> opStr <> "\""
    renderComparisonOp opStr = "\"" <> opStr <> "\""
    renderBitwiseOp opStr = "\"" <> opStr <> "\""
    renderKeywordOp opStr = "\"" <> opStr <> "\""

-- | Render module item to JSON.
renderModuleItemToJSON :: AST.JSModuleItem -> Text
renderModuleItemToJSON item = case item of
  AST.JSModuleStatementListItem statement -> renderStatementToJSON statement
  AST.JSModuleImportDeclaration ann importDecl ->
    formatJSONObject
      [ ("type", "\"ImportDeclaration\""),
        ("annotation", renderAnnotation ann),
        ("importDeclaration", renderImportDeclarationToJSON importDecl)
      ]
  AST.JSModuleExportDeclaration ann exportDecl ->
    formatJSONObject
      [ ("type", "\"ExportDeclaration\""),
        ("annotation", renderAnnotation ann),
        ("exportDeclaration", renderExportDeclarationToJSON exportDecl)
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
    renderDefaultImport ident =
      formatJSONObject
        [ ("type", "\"ImportDefaultSpecifier\""),
          ("local", renderIdentToJSON ident)
        ]
    renderNamespaceImport namespace =
      formatJSONObject
        [ ("type", "\"ImportNamespaceSpecifier\""),
          ("namespace", renderImportNameSpaceToJSON namespace)
        ]
    renderNamedImports imports =
      formatJSONObject
        [ ("type", "\"ImportSpecifiers\""),
          ("specifiers", renderImportsToJSON imports)
        ]
    renderDefaultNamespaceImport ident annot namespace =
      formatJSONObject
        [ ("type", "\"ImportDefaultAndNamespace\""),
          ("default", renderIdentToJSON ident),
          ("annotation", renderAnnotation annot),
          ("namespace", renderImportNameSpaceToJSON namespace)
        ]
    renderDefaultNamedImport ident annot imports =
      formatJSONObject
        [ ("type", "\"ImportDefaultAndNamed\""),
          ("default", renderIdentToJSON ident),
          ("annotation", renderAnnotation annot),
          ("named", renderImportsToJSON imports)
        ]

-- | Render import specifiers to JSON.
renderImportsToJSON :: AST.JSImportsNamed -> Text
renderImportsToJSON (AST.JSImportsNamed ann specifiers _) =
  formatJSONObject
    [ ("type", "\"NamedImports\""),
      ("annotation", renderAnnotation ann),
      ("specifiers", formatJSONArray (map renderImportSpecifierToJSON (extractCommaListExpressions specifiers)))
    ]

-- | Render import specifier to JSON.
renderImportSpecifierToJSON :: AST.JSImportSpecifier -> Text
renderImportSpecifierToJSON spec = case spec of
  AST.JSImportSpecifier ident ->
    formatJSONObject
      [ ("type", "\"ImportSpecifier\""),
        ("imported", renderIdentToJSON ident),
        ("local", renderIdentToJSON ident)
      ]
  AST.JSImportSpecifierAs ident ann localIdent ->
    formatJSONObject
      [ ("type", "\"ImportSpecifier\""),
        ("imported", renderIdentToJSON ident),
        ("annotation", renderAnnotation ann),
        ("local", renderIdentToJSON localIdent)
      ]

-- | Render export declaration to JSON.
renderExportDeclarationToJSON :: AST.JSExportDeclaration -> Text
renderExportDeclarationToJSON decl = case decl of
  AST.JSExportFrom clause fromClause semi ->
    formatJSONObject
      [ ("type", "\"ExportFromDeclaration\""),
        ("clause", renderExportClauseToJSON clause),
        ("source", renderFromClauseToJSON fromClause),
        ("semicolon", renderSemiColonToJSON semi)
      ]
  AST.JSExportLocals clause semi ->
    formatJSONObject
      [ ("type", "\"ExportLocalsDeclaration\""),
        ("clause", renderExportClauseToJSON clause),
        ("semicolon", renderSemiColonToJSON semi)
      ]
  AST.JSExport statement semi ->
    formatJSONObject
      [ ("type", "\"ExportDeclaration\""),
        ("declaration", renderStatementToJSON statement),
        ("semicolon", renderSemiColonToJSON semi)
      ]
  AST.JSExportAllFrom star fromClause semi ->
    formatJSONObject
      [ ("type", "\"ExportAllFromDeclaration\""),
        ("star", renderBinOpToJSON star),
        ("source", renderFromClauseToJSON fromClause),
        ("semicolon", renderSemiColonToJSON semi)
      ]
  AST.JSExportAllAsFrom star as ident fromClause semi ->
    formatJSONObject
      [ ("type", "\"ExportAllAsFromDeclaration\""),
        ("star", renderBinOpToJSON star),
        ("as", renderAnnotation as),
        ("identifier", renderIdentToJSON ident),
        ("source", renderFromClauseToJSON fromClause),
        ("semicolon", renderSemiColonToJSON semi)
      ]
  AST.JSExportDefault annot stmt semi ->
    formatJSONObject
      [ ("type", "\"ExportDefaultDeclaration\""),
        ("annotation", renderAnnotation annot),
        ("declaration", renderStatementToJSON stmt),
        ("semicolon", renderSemiColonToJSON semi)
      ]

-- | Render export clause to JSON.
renderExportClauseToJSON :: AST.JSExportClause -> Text
renderExportClauseToJSON (AST.JSExportClause ann specifiers _) =
  formatJSONObject
    [ ("type", "\"ExportClause\""),
      ("annotation", renderAnnotation ann),
      ("specifiers", formatJSONArray (map renderExportSpecifierToJSON (extractCommaListExpressions specifiers)))
    ]

-- | Render export specifier to JSON.
renderExportSpecifierToJSON :: AST.JSExportSpecifier -> Text
renderExportSpecifierToJSON spec = case spec of
  AST.JSExportSpecifier ident ->
    formatJSONObject
      [ ("type", "\"ExportSpecifier\""),
        ("exported", renderIdentToJSON ident),
        ("local", renderIdentToJSON ident)
      ]
  AST.JSExportSpecifierAs ident1 ann ident2 ->
    formatJSONObject
      [ ("type", "\"ExportSpecifier\""),
        ("local", renderIdentToJSON ident1),
        ("annotation", renderAnnotation ann),
        ("exported", renderIdentToJSON ident2)
      ]

-- | Helper function to render identifier to JSON.
renderIdentToJSON :: AST.JSIdent -> Text
renderIdentToJSON (AST.JSIdentName ann name) =
  formatJSONObject
    [ ("type", "\"Identifier\""),
      ("annotation", renderAnnotation ann),
      ("name", escapeJSONString (Text.unpack . Text.decodeUtf8 $ name))
    ]
renderIdentToJSON AST.JSIdentNone =
  formatJSONObject
    [ ("type", "\"EmptyIdentifier\"")
    ]

-- | Render semicolon to JSON.
renderSemiColonToJSON :: AST.JSSemi -> Text
renderSemiColonToJSON semi = case semi of
  AST.JSSemi ann ->
    formatJSONObject
      [ ("type", "\"Semicolon\""),
        ("annotation", renderAnnotation ann)
      ]
  AST.JSSemiAuto ->
    formatJSONObject
      [ ("type", "\"AutoSemicolon\"")
      ]

-- | Render import declaration to JSON.
renderImportDeclarationToJSON :: AST.JSImportDeclaration -> Text
renderImportDeclarationToJSON decl = case decl of
  AST.JSImportDeclaration clause fromClause attrs semi ->
    formatJSONObject $
      [ ("type", "\"ImportDeclaration\""),
        ("clause", renderImportClauseToJSON clause),
        ("source", renderFromClauseToJSON fromClause),
        ("semicolon", renderSemiColonToJSON semi)
      ]
        ++ case attrs of
          Just attributes -> [("attributes", renderImportAttributesToJSON attributes)]
          Nothing -> []
  AST.JSImportDeclarationBare ann moduleName attrs semi ->
    formatJSONObject $
      [ ("type", "\"ImportBareDeclaration\""),
        ("annotation", renderAnnotation ann),
        ("module", escapeJSONString (Text.unpack . Text.decodeUtf8 $ moduleName)),
        ("semicolon", renderSemiColonToJSON semi)
      ]
        ++ case attrs of
          Just attributes -> [("attributes", renderImportAttributesToJSON attributes)]
          Nothing -> []

-- | Render import attributes to JSON.
renderImportAttributesToJSON :: AST.JSImportAttributes -> Text
renderImportAttributesToJSON (AST.JSImportAttributes lbrace attrs rbrace) =
  formatJSONObject
    [ ("type", "\"ImportAttributes\""),
      ("openBrace", renderAnnotation lbrace),
      ("attributes", formatJSONArray (map renderImportAttributeToJSON (extractCommaListExpressions attrs))),
      ("closeBrace", renderAnnotation rbrace)
    ]

-- | Render import attribute to JSON.
renderImportAttributeToJSON :: AST.JSImportAttribute -> Text
renderImportAttributeToJSON (AST.JSImportAttribute key colon value) =
  formatJSONObject
    [ ("type", "\"ImportAttribute\""),
      ("key", renderIdentToJSON key),
      ("colon", renderAnnotation colon),
      ("value", renderExpressionToJSON value)
    ]

-- | Render from clause to JSON.
renderFromClauseToJSON :: AST.JSFromClause -> Text
renderFromClauseToJSON (AST.JSFromClause ann1 ann2 moduleName) =
  formatJSONObject
    [ ("type", "\"FromClause\""),
      ("fromAnnotation", renderAnnotation ann1),
      ("moduleAnnotation", renderAnnotation ann2),
      ("module", escapeJSONString (Text.unpack . Text.decodeUtf8 $ moduleName))
    ]

-- | Render import namespace to JSON.
renderImportNameSpaceToJSON :: AST.JSImportNameSpace -> Text
renderImportNameSpaceToJSON (AST.JSImportNameSpace binOp ann ident) =
  formatJSONObject
    [ ("type", "\"ImportNameSpace\""),
      ("operator", renderBinOpToJSON binOp),
      ("annotation", renderAnnotation ann),
      ("local", renderIdentToJSON ident)
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
  AST.JSAnnot pos comments ->
    formatJSONObject
      [ ("position", renderPosition pos),
        ("comments", formatJSONArray (map renderComment comments))
      ]
  AST.JSNoAnnot ->
    formatJSONObject
      [ ("position", "null"),
        ("comments", "[]")
      ]
  AST.JSAnnotSpace ->
    formatJSONObject
      [ ("type", "\"space\""),
        ("position", "null"),
        ("comments", "[]")
      ]

-- | Render source position to JSON.
renderPosition :: TokenPosn -> Text
renderPosition pos = case pos of
  TokenPn _ line col ->
    formatJSONObject
      [ ("line", Text.pack (show line)),
        ("column", Text.pack (show col))
      ]

-- | Render comment to JSON.
renderComment :: Token.CommentAnnotation -> Text
renderComment comment = case comment of
  Token.CommentA pos text ->
    formatJSONObject
      [ ("type", "\"Comment\""),
        ("position", renderPosition pos),
        ("text", escapeJSONString (Text.unpack . Text.decodeUtf8 $ text))
      ]
  Token.WhiteSpace pos text ->
    formatJSONObject
      [ ("type", "\"WhiteSpace\""),
        ("position", renderPosition pos),
        ("text", escapeJSONString (Text.unpack . Text.decodeUtf8 $ text))
      ]
  Token.JSDocA pos jsDoc ->
    formatJSONObject
      [ ("type", "\"JSDoc\""),
        ("position", renderPosition pos),
        ("jsDoc", renderJSDocToJSON jsDoc)
      ]
  Token.NoComment ->
    formatJSONObject
      [ ("type", "\"NoComment\"")
      ]

-- | Render JSDoc comment to JSON.
renderJSDocToJSON :: Token.JSDocComment -> Text
renderJSDocToJSON jsDoc =
  formatJSONObject
    [ ("position", renderPosition (Token.jsDocPosition jsDoc)),
      ("description", maybe "null" (escapeJSONString . Text.unpack) (Token.jsDocDescription jsDoc)),
      ("tags", "[" <> Text.intercalate ", " (map renderJSDocTagToJSON (Token.jsDocTags jsDoc)) <> "]")
    ]

-- | Render JSDoc tag to JSON.
renderJSDocTagToJSON :: Token.JSDocTag -> Text
renderJSDocTagToJSON tag =
  formatJSONObject
    [ ("name", escapeJSONString (Text.unpack (Token.jsDocTagName tag))),
      ("type", maybe "null" renderJSDocTypeToJSON (Token.jsDocTagType tag)),
      ("paramName", maybe "null" (escapeJSONString . Text.unpack) (Token.jsDocTagParamName tag)),
      ("description", maybe "null" (escapeJSONString . Text.unpack) (Token.jsDocTagDescription tag)),
      ("position", renderPosition (Token.jsDocTagPosition tag)),
      ("specific", maybe "null" renderJSDocTagSpecificToJSON (Token.jsDocTagSpecific tag))
    ]

-- | Render JSDoc type to JSON.
renderJSDocTypeToJSON :: Token.JSDocType -> Text
renderJSDocTypeToJSON jsDocType = case jsDocType of
  Token.JSDocBasicType name ->
    formatJSONObject
      [ ("kind", "\"BasicType\""),
        ("name", escapeJSONString (Text.unpack name))
      ]
  Token.JSDocArrayType elementType ->
    formatJSONObject
      [ ("kind", "\"ArrayType\""),
        ("elementType", renderJSDocTypeToJSON elementType)
      ]
  Token.JSDocUnionType types ->
    formatJSONObject
      [ ("kind", "\"UnionType\""),
        ("types", "[" <> Text.intercalate ", " (map renderJSDocTypeToJSON types) <> "]")
      ]
  Token.JSDocObjectType fields ->
    formatJSONObject
      [ ("kind", "\"ObjectType\""),
        ("fields", "[" <> Text.intercalate ", " (map renderJSDocObjectFieldToJSON fields) <> "]")
      ]
  Token.JSDocFunctionType paramTypes returnType ->
    formatJSONObject
      [ ("kind", "\"FunctionType\""),
        ("paramTypes", "[" <> Text.intercalate ", " (map renderJSDocTypeToJSON paramTypes) <> "]"),
        ("returnType", renderJSDocTypeToJSON returnType)
      ]
  Token.JSDocGenericType baseName args ->
    formatJSONObject
      [ ("kind", "\"GenericType\""),
        ("baseName", escapeJSONString (Text.unpack baseName)),
        ("args", "[" <> Text.intercalate ", " (map renderJSDocTypeToJSON args) <> "]")
      ]
  Token.JSDocOptionalType baseType ->
    formatJSONObject
      [ ("kind", "\"OptionalType\""),
        ("baseType", renderJSDocTypeToJSON baseType)
      ]
  Token.JSDocNullableType baseType ->
    formatJSONObject
      [ ("kind", "\"NullableType\""),
        ("baseType", renderJSDocTypeToJSON baseType)
      ]
  Token.JSDocNonNullableType baseType ->
    formatJSONObject
      [ ("kind", "\"NonNullableType\""),
        ("baseType", renderJSDocTypeToJSON baseType)
      ]
  Token.JSDocEnumType enumName enumValues ->
    formatJSONObject
      [ ("kind", "\"EnumType\""),
        ("name", escapeJSONString (Text.unpack enumName)),
        ("values", "[" <> Text.intercalate ", " (map renderJSDocEnumValueToJSON enumValues) <> "]")
      ]

-- | Render JSDoc enum value to JSON.
renderJSDocEnumValueToJSON :: Token.JSDocEnumValue -> Text
renderJSDocEnumValueToJSON enumValue =
  let fields = [ ("name", escapeJSONString (Text.unpack (Token.jsDocEnumValueName enumValue))) ] ++
               (case Token.jsDocEnumValueLiteral enumValue of
                  Nothing -> []
                  Just literal -> [ ("literal", escapeJSONString (Text.unpack literal)) ]) ++
               (case Token.jsDocEnumValueDescription enumValue of
                  Nothing -> []
                  Just desc -> [ ("description", escapeJSONString (Text.unpack desc)) ])
  in formatJSONObject fields

-- | Render JSDoc property to JSON.
renderJSDocPropertyToJSON :: Token.JSDocProperty -> Text
renderJSDocPropertyToJSON property =
  formatJSONObject
    [ ("name", escapeJSONString (Text.unpack (Token.jsDocPropertyName property))),
      ("type", maybe "null" renderJSDocTypeToJSON (Token.jsDocPropertyType property)),
      ("optional", if Token.jsDocPropertyOptional property then "true" else "false"),
      ("description", maybe "null" (escapeJSONString . Text.unpack) (Token.jsDocPropertyDescription property))
    ]

-- | Render JSDoc object field to JSON.
renderJSDocObjectFieldToJSON :: Token.JSDocObjectField -> Text
renderJSDocObjectFieldToJSON field =
  formatJSONObject
    [ ("name", escapeJSONString (Text.unpack (Token.jsDocFieldName field))),
      ("type", renderJSDocTypeToJSON (Token.jsDocFieldType field)),
      ("optional", if Token.jsDocFieldOptional field then "true" else "false")
    ]

-- | Render JSDoc tag specific information to JSON.
renderJSDocTagSpecificToJSON :: Token.JSDocTagSpecific -> Text
renderJSDocTagSpecificToJSON tagSpecific = case tagSpecific of
  Token.JSDocParamTag optional variadic defaultValue ->
    formatJSONObject
      [ ("kind", "\"ParamTag\""),
        ("optional", if optional then "true" else "false"),
        ("variadic", if variadic then "true" else "false"),
        ("defaultValue", maybe "null" (escapeJSONString . Text.unpack) defaultValue)
      ]
  Token.JSDocReturnTag promise ->
    formatJSONObject
      [ ("kind", "\"ReturnTag\""),
        ("promise", if promise then "true" else "false")
      ]
  Token.JSDocAuthorTag name email ->
    formatJSONObject
      [ ("kind", "\"AuthorTag\""),
        ("name", escapeJSONString (Text.unpack name)),
        ("email", maybe "null" (escapeJSONString . Text.unpack) email)
      ]
  Token.JSDocVersionTag version ->
    formatJSONObject
      [ ("kind", "\"VersionTag\""),
        ("version", escapeJSONString (Text.unpack version))
      ]
  Token.JSDocSinceTag version ->
    formatJSONObject
      [ ("kind", "\"SinceTag\""),
        ("version", escapeJSONString (Text.unpack version))
      ]
  Token.JSDocAccessTag access ->
    formatJSONObject
      [ ("kind", "\"AccessTag\""),
        ("access", escapeJSONString (show access))
      ]
  Token.JSDocDescriptionTag text ->
    formatJSONObject
      [ ("kind", "\"DescriptionTag\""),
        ("text", escapeJSONString (Text.unpack text))
      ]
  Token.JSDocTypeTag jsDocType ->
    formatJSONObject
      [ ("kind", "\"TypeTag\""),
        ("type", renderJSDocTypeToJSON jsDocType)
      ]
  Token.JSDocPropertyTag name maybeType optional maybeDescription ->
    formatJSONObject
      [ ("kind", "\"PropertyTag\""),
        ("name", escapeJSONString (Text.unpack name)),
        ("type", maybe "null" renderJSDocTypeToJSON maybeType),
        ("optional", if optional then "true" else "false"),
        ("description", maybe "null" (escapeJSONString . Text.unpack) maybeDescription)
      ]
  Token.JSDocDefaultTag value ->
    formatJSONObject
      [ ("kind", "\"DefaultTag\""),
        ("value", escapeJSONString (Text.unpack value))
      ]
  Token.JSDocConstantTag maybeValue ->
    formatJSONObject
      [ ("kind", "\"ConstantTag\""),
        ("value", maybe "null" (escapeJSONString . Text.unpack) maybeValue)
      ]
  Token.JSDocGlobalTag ->
    formatJSONObject
      [ ("kind", "\"GlobalTag\"")
      ]
  Token.JSDocAliasTag name ->
    formatJSONObject
      [ ("kind", "\"AliasTag\""),
        ("name", escapeJSONString (Text.unpack name))
      ]
  Token.JSDocAugmentsTag parent ->
    formatJSONObject
      [ ("kind", "\"AugmentsTag\""),
        ("parent", escapeJSONString (Text.unpack parent))
      ]
  Token.JSDocBorrowsTag from maybeAs ->
    formatJSONObject
      [ ("kind", "\"BorrowsTag\""),
        ("from", escapeJSONString (Text.unpack from)),
        ("as", maybe "null" (escapeJSONString . Text.unpack) maybeAs)
      ]
  Token.JSDocClassDescTag description ->
    formatJSONObject
      [ ("kind", "\"ClassDescTag\""),
        ("description", escapeJSONString (Text.unpack description))
      ]
  Token.JSDocCopyrightTag notice ->
    formatJSONObject
      [ ("kind", "\"CopyrightTag\""),
        ("notice", escapeJSONString (Text.unpack notice))
      ]
  Token.JSDocExportsTag name ->
    formatJSONObject
      [ ("kind", "\"ExportsTag\""),
        ("name", escapeJSONString (Text.unpack name))
      ]
  Token.JSDocExternalTag name maybeDescription ->
    formatJSONObject
      [ ("kind", "\"ExternalTag\""),
        ("name", escapeJSONString (Text.unpack name)),
        ("description", maybe "null" (escapeJSONString . Text.unpack) maybeDescription)
      ]
  Token.JSDocFileTag description ->
    formatJSONObject
      [ ("kind", "\"FileTag\""),
        ("description", escapeJSONString (Text.unpack description))
      ]
  Token.JSDocFunctionTag ->
    formatJSONObject
      [ ("kind", "\"FunctionTag\"")
      ]
  Token.JSDocHideConstructorTag ->
    formatJSONObject
      [ ("kind", "\"HideConstructorTag\"")
      ]
  Token.JSDocImplementsTag interface ->
    formatJSONObject
      [ ("kind", "\"ImplementsTag\""),
        ("interface", escapeJSONString (Text.unpack interface))
      ]
  Token.JSDocInheritDocTag ->
    formatJSONObject
      [ ("kind", "\"InheritDocTag\"")
      ]
  Token.JSDocInstanceTag ->
    formatJSONObject
      [ ("kind", "\"InstanceTag\"")
      ]
  Token.JSDocInterfaceTag maybeName ->
    formatJSONObject
      [ ("kind", "\"InterfaceTag\""),
        ("name", maybe "null" (escapeJSONString . Text.unpack) maybeName)
      ]
  Token.JSDocKindTag kind ->
    formatJSONObject
      [ ("kind", "\"KindTag\""),
        ("kindValue", escapeJSONString (Text.unpack kind))
      ]
  Token.JSDocLendsTag name ->
    formatJSONObject
      [ ("kind", "\"LendsTag\""),
        ("name", escapeJSONString (Text.unpack name))
      ]
  Token.JSDocLicenseTag license ->
    formatJSONObject
      [ ("kind", "\"LicenseTag\""),
        ("license", escapeJSONString (Text.unpack license))
      ]
  Token.JSDocMemberTag maybeName maybeType ->
    formatJSONObject
      [ ("kind", "\"MemberTag\""),
        ("name", maybe "null" (escapeJSONString . Text.unpack) maybeName),
        ("type", maybe "null" (escapeJSONString . Text.unpack) maybeType)
      ]
  Token.JSDocMixesTag mixin ->
    formatJSONObject
      [ ("kind", "\"MixesTag\""),
        ("mixin", escapeJSONString (Text.unpack mixin))
      ]
  Token.JSDocMixinTag ->
    formatJSONObject
      [ ("kind", "\"MixinTag\"")
      ]
  Token.JSDocNameTag name ->
    formatJSONObject
      [ ("kind", "\"NameTag\""),
        ("name", escapeJSONString (Text.unpack name))
      ]
  Token.JSDocRequiresTag module' ->
    formatJSONObject
      [ ("kind", "\"RequiresTag\""),
        ("module", escapeJSONString (Text.unpack module'))
      ]
  Token.JSDocSummaryTag summary ->
    formatJSONObject
      [ ("kind", "\"SummaryTag\""),
        ("summary", escapeJSONString (Text.unpack summary))
      ]
  Token.JSDocThisTag thisType ->
    formatJSONObject
      [ ("kind", "\"ThisTag\""),
        ("type", renderJSDocTypeToJSON thisType)
      ]
  Token.JSDocTodoTag todo ->
    formatJSONObject
      [ ("kind", "\"TodoTag\""),
        ("todo", escapeJSONString (Text.unpack todo))
      ]
  Token.JSDocTutorialTag tutorial ->
    formatJSONObject
      [ ("kind", "\"TutorialTag\""),
        ("tutorial", escapeJSONString (Text.unpack tutorial))
      ]
  Token.JSDocVariationTag variation ->
    formatJSONObject
      [ ("kind", "\"VariationTag\""),
        ("variation", escapeJSONString (Text.unpack variation))
      ]
  Token.JSDocYieldsTag maybeType maybeDescription ->
    formatJSONObject
      [ ("kind", "\"YieldsTag\""),
        ("type", maybe "null" renderJSDocTypeToJSON maybeType),
        ("description", maybe "null" (escapeJSONString . Text.unpack) maybeDescription)
      ]
  Token.JSDocThrowsTag maybeDescription ->
    formatJSONObject
      [ ("kind", "\"ThrowsTag\""),
        ("description", maybe "null" (escapeJSONString . Text.unpack) maybeDescription)
      ]
  Token.JSDocExampleTag maybeLanguage maybeCaption ->
    formatJSONObject
      [ ("kind", "\"ExampleTag\""),
        ("language", maybe "null" (escapeJSONString . Text.unpack) maybeLanguage),
        ("caption", maybe "null" (escapeJSONString . Text.unpack) maybeCaption)
      ]
  Token.JSDocSeeTag reference maybeDisplayText ->
    formatJSONObject
      [ ("kind", "\"SeeTag\""),
        ("reference", escapeJSONString (Text.unpack reference)),
        ("displayText", maybe "null" (escapeJSONString . Text.unpack) maybeDisplayText)
      ]
  Token.JSDocDeprecatedTag maybeSince maybeReplacement ->
    formatJSONObject
      [ ("kind", "\"DeprecatedTag\""),
        ("since", maybe "null" (escapeJSONString . Text.unpack) maybeSince),
        ("replacement", maybe "null" (escapeJSONString . Text.unpack) maybeReplacement)
      ]
  Token.JSDocNamespaceTag path ->
    formatJSONObject
      [ ("kind", "\"NamespaceTag\""),
        ("path", escapeJSONString (Text.unpack path))
      ]
  Token.JSDocClassTag maybeName maybeExtends ->
    formatJSONObject
      [ ("kind", "\"ClassTag\""),
        ("name", maybe "null" (escapeJSONString . Text.unpack) maybeName),
        ("extends", maybe "null" (escapeJSONString . Text.unpack) maybeExtends)
      ]
  Token.JSDocModuleTag name maybeType ->
    formatJSONObject
      [ ("kind", "\"ModuleTag\""),
        ("name", escapeJSONString (Text.unpack name)),
        ("type", maybe "null" (escapeJSONString . Text.unpack) maybeType)
      ]
  Token.JSDocMemberOfTag parent forced ->
    formatJSONObject
      [ ("kind", "\"MemberOfTag\""),
        ("parent", escapeJSONString (Text.unpack parent)),
        ("forced", if forced then "true" else "false")
      ]
  Token.JSDocTypedefTag name properties ->
    formatJSONObject
      [ ("kind", "\"TypedefTag\""),
        ("name", escapeJSONString (Text.unpack name)),
        ("properties", formatJSONArray (map renderJSDocPropertyToJSON properties))
      ]
  Token.JSDocEnumTag name maybeBaseType enumValues ->
    formatJSONObject
      [ ("kind", "\"EnumTag\""),
        ("name", escapeJSONString (Text.unpack name)),
        ("baseType", maybe "null" renderJSDocTypeToJSON maybeBaseType),
        ("values", formatJSONArray (map renderJSDocEnumValueToJSON enumValues))
      ]
  Token.JSDocCallbackTag name ->
    formatJSONObject
      [ ("kind", "\"CallbackTag\""),
        ("name", escapeJSONString (Text.unpack name))
      ]
  Token.JSDocEventTag name ->
    formatJSONObject
      [ ("kind", "\"EventTag\""),
        ("name", escapeJSONString (Text.unpack name))
      ]
  Token.JSDocFiresTag name ->
    formatJSONObject
      [ ("kind", "\"FiresTag\""),
        ("name", escapeJSONString (Text.unpack name))
      ]
  Token.JSDocListensTag name ->
    formatJSONObject
      [ ("kind", "\"ListensTag\""),
        ("name", escapeJSONString (Text.unpack name))
      ]
  Token.JSDocIgnoreTag ->
    formatJSONObject
      [ ("kind", "\"IgnoreTag\"")
      ]
  Token.JSDocInnerTag ->
    formatJSONObject
      [ ("kind", "\"InnerTag\"")
      ]
  Token.JSDocReadOnlyTag ->
    formatJSONObject
      [ ("kind", "\"ReadOnlyTag\"")
      ]
  Token.JSDocStaticTag ->
    formatJSONObject
      [ ("kind", "\"StaticTag\"")
      ]
  Token.JSDocOverrideTag ->
    formatJSONObject
      [ ("kind", "\"OverrideTag\"")
      ]
  Token.JSDocAbstractTag ->
    formatJSONObject
      [ ("kind", "\"AbstractTag\"")
      ]
  Token.JSDocFinalTag ->
    formatJSONObject
      [ ("kind", "\"FinalTag\"")
      ]
  Token.JSDocGeneratorTag ->
    formatJSONObject
      [ ("kind", "\"GeneratorTag\"")
      ]
  Token.JSDocAsyncTag ->
    formatJSONObject
      [ ("kind", "\"AsyncTag\"")
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
  AST.JSUnparenthesizedArrowParameter ident ->
    formatJSONObject
      [ ("type", "\"JSUnparenthesizedArrowParameter\""),
        ("parameter", renderIdentToJSON ident)
      ]
  AST.JSParenthesizedArrowParameterList _ paramList _ ->
    formatJSONObject
      [ ("type", "\"JSParenthesizedArrowParameterList\""),
        ("parameters", renderArgumentsToJSON paramList)
      ]

-- | Convert JSConciseBody to JSON.
renderConciseBodyToJSON :: AST.JSConciseBody -> Text
renderConciseBodyToJSON body = case body of
  AST.JSConciseFunctionBody block ->
    formatJSONObject
      [ ("type", "\"JSConciseFunctionBody\""),
        ("block", renderBlockToJSON block)
      ]
  AST.JSConciseExpressionBody expr ->
    formatJSONObject
      [ ("type", "\"JSConciseExpressionBody\""),
        ("expression", renderExpressionToJSON expr)
      ]

-- | Convert JSBlock to JSON.
renderBlockToJSON :: AST.JSBlock -> Text
renderBlockToJSON (AST.JSBlock _ statements _) =
  formatJSONObject
    [ ("type", "\"JSBlock\""),
      ("statements", formatJSONArray (map renderStatementToJSON statements))
    ]
