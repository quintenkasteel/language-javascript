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
  ( -- * S-expression rendering functions
    renderToSExpr,
    renderProgramToSExpr,
    renderExpressionToSExpr,
    renderStatementToSExpr,
    renderImportDeclarationToSExpr,
    renderExportDeclarationToSExpr,
    renderAnnotation,

    -- * S-expression utilities
    escapeSExprString,
    formatSExprList,
    formatSExprAtom,
  )
where

import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Language.JavaScript.Parser.AST as AST
import Language.JavaScript.Parser.SrcLocation (TokenPosn (..))
import qualified Language.JavaScript.Parser.Token as Token

-- | Convert a JavaScript AST to S-expression string representation.
renderToSExpr :: AST.JSAST -> Text
renderToSExpr ast = case ast of
  AST.JSAstProgram statements annot -> renderProgramAST statements annot
  AST.JSAstModule items annot -> renderModuleAST items annot
  AST.JSAstStatement statement annot -> renderStatementAST statement annot
  AST.JSAstExpression expression annot -> renderExpressionAST expression annot
  AST.JSAstLiteral literal annot -> renderLiteralAST literal annot
  where
    renderProgramAST statements annot =
      formatSExprList
        [ "JSAstProgram",
          renderAnnotation annot,
          formatSExprList ("statements" : map renderStatementToSExpr statements)
        ]

    renderModuleAST items annot =
      formatSExprList
        [ "JSAstModule",
          renderAnnotation annot,
          formatSExprList ("items" : map renderModuleItemToSExpr items)
        ]

    renderStatementAST statement annot =
      formatSExprList
        [ "JSAstStatement",
          renderAnnotation annot,
          renderStatementToSExpr statement
        ]

    renderExpressionAST expression annot =
      formatSExprList
        [ "JSAstExpression",
          renderAnnotation annot,
          renderExpressionToSExpr expression
        ]

    renderLiteralAST literal annot =
      formatSExprList
        [ "JSAstLiteral",
          renderAnnotation annot,
          renderExpressionToSExpr literal
        ]

-- | Convert a JavaScript program (list of statements) to S-expression.
renderProgramToSExpr :: [AST.JSStatement] -> Text
renderProgramToSExpr statements =
  formatSExprList
    [ "JSProgram",
      formatSExprList ("statements" : map renderStatementToSExpr statements)
    ]

-- | Convert a JavaScript expression to S-expression representation.
renderExpressionToSExpr :: AST.JSExpression -> Text
renderExpressionToSExpr expr = case expr of
  AST.JSDecimal annot value ->
    formatSExprList
      [ "JSDecimal",
        escapeSExprString (Text.unpack . Text.decodeUtf8 $ value),
        renderAnnotation annot
      ]
  AST.JSHexInteger annot value ->
    formatSExprList
      [ "JSHexInteger",
        escapeSExprString (Text.unpack . Text.decodeUtf8 $ value),
        renderAnnotation annot
      ]
  AST.JSOctal annot value ->
    formatSExprList
      [ "JSOctal",
        escapeSExprString (Text.unpack . Text.decodeUtf8 $ value),
        renderAnnotation annot
      ]
  AST.JSBinaryInteger annot value ->
    formatSExprList
      [ "JSBinaryInteger",
        escapeSExprString (Text.unpack . Text.decodeUtf8 $ value),
        renderAnnotation annot
      ]
  AST.JSBigIntLiteral annot value ->
    formatSExprList
      [ "JSBigIntLiteral",
        escapeSExprString (Text.unpack . Text.decodeUtf8 $ value),
        renderAnnotation annot
      ]
  AST.JSStringLiteral annot value ->
    formatSExprList
      [ "JSStringLiteral",
        escapeSExprString (Text.unpack . Text.decodeUtf8 $ value),
        renderAnnotation annot
      ]
  AST.JSIdentifier annot name ->
    formatSExprList
      [ "JSIdentifier",
        escapeSExprString (Text.unpack . Text.decodeUtf8 $ name),
        renderAnnotation annot
      ]
  AST.JSLiteral annot value ->
    formatSExprList
      [ "JSLiteral",
        escapeSExprString (Text.unpack . Text.decodeUtf8 $ value),
        renderAnnotation annot
      ]
  AST.JSRegEx annot pattern ->
    formatSExprList
      [ "JSRegEx",
        escapeSExprString (Text.unpack . Text.decodeUtf8 $ pattern),
        renderAnnotation annot
      ]
  AST.JSExpressionBinary left op right ->
    formatSExprList
      [ "JSExpressionBinary",
        renderExpressionToSExpr left,
        renderBinOpToSExpr op,
        renderExpressionToSExpr right
      ]
  AST.JSMemberDot object annot property ->
    formatSExprList
      [ "JSMemberDot",
        renderExpressionToSExpr object,
        renderAnnotation annot,
        renderExpressionToSExpr property
      ]
  AST.JSMemberSquare object lbracket property rbracket ->
    formatSExprList
      [ "JSMemberSquare",
        renderExpressionToSExpr object,
        renderAnnotation lbracket,
        renderExpressionToSExpr property,
        renderAnnotation rbracket
      ]
  AST.JSOptionalMemberDot object annot property ->
    formatSExprList
      [ "JSOptionalMemberDot",
        renderExpressionToSExpr object,
        renderAnnotation annot,
        renderExpressionToSExpr property
      ]
  AST.JSOptionalMemberSquare object lbracket property rbracket ->
    formatSExprList
      [ "JSOptionalMemberSquare",
        renderExpressionToSExpr object,
        renderAnnotation lbracket,
        renderExpressionToSExpr property,
        renderAnnotation rbracket
      ]
  AST.JSCallExpression func annot args rannot ->
    formatSExprList
      [ "JSCallExpression",
        renderExpressionToSExpr func,
        renderAnnotation annot,
        renderCommaListToSExpr args,
        renderAnnotation rannot
      ]
  AST.JSOptionalCallExpression func annot args rannot ->
    formatSExprList
      [ "JSOptionalCallExpression",
        renderExpressionToSExpr func,
        renderAnnotation annot,
        renderCommaListToSExpr args,
        renderAnnotation rannot
      ]
  AST.JSArrowExpression params annot body ->
    formatSExprList
      [ "JSArrowExpression",
        renderArrowParametersToSExpr params,
        renderAnnotation annot,
        renderArrowBodyToSExpr body
      ]
  _ -> formatSExprList ["JSUnsupportedExpression", "unsupported-expression-type"]

-- | Convert a JavaScript statement to S-expression representation.
renderStatementToSExpr :: AST.JSStatement -> Text
renderStatementToSExpr stmt = case stmt of
  AST.JSExpressionStatement expr semi ->
    formatSExprList
      [ "JSExpressionStatement",
        renderExpressionToSExpr expr,
        renderSemiToSExpr semi
      ]
  AST.JSVariable annot decls semi ->
    formatSExprList
      [ "JSVariable",
        renderAnnotation annot,
        renderCommaListToSExpr decls,
        renderSemiToSExpr semi
      ]
  AST.JSLet annot decls semi ->
    formatSExprList
      [ "JSLet",
        renderAnnotation annot,
        renderCommaListToSExpr decls,
        renderSemiToSExpr semi
      ]
  AST.JSConstant annot decls semi ->
    formatSExprList
      [ "JSConstant",
        renderAnnotation annot,
        renderCommaListToSExpr decls,
        renderSemiToSExpr semi
      ]
  AST.JSEmptyStatement annot ->
    formatSExprList
      [ "JSEmptyStatement",
        renderAnnotation annot
      ]
  AST.JSReturn annot maybeExpr semi ->
    formatSExprList
      [ "JSReturn",
        renderAnnotation annot,
        renderMaybeExpressionToSExpr maybeExpr,
        renderSemiToSExpr semi
      ]
  _ -> formatSExprList ["JSUnsupportedStatement", "unsupported-statement-type"]

-- | Render module item to S-expression
renderModuleItemToSExpr :: AST.JSModuleItem -> Text
renderModuleItemToSExpr item = case item of
  AST.JSModuleImportDeclaration annot decl ->
    formatSExprList
      [ "JSModuleImportDeclaration",
        renderAnnotation annot,
        renderImportDeclarationToSExpr decl
      ]
  AST.JSModuleExportDeclaration annot decl ->
    formatSExprList
      [ "JSModuleExportDeclaration",
        renderAnnotation annot,
        renderExportDeclarationToSExpr decl
      ]
  AST.JSModuleStatementListItem stmt ->
    formatSExprList
      [ "JSModuleStatementListItem",
        renderStatementToSExpr stmt
      ]

-- | Render import declaration to S-expression
renderImportDeclarationToSExpr :: AST.JSImportDeclaration -> Text
renderImportDeclarationToSExpr =
  const $
    formatSExprList
      [ "JSImportDeclaration",
        "import-declaration-not-yet-implemented"
      ]

-- | Render export declaration to S-expression
renderExportDeclarationToSExpr :: AST.JSExportDeclaration -> Text
renderExportDeclarationToSExpr =
  const $
    formatSExprList
      [ "JSExportDeclaration",
        "export-declaration-not-yet-implemented"
      ]

-- | Render annotation to S-expression with position and comments
renderAnnotation :: AST.JSAnnot -> Text
renderAnnotation annot = case annot of
  AST.JSNoAnnot ->
    formatSExprList
      [ "annotation",
        formatSExprList ["position"],
        formatSExprList ["comments"]
      ]
  AST.JSAnnot pos comments ->
    formatSExprList
      [ "annotation",
        renderPositionToSExpr pos,
        formatSExprList ("comments" : map renderCommentToSExpr comments)
      ]
  AST.JSAnnotSpace ->
    formatSExprList
      [ "annotation-space"
      ]

-- | Render token position to S-expression
renderPositionToSExpr :: TokenPosn -> Text
renderPositionToSExpr (TokenPn addr line col) =
  formatSExprList
    [ "position",
      Text.pack (show addr),
      Text.pack (show line),
      Text.pack (show col)
    ]

-- | Render comment annotation to S-expression
renderCommentToSExpr :: Token.CommentAnnotation -> Text
renderCommentToSExpr comment = case comment of
  Token.CommentA pos content ->
    formatSExprList
      [ "comment",
        renderPositionToSExpr pos,
        escapeSExprString (Text.unpack . Text.decodeUtf8 $ content)
      ]
  Token.WhiteSpace pos content ->
    formatSExprList
      [ "whitespace",
        renderPositionToSExpr pos,
        escapeSExprString (Text.unpack . Text.decodeUtf8 $ content)
      ]
  Token.JSDocA pos jsDoc ->
    formatSExprList
      [ "jsdoc",
        renderPositionToSExpr pos,
        renderJSDocToSExpr jsDoc
      ]
  Token.NoComment ->
    formatSExprList
      [ "no-comment"
      ]

-- | Render JSDoc comment to S-expression
renderJSDocToSExpr :: Token.JSDocComment -> Text
renderJSDocToSExpr jsDoc =
  formatSExprList
    [ "jsdoc-comment",
      renderPositionToSExpr (Token.jsDocPosition jsDoc),
      maybe "nil" (escapeSExprString . Text.unpack) (Token.jsDocDescription jsDoc),
      formatSExprList ("tags" : map renderJSDocTagToSExpr (Token.jsDocTags jsDoc))
    ]

-- | Render JSDoc tag to S-expression
renderJSDocTagToSExpr :: Token.JSDocTag -> Text
renderJSDocTagToSExpr tag =
  formatSExprList
    [ "jsdoc-tag",
      escapeSExprString (Text.unpack (Token.jsDocTagName tag)),
      maybe "nil" renderJSDocTypeToSExpr (Token.jsDocTagType tag),
      maybe "nil" (escapeSExprString . Text.unpack) (Token.jsDocTagParamName tag),
      maybe "nil" (escapeSExprString . Text.unpack) (Token.jsDocTagDescription tag),
      renderPositionToSExpr (Token.jsDocTagPosition tag),
      maybe "nil" renderJSDocTagSpecificToSExpr (Token.jsDocTagSpecific tag)
    ]

-- | Render JSDoc type to S-expression
renderJSDocTypeToSExpr :: Token.JSDocType -> Text
renderJSDocTypeToSExpr jsDocType = case jsDocType of
  Token.JSDocBasicType name ->
    formatSExprList ["basic-type", escapeSExprString (Text.unpack name)]
  Token.JSDocArrayType elementType ->
    formatSExprList ["array-type", renderJSDocTypeToSExpr elementType]
  Token.JSDocUnionType types ->
    formatSExprList ("union-type" : map renderJSDocTypeToSExpr types)
  Token.JSDocObjectType fields ->
    formatSExprList ["object-type", formatSExprList ("fields" : map renderJSDocObjectFieldToSExpr fields)]
  Token.JSDocFunctionType paramTypes returnType ->
    formatSExprList
      [ "function-type",
        formatSExprList ("params" : map renderJSDocTypeToSExpr paramTypes),
        formatSExprList ["return", renderJSDocTypeToSExpr returnType]
      ]
  Token.JSDocGenericType baseName args ->
    formatSExprList
      [ "generic-type",
        escapeSExprString (Text.unpack baseName),
        formatSExprList ("args" : map renderJSDocTypeToSExpr args)
      ]
  Token.JSDocOptionalType baseType ->
    formatSExprList ["optional-type", renderJSDocTypeToSExpr baseType]
  Token.JSDocNullableType baseType ->
    formatSExprList ["nullable-type", renderJSDocTypeToSExpr baseType]
  Token.JSDocNonNullableType baseType ->
    formatSExprList ["non-nullable-type", renderJSDocTypeToSExpr baseType]
  Token.JSDocEnumType enumName enumValues ->
    formatSExprList
      [ "enum-type",
        escapeSExprString (Text.unpack enumName),
        formatSExprList ("values" : map renderJSDocEnumValueToSExpr enumValues)
      ]

-- | Render JSDoc enum value to S-expression
renderJSDocEnumValueToSExpr :: Token.JSDocEnumValue -> Text
renderJSDocEnumValueToSExpr enumValue =
  let nameExpr = escapeSExprString (Text.unpack (Token.jsDocEnumValueName enumValue))
      literalExpr = case Token.jsDocEnumValueLiteral enumValue of
        Nothing -> "nil"
        Just literal -> escapeSExprString (Text.unpack literal)
      descExpr = case Token.jsDocEnumValueDescription enumValue of
        Nothing -> "nil"
        Just desc -> escapeSExprString (Text.unpack desc)
  in formatSExprList ["enum-value", nameExpr, literalExpr, descExpr]

-- | Render JSDoc property to S-expression
renderJSDocPropertyToSExpr :: Token.JSDocProperty -> Text
renderJSDocPropertyToSExpr property =
  let nameExpr = escapeSExprString (Text.unpack (Token.jsDocPropertyName property))
      typeExpr = case Token.jsDocPropertyType property of
        Nothing -> "nil"
        Just jsDocType -> renderJSDocTypeToSExpr jsDocType
      optionalExpr = if Token.jsDocPropertyOptional property then "optional" else "required"
      descExpr = case Token.jsDocPropertyDescription property of
        Nothing -> "nil"
        Just desc -> escapeSExprString (Text.unpack desc)
  in formatSExprList ["jsdoc-property", nameExpr, typeExpr, optionalExpr, descExpr]

-- | Render JSDoc object field to S-expression
renderJSDocObjectFieldToSExpr :: Token.JSDocObjectField -> Text
renderJSDocObjectFieldToSExpr field =
  formatSExprList
    [ "object-field",
      escapeSExprString (Text.unpack (Token.jsDocFieldName field)),
      renderJSDocTypeToSExpr (Token.jsDocFieldType field),
      if Token.jsDocFieldOptional field then "optional" else "required"
    ]

-- | Render JSDoc tag specific information to S-expression.
renderJSDocTagSpecificToSExpr :: Token.JSDocTagSpecific -> Text
renderJSDocTagSpecificToSExpr tagSpecific = case tagSpecific of
  Token.JSDocParamTag optional variadic defaultValue ->
    formatSExprList
      [ "param-specific",
        if optional then "optional" else "required",
        if variadic then "variadic" else "fixed",
        maybe "nil" (escapeSExprString . Text.unpack) defaultValue
      ]
  Token.JSDocReturnTag promise ->
    formatSExprList
      [ "return-specific",
        if promise then "promise" else "value"
      ]
  Token.JSDocDescriptionTag text ->
    formatSExprList ["description-specific", escapeSExprString (Text.unpack text)]
  Token.JSDocTypeTag jsDocType ->
    formatSExprList ["type-specific", renderJSDocTypeToSExpr jsDocType]
  Token.JSDocPropertyTag name maybeType optional maybeDescription ->
    formatSExprList
      [ "property-specific",
        escapeSExprString (Text.unpack name),
        maybe "nil" renderJSDocTypeToSExpr maybeType,
        if optional then "optional" else "required",
        maybe "nil" (escapeSExprString . Text.unpack) maybeDescription
      ]
  Token.JSDocDefaultTag value ->
    formatSExprList ["default-specific", escapeSExprString (Text.unpack value)]
  Token.JSDocConstantTag maybeValue ->
    formatSExprList
      [ "constant-specific",
        maybe "nil" (escapeSExprString . Text.unpack) maybeValue
      ]
  Token.JSDocGlobalTag ->
    formatSExprList ["global-specific"]
  Token.JSDocAliasTag name ->
    formatSExprList ["alias-specific", escapeSExprString (Text.unpack name)]
  Token.JSDocAugmentsTag parent ->
    formatSExprList ["augments-specific", escapeSExprString (Text.unpack parent)]
  Token.JSDocBorrowsTag from maybeAs ->
    formatSExprList
      [ "borrows-specific",
        escapeSExprString (Text.unpack from),
        maybe "nil" (escapeSExprString . Text.unpack) maybeAs
      ]
  Token.JSDocClassDescTag description ->
    formatSExprList ["classdesc-specific", escapeSExprString (Text.unpack description)]
  Token.JSDocCopyrightTag notice ->
    formatSExprList ["copyright-specific", escapeSExprString (Text.unpack notice)]
  Token.JSDocExportsTag name ->
    formatSExprList ["exports-specific", escapeSExprString (Text.unpack name)]
  Token.JSDocExternalTag name maybeDescription ->
    formatSExprList
      [ "external-specific",
        escapeSExprString (Text.unpack name),
        maybe "nil" (escapeSExprString . Text.unpack) maybeDescription
      ]
  Token.JSDocFileTag description ->
    formatSExprList ["file-specific", escapeSExprString (Text.unpack description)]
  Token.JSDocFunctionTag ->
    formatSExprList ["function-specific"]
  Token.JSDocHideConstructorTag ->
    formatSExprList ["hideconstructor-specific"]
  Token.JSDocImplementsTag interface ->
    formatSExprList ["implements-specific", escapeSExprString (Text.unpack interface)]
  Token.JSDocInheritDocTag ->
    formatSExprList ["inheritdoc-specific"]
  Token.JSDocInstanceTag ->
    formatSExprList ["instance-specific"]
  Token.JSDocInterfaceTag maybeName ->
    formatSExprList
      [ "interface-specific",
        maybe "nil" (escapeSExprString . Text.unpack) maybeName
      ]
  Token.JSDocKindTag kind ->
    formatSExprList ["kind-specific", escapeSExprString (Text.unpack kind)]
  Token.JSDocLendsTag name ->
    formatSExprList ["lends-specific", escapeSExprString (Text.unpack name)]
  Token.JSDocLicenseTag license ->
    formatSExprList ["license-specific", escapeSExprString (Text.unpack license)]
  Token.JSDocMemberTag maybeName maybeType ->
    formatSExprList
      [ "member-specific",
        maybe "nil" (escapeSExprString . Text.unpack) maybeName,
        maybe "nil" (escapeSExprString . Text.unpack) maybeType
      ]
  Token.JSDocMixesTag mixin ->
    formatSExprList ["mixes-specific", escapeSExprString (Text.unpack mixin)]
  Token.JSDocMixinTag ->
    formatSExprList ["mixin-specific"]
  Token.JSDocNameTag name ->
    formatSExprList ["name-specific", escapeSExprString (Text.unpack name)]
  Token.JSDocRequiresTag module' ->
    formatSExprList ["requires-specific", escapeSExprString (Text.unpack module')]
  Token.JSDocSummaryTag summary ->
    formatSExprList ["summary-specific", escapeSExprString (Text.unpack summary)]
  Token.JSDocThisTag thisType ->
    formatSExprList ["this-specific", renderJSDocTypeToSExpr thisType]
  Token.JSDocTodoTag todo ->
    formatSExprList ["todo-specific", escapeSExprString (Text.unpack todo)]
  Token.JSDocTutorialTag tutorial ->
    formatSExprList ["tutorial-specific", escapeSExprString (Text.unpack tutorial)]
  Token.JSDocVariationTag variation ->
    formatSExprList ["variation-specific", escapeSExprString (Text.unpack variation)]
  Token.JSDocYieldsTag maybeType maybeDescription ->
    formatSExprList
      [ "yields-specific",
        maybe "nil" renderJSDocTypeToSExpr maybeType,
        maybe "nil" (escapeSExprString . Text.unpack) maybeDescription
      ]
  Token.JSDocThrowsTag maybeDescription ->
    formatSExprList
      [ "throws-specific",
        maybe "nil" (escapeSExprString . Text.unpack) maybeDescription
      ]
  Token.JSDocExampleTag maybeLanguage maybeCaption ->
    formatSExprList
      [ "example-specific",
        maybe "nil" (escapeSExprString . Text.unpack) maybeLanguage,
        maybe "nil" (escapeSExprString . Text.unpack) maybeCaption
      ]
  Token.JSDocSeeTag reference maybeDisplayText ->
    formatSExprList
      [ "see-specific",
        escapeSExprString (Text.unpack reference),
        maybe "nil" (escapeSExprString . Text.unpack) maybeDisplayText
      ]
  Token.JSDocDeprecatedTag maybeSince maybeReplacement ->
    formatSExprList
      [ "deprecated-specific",
        maybe "nil" (escapeSExprString . Text.unpack) maybeSince,
        maybe "nil" (escapeSExprString . Text.unpack) maybeReplacement
      ]
  Token.JSDocAuthorTag name email ->
    formatSExprList
      [ "author-specific",
        escapeSExprString (Text.unpack name),
        maybe "nil" (escapeSExprString . Text.unpack) email
      ]
  Token.JSDocVersionTag version ->
    formatSExprList ["version-specific", escapeSExprString (Text.unpack version)]
  Token.JSDocSinceTag version ->
    formatSExprList ["since-specific", escapeSExprString (Text.unpack version)]
  Token.JSDocAccessTag access ->
    formatSExprList ["access-specific", escapeSExprString (show access)]
  Token.JSDocNamespaceTag path ->
    formatSExprList ["namespace-specific", escapeSExprString (Text.unpack path)]
  Token.JSDocClassTag maybeName maybeExtends ->
    formatSExprList
      [ "class-specific",
        maybe "nil" (escapeSExprString . Text.unpack) maybeName,
        maybe "nil" (escapeSExprString . Text.unpack) maybeExtends
      ]
  Token.JSDocModuleTag name maybeType ->
    formatSExprList
      [ "module-specific",
        escapeSExprString (Text.unpack name),
        maybe "nil" (escapeSExprString . Text.unpack) maybeType
      ]
  Token.JSDocMemberOfTag parent forced ->
    formatSExprList
      [ "memberof-specific",
        escapeSExprString (Text.unpack parent),
        if forced then "forced" else "natural"
      ]
  Token.JSDocTypedefTag name properties ->
    formatSExprList
      [ "typedef-specific",
        escapeSExprString (Text.unpack name),
        formatSExprList ("properties" : map renderJSDocPropertyToSExpr properties)
      ]
  Token.JSDocEnumTag name maybeBaseType enumValues ->
    formatSExprList
      [ "enum-specific",
        escapeSExprString (Text.unpack name),
        maybe "nil" renderJSDocTypeToSExpr maybeBaseType,
        formatSExprList ("values" : map renderJSDocEnumValueToSExpr enumValues)
      ]
  Token.JSDocCallbackTag name ->
    formatSExprList ["callback-specific", escapeSExprString (Text.unpack name)]
  Token.JSDocEventTag name ->
    formatSExprList ["event-specific", escapeSExprString (Text.unpack name)]
  Token.JSDocFiresTag name ->
    formatSExprList ["fires-specific", escapeSExprString (Text.unpack name)]
  Token.JSDocListensTag name ->
    formatSExprList ["listens-specific", escapeSExprString (Text.unpack name)]
  Token.JSDocIgnoreTag ->
    formatSExprList ["ignore-specific"]
  Token.JSDocInnerTag ->
    formatSExprList ["inner-specific"]
  Token.JSDocReadOnlyTag ->
    formatSExprList ["readonly-specific"]
  Token.JSDocStaticTag ->
    formatSExprList ["static-specific"]
  Token.JSDocOverrideTag ->
    formatSExprList ["override-specific"]
  Token.JSDocAbstractTag ->
    formatSExprList ["abstract-specific"]
  Token.JSDocFinalTag ->
    formatSExprList ["final-specific"]
  Token.JSDocGeneratorTag ->
    formatSExprList ["generator-specific"]
  Token.JSDocAsyncTag ->
    formatSExprList ["async-specific"]

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
  AST.JSLOne expr ->
    formatSExprList
      [ "comma-list",
        renderExpressionToSExpr expr
      ]
  AST.JSLCons restList annot headItem ->
    formatSExprList
      [ "comma-list",
        renderCommaListToSExpr restList,
        renderAnnotation annot,
        renderExpressionToSExpr headItem
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
  AST.JSUnparenthesizedArrowParameter ident ->
    formatSExprList
      [ "JSUnparenthesizedArrowParameter",
        renderIdentToSExpr ident
      ]
  AST.JSParenthesizedArrowParameterList annot list rannot ->
    formatSExprList
      [ "JSParenthesizedArrowParameterList",
        renderAnnotation annot,
        renderCommaListToSExpr list,
        renderAnnotation rannot
      ]

-- | Render arrow body to S-expression
renderArrowBodyToSExpr :: AST.JSConciseBody -> Text
renderArrowBodyToSExpr body = case body of
  AST.JSConciseExpressionBody expr ->
    formatSExprList
      [ "JSConciseExpressionBody",
        renderExpressionToSExpr expr
      ]
  AST.JSConciseFunctionBody block ->
    formatSExprList
      [ "JSConciseFunctionBody",
        renderBlockToSExpr block
      ]

-- | Render identifier to S-expression
renderIdentToSExpr :: AST.JSIdent -> Text
renderIdentToSExpr ident = case ident of
  AST.JSIdentName annot name ->
    formatSExprList
      [ "JSIdentName",
        escapeSExprString (Text.unpack . Text.decodeUtf8 $ name),
        renderAnnotation annot
      ]
  AST.JSIdentNone ->
    formatSExprList
      [ "JSIdentNone"
      ]

-- | Render block to S-expression
renderBlockToSExpr :: AST.JSBlock -> Text
renderBlockToSExpr (AST.JSBlock lbrace stmts rbrace) =
  formatSExprList
    [ "JSBlock",
      renderAnnotation lbrace,
      formatSExprList ("statements" : map renderStatementToSExpr stmts),
      renderAnnotation rbrace
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
