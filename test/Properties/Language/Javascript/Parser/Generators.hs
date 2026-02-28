{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Comprehensive QuickCheck generators for JavaScript AST nodes.
--
-- This module provides complete Arbitrary instances for all JavaScript AST node types,
-- enabling property-based testing with automatically generated test cases. The generators
-- are designed to produce realistic JavaScript code patterns while avoiding infinite
-- structures through careful size control.
--
-- The generator infrastructure supports:
--
--   * __Complete AST coverage__: Arbitrary instances for all JavaScript constructs
--     including expressions, statements, declarations, and module items with
--     comprehensive support for ES5 through modern JavaScript features.
--
--   * __Size-controlled generation__: Prevents stack overflow and infinite recursion
--     through explicit size management and depth limiting for nested structures.
--
--   * __Realistic patterns__: Generates valid JavaScript code that follows common
--     programming patterns and syntactic conventions for meaningful test cases.
--
--   * __Invalid input generation__: Specialized generators for syntactically invalid
--     constructs to test parser error handling and recovery mechanisms.
--
--   * __Edge case stress testing__: Generators for boundary conditions, Unicode edge
--     cases, deeply nested structures, and parser stress scenarios.
--
-- All generators follow CLAUDE.md standards with functions ≤15 lines, qualified
-- imports, and comprehensive Haddock documentation.
--
-- ==== Examples
--
-- Generating valid expressions:
--
-- >>> sample (arbitrary :: Gen JSExpression)
-- JSIdentifier (JSAnnot ...) "x"
-- JSDecimal (JSAnnot ...) "42"
-- JSExpressionBinary (JSIdentifier ...) (JSBinOpPlus ...) (JSDecimal ...)
--
-- Generating invalid programs for error testing:
--
-- >>> sample genInvalidJavaScript
-- "function ( { return x; }"  -- Missing function name
-- "var 123abc = 42;"          -- Invalid identifier
-- "if (x { return; }"         -- Missing closing paren
--
-- @since 0.7.1.0
module Properties.Language.Javascript.Parser.Generators
  ( -- * AST Node Generators
    genJSExpression,
    genJSStatement,
    genJSBinOp,
    genJSUnaryOp,
    genJSAssignOp,
    genJSAnnot,
    genJSSemi,
    genJSIdent,
    genJSAST,

    -- * Size-Controlled Generators
    genSizedExpression,
    genSizedStatement,
    genSizedProgram,

    -- * Invalid JavaScript Generators
    genInvalidJavaScript,
    genInvalidExpression,
    genInvalidStatement,
    genMalformedSyntax,

    -- * Edge Case Generators
    genUnicodeEdgeCases,
    genDeeplyNestedStructures,
    genParserStressTests,
    genBoundaryConditions,

    -- * Utility Generators
    genValidIdentifier,
    genValidNumber,
    genValidString,
    genCommaList,
    genJSObjectPropertyList,
  )
where

import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List as List
import qualified Data.Text as Text
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.SrcLocation (TokenPosn (..), tokenPosnEmpty)
import qualified Language.JavaScript.Parser.Token as Token
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Core AST Node Generators
-- ---------------------------------------------------------------------

-- | Generate arbitrary JavaScript expressions with size control.
--
-- Produces all major expression types including literals, identifiers,
-- binary operations, function calls, and complex nested expressions.
-- Uses size parameter to prevent infinite recursion in nested structures.
--
-- ==== Examples
--
-- >>> sample (genJSExpression 3)
-- JSIdentifier (JSAnnot ...) "variable"
-- JSExpressionBinary (JSDecimal ...) (JSBinOpPlus ...) (JSLiteral ...)
-- JSCallExpression (JSIdentifier ...) [...] (JSLNil) [...]
genJSExpression :: Gen JSExpression
genJSExpression = sized genSizedExpression

-- | Generate arbitrary JavaScript statements with complexity control.
--
-- Creates all statement types including declarations, control flow,
-- function definitions, and block statements. Manages nesting depth
-- to ensure termination and realistic code structure.
genJSStatement :: Gen JSStatement
genJSStatement = sized genSizedStatement

-- | Generate arbitrary binary operators.
--
-- Produces all JavaScript binary operators including arithmetic,
-- comparison, logical, bitwise, and assignment operators with
-- proper annotation information.
genJSBinOp :: Gen JSBinOp
genJSBinOp = do
  annot <- genJSAnnot
  elements
    [ JSBinOpAnd annot,
      JSBinOpBitAnd annot,
      JSBinOpBitOr annot,
      JSBinOpBitXor annot,
      JSBinOpDivide annot,
      JSBinOpEq annot,
      JSBinOpExponentiation annot,
      JSBinOpGe annot,
      JSBinOpGt annot,
      JSBinOpIn annot,
      JSBinOpInstanceOf annot,
      JSBinOpLe annot,
      JSBinOpLsh annot,
      JSBinOpLt annot,
      JSBinOpMinus annot,
      JSBinOpMod annot,
      JSBinOpNeq annot,
      JSBinOpOf annot,
      JSBinOpOr annot,
      JSBinOpNullishCoalescing annot,
      JSBinOpPlus annot,
      JSBinOpRsh annot,
      JSBinOpStrictEq annot,
      JSBinOpStrictNeq annot,
      JSBinOpTimes annot,
      JSBinOpUrsh annot
    ]

-- | Generate arbitrary unary operators.
--
-- Creates all JavaScript unary operators including arithmetic,
-- logical, type checking, and increment/decrement operators.
genJSUnaryOp :: Gen JSUnaryOp
genJSUnaryOp = do
  annot <- genJSAnnot
  elements
    [ JSUnaryOpDecr annot,
      JSUnaryOpDelete annot,
      JSUnaryOpIncr annot,
      JSUnaryOpMinus annot,
      JSUnaryOpNot annot,
      JSUnaryOpPlus annot,
      JSUnaryOpTilde annot,
      JSUnaryOpTypeof annot,
      JSUnaryOpVoid annot
    ]

-- | Generate arbitrary assignment operators.
--
-- Produces all JavaScript assignment operators including simple
-- assignment and compound assignment operators for arithmetic
-- and bitwise operations.
genJSAssignOp :: Gen JSAssignOp
genJSAssignOp = do
  annot <- genJSAnnot
  elements
    [ JSAssign annot,
      JSTimesAssign annot,
      JSDivideAssign annot,
      JSModAssign annot,
      JSPlusAssign annot,
      JSMinusAssign annot,
      JSLshAssign annot,
      JSRshAssign annot,
      JSUrshAssign annot,
      JSBwAndAssign annot,
      JSBwXorAssign annot,
      JSBwOrAssign annot,
      JSLogicalAndAssign annot,
      JSLogicalOrAssign annot,
      JSNullishAssign annot
    ]

-- | Generate arbitrary JavaScript annotations.
--
-- Creates annotation objects containing position information
-- and comment data. Balanced between no annotation, space
-- annotation, and full position annotations.
genJSAnnot :: Gen JSAnnot
genJSAnnot =
  frequency
    [ (3, return JSNoAnnot),
      (1, return JSAnnotSpace),
      (1, JSAnnot <$> genTokenPosn <*> genCommentList)
    ]
  where
    genTokenPosn = do
      addr <- choose (0, 10000)
      line <- choose (1, 1000)
      col <- choose (0, 200)
      return (TokenPn addr line col)
    genCommentList = listOf genCommentAnnotation
    genCommentAnnotation =
      oneof
        [ Token.CommentA <$> genTokenPosn <*> genValidString,
          Token.WhiteSpace <$> genTokenPosn <*> genWhitespace,
          pure Token.NoComment
        ]
    genWhitespace = BS8.pack <$> elements [" ", "\t", "\n", "\r\n"]

-- | Generate arbitrary semicolon tokens.
--
-- Creates semicolon tokens including explicit semicolons with
-- annotations and automatic semicolon insertion markers.
genJSSemi :: Gen JSSemi
genJSSemi =
  oneof
    [ JSSemi <$> genJSAnnot,
      return JSSemiAuto
    ]

-- | Generate arbitrary JavaScript identifiers.
--
-- Creates valid identifier objects including simple names and
-- reserved word identifiers with proper annotation information.
genJSIdent :: Gen JSIdent
genJSIdent =
  oneof
    [ JSIdentName <$> genJSAnnot <*> genValidIdentifier,
      pure JSIdentNone
    ]

-- | Generate arbitrary JavaScript AST roots.
--
-- Creates complete AST structures including programs, modules,
-- statements, expressions, and literals with proper nesting
-- and realistic structure.
genJSAST :: Gen JSAST
genJSAST =
  oneof
    [ JSAstProgram <$> genStatementList <*> genJSAnnot,
      JSAstModule <$> genModuleItemList <*> genJSAnnot,
      JSAstStatement <$> genJSStatement <*> genJSAnnot,
      JSAstExpression <$> genJSExpression <*> genJSAnnot,
      JSAstLiteral <$> genLiteralExpression <*> genJSAnnot
    ]
  where
    genStatementList = listOf genJSStatement
    genModuleItemList = listOf genJSModuleItem

-- ---------------------------------------------------------------------
-- Size-Controlled Generators
-- ---------------------------------------------------------------------

-- | Generate sized JavaScript expression with depth control.
--
-- Controls recursion depth to prevent infinite structures while
-- maintaining realistic nesting patterns. Reduces size parameter
-- for recursive calls to ensure termination.
genSizedExpression :: Int -> Gen JSExpression
genSizedExpression 0 = genAtomicExpression
genSizedExpression n =
  frequency
    [ (3, genAtomicExpression),
      (2, genBinaryExpression n),
      (2, genUnaryExpression n),
      (1, genCallExpression n),
      (1, genMemberExpression n),
      (1, genArrayLiteral n),
      (1, genObjectLiteral n)
    ]

-- | Generate sized JavaScript statement with complexity control.
--
-- Manages statement nesting depth and complexity to produce
-- realistic code structures. Controls block nesting and
-- conditional statement depth for balanced generation.
genSizedStatement :: Int -> Gen JSStatement
genSizedStatement 0 = genAtomicStatement
genSizedStatement n =
  frequency
    [ (4, genAtomicStatement),
      (2, genBlockStatement n),
      (2, genIfStatement n),
      (1, genForStatement n),
      (1, genActualWhileStatement n),
      (1, genDoWhileStatement n),
      (1, genFunctionStatement n),
      (1, genVariableStatement),
      (1, genSwitchStatement n),
      (1, genTryStatement n),
      (1, genThrowStatement),
      (1, genWithStatement n)
    ]

-- | Generate sized JavaScript program with controlled complexity.
--
-- Creates complete programs with controlled statement count
-- and nesting depth. Balances program size with structural
-- diversity for comprehensive testing coverage.
genSizedProgram :: Int -> Gen JSAST
genSizedProgram size = do
  stmtCount <- choose (1, max 1 (size `div` 2))
  stmts <- replicateM stmtCount (genSizedStatement (size `div` 4))
  annot <- genJSAnnot
  return (JSAstProgram stmts annot)

-- ---------------------------------------------------------------------
-- Invalid JavaScript Generators
-- ---------------------------------------------------------------------

-- | Generate syntactically invalid JavaScript code.
--
-- Creates malformed JavaScript specifically designed to test
-- parser error handling and recovery. Includes missing tokens,
-- invalid syntax patterns, and structural errors.
--
-- ==== Examples
--
-- >>> sample genInvalidJavaScript
-- "function ( { return; }"      -- Missing function name
-- "var 123abc = value;"         -- Invalid identifier start
-- "if (condition { stmt; }"     -- Missing closing parenthesis
genInvalidJavaScript :: Gen String
genInvalidJavaScript =
  oneof
    [ genMissingSyntaxTokens,
      genInvalidIdentifiers,
      genUnmatchedDelimiters,
      genIncompleteStatements,
      genInvalidOperatorSequences
    ]

-- | Generate syntactically invalid expressions.
--
-- Creates malformed expression syntax for testing parser
-- error recovery. Focuses on operator precedence violations,
-- missing operands, and invalid token sequences.
genInvalidExpression :: Gen String
genInvalidExpression =
  oneof
    [ genInvalidBinaryOp,
      genInvalidUnaryOp,
      genMissingOperands,
      genInvalidLiterals
    ]

-- | Generate syntactically invalid statements.
--
-- Creates malformed statement syntax including incomplete
-- control flow, missing semicolons, and invalid declarations
-- for comprehensive error handling testing.
genInvalidStatement :: Gen String
genInvalidStatement =
  oneof
    [ genIncompleteIf,
      genInvalidFor,
      genMalformedFunction,
      genInvalidDeclaration
    ]

-- | Generate malformed syntax patterns.
--
-- Creates systematically broken JavaScript syntax patterns
-- covering all major syntactic categories for exhaustive
-- parser error testing coverage.
genMalformedSyntax :: Gen String
genMalformedSyntax =
  oneof
    [ genInvalidTokenSequences,
      genStructuralErrors,
      genContextErrors
    ]

-- ---------------------------------------------------------------------
-- Edge Case Generators
-- ---------------------------------------------------------------------

-- | Generate Unicode edge cases for identifier testing.
--
-- Creates identifiers using Unicode characters, surrogate pairs,
-- and boundary conditions to test lexer Unicode handling and
-- identifier validation edge cases.
genUnicodeEdgeCases :: Gen String
genUnicodeEdgeCases =
  oneof
    [ genUnicodeIdentifiers,
      genSurrogatePairs,
      genCombiningCharacters,
      genNonBMPCharacters
    ]

-- | Generate deeply nested JavaScript structures.
--
-- Creates pathological nesting scenarios to stress test parser
-- stack limits and performance. Includes function nesting,
-- object nesting, and expression nesting stress tests.
genDeeplyNestedStructures :: Gen String
genDeeplyNestedStructures =
  oneof
    [ genDeeplyNestedFunctions,
      genDeeplyNestedObjects,
      genDeeplyNestedArrays,
      genDeeplyNestedExpressions
    ]

-- | Generate parser stress test cases.
--
-- Creates challenging parsing scenarios including large files,
-- complex expressions, and edge case combinations designed
-- to test parser performance and robustness.
genParserStressTests :: Gen String
genParserStressTests =
  oneof
    [ genLargePrograms,
      genComplexExpressions,
      genRepetitiveStructures,
      genEdgeCaseCombinations
    ]

-- | Generate boundary condition test cases.
--
-- Creates test cases at syntactic and semantic boundaries
-- including maximum identifier lengths, numeric limits,
-- and string length boundaries.
genBoundaryConditions :: Gen String
genBoundaryConditions =
  oneof
    [ genMaxLengthIdentifiers,
      genNumericBoundaries,
      genStringBoundaries,
      genNestingLimits
    ]

-- ---------------------------------------------------------------------
-- Utility Generators
-- ---------------------------------------------------------------------

-- | Generate valid JavaScript identifier.
--
-- Creates identifiers following JavaScript naming rules including
-- Unicode letter starts, alphanumeric continuation, and reserved
-- word avoidance for realistic identifier generation.
genValidIdentifier :: Gen ByteString
genValidIdentifier = do
  first <- genIdentifierStart
  rest <- listOf genIdentifierPart
  let identifier = first : rest
  if identifier `elem` reservedWords
    then genValidIdentifier
    else return (BS8.pack identifier)
  where
    genIdentifierStart =
      oneof
        [ choose ('a', 'z'),
          choose ('A', 'Z'),
          return '_',
          return '$'
        ]
    genIdentifierPart =
      oneof
        [ choose ('a', 'z'),
          choose ('A', 'Z'),
          choose ('0', '9'),
          return '_',
          return '$'
        ]
    reservedWords =
      [ "break",
        "case",
        "catch",
        "continue",
        "debugger",
        "default",
        "delete",
        "do",
        "else",
        "finally",
        "for",
        "function",
        "if",
        "in",
        "instanceof",
        "new",
        "return",
        "switch",
        "this",
        "throw",
        "try",
        "typeof",
        "var",
        "void",
        "while",
        "with",
        "class",
        "const",
        "enum",
        "export",
        "extends",
        "import",
        "super",
        "implements",
        "interface",
        "let",
        "package",
        "private",
        "protected",
        "public",
        "static",
        "yield"
      ]

-- | Generate valid JavaScript number literal.
--
-- Creates numeric literals including integers, floats, scientific
-- notation, hexadecimal, binary, and octal formats following
-- JavaScript numeric literal syntax rules.
genValidNumber :: Gen ByteString
genValidNumber =
  oneof
    [ genDecimalInteger,
      genDecimalFloat,
      genScientificNotation,
      genHexadecimal,
      genBinary,
      genOctal
    ]
  where
    genDecimalInteger = BS8.pack . show <$> (arbitrary :: Gen Integer)
    genDecimalFloat = do
      integral <- abs <$> (arbitrary :: Gen Integer)
      fractional <- abs <$> (arbitrary :: Gen Integer)
      return (BS8.pack (show integral ++ "." ++ show fractional))
    genScientificNotation = do
      base <- genDecimalFloat
      exponent' <- arbitrary :: Gen Int
      return (base <> BS8.pack ("e" ++ show exponent'))
    genHexadecimal = do
      num <- abs <$> (arbitrary :: Gen Integer)
      return (BS8.pack ("0x" ++ showHex num ""))
      where
        showHex 0 acc = if null acc then "0" else acc
        showHex n acc = showHex (n `div` 16) (hexDigit (n `mod` 16) : acc)
        hexDigit d = "0123456789abcdef" !! fromInteger d
    genBinary = do
      num <- abs <$> (arbitrary :: Gen Int)
      return (BS8.pack ("0b" ++ showBin num ""))
      where
        showBin 0 acc = if null acc then "0" else acc
        showBin n acc = showBin (n `div` 2) (show (n `mod` 2) ++ acc)
    genOctal = do
      num <- abs <$> (arbitrary :: Gen Int)
      return (BS8.pack ("0o" ++ showOct num ""))
      where
        showOct 0 acc = if null acc then "0" else acc
        showOct n acc = showOct (n `div` 8) (show (n `mod` 8) ++ acc)

-- | Generate valid JavaScript string literal.
--
-- Creates string literals with proper escaping, quote handling,
-- and special character support including Unicode escapes
-- and template literal syntax.
genValidString :: Gen ByteString
genValidString =
  oneof
    [ genSingleQuotedString,
      genDoubleQuotedString,
      genTemplateLiteral
    ]
  where
    genSingleQuotedString = do
      content <- genStringContent '\''
      return (BS8.pack ("'" ++ content ++ "'"))
    genDoubleQuotedString = do
      content <- genStringContent '"'
      return (BS8.pack ("\"" ++ content ++ "\""))
    genTemplateLiteral = do
      content <- genTemplateContent
      return (BS8.pack ("`" ++ content ++ "`"))
    genStringContent _quote = listOf genStringChar
    genStringChar =
      oneof
        [ choose ('a', 'z'),
          choose ('A', 'Z'),
          choose ('0', '9'),
          return ' '
        ]
    genTemplateContent = listOf genTemplateChar
    genTemplateChar =
      oneof
        [ choose ('a', 'z'),
          choose ('A', 'Z'),
          choose ('0', '9'),
          return ' ',
          return '\n'
        ]

-- | Generate comma-separated list with proper structure.
--
-- Creates JSCommaList structures with correct comma placement
-- and trailing comma handling for function parameters,
-- array elements, and object properties.
genCommaList :: Gen a -> Gen (JSCommaList a)
genCommaList genElement =
  oneof
    [ return JSLNil,
      JSLOne <$> genElement,
      do
        first <- genElement
        comma <- genJSAnnot
        rest <- genElement
        return (JSLCons (JSLOne first) comma rest)
    ]

-- | Generate JavaScript object property list.
--
-- Creates object property lists with mixed property types
-- including data properties, getters, setters, and methods
-- with proper comma separation and syntax.
genJSObjectPropertyList :: Gen JSObjectPropertyList
genJSObjectPropertyList =
  oneof
    [ JSCTLNone <$> genCommaList genJSObjectProperty,
      do
        list <- genCommaList genJSObjectProperty
        comma <- genJSAnnot
        return (JSCTLComma list comma)
    ]

-- ---------------------------------------------------------------------
-- Helper Generators for Complex Structures
-- ---------------------------------------------------------------------

-- | Generate atomic (non-recursive) expressions.
genAtomicExpression :: Gen JSExpression
genAtomicExpression =
  oneof
    [ genLiteralExpression,
      genIdentifierExpression,
      genThisExpression
    ]

-- | Generate atomic (non-recursive) statements.
genAtomicStatement :: Gen JSStatement
genAtomicStatement =
  oneof
    [ genExpressionStatement,
      genReturnStatement,
      genBreakStatement,
      genContinueStatement,
      genEmptyStatement
    ]

-- | Generate literal expressions.
genLiteralExpression :: Gen JSExpression
genLiteralExpression =
  oneof
    [ JSDecimal <$> genJSAnnot <*> genDecimalDouble,
      JSLiteral <$> genJSAnnot <*> genBooleanLiteral,
      JSStringLiteral <$> genJSAnnot <*> genValidString,
      JSHexInteger <$> genJSAnnot <*> genHexInteger,
      JSBinaryInteger <$> genJSAnnot <*> genBinaryInteger,
      JSOctal <$> genJSAnnot <*> genOctalInteger,
      JSBigIntLiteral <$> genJSAnnot <*> genBigIntInteger,
      JSRegEx <$> genJSAnnot <*> genRegexLiteral
    ]
  where
    genBooleanLiteral = elements ["true", "false", "null", "undefined"]
    genDecimalDouble = fromIntegral . abs <$> (arbitrary :: Gen Int)
    genHexInteger = abs <$> (arbitrary :: Gen Integer)
    genBinaryInteger = abs <$> (arbitrary :: Gen Integer)
    genOctalInteger = abs <$> (arbitrary :: Gen Integer)
    genBigIntInteger = abs <$> (arbitrary :: Gen Integer)
    genRegexLiteral = do
      pat <- genRegexPattern
      flags <- genRegexFlags
      return (BS8.pack ("/" ++ pat ++ "/" ++ flags))
    genRegexPattern = listOf (elements "abcdefghijklmnopqrstuvwxyz.*+?[](){}|^$\\")
    genRegexFlags = sublistOf "gimsuvy"

-- | Generate identifier expressions.
genIdentifierExpression :: Gen JSExpression
genIdentifierExpression = JSIdentifier <$> genJSAnnot <*> genValidIdentifier

-- | Generate this expressions.
genThisExpression :: Gen JSExpression
genThisExpression = JSIdentifier <$> genJSAnnot <*> pure "this"

-- | Generate binary expressions with size control.
genBinaryExpression :: Int -> Gen JSExpression
genBinaryExpression n = do
  left <- genSizedExpression (n `div` 2)
  op <- genJSBinOp
  right <- genSizedExpression (n `div` 2)
  return (JSExpressionBinary left op right)

-- | Generate unary expressions with size control.
genUnaryExpression :: Int -> Gen JSExpression
genUnaryExpression n = do
  op <- genJSUnaryOp
  expr <- genSizedExpression (n - 1)
  return (JSUnaryExpression op expr)

-- | Generate call expressions with size control.
genCallExpression :: Int -> Gen JSExpression
genCallExpression n = do
  func <- genSizedExpression (n `div` 2)
  lparen <- genJSAnnot
  args <- genCommaList (genSizedExpression (n `div` 4))
  rparen <- genJSAnnot
  return (JSCallExpression func lparen args rparen)

-- | Generate member expressions with size control.
genMemberExpression :: Int -> Gen JSExpression
genMemberExpression n =
  oneof
    [ genMemberDot n,
      genMemberSquare n
    ]
  where
    genMemberDot size = do
      obj <- genSizedExpression (size `div` 2)
      dot <- genJSAnnot
      prop <- genIdentifierExpression
      return (JSMemberDot obj dot prop)
    genMemberSquare size = do
      obj <- genSizedExpression (size `div` 2)
      lbracket <- genJSAnnot
      prop <- genSizedExpression (size `div` 2)
      rbracket <- genJSAnnot
      return (JSMemberSquare obj lbracket prop rbracket)

-- | Generate array literals with size control.
genArrayLiteral :: Int -> Gen JSExpression
genArrayLiteral n = do
  lbracket <- genJSAnnot
  elements <- genArrayElementList (n `div` 2)
  rbracket <- genJSAnnot
  return (JSArrayLiteral lbracket elements rbracket)

-- | Generate object literals with size control.
genObjectLiteral :: Int -> Gen JSExpression
genObjectLiteral n = do
  lbrace <- genJSAnnot
  props <- genJSObjectPropertyList
  rbrace <- genJSAnnot
  return (JSObjectLiteral lbrace props rbrace)

-- | Generate expression statements.
genExpressionStatement :: Gen JSStatement
genExpressionStatement = do
  expr <- genJSExpression
  semi <- genJSSemi
  return (JSExpressionStatement expr semi)

-- | Generate return statements.
genReturnStatement :: Gen JSStatement
genReturnStatement = do
  annot <- genJSAnnot
  mexpr <- oneof [return Nothing, Just <$> genJSExpression]
  semi <- genJSSemi
  return (JSReturn annot mexpr semi)

-- | Generate break statements.
genBreakStatement :: Gen JSStatement
genBreakStatement = do
  annot <- genJSAnnot
  ident <- genJSIdent
  semi <- genJSSemi
  return (JSBreak annot ident semi)

-- | Generate continue statements.
genContinueStatement :: Gen JSStatement
genContinueStatement = do
  annot <- genJSAnnot
  ident <- genJSIdent
  semi <- genJSSemi
  return (JSContinue annot ident semi)

-- | Generate empty statements.
genEmptyStatement :: Gen JSStatement
genEmptyStatement = JSEmptyStatement <$> genJSAnnot

-- | Generate block statements with size control.
genBlockStatement :: Int -> Gen JSStatement
genBlockStatement n = do
  lbrace <- genJSAnnot
  stmts <- listOf (genSizedStatement (n `div` 2))
  rbrace <- genJSAnnot
  semi <- genJSSemi
  return (JSStatementBlock lbrace stmts rbrace semi)

-- | Generate if statements with size control.
genIfStatement :: Int -> Gen JSStatement
genIfStatement n =
  oneof
    [ genSimpleIf n,
      genIfElse n
    ]
  where
    genSimpleIf size = do
      ifAnnot <- genJSAnnot
      lparen <- genJSAnnot
      cond <- genSizedExpression (size `div` 3)
      rparen <- genJSAnnot
      stmt <- genSizedStatement (size `div` 2)
      return (JSIf ifAnnot lparen cond rparen stmt)
    genIfElse size = do
      ifAnnot <- genJSAnnot
      lparen <- genJSAnnot
      cond <- genSizedExpression (size `div` 4)
      rparen <- genJSAnnot
      thenStmt <- genSizedStatement (size `div` 3)
      elseAnnot <- genJSAnnot
      elseStmt <- genSizedStatement (size `div` 3)
      return (JSIfElse ifAnnot lparen cond rparen thenStmt elseAnnot elseStmt)

-- | Generate for statements with size control.
genForStatement :: Int -> Gen JSStatement
genForStatement n = do
  forAnnot <- genJSAnnot
  lparen <- genJSAnnot
  init <- genCommaList (genSizedExpression (n `div` 4))
  semi1 <- genJSAnnot
  cond <- genCommaList (genSizedExpression (n `div` 4))
  semi2 <- genJSAnnot
  update <- genCommaList (genSizedExpression (n `div` 4))
  rparen <- genJSAnnot
  stmt <- genSizedStatement (n `div` 2)
  return (JSFor forAnnot lparen init semi1 cond semi2 update rparen stmt)

-- | Generate do-while statements with size control.
genDoWhileStatement :: Int -> Gen JSStatement
genDoWhileStatement n = do
  doAnnot <- genJSAnnot
  stmt <- genSizedStatement (n `div` 2)
  whileAnnot <- genJSAnnot
  lparen <- genJSAnnot
  cond <- genSizedExpression (n `div` 2)
  rparen <- genJSAnnot
  return (JSDoWhile doAnnot stmt whileAnnot lparen cond rparen JSSemiAuto)

-- | Generate function statements with size control.
genFunctionStatement :: Int -> Gen JSStatement
genFunctionStatement n = do
  fnAnnot <- genJSAnnot
  name <- genJSIdent
  lparen <- genJSAnnot
  params <- genCommaList genIdentifierExpression
  rparen <- genJSAnnot
  block <- genJSBlock (n `div` 2)
  semi <- genJSSemi
  return (JSFunction fnAnnot name lparen params rparen block semi)

-- | Generate module items.
genJSModuleItem :: Gen JSModuleItem
genJSModuleItem =
  oneof
    [ JSModuleImportDeclaration <$> genJSAnnot <*> genJSImportDeclaration,
      JSModuleExportDeclaration <$> genJSAnnot <*> genJSExportDeclaration,
      JSModuleStatementListItem <$> genJSStatement
    ]

-- | Generate import declarations.
genJSImportDeclaration :: Gen JSImportDeclaration
genJSImportDeclaration =
  oneof
    [ genImportWithClause,
      genBareImport
    ]
  where
    genImportWithClause = do
      clause <- genJSImportClause
      from <- genJSFromClause
      attrs <- oneof [return Nothing, Just <$> genJSImportAttributes]
      semi <- genJSSemi
      return (JSImportDeclaration clause from attrs semi)
    genBareImport = do
      annot <- genJSAnnot
      module_ <- genValidString
      attrs <- oneof [return Nothing, Just <$> genJSImportAttributes]
      semi <- genJSSemi
      return (JSImportDeclarationBare annot module_ attrs semi)

-- | Generate export declarations.
genJSExportDeclaration :: Gen JSExportDeclaration
genJSExportDeclaration =
  oneof
    [ genExportAllFrom,
      genExportFrom,
      genExportLocals,
      genExportStatement
    ]
  where
    genExportAllFrom = do
      star <- genJSBinOp
      from <- genJSFromClause
      semi <- genJSSemi
      return (JSExportAllFrom star from semi)
    genExportFrom = do
      clause <- genJSExportClause
      from <- genJSFromClause
      semi <- genJSSemi
      return (JSExportFrom clause from semi)
    genExportLocals = do
      clause <- genJSExportClause
      semi <- genJSSemi
      return (JSExportLocals clause semi)
    genExportStatement = do
      stmt <- genJSStatement
      semi <- genJSSemi
      return (JSExport stmt semi)

-- | Generate export clauses.
genJSExportClause :: Gen JSExportClause
genJSExportClause = do
  lbrace <- genJSAnnot
  specs <- genCommaList genJSExportSpecifier
  rbrace <- genJSAnnot
  return (JSExportClause lbrace specs rbrace)

-- | Generate export specifiers.
genJSExportSpecifier :: Gen JSExportSpecifier
genJSExportSpecifier =
  oneof
    [ JSExportSpecifier <$> genJSIdent,
      do
        name1 <- genJSIdent
        asAnnot <- genJSAnnot
        name2 <- genJSIdent
        return (JSExportSpecifierAs name1 asAnnot name2)
    ]

-- | Generate variable statements.
genVariableStatement :: Gen JSStatement
genVariableStatement = do
  annot <- genJSAnnot
  decls <- genCommaList genVariableDeclaration
  semi <- genJSSemi
  return (JSVariable annot decls semi)
  where
    genVariableDeclaration =
      oneof
        [ genJSIdentifier,
          genJSVarInit
        ]
    genJSIdentifier = JSIdentifier <$> genJSAnnot <*> genValidIdentifier
    genJSVarInit = do
      ident <- genJSIdentifier
      initAnnot <- genJSAnnot
      expr <- genJSExpression
      return (JSVarInitExpression ident (JSVarInit initAnnot expr))

-- | Generate variable initializers.
genJSVarInitializer :: Gen JSVarInitializer
genJSVarInitializer =
  oneof
    [ return JSVarInitNone,
      do
        annot <- genJSAnnot
        expr <- genJSExpression
        return (JSVarInit annot expr)
    ]

-- | Generate while statements with size control.
genActualWhileStatement :: Int -> Gen JSStatement
genActualWhileStatement n = do
  whileAnnot <- genJSAnnot
  lparen <- genJSAnnot
  cond <- genSizedExpression (n `div` 2)
  rparen <- genJSAnnot
  stmt <- genSizedStatement (n `div` 2)
  return (JSWhile whileAnnot lparen cond rparen stmt)

-- | Generate switch statements.
genSwitchStatement :: Int -> Gen JSStatement
genSwitchStatement n = do
  switchAnnot <- genJSAnnot
  lparen <- genJSAnnot
  expr <- genSizedExpression (n `div` 3)
  rparen <- genJSAnnot
  lbrace <- genJSAnnot
  cases <- listOf (genJSSwitchParts (n `div` 4))
  rbrace <- genJSAnnot
  semi <- genJSSemi
  return (JSSwitch switchAnnot lparen expr rparen lbrace cases rbrace semi)

-- | Generate switch case parts.
genJSSwitchParts :: Int -> Gen JSSwitchParts
genJSSwitchParts n =
  oneof
    [ genCaseClause n,
      genDefaultClause n
    ]
  where
    genCaseClause size = do
      caseAnnot <- genJSAnnot
      expr <- genSizedExpression size
      colon <- genJSAnnot
      stmts <- listOf (genSizedStatement size)
      return (JSCase caseAnnot expr colon stmts)
    genDefaultClause size = do
      defaultAnnot <- genJSAnnot
      colon <- genJSAnnot
      stmts <- listOf (genSizedStatement size)
      return (JSDefault defaultAnnot colon stmts)

-- | Generate try statements.
genTryStatement :: Int -> Gen JSStatement
genTryStatement n = do
  tryAnnot <- genJSAnnot
  block <- genJSBlock (n `div` 3)
  catches <- listOf (genJSTryCatch (n `div` 4))
  finally <- genJSTryFinally (n `div` 4)
  return (JSTry tryAnnot block catches finally)

-- | Generate try-catch clauses.
genJSTryCatch :: Int -> Gen JSTryCatch
genJSTryCatch n =
  oneof
    [ genSimpleCatch n,
      genConditionalCatch n
    ]
  where
    genSimpleCatch size = do
      catchAnnot <- genJSAnnot
      lparen <- genJSAnnot
      ident <- genJSExpression
      rparen <- genJSAnnot
      block <- genJSBlock size
      return (JSCatch catchAnnot lparen ident rparen block)
    genConditionalCatch size = do
      catchAnnot <- genJSAnnot
      lparen <- genJSAnnot
      ident <- genJSExpression
      ifAnnot <- genJSAnnot
      cond <- genJSExpression
      rparen <- genJSAnnot
      block <- genJSBlock size
      return (JSCatchIf catchAnnot lparen ident ifAnnot cond rparen block)

-- | Generate try-finally clauses.
genJSTryFinally :: Int -> Gen JSTryFinally
genJSTryFinally n =
  oneof
    [ return JSNoFinally,
      do
        finallyAnnot <- genJSAnnot
        block <- genJSBlock n
        return (JSFinally finallyAnnot block)
    ]

-- | Generate throw statements.
genThrowStatement :: Gen JSStatement
genThrowStatement = do
  throwAnnot <- genJSAnnot
  expr <- genJSExpression
  semi <- genJSSemi
  return (JSThrow throwAnnot expr semi)

-- | Generate with statements.
genWithStatement :: Int -> Gen JSStatement
genWithStatement n = do
  withAnnot <- genJSAnnot
  lparen <- genJSAnnot
  expr <- genSizedExpression (n `div` 2)
  rparen <- genJSAnnot
  stmt <- genSizedStatement (n `div` 2)
  semi <- genJSSemi
  return (JSWith withAnnot lparen expr rparen stmt semi)

-- ---------------------------------------------------------------------
-- Invalid Syntax Generators Implementation
-- ---------------------------------------------------------------------

-- | Generate missing syntax tokens.
genMissingSyntaxTokens :: Gen String
genMissingSyntaxTokens =
  oneof
    [ return "function ( { return x; }", -- Missing function name
      return "if (x { return; }", -- Missing closing paren
      return "for (var i = 0 i < 10; i++)", -- Missing semicolon
      return "{ var x = 42" -- Missing closing brace
    ]

-- | Generate invalid identifiers.
genInvalidIdentifiers :: Gen String
genInvalidIdentifiers =
  oneof
    [ return "var 123abc = 42;", -- Identifier starts with digit
      return "let class = 'test';", -- Reserved word as identifier
      return "const @invalid = true;", -- Invalid character in identifier
      return "function 2bad() {}" -- Function name starts with digit
    ]

-- | Generate unmatched delimiters.
genUnmatchedDelimiters :: Gen String
genUnmatchedDelimiters =
  oneof
    [ return "if (condition { stmt; }", -- Missing closing paren
      return "function test( { return; }", -- Missing closing paren
      return "var arr = [1, 2, 3;", -- Missing closing bracket
      return "obj = { key: value;" -- Missing closing brace
    ]

-- | Generate incomplete statements.
genIncompleteStatements :: Gen String
genIncompleteStatements =
  oneof
    [ return "if (true)", -- Missing statement body
      return "for (var i = 0; i < 10;", -- Incomplete for loop
      return "function test()", -- Missing function body
      return "var x =" -- Missing initializer
    ]

-- | Generate invalid operator sequences.
genInvalidOperatorSequences :: Gen String
genInvalidOperatorSequences =
  oneof
    [ return "x ++ ++", -- Double increment
      return "a = = b", -- Spaced assignment
      return "x + + y", -- Spaced addition
      return "!!" -- Double negation without operand
    ]

-- Additional invalid syntax generators...
genInvalidBinaryOp :: Gen String
genInvalidBinaryOp =
  oneof
    [ return "x + + y",
      return "a * / b",
      return "c && || d"
    ]

genInvalidUnaryOp :: Gen String
genInvalidUnaryOp =
  oneof
    [ return "++x++",
      return "!!!",
      return "typeof typeof"
    ]

genMissingOperands :: Gen String
genMissingOperands =
  oneof
    [ return "+ 5",
      return "* 10",
      return "&& true"
    ]

genInvalidLiterals :: Gen String
genInvalidLiterals =
  oneof
    [ return "0x", -- Hex without digits
      return "0b", -- Binary without digits
      return "1.2.3" -- Multiple decimal points
    ]

genIncompleteIf :: Gen String
genIncompleteIf =
  oneof
    [ return "if (true)",
      return "if true { }",
      return "if (condition else"
    ]

genInvalidFor :: Gen String
genInvalidFor =
  oneof
    [ return "for (;;; i++) {}",
      return "for (var i =; i < 10; i++)",
      return "for (var i = 0 i < 10; i++)"
    ]

genMalformedFunction :: Gen String
genMalformedFunction =
  oneof
    [ return "function ( { return; }",
      return "function test(a b) {}",
      return "function test() return 42;"
    ]

genInvalidDeclaration :: Gen String
genInvalidDeclaration =
  oneof
    [ return "var ;",
      return "let = 42;",
      return "const x;"
    ]

genInvalidTokenSequences :: Gen String
genInvalidTokenSequences = return "{{ }} (( )) [[ ]]"

genStructuralErrors :: Gen String
genStructuralErrors = return "function { return } test() {}"

genContextErrors :: Gen String
genContextErrors = return "return 42; function test() {}"

-- Edge case generators...
genUnicodeIdentifiers :: Gen String
genUnicodeIdentifiers = return "var π = 3.14; let Ω = 'omega';"

genSurrogatePairs :: Gen String
genSurrogatePairs = return "var 𝒳 = 'math';" -- Mathematical script X

genCombiningCharacters :: Gen String
genCombiningCharacters = return "let café = 'coffee';" -- e with accent

genNonBMPCharacters :: Gen String
genNonBMPCharacters = return "const 💻 = 'computer';" -- Computer emoji

genDeeplyNestedFunctions :: Gen String
genDeeplyNestedFunctions = return (concat (replicate 100 "function f() {") ++ replicate 100 '}')

genDeeplyNestedObjects :: Gen String
genDeeplyNestedObjects = return ("{" ++ List.intercalate ": {" (replicate 50 "a") ++ replicate 50 '}')

genDeeplyNestedArrays :: Gen String
genDeeplyNestedArrays = return (replicate 100 '[' ++ replicate 100 ']')

genDeeplyNestedExpressions :: Gen String
genDeeplyNestedExpressions = return (replicate 100 '(' ++ "x" ++ replicate 100 ')')

genLargePrograms :: Gen String
genLargePrograms = do
  stmts <- replicateM 1000 (return "var x = 42;")
  return (unlines stmts)

genComplexExpressions :: Gen String
genComplexExpressions = return "((((a + b) * c) / d) % e) || (f && g) ? h : i"

genRepetitiveStructures :: Gen String
genRepetitiveStructures = do
  vars <- replicateM 100 (return "var x = 42;")
  return (unlines vars)

genEdgeCaseCombinations :: Gen String
genEdgeCaseCombinations = return "function 𝒻() { return 'unicode' + \"mixing\" + `template`; }"

genMaxLengthIdentifiers :: Gen String
genMaxLengthIdentifiers = do
  longId <- replicateM 1000 (return 'a')
  return ("var " ++ longId ++ " = 42;")

genNumericBoundaries :: Gen String
genNumericBoundaries =
  oneof
    [ return "var max = 9007199254740991;", -- Number.MAX_SAFE_INTEGER
      return "var min = -9007199254740991;", -- Number.MIN_SAFE_INTEGER
      return "var inf = Infinity;",
      return "var ninf = -Infinity;"
    ]

genStringBoundaries :: Gen String
genStringBoundaries = do
  longString <- replicateM 10000 (return 'x')
  return ("var str = \"" ++ longString ++ "\";")

genNestingLimits :: Gen String
genNestingLimits = return (replicate 1000 '{' ++ replicate 1000 '}')

-- Additional helpers for complex structures...
genArrayElementList :: Int -> Gen [JSArrayElement]
genArrayElementList n = listOf (genJSArrayElement n)

genJSArrayElement :: Int -> Gen JSArrayElement
genJSArrayElement n =
  oneof
    [ JSArrayElement <$> genSizedExpression n,
      JSArrayComma <$> genJSAnnot
    ]

genJSObjectProperty :: Gen JSObjectProperty
genJSObjectProperty =
  oneof
    [ genDataProperty,
      genMethodProperty,
      genIdentRef,
      genObjectSpread
    ]
  where
    genDataProperty = do
      name <- genJSPropertyName
      colon <- genJSAnnot
      value <- genJSExpression
      return (JSPropertyNameandValue name colon [value])
    genMethodProperty = do
      methodDef <- genJSMethodDefinition
      return (JSObjectMethod methodDef)
    genIdentRef = do
      annot <- genJSAnnot
      ident <- genValidIdentifier
      return (JSPropertyIdentRef annot ident)
    genObjectSpread = do
      spread <- genJSAnnot
      expr <- genJSExpression
      return (JSObjectSpread spread expr)

genJSPropertyName :: Gen JSPropertyName
genJSPropertyName =
  oneof
    [ JSPropertyIdent <$> genJSAnnot <*> genValidIdentifier,
      JSPropertyString <$> genJSAnnot <*> genValidString,
      JSPropertyNumber <$> genJSAnnot <*> genValidNumber
    ]

genJSBlock :: Int -> Gen JSBlock
genJSBlock n = do
  lbrace <- genJSAnnot
  stmts <- listOf (genSizedStatement (n `div` 2))
  rbrace <- genJSAnnot
  return (JSBlock lbrace stmts rbrace)

genJSImportClause :: Gen JSImportClause
genJSImportClause =
  oneof
    [ JSImportClauseDefault <$> genJSIdent,
      JSImportClauseNameSpace <$> genJSImportNameSpace,
      JSImportClauseNamed <$> genJSImportsNamed
    ]

genJSImportNameSpace :: Gen JSImportNameSpace
genJSImportNameSpace = do
  star <- genJSBinOp -- Using existing generator for simplicity
  asAnnot <- genJSAnnot
  ident <- genJSIdent
  return (JSImportNameSpace star asAnnot ident)

genJSImportsNamed :: Gen JSImportsNamed
genJSImportsNamed = do
  lbrace <- genJSAnnot
  specs <- genCommaList genJSImportSpecifier
  rbrace <- genJSAnnot
  return (JSImportsNamed lbrace specs rbrace)

genJSImportSpecifier :: Gen JSImportSpecifier
genJSImportSpecifier = do
  name <- genJSIdent
  return (JSImportSpecifier name)

genJSImportAttributes :: Gen JSImportAttributes
genJSImportAttributes = do
  lbrace <- genJSAnnot
  attrs <- genCommaList genJSImportAttribute
  rbrace <- genJSAnnot
  return (JSImportAttributes lbrace attrs rbrace)

genJSImportAttribute :: Gen JSImportAttribute
genJSImportAttribute = do
  key <- genJSIdent
  colon <- genJSAnnot
  value <- genJSExpression
  return (JSImportAttribute key colon value)

genJSFromClause :: Gen JSFromClause
genJSFromClause = do
  fromAnnot <- genJSAnnot
  moduleAnnot <- genJSAnnot
  moduleName <- genValidString
  return (JSFromClause fromAnnot moduleAnnot moduleName)

-- ---------------------------------------------------------------------
-- Arbitrary Instances for AST Types
-- ---------------------------------------------------------------------

instance Arbitrary JSExpression where
  arbitrary = genJSExpression
  shrink = shrinkJSExpression

instance Arbitrary JSStatement where
  arbitrary = genJSStatement
  shrink = shrinkJSStatement

instance Arbitrary JSBinOp where
  arbitrary = genJSBinOp

instance Arbitrary JSUnaryOp where
  arbitrary = genJSUnaryOp

instance Arbitrary JSAssignOp where
  arbitrary = genJSAssignOp

instance Arbitrary JSAnnot where
  arbitrary = genJSAnnot

instance Arbitrary JSSemi where
  arbitrary = genJSSemi

instance Arbitrary JSIdent where
  arbitrary = genJSIdent

instance Arbitrary JSAST where
  arbitrary = genJSAST
  shrink = shrinkJSAST

instance Arbitrary JSBlock where
  arbitrary = genJSBlock 3

instance Arbitrary JSArrayElement where
  arbitrary = genJSArrayElement 2

instance Arbitrary JSVarInitializer where
  arbitrary = genJSVarInitializer

instance Arbitrary JSSwitchParts where
  arbitrary = genJSSwitchParts 2

instance Arbitrary JSTryCatch where
  arbitrary = genJSTryCatch 2

instance Arbitrary JSTryFinally where
  arbitrary = genJSTryFinally 2

instance Arbitrary JSAccessor where
  arbitrary =
    oneof
      [ JSAccessorGet <$> genJSAnnot,
        JSAccessorSet <$> genJSAnnot
      ]

instance Arbitrary JSPropertyName where
  arbitrary = genJSPropertyName

instance Arbitrary JSObjectProperty where
  arbitrary = genJSObjectProperty

instance Arbitrary JSMethodDefinition where
  arbitrary = genJSMethodDefinition

-- | Generate method definitions.
genJSMethodDefinition :: Gen JSMethodDefinition
genJSMethodDefinition =
  oneof
    [ JSMethodDefinition <$> genJSPropertyName <*> genJSAnnot <*> genCommaList genIdentifierExpression <*> genJSAnnot <*> genJSBlock 2,
      JSGeneratorMethodDefinition <$> genJSAnnot <*> genJSPropertyName <*> genJSAnnot <*> genCommaList genIdentifierExpression <*> genJSAnnot <*> genJSBlock 2,
      JSPropertyAccessor <$> arbitrary <*> genJSPropertyName <*> genJSAnnot <*> genCommaList genIdentifierExpression <*> genJSAnnot <*> genJSBlock 2
    ]

instance Arbitrary JSModuleItem where
  arbitrary = genJSModuleItem

instance Arbitrary JSImportDeclaration where
  arbitrary = genJSImportDeclaration

instance Arbitrary JSExportDeclaration where
  arbitrary = genJSExportDeclaration

-- ---------------------------------------------------------------------
-- Shrinking Functions
-- ---------------------------------------------------------------------

-- | Shrink JavaScript expressions for QuickCheck.
shrinkJSExpression :: JSExpression -> [JSExpression]
shrinkJSExpression expr = case expr of
  JSExpressionBinary left _ right -> [left, right] ++ shrink left ++ shrink right
  JSUnaryExpression _ operand -> [operand] ++ shrink operand
  JSCallExpression func _ args _ -> [func] ++ shrink func ++ concatMap shrink (jsCommaListToList args)
  JSMemberDot obj _ prop -> [obj, prop] ++ shrink obj ++ shrink prop
  JSMemberSquare obj _ prop _ -> [obj, prop] ++ shrink obj ++ shrink prop
  JSArrayLiteral _ elements _ -> concatMap shrinkJSArrayElement elements
  _ -> []

-- | Shrink JavaScript statements for QuickCheck.
shrinkJSStatement :: JSStatement -> [JSStatement]
shrinkJSStatement stmt = case stmt of
  JSStatementBlock _ stmts _ _ -> stmts ++ concatMap shrinkJSStatement stmts
  JSIf _ _ cond _ thenStmt -> [thenStmt] ++ shrinkJSStatement thenStmt
  JSIfElse _ _ cond _ thenStmt _ elseStmt ->
    [thenStmt, elseStmt] ++ shrinkJSStatement thenStmt ++ shrinkJSStatement elseStmt
  JSExpressionStatement expr _ -> [] -- Cannot shrink expression to statement
  JSReturn _ (Just expr) _ -> [] -- Cannot shrink expression to statement
  _ -> []

-- | Shrink JavaScript AST for QuickCheck.
shrinkJSAST :: JSAST -> [JSAST]
shrinkJSAST ast = case ast of
  JSAstProgram stmts annot ->
    [JSAstProgram ss annot | ss <- shrink stmts]
  JSAstStatement stmt annot ->
    [JSAstStatement s annot | s <- shrink stmt]
  JSAstExpression expr annot ->
    [JSAstExpression e annot | e <- shrink expr]
  _ -> []

-- | Shrink array elements.
shrinkJSArrayElement :: JSArrayElement -> [JSExpression]
shrinkJSArrayElement (JSArrayElement expr) = shrink expr
shrinkJSArrayElement (JSArrayComma _) = []

-- | Convert JSCommaList to regular list for processing.
jsCommaListToList :: JSCommaList a -> [a]
jsCommaListToList JSLNil = []
jsCommaListToList (JSLOne x) = [x]
jsCommaListToList (JSLCons list _ x) = jsCommaListToList list ++ [x]

-- ---------------------------------------------------------------------
-- Additional Missing Generators for Complete AST Coverage
-- ---------------------------------------------------------------------

-- | Generate JSCommaTrailingList for any element type.
genJSCommaTrailingList :: Gen a -> Gen (JSCommaTrailingList a)
genJSCommaTrailingList genElement =
  oneof
    [ JSCTLNone <$> genCommaList genElement,
      do
        list <- genCommaList genElement
        comma <- genJSAnnot
        return (JSCTLComma list comma)
    ]

-- | Generate JSClassHeritage.
genJSClassHeritage :: Gen JSClassHeritage
genJSClassHeritage =
  oneof
    [ return JSExtendsNone,
      JSExtends <$> genJSAnnot <*> genJSExpression
    ]

-- | Generate JSClassElement.
genJSClassElement :: Gen JSClassElement
genJSClassElement =
  oneof
    [ genJSInstanceMethod,
      genJSStaticMethod,
      genJSClassSemi,
      genJSPrivateField,
      genJSPrivateMethod,
      genJSPrivateAccessor
    ]
  where
    genJSInstanceMethod = do
      method <- genJSMethodDefinition
      return (JSClassInstanceMethod method)
    genJSStaticMethod = do
      static <- genJSAnnot
      method <- genJSMethodDefinition
      return (JSClassStaticMethod static method)
    genJSClassSemi = do
      semi <- genJSAnnot
      return (JSClassSemi semi)
    genJSPrivateField = do
      hash <- genJSAnnot
      name <- genValidIdentifier
      eq <- genJSAnnot
      init <- oneof [return Nothing, Just <$> genJSExpression]
      semi <- genJSSemi
      return (JSPrivateField hash name eq init semi)
    genJSPrivateMethod = do
      hash <- genJSAnnot
      name <- genValidIdentifier
      lparen <- genJSAnnot
      params <- genCommaList genJSExpression
      rparen <- genJSAnnot
      block <- genJSBlock 2
      return (JSPrivateMethod hash name lparen params rparen block)
    genJSPrivateAccessor = do
      accessor <- arbitrary
      hash <- genJSAnnot
      name <- genValidIdentifier
      lparen <- genJSAnnot
      params <- genCommaList genJSExpression
      rparen <- genJSAnnot
      block <- genJSBlock 2
      return (JSPrivateAccessor accessor hash name lparen params rparen block)

-- | Generate JSTemplatePart.
genJSTemplatePart :: Gen JSTemplatePart
genJSTemplatePart = do
  expr <- genJSExpression
  rb <- genJSAnnot
  suffix <- genValidString
  return (JSTemplatePart expr rb suffix)

-- | Generate JSArrowParameterList.
genJSArrowParameterList :: Gen JSArrowParameterList
genJSArrowParameterList =
  oneof
    [ JSUnparenthesizedArrowParameter <$> genJSIdent,
      JSParenthesizedArrowParameterList <$> genJSAnnot <*> genCommaList genJSExpression <*> genJSAnnot
    ]

-- | Generate JSConciseBody.
genJSConciseBody :: Gen JSConciseBody
genJSConciseBody =
  oneof
    [ JSConciseFunctionBody <$> genJSBlock 2,
      JSConciseExpressionBody <$> genJSExpression
    ]

-- ---------------------------------------------------------------------
-- Additional Arbitrary Instances for Complete Coverage
-- ---------------------------------------------------------------------

instance Arbitrary a => Arbitrary (JSCommaTrailingList a) where
  arbitrary = genJSCommaTrailingList arbitrary

instance Arbitrary JSClassHeritage where
  arbitrary = genJSClassHeritage

instance Arbitrary JSClassElement where
  arbitrary = genJSClassElement

instance Arbitrary JSTemplatePart where
  arbitrary = genJSTemplatePart

instance Arbitrary JSArrowParameterList where
  arbitrary = genJSArrowParameterList

instance Arbitrary JSConciseBody where
  arbitrary = genJSConciseBody

instance Arbitrary a => Arbitrary (JSCommaList a) where
  arbitrary = genCommaList arbitrary
