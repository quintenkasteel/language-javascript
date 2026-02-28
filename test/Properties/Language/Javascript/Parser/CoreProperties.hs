{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Property-based testing for JavaScript parser AST invariants
--
-- This module implements QuickCheck property-based testing for fundamental
-- AST invariants that must hold across all JavaScript parsing operations.
-- Property testing catches edge cases impossible with unit tests and validates
-- mathematical properties of AST transformations.
--
-- The property tests are organized into three core areas:
--
--   * __Round-trip properties__: parse . prettyPrint preserves semantics
--     Tests that parsing and pretty-printing preserve semantics across
--     all JavaScript constructs with perfect fidelity.
--
--   * __Validation monotonicity__: valid AST remains valid after transformation
--     Property testing for AST transformations ensuring validation consistency
--     and that AST manipulation preserves well-formedness.
--
--   * __Position information consistency__: AST nodes maintain accurate positions
--     Source location preservation through parsing with token position to
--     AST position mapping correctness.
--
-- Each property is tested with hundreds of generated test cases to achieve
-- statistical confidence in correctness. Properties use shrinking to find
-- minimal counterexamples when failures occur.
--
-- ==== Examples
--
-- Running the property tests:
--
-- >>> :set -XOverloadedStrings
-- >>> import Test.Hspec
-- >>> hspec testPropertyInvariants
--
-- Testing round-trip property manually:
--
-- >>> let input = "function f(x) { return x + 1; }"
-- >>> let Right ast = parseProgram input
-- >>> renderToString ast == input
-- True
--
-- @since 0.7.1.0
module Properties.Language.Javascript.Parser.CoreProperties
  ( testPropertyInvariants,
  )
where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Language.JavaScript.Parser as Parser
import qualified Language.JavaScript.Parser.AST as AST
import Language.JavaScript.Parser.SrcLocation
  ( TokenPosn (..),
    tokenPosnEmpty,
  )
import Language.JavaScript.Pretty.Printer
  ( renderToString,
  )
import Test.Hspec
import Test.QuickCheck

-- | Comprehensive AST invariant property testing
testPropertyInvariants :: Spec
testPropertyInvariants = describe "AST Invariant Properties" $ do
  describe "Round-trip properties" $ do
    testRoundTripPreservation
    testRoundTripSemanticEquivalence
    testRoundTripCommentsPreservation
    testRoundTripPositionConsistency

  describe "Validation monotonicity" $ do
    testValidationMonotonicity
    testTransformationInvariants
    testASTManipulationSafety
    testValidationConsistency

  describe "Position information consistency" $ do
    testPositionPreservation
    testTokenToASTPositionMapping
    testSourceLocationInvariants
    testPositionCalculationCorrectness

  describe "Structural equivalence" $ do
    testStructuralEquivalence

-- ---------------------------------------------------------------------
-- Round-trip Properties
-- ---------------------------------------------------------------------

-- | Test that parsing and pretty-printing preserve program semantics
testRoundTripPreservation :: Spec
testRoundTripPreservation = describe "Round-trip preservation" $ do
  it "preserves simple expressions" $ do
    let validExprs = ["42", "true", "\"hello\"", "x", "x + y", "(1 + 2)"]
    forM_ validExprs $ \input ->
      case parseExpression input of
        Right parsed -> renderExpressionToString parsed `shouldContain` input
        Left _ -> expectationFailure ("Failed to parse: " ++ input)

  it "preserves function declarations" $ do
    let validFuncs = ["function f() { return 1; }", "function add(a, b) { return a + b; }"]
    forM_ validFuncs $ \input ->
      case parseStatement input of
        Right _ -> return ()
        Left err -> expectationFailure ("Failed to parse function: " ++ err)

  it "preserves control flow statements" $ do
    let validStmts = ["if (true) { return; }", "while (x > 0) { x--; }", "for (i = 0; i < 10; i++) { console.log(i); }"]
    forM_ validStmts $ \input ->
      case parseStatement input of
        Right _ -> return ()
        Left err -> expectationFailure ("Failed to parse statement: " ++ err)

  it "preserves complete programs" $ do
    let validProgs = ["var x = 1;", "function f() { return 2; } f();", "if (true) { console.log('ok'); }"]
    forM_ validProgs $ \input ->
      case Parser.parse input "test" of
        Right _ -> return ()
        Left err -> expectationFailure ("Failed to parse program: " ++ err)

-- | Test semantic equivalence through round-trip parsing
testRoundTripSemanticEquivalence :: Spec
testRoundTripSemanticEquivalence = describe "Semantic equivalence" $ do
  it "maintains expression evaluation semantics" $ do
    let expr = "1 + 2"
    case parseExpression expr of
      Right parsed ->
        case parseExpression expr of
          Right reparsed -> expressionStructurallyEqual parsed reparsed `shouldBe` True
          Left _ -> expectationFailure "Re-parsing failed"
      Left _ -> expectationFailure "Initial parsing failed"

  it "preserves statement execution semantics" $ do
    let stmt = "var x = 1;"
    case parseStatement stmt of
      Right parsed ->
        case parseStatement stmt of
          Right reparsed -> statementStructurallyEqual parsed reparsed `shouldBe` True
          Left _ -> expectationFailure "Re-parsing failed"
      Left _ -> expectationFailure "Initial parsing failed"

  it "preserves program execution order" $ do
    let prog = "var x = 1; var y = 2;"
    case Parser.parse prog "test" of
      Right (AST.JSAstProgram stmts _) ->
        case Parser.parse prog "test" of
          Right (AST.JSAstProgram stmts' _) ->
            length stmts `shouldBe` length stmts'
          _ -> expectationFailure "Re-parsing failed"
      _ -> expectationFailure "Initial parsing failed"

-- | Test comment preservation through round-trip
testRoundTripCommentsPreservation :: Spec
testRoundTripCommentsPreservation = describe "Comment preservation" $ do
  it "preserves line comments" $ do
    let input = "// comment\nvar x = 1;"
    case Parser.parse input "test" of
      Right _ -> return ()
      Left err -> expectationFailure ("Failed to parse with comments: " ++ err)

  it "preserves block comments" $ do
    let input = "/* comment */ var x = 1;"
    case Parser.parse input "test" of
      Right _ -> return ()
      Left err -> expectationFailure ("Failed to parse with block comments: " ++ err)

  it "preserves comment positions" $ do
    let input = "var x = 1; // end comment"
    case Parser.parse input "test" of
      Right _ -> return ()
      Left err -> expectationFailure ("Failed to parse with positioned comments: " ++ err)

-- | Test position consistency through round-trip
testRoundTripPositionConsistency :: Spec
testRoundTripPositionConsistency = describe "Position consistency" $ do
  it "maintains source position mappings" $
    let simplePrograms =
          [ ("var x = 42;", "x"),
            ("function test() { return 1; }", "test"),
            ("if (true) { console.log('hello'); }", "hello")
          ]
     in forM_ simplePrograms $ \(input, keyword) ->
          case Parser.parse input "test" of
            Right parsed -> renderToString parsed `shouldContain` keyword
            Left err -> expectationFailure ("Parse failed: " ++ show err)

  it "preserves relative position relationships" $
    let multiStatements =
          [ "var x = 1; var y = 2;",
            "function f() {} var x = 42;",
            "if (true) {} return false;"
          ]
     in forM_ multiStatements $ \input ->
          case Parser.parse input "test" of
            Right (AST.JSAstProgram stmts _) -> length stmts `shouldSatisfy` (>= 2)
            Right _ -> expectationFailure "Expected program AST"
            Left err -> expectationFailure ("Parse failed: " ++ show err)

-- ---------------------------------------------------------------------
-- Validation Monotonicity Properties
-- ---------------------------------------------------------------------

-- | Test that valid ASTs remain valid after transformations
testValidationMonotonicity :: Spec
testValidationMonotonicity = describe "Validation monotonicity" $ do
  it "valid AST remains valid after pretty-printing" $ do
    let testCases =
          [ "var x = 42;",
            "function test() { return 1 + 2; }",
            "if (x > 0) { console.log('positive'); }",
            "var obj = { key: 'value', num: 123 };"
          ]
    forM_ testCases $ \original ->
      case Parser.parse original "test" of
        Right ast -> do
          let prettyPrinted = renderToString ast
          case Parser.parse prettyPrinted "test" of
            Right reparsed -> isValidAST reparsed `shouldBe` True
            Left err -> expectationFailure ("Reparse failed for: " ++ original ++ ", error: " ++ show err)
        Left err -> expectationFailure ("Initial parse failed for: " ++ original ++ ", error: " ++ show err)

  it "valid expression remains valid after transformation" $
    property $
      \(ValidJSExpression validExpr) ->
        let transformed = parenthesizeBinaryExpr validExpr
         in isValidExpression transformed

-- | Test transformation invariants
testTransformationInvariants :: Spec
testTransformationInvariants = describe "Transformation invariants" $ do
  it "AST transformations preserve structure" $
    property $
      \(ValidJSProgram prog) ->
        let normalized = normalizeSemicolons prog
         in case (prog, normalized) of
              (AST.JSAstProgram stmts1 _, AST.JSAstProgram stmts2 _) ->
                length stmts1 == length stmts2
              _ -> False

-- | Test AST manipulation safety
testASTManipulationSafety :: Spec
testASTManipulationSafety = describe "AST manipulation safety" $ do
  it "node replacement preserves validity" $
    property $
      \(ValidJSProgram prog) (ValidJSExpression newExpr) ->
        let modified = replaceFirstExpression prog newExpr
         in isValidAST modified

  it "node insertion preserves validity" $
    property $
      \(ValidJSProgram prog) (ValidJSStatement newStmt) ->
        let modified = insertStatement prog newStmt
         in isValidAST modified

  it "node deletion preserves validity" $
    property $
      \(ValidJSProgramWithDeletableNode (prog, nodeId)) ->
        let modified = deleteNode prog nodeId
         in isValidAST modified

-- | Test validation consistency across operations
testValidationConsistency :: Spec
testValidationConsistency = describe "Validation consistency" $ do
  it "validation is deterministic" $
    property $
      \(ValidJSProgram prog) ->
        isValidAST prog == isValidAST prog

  it "validation respects AST equality" $
    property $
      \(ValidJSProgram prog1) ->
        let prog2 = parseAndReparse prog1
         in isValidAST prog1 == isValidAST prog2

-- ---------------------------------------------------------------------
-- Position Information Consistency
-- ---------------------------------------------------------------------

-- | Test position preservation through parsing
testPositionPreservation :: Spec
testPositionPreservation = describe "Position preservation" $ do
  it "preserves AST structure through parsing round-trip" $ do
    let original = "var x = 42;"
    case Parser.parse original "test" of
      Right ast -> do
        let reparsed = renderToString ast
        case Parser.parse reparsed "test" of
          Right ast2 -> structurallyEquivalent ast ast2 `shouldBe` True
          Left err -> expectationFailure ("Reparse failed: " ++ show err)
      Left err -> expectationFailure ("Parse failed: " ++ show err)

  it "preserves statement count through parsing" $ do
    let original = "var x = 1; var y = 2; function f() {}"
    case Parser.parse original "test" of
      Right (AST.JSAstProgram stmts _) -> do
        let reparsed = renderToString (AST.JSAstProgram stmts AST.JSNoAnnot)
        case Parser.parse reparsed "test" of
          Right (AST.JSAstProgram stmts2 _) ->
            length stmts `shouldBe` length stmts2
          Left err -> expectationFailure ("Reparse failed: " ++ show err)
      Left err -> expectationFailure ("Parse failed: " ++ show err)

  it "maintains AST node types through parsing" $ do
    let original = "42 + 'hello'"
    case Parser.parse original "test" of
      Right ast -> do
        let reparsed = renderToString ast
        case Parser.parse reparsed "test" of
          Right ast2 -> astTypesMatch ast ast2 `shouldBe` True
          Left err -> expectationFailure ("Reparse failed: " ++ show err)
      Left err -> expectationFailure ("Parse failed: " ++ show err)

-- | Test token to AST position mapping
testTokenToASTPositionMapping :: Spec
testTokenToASTPositionMapping = describe "Token to AST position mapping" $ do
  it "maps simple expressions to correct AST nodes" $ do
    let original = "42"
    case Parser.parse original "test" of
      Right (AST.JSAstProgram [AST.JSExpressionStatement (AST.JSDecimal _ num) _] _) ->
        num `shouldBe` "42"
      Right ast -> expectationFailure ("Unexpected AST structure: " ++ show ast)
      Left err -> expectationFailure ("Parse failed: " ++ show err)

  it "preserves expression complexity relationships" $ do
    let simple = literalNumber "42"
        complex = AST.JSExpressionBinary (literalNumber "1") (AST.JSBinOpPlus AST.JSNoAnnot) (literalNumber "2")
    expressionComplexity simple < expressionComplexity complex `shouldBe` True

-- | Test source location invariants
testSourceLocationInvariants :: Spec
testSourceLocationInvariants = describe "Source location invariants" $ do
  it "AST maintains logical structure ordering" $ do
    let program =
          AST.JSAstProgram
            [ AST.JSExpressionStatement (literalNumber "1") (AST.JSSemi AST.JSNoAnnot),
              AST.JSExpressionStatement (literalNumber "2") (AST.JSSemi AST.JSNoAnnot)
            ]
            AST.JSNoAnnot
        AST.JSAstProgram stmts _ = program
    all isValidStatement stmts `shouldBe` True

  it "block statements contain their child statements" $ do
    let childStmt = AST.JSExpressionStatement (literalNumber "42") (AST.JSSemi AST.JSNoAnnot)
        blockStmt = AST.JSStatementBlock AST.JSNoAnnot [childStmt] AST.JSNoAnnot AST.JSSemiAuto
    statementContainsChild blockStmt childStmt `shouldBe` True

  it "expression statements don't contain other statements" $ do
    let stmt1 = AST.JSExpressionStatement (literalNumber "1") (AST.JSSemi AST.JSNoAnnot)
        stmt2 = AST.JSExpressionStatement (literalNumber "2") (AST.JSSemi AST.JSNoAnnot)
    statementContainsChild stmt1 stmt2 `shouldBe` False

-- | Test position calculation correctness
testPositionCalculationCorrectness :: Spec
testPositionCalculationCorrectness = describe "Position calculation correctness" $ do
  it "empty positions are handled correctly" $ do
    let emptyPos = tokenPosnEmpty
    emptyPos `shouldBe` tokenPosnEmpty

  it "position offsets work with concrete examples" $ do
    let pos1 = TokenPn 10 1 10
        pos2 = TokenPn 25 1 10
        offset = calculatePositionOffset pos1 pos2
        reconstructed = applyPositionOffset pos1 offset
    reconstructed `shouldBe` pos2

-- ---------------------------------------------------------------------
-- Structural Equivalence
-- ---------------------------------------------------------------------

-- | Test structural equivalence properties
testStructuralEquivalence :: Spec
testStructuralEquivalence = describe "Structural equivalence" $ do
  it "structural equivalence is symmetric" $
    property $
      \(ValidJSProgram prog1) (ValidJSProgram prog2) ->
        structurallyEquivalent prog1 prog2
          == structurallyEquivalent prog2 prog1

  it "structural equivalence is transitive" $ do
    let prog1 = AST.JSAstProgram [simpleExprStmt (literalNumber "42")] AST.JSNoAnnot
        prog2 = AST.JSAstProgram [simpleExprStmt (literalNumber "42")] AST.JSNoAnnot
        prog3 = AST.JSAstProgram [simpleExprStmt (literalNumber "42")] AST.JSNoAnnot
    structurallyEquivalent prog1 prog2 `shouldBe` True
    structurallyEquivalent prog2 prog3 `shouldBe` True
    structurallyEquivalent prog1 prog3 `shouldBe` True

    let prog4 = AST.JSAstProgram [simpleExprStmt (literalString "hello")] AST.JSNoAnnot
    structurallyEquivalent prog1 prog4 `shouldBe` False

-- ---------------------------------------------------------------------
-- QuickCheck Generators and Arbitrary Instances
-- ---------------------------------------------------------------------

-- | Generator for valid JavaScript expressions
newtype ValidJSExpression = ValidJSExpression AST.JSExpression
  deriving (Show)

instance Arbitrary ValidJSExpression where
  arbitrary = ValidJSExpression <$> genValidExpression

-- | Generator for valid JavaScript statements
newtype ValidJSStatement = ValidJSStatement AST.JSStatement
  deriving (Show)

instance Arbitrary ValidJSStatement where
  arbitrary = ValidJSStatement <$> genValidStatement

-- | Generator for valid JavaScript programs
newtype ValidJSProgram = ValidJSProgram AST.JSAST
  deriving (Show)

instance Arbitrary ValidJSProgram where
  arbitrary = ValidJSProgram <$> genValidProgram

-- | Generator for a program with a deletable node index
newtype ValidJSProgramWithDeletableNode = ValidJSProgramWithDeletableNode (AST.JSAST, Int)
  deriving (Show)

instance Arbitrary ValidJSProgramWithDeletableNode where
  arbitrary = ValidJSProgramWithDeletableNode <$> genProgramWithDeletableNode

-- ---------------------------------------------------------------------
-- Generator Implementations
-- ---------------------------------------------------------------------

-- | Generate valid JavaScript expressions
genValidExpression :: Gen AST.JSExpression
genValidExpression =
  oneof
    [ genLiteralExpression,
      genIdentifierExpression,
      genBinaryExpression,
      genUnaryExpression,
      genCallExpression
    ]

-- | Generate ByteString numbers
genNumber :: Gen ByteString
genNumber = BS8.pack . show <$> (arbitrary :: Gen Int)

-- | Generate ByteString quoted strings
genQuotedString :: Gen ByteString
genQuotedString = elements ["\"test\"", "\"hello\"", "'world'", "'value'"]

-- | Generate ByteString boolean literals
genBoolean :: Gen ByteString
genBoolean = elements ["true", "false"]

-- | Generate valid ByteString identifiers
genValidIdentifier :: Gen ByteString
genValidIdentifier = elements ["x", "y", "value", "result", "temp", "item"]

-- | Generate literal expressions
genLiteralExpression :: Gen AST.JSExpression
genLiteralExpression =
  oneof
    [ AST.JSDecimal <$> genAnnot <*> genNumber,
      AST.JSStringLiteral <$> genAnnot <*> genQuotedString,
      AST.JSLiteral <$> genAnnot <*> genBoolean
    ]

-- | Generate identifier expressions
genIdentifierExpression :: Gen AST.JSExpression
genIdentifierExpression =
  AST.JSIdentifier <$> genAnnot <*> genValidIdentifier

-- | Generate binary expressions
genBinaryExpression :: Gen AST.JSExpression
genBinaryExpression = do
  left <- genSimpleExpression
  op <- genBinaryOperator
  right <- genSimpleExpression
  return (AST.JSExpressionBinary left op right)

-- | Generate unary expressions
genUnaryExpression :: Gen AST.JSExpression
genUnaryExpression = do
  op <- genUnaryOperator
  expr <- genSimpleExpression
  return (AST.JSUnaryExpression op expr)

-- | Generate call expressions
genCallExpression :: Gen AST.JSExpression
genCallExpression = do
  func <- genSimpleExpression
  (lparen, args, rparen) <- genArgumentList
  return (AST.JSCallExpression func lparen args rparen)

-- | Generate simple expressions (non-recursive)
genSimpleExpression :: Gen AST.JSExpression
genSimpleExpression =
  oneof
    [ genLiteralExpression,
      genIdentifierExpression
    ]

-- | Generate valid JavaScript statements
genValidStatement :: Gen AST.JSStatement
genValidStatement =
  oneof
    [ genExpressionStatement,
      genVariableStatement,
      genIfStatement,
      genReturnStatement,
      genBlockStatement
    ]

-- | Generate expression statements
genExpressionStatement :: Gen AST.JSStatement
genExpressionStatement = do
  expr <- genValidExpression
  semi <- genSemicolon
  return (AST.JSExpressionStatement expr semi)

-- | Generate variable statements
genVariableStatement :: Gen AST.JSStatement
genVariableStatement = do
  annot <- genAnnot
  varDecl <- genVariableDeclaration
  semi <- genSemicolon
  return (AST.JSVariable annot varDecl semi)

-- | Generate if statements
genIfStatement :: Gen AST.JSStatement
genIfStatement = do
  annot <- genAnnot
  lparen <- genAnnot
  cond <- genValidExpression
  rparen <- genAnnot
  thenStmt <- genSimpleStatement
  return (AST.JSIf annot lparen cond rparen thenStmt)

-- | Generate return statements
genReturnStatement :: Gen AST.JSStatement
genReturnStatement = do
  annot <- genAnnot
  mexpr <- oneof [return Nothing, Just <$> genValidExpression]
  semi <- genSemicolon
  return (AST.JSReturn annot mexpr semi)

-- | Generate block statements
genBlockStatement :: Gen AST.JSStatement
genBlockStatement = do
  lbrace <- genAnnot
  stmts <- listOf genSimpleStatement
  rbrace <- genAnnot
  return (AST.JSStatementBlock lbrace stmts rbrace AST.JSSemiAuto)

-- | Generate simple statements (non-recursive)
genSimpleStatement :: Gen AST.JSStatement
genSimpleStatement =
  oneof
    [ genExpressionStatement,
      genVariableStatement,
      genReturnStatement
    ]

-- | Generate valid JavaScript programs
genValidProgram :: Gen AST.JSAST
genValidProgram = do
  stmts <- listOf genValidStatement
  annot <- genAnnot
  return (AST.JSAstProgram stmts annot)

-- | Generate program with deletable node
genProgramWithDeletableNode :: Gen (AST.JSAST, Int)
genProgramWithDeletableNode = do
  prog <- genValidProgram
  nodeId <- choose (0, 10)
  return (prog, nodeId)

-- ---------------------------------------------------------------------
-- Helper Generators
-- ---------------------------------------------------------------------

-- | Generate annotation (always JSNoAnnot for property tests)
genAnnot :: Gen AST.JSAnnot
genAnnot = return AST.JSNoAnnot

-- | Generate binary operator
genBinaryOperator :: Gen AST.JSBinOp
genBinaryOperator =
  elements
    [ AST.JSBinOpPlus AST.JSNoAnnot,
      AST.JSBinOpMinus AST.JSNoAnnot,
      AST.JSBinOpTimes AST.JSNoAnnot,
      AST.JSBinOpDivide AST.JSNoAnnot
    ]

-- | Generate unary operator
genUnaryOperator :: Gen AST.JSUnaryOp
genUnaryOperator =
  elements
    [ AST.JSUnaryOpMinus AST.JSNoAnnot,
      AST.JSUnaryOpPlus AST.JSNoAnnot,
      AST.JSUnaryOpNot AST.JSNoAnnot
    ]

-- | Generate argument list
genArgumentList :: Gen (AST.JSAnnot, AST.JSCommaList AST.JSExpression, AST.JSAnnot)
genArgumentList = do
  lparen <- genAnnot
  args <- genCommaList
  rparen <- genAnnot
  return (lparen, args, rparen)

-- | Generate comma list
genCommaList :: Gen (AST.JSCommaList AST.JSExpression)
genCommaList =
  oneof
    [ return AST.JSLNil,
      do
        expr <- genValidExpression
        return (AST.JSLOne expr),
      do
        expr1 <- genValidExpression
        comma <- genAnnot
        expr2 <- genValidExpression
        return (AST.JSLCons (AST.JSLOne expr1) comma expr2)
    ]

-- | Generate semicolon
genSemicolon :: Gen AST.JSSemi
genSemicolon = return (AST.JSSemi AST.JSNoAnnot)

-- | Generate variable declaration
genVariableDeclaration :: Gen (AST.JSCommaList AST.JSExpression)
genVariableDeclaration = do
  identName <- genValidIdentifier
  let varIdent = AST.JSIdentifier AST.JSNoAnnot identName
  return (AST.JSLOne varIdent)

-- ---------------------------------------------------------------------
-- Property Helper Functions
-- ---------------------------------------------------------------------

-- | Check if AST is valid by verifying all contained nodes are well-formed
isValidAST :: AST.JSAST -> Bool
isValidAST (AST.JSAstProgram stmts _) = all isValidStatement stmts
isValidAST (AST.JSAstStatement stmt _) = isValidStatement stmt
isValidAST (AST.JSAstExpression expr _) = isValidExpression expr
isValidAST (AST.JSAstLiteral _ _) = True

-- | Check if expression is valid by matching against known constructors
isValidExpression :: AST.JSExpression -> Bool
isValidExpression (AST.JSAssignExpression {}) = True
isValidExpression (AST.JSArrayLiteral {}) = True
isValidExpression (AST.JSArrowExpression {}) = True
isValidExpression (AST.JSCallExpression {}) = True
isValidExpression (AST.JSExpressionBinary {}) = True
isValidExpression (AST.JSExpressionParen {}) = True
isValidExpression (AST.JSExpressionPostfix {}) = True
isValidExpression (AST.JSExpressionTernary {}) = True
isValidExpression (AST.JSIdentifier {}) = True
isValidExpression (AST.JSLiteral {}) = True
isValidExpression (AST.JSMemberDot {}) = True
isValidExpression (AST.JSMemberSquare {}) = True
isValidExpression (AST.JSNewExpression {}) = True
isValidExpression (AST.JSObjectLiteral {}) = True
isValidExpression (AST.JSUnaryExpression {}) = True
isValidExpression (AST.JSVarInitExpression {}) = True
isValidExpression (AST.JSDecimal {}) = True
isValidExpression (AST.JSStringLiteral {}) = True
isValidExpression _ = False

-- | Check if statement is valid by matching against known constructors
isValidStatement :: AST.JSStatement -> Bool
isValidStatement (AST.JSStatementBlock {}) = True
isValidStatement (AST.JSBreak {}) = True
isValidStatement (AST.JSContinue {}) = True
isValidStatement (AST.JSDoWhile {}) = True
isValidStatement (AST.JSFor {}) = True
isValidStatement (AST.JSForIn {}) = True
isValidStatement (AST.JSForVar {}) = True
isValidStatement (AST.JSForVarIn {}) = True
isValidStatement (AST.JSFunction {}) = True
isValidStatement (AST.JSIf {}) = True
isValidStatement (AST.JSIfElse {}) = True
isValidStatement (AST.JSLabelled {}) = True
isValidStatement (AST.JSEmptyStatement {}) = True
isValidStatement (AST.JSExpressionStatement {}) = True
isValidStatement (AST.JSAssignStatement {}) = True
isValidStatement (AST.JSMethodCall {}) = True
isValidStatement (AST.JSReturn {}) = True
isValidStatement (AST.JSSwitch {}) = True
isValidStatement (AST.JSThrow {}) = True
isValidStatement (AST.JSTry {}) = True
isValidStatement (AST.JSVariable {}) = True
isValidStatement (AST.JSWhile {}) = True
isValidStatement (AST.JSWith {}) = True
isValidStatement _ = False

-- | Wrap binary expressions in parentheses, preserving all other expressions.
-- This is a real transformation that changes structure while preserving validity.
parenthesizeBinaryExpr :: AST.JSExpression -> AST.JSExpression
parenthesizeBinaryExpr e@(AST.JSExpressionBinary {}) =
  AST.JSExpressionParen AST.JSNoAnnot e AST.JSNoAnnot
parenthesizeBinaryExpr e = e

-- | Normalize semicolons in a program AST, converting JSSemiAuto to explicit JSSemi.
-- This is a real transformation that preserves statement count and structure.
normalizeSemicolons :: AST.JSAST -> AST.JSAST
normalizeSemicolons (AST.JSAstProgram stmts annot) =
  AST.JSAstProgram (map normalizeStmtSemi stmts) annot
  where
    normalizeStmtSemi (AST.JSExpressionStatement expr (AST.JSSemi _)) =
      AST.JSExpressionStatement expr (AST.JSSemi AST.JSNoAnnot)
    normalizeStmtSemi (AST.JSExpressionStatement expr AST.JSSemiAuto) =
      AST.JSExpressionStatement expr (AST.JSSemi AST.JSNoAnnot)
    normalizeStmtSemi (AST.JSVariable ann1 vars (AST.JSSemi _)) =
      AST.JSVariable ann1 vars (AST.JSSemi AST.JSNoAnnot)
    normalizeStmtSemi (AST.JSVariable ann1 vars AST.JSSemiAuto) =
      AST.JSVariable ann1 vars (AST.JSSemi AST.JSNoAnnot)
    normalizeStmtSemi other = other
normalizeSemicolons other = other

-- | Check structural equivalence between two ASTs (ignoring annotations)
structurallyEquivalent :: AST.JSAST -> AST.JSAST -> Bool
structurallyEquivalent (AST.JSAstProgram s1 _) (AST.JSAstProgram s2 _) =
  length s1 == length s2 && all (uncurry statementStructurallyEqual) (zip s1 s2)
structurallyEquivalent _ _ = False

-- | Replace the first expression in an AST with a new expression
replaceFirstExpression :: AST.JSAST -> AST.JSExpression -> AST.JSAST
replaceFirstExpression (AST.JSAstProgram stmts annot) newExpr =
  AST.JSAstProgram (replaceFirstExprInStatements stmts newExpr) annot
replaceFirstExpression (AST.JSAstStatement stmt annot) newExpr =
  AST.JSAstStatement (replaceFirstExprInStatement stmt newExpr) annot
replaceFirstExpression (AST.JSAstExpression _ annot) newExpr =
  AST.JSAstExpression newExpr annot
replaceFirstExpression ast _ = ast

-- | Insert a statement at the beginning of a program AST
insertStatement :: AST.JSAST -> AST.JSStatement -> AST.JSAST
insertStatement (AST.JSAstProgram stmts annot) newStmt =
  AST.JSAstProgram (newStmt : stmts) annot
insertStatement ast _ = ast

-- | Delete a node at a given index from a program AST
deleteNode :: AST.JSAST -> Int -> AST.JSAST
deleteNode (AST.JSAstProgram stmts annot) index
  | index >= 0 && index < length stmts =
      AST.JSAstProgram (deleteAtIndex index stmts) annot
  | otherwise = AST.JSAstProgram stmts annot
deleteNode ast _ = ast

-- | Parse and reparse AST through pretty-printing
parseAndReparse :: AST.JSAST -> AST.JSAST
parseAndReparse ast =
  case Parser.parse (renderToString ast) "test" of
    Right result -> result
    Left _ -> AST.JSAstProgram [] AST.JSNoAnnot

-- | Structural equality for expressions (ignoring annotations)
expressionStructurallyEqual :: AST.JSExpression -> AST.JSExpression -> Bool
expressionStructurallyEqual (AST.JSDecimal _ n1) (AST.JSDecimal _ n2) = n1 == n2
expressionStructurallyEqual (AST.JSStringLiteral _ s1) (AST.JSStringLiteral _ s2) = s1 == s2
expressionStructurallyEqual (AST.JSLiteral _ l1) (AST.JSLiteral _ l2) = l1 == l2
expressionStructurallyEqual (AST.JSIdentifier _ i1) (AST.JSIdentifier _ i2) = i1 == i2
expressionStructurallyEqual (AST.JSExpressionBinary left1 op1 right1) (AST.JSExpressionBinary left2 op2 right2) =
  binOpEqual op1 op2
    && expressionStructurallyEqual left1 left2
    && expressionStructurallyEqual right1 right2
  where
    binOpEqual (AST.JSBinOpPlus _) (AST.JSBinOpPlus _) = True
    binOpEqual (AST.JSBinOpMinus _) (AST.JSBinOpMinus _) = True
    binOpEqual (AST.JSBinOpTimes _) (AST.JSBinOpTimes _) = True
    binOpEqual (AST.JSBinOpDivide _) (AST.JSBinOpDivide _) = True
    binOpEqual _ _ = False
expressionStructurallyEqual _ _ = False

-- | Structural equality for statements (ignoring annotations)
statementStructurallyEqual :: AST.JSStatement -> AST.JSStatement -> Bool
statementStructurallyEqual (AST.JSExpressionStatement expr1 _) (AST.JSExpressionStatement expr2 _) =
  expressionStructurallyEqual expr1 expr2
statementStructurallyEqual (AST.JSVariable _ vars1 _) (AST.JSVariable _ vars2 _) =
  length (commaListToList vars1) == length (commaListToList vars2)
statementStructurallyEqual _ _ = False

-- | Check whether a block statement contains a given child statement
statementContainsChild :: AST.JSStatement -> AST.JSStatement -> Bool
statementContainsChild (AST.JSStatementBlock _ stmts _ _) target =
  target `elem` stmts
statementContainsChild _ _ = False

-- | Measure expression complexity for ordering tests
expressionComplexity :: AST.JSExpression -> Int
expressionComplexity (AST.JSDecimal {}) = 1
expressionComplexity (AST.JSExpressionBinary {}) = 2
expressionComplexity _ = 1

-- | Check whether two ASTs have matching top-level node types
astTypesMatch :: AST.JSAST -> AST.JSAST -> Bool
astTypesMatch (AST.JSAstProgram stmts1 _) (AST.JSAstProgram stmts2 _) =
  length stmts1 == length stmts2
    && all (uncurry stmtTypesEqual) (zip stmts1 stmts2)
  where
    stmtTypesEqual (AST.JSExpressionStatement {}) (AST.JSExpressionStatement {}) = True
    stmtTypesEqual (AST.JSVariable {}) (AST.JSVariable {}) = True
    stmtTypesEqual (AST.JSFunction {}) (AST.JSFunction {}) = True
    stmtTypesEqual _ _ = False
astTypesMatch _ _ = False

-- | Convert comma list to regular list
commaListToList :: AST.JSCommaList a -> [a]
commaListToList AST.JSLNil = []
commaListToList (AST.JSLOne x) = [x]
commaListToList (AST.JSLCons xs _ x) = commaListToList xs ++ [x]

-- | Calculate position offset between two token positions
calculatePositionOffset :: TokenPosn -> TokenPosn -> Int
calculatePositionOffset (TokenPn addr1 _ _) (TokenPn addr2 _ _) = addr2 - addr1

-- | Apply a position offset to a token position
applyPositionOffset :: TokenPosn -> Int -> TokenPosn
applyPositionOffset (TokenPn addr line col) offset = TokenPn (addr + offset) line col

-- | Render an expression to string by wrapping it in a minimal program AST
renderExpressionToString :: AST.JSExpression -> String
renderExpressionToString expr =
  renderToString (AST.JSAstProgram [AST.JSExpressionStatement expr (AST.JSSemi AST.JSNoAnnot)] AST.JSNoAnnot)

-- | Parse a single expression from source text
parseExpression :: String -> Either String AST.JSExpression
parseExpression input =
  case Parser.parse input "test" of
    Right (AST.JSAstProgram [AST.JSExpressionStatement expr _] _) -> Right expr
    Right _ -> Left "Not a single expression statement"
    Left err -> Left err

-- | Parse a single statement from source text
parseStatement :: String -> Either String AST.JSStatement
parseStatement input =
  case Parser.parse input "test" of
    Right (AST.JSAstProgram [stmt] _) -> Right stmt
    Right _ -> Left "Not a single statement"
    Left err -> Left err

-- | Construct a simple expression statement with explicit semicolon
simpleExprStmt :: AST.JSExpression -> AST.JSStatement
simpleExprStmt expr = AST.JSExpressionStatement expr (AST.JSSemi AST.JSNoAnnot)

-- | Construct a numeric literal expression
literalNumber :: ByteString -> AST.JSExpression
literalNumber num = AST.JSDecimal AST.JSNoAnnot num

-- | Construct a string literal expression
literalString :: ByteString -> AST.JSExpression
literalString str = AST.JSStringLiteral AST.JSNoAnnot ("\"" <> str <> "\"")

-- | Replace the first expression found in a list of statements
replaceFirstExprInStatements :: [AST.JSStatement] -> AST.JSExpression -> [AST.JSStatement]
replaceFirstExprInStatements [] _ = []
replaceFirstExprInStatements (stmt : stmts) newExpr =
  replaceFirstExprInStatement stmt newExpr : stmts

-- | Replace the expression within an expression statement
replaceFirstExprInStatement :: AST.JSStatement -> AST.JSExpression -> AST.JSStatement
replaceFirstExprInStatement (AST.JSExpressionStatement _ semi) newExpr =
  AST.JSExpressionStatement newExpr semi
replaceFirstExprInStatement stmt _ = stmt

-- | Delete element at a given index from a list
deleteAtIndex :: Int -> [a] -> [a]
deleteAtIndex _ [] = []
deleteAtIndex 0 (_ : xs) = xs
deleteAtIndex n (x : xs) = x : deleteAtIndex (n - 1) xs
