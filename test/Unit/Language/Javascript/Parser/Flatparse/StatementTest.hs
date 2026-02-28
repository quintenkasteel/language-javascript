{-# LANGUAGE OverloadedStrings #-}

-- | Comprehensive unit tests for flatparse Statement parsing.
--
-- This module provides thorough testing of all JavaScript statement types
-- parsed by the flatparse Statement module. Tests cover:
--
--   * Basic statements (expression, block, empty)
--   * Variable declarations (var, let, const)
--   * Control flow statements (if/else, loops, switch)
--   * Jump statements (return, break, continue, throw)
--   * Function and class declarations
--   * Exception handling (try/catch/finally)
--   * Error cases and edge conditions
--
-- All tests use actual parsing rather than mock functions to ensure
-- real functionality is validated.
module Unit.Language.Javascript.Parser.Flatparse.StatementTest
  ( testStatementParsing,
  )
where

import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.Grammar
import Test.Hspec
import FlatParse.Basic


-- | Test helper to parse statements
parseStatement :: Text -> Either String JSStatement
parseStatement input =
  case runParser statement (Text.encodeUtf8 input) of
    OK result _ -> Right result
    Fail -> Left "Parse failed"
    Err e -> Left ("Parse error: " <> show e)

-- | Test helper to parse statement lists
_parseStatements :: Text -> Either String [JSStatement]
_parseStatements input =
  case runParser statementList (Text.encodeUtf8 input) of
    OK result _ -> Right result
    Fail -> Left "Parse failed"
    Err e -> Left ("Parse error: " <> show e)

-- | Main test specification for statement parsing
testStatementParsing :: Spec
testStatementParsing = describe "Flatparse Statement Parser Tests" $ do

  describe "Basic Statement Parsing" $ do
    testBasicStatements
    testBlockStatements
    testEmptyStatements

  describe "Variable Declaration Parsing" $ do
    testVariableDeclarations
    testVariableDeclarators

  describe "Control Flow Statement Parsing" $ do
    testIfStatements
    testLoopStatements
    testSwitchStatements

  describe "Jump Statement Parsing" $ do
    testJumpStatements

  describe "Function Declaration Parsing" $ do
    testFunctionDeclarations

  describe "Class Declaration Parsing" $ do
    testClassDeclarations

  describe "Exception Handling Parsing" $ do
    testTryStatements

  describe "Error Cases" $ do
    testErrorConditions

-- | Test basic statement types
testBasicStatements :: Spec
testBasicStatements = describe "expression statements" $ do
  it "parses simple expression statements" $ do
    case parseStatement "x;" of
      Right (JSExpressionStatement (JSIdentifier _ "x") _) ->
        pure ()
      _ -> expectationFailure "Expected expression statement with identifier"

  it "parses assignment expression statements" $ do
    case parseStatement "x = 42;" of
      Right (JSAssignStatement _ (JSAssign _) _ _) ->
        pure ()
      _ -> expectationFailure "Expected assignment expression statement"

  it "parses function call statements" $ do
    case parseStatement "console.log('hello');" of
      Right (JSMethodCall _ _ _ _ _) ->
        pure ()
      _ -> expectationFailure "Expected function call statement"

-- | Test block statements
testBlockStatements :: Spec
testBlockStatements = describe "block statements" $ do
  it "parses empty blocks" $ do
    case parseStatement "{}" of
      Right (JSStatementBlock _ stmts _ _) ->
        length stmts `shouldBe` 0
      _ -> expectationFailure "Expected empty block statement"

  it "parses blocks with single statement" $ do
    case parseStatement "{ x = 1; }" of
      Right (JSStatementBlock _ stmts _ _) ->
        length stmts `shouldBe` 1
      _ -> expectationFailure "Expected block with one statement"

  it "parses blocks with multiple statements" $ do
    case parseStatement "{ x = 1; y = 2; z = 3; }" of
      Right (JSStatementBlock _ stmts _ _) ->
        length stmts `shouldBe` 3
      _ -> expectationFailure "Expected block with three statements"

  it "parses nested blocks" $ do
    case parseStatement "{ { x = 1; } }" of
      Right (JSStatementBlock _ outerStmts _ _) -> do
        length outerStmts `shouldBe` 1
        case outerStmts of
          [JSStatementBlock _ innerStmts _ _] ->
            length innerStmts `shouldBe` 1
          _ -> expectationFailure "Expected nested block"
      _ -> expectationFailure "Expected outer block statement"

-- | Test empty statements
testEmptyStatements :: Spec
testEmptyStatements = describe "empty statements" $ do
  it "parses single semicolon" $ do
    case parseStatement ";" of
      Right (JSEmptyStatement _) ->
        pure ()
      _ -> expectationFailure "Expected empty statement"

-- | Test variable declarations
testVariableDeclarations :: Spec
testVariableDeclarations = describe "variable declarations" $ do
  it "parses var declarations" $ do
    case parseStatement "var x;" of
      Right (JSVariable _ decls _) ->
        length (fromCommaList decls) `shouldBe` 1
      _ -> expectationFailure "Expected var declaration"

  it "parses let declarations" $ do
    case parseStatement "let y = 42;" of
      Right (JSLet _ decls _) ->
        length (fromCommaList decls) `shouldBe` 1
      _ -> expectationFailure "Expected let declaration"

  it "parses const declarations" $ do
    case parseStatement "const z = 'hello';" of
      Right (JSConstant _ decls _) ->
        length (fromCommaList decls) `shouldBe` 1
      _ -> expectationFailure "Expected const declaration"

  it "parses multiple declarators" $ do
    case parseStatement "var a = 1, b = 2, c;" of
      Right (JSVariable _ decls _) ->
        length (fromCommaList decls) `shouldBe` 3
      _ -> expectationFailure "Expected declaration with three declarators"

-- | Test variable declarators
testVariableDeclarators :: Spec
testVariableDeclarators = describe "variable declarators" $ do
  it "parses declarator without initializer" $ do
    case parseStatement "var x;" of
      Right (JSVariable _ decls _) ->
        case fromCommaList decls of
          [JSVarInitExpression (JSIdentifier _ "x") JSVarInitNone] ->
            pure ()
          _ -> expectationFailure "Expected declarator without initializer"
      _ -> expectationFailure "Expected variable declaration"

  it "parses declarator with initializer" $ do
    case parseStatement "var x = 42;" of
      Right (JSVariable _ decls _) ->
        case fromCommaList decls of
          [JSVarInitExpression (JSIdentifier _ "x") (JSVarInit _ _)] ->
            pure ()
          _ -> expectationFailure "Expected declarator with initializer"
      _ -> expectationFailure "Expected variable declaration"

-- | Test if statements
testIfStatements :: Spec
testIfStatements = describe "if statements" $ do
  it "parses if without else" $ do
    case parseStatement "if (true) x = 1;" of
      Right (JSIf _ _ _ _ _) ->
        pure ()
      _ -> expectationFailure "Expected if statement without else"

  it "parses if with else" $ do
    case parseStatement "if (true) x = 1; else y = 2;" of
      Right (JSIfElse _ _ _ _ _ _ _) ->
        pure ()
      _ -> expectationFailure "Expected if statement with else"

  it "parses nested if statements" $ do
    case parseStatement "if (a) if (b) x = 1; else y = 2;" of
      Right (JSIf _ _ _ _ (JSIfElse _ _ _ _ _ _ _)) ->
        pure ()
      _ -> expectationFailure "Expected nested if statements"

-- | Test loop statements
testLoopStatements :: Spec
testLoopStatements = describe "loop statements" $ do
  it "parses while loops" $ do
    case parseStatement "while (true) x++;" of
      Right (JSWhile _ _ _ _ _) ->
        pure ()
      _ -> expectationFailure "Expected while statement"

  it "parses for loops with all parts" $ do
    case parseStatement "for (var i = 0; i < 10; i++) x++;" of
      Right (JSForVar _ _ _ _ _ _ _ _ _ _) ->
        pure ()
      _ -> expectationFailure "Expected complete for statement"

  it "parses for loops with missing parts" $ do
    case parseStatement "for (;;) x++;" of
      Right (JSFor _ _ _ _ _ _ _ _ _) ->
        pure ()
      _ -> expectationFailure "Expected for statement with missing parts"

-- | Test switch statements
testSwitchStatements :: Spec
testSwitchStatements = describe "switch statements" $ do
  it "parses basic switch statements" $ do
    case parseStatement "switch (x) {}" of
      Right (JSSwitch _ _ _ _ _ _ _ _) ->
        pure ()
      _ -> expectationFailure "Expected switch statement"

-- | Test jump statements
testJumpStatements :: Spec
testJumpStatements = describe "jump statements" $ do
  it "parses return without value" $ do
    case parseStatement "return;" of
      Right (JSReturn _ Nothing _) ->
        pure ()
      _ -> expectationFailure "Expected return without value"

  it "parses return with value" $ do
    case parseStatement "return 42;" of
      Right (JSReturn _ (Just _) _) ->
        pure ()
      _ -> expectationFailure "Expected return with value"

  it "parses break without label" $ do
    case parseStatement "break;" of
      Right (JSBreak _ JSIdentNone _) ->
        pure ()
      _ -> expectationFailure "Expected break without label"

  it "parses break with label" $ do
    case parseStatement "break loop;" of
      Right (JSBreak _ (JSIdentName _ "loop") _) ->
        pure ()
      _ -> expectationFailure "Expected break with label"

  it "parses continue without label" $ do
    case parseStatement "continue;" of
      Right (JSContinue _ JSIdentNone _) ->
        pure ()
      _ -> expectationFailure "Expected continue without label"

  it "parses continue with label" $ do
    case parseStatement "continue loop;" of
      Right (JSContinue _ (JSIdentName _ "loop") _) ->
        pure ()
      _ -> expectationFailure "Expected continue with label"

  it "parses throw statements" $ do
    case parseStatement "throw new Error('test');" of
      Right (JSThrow _ _ _) ->
        pure ()
      _ -> expectationFailure "Expected throw statement"

-- | Test function declarations
testFunctionDeclarations :: Spec
testFunctionDeclarations = describe "function declarations" $ do
  it "parses function without parameters" $ do
    case parseStatement "function foo() {}" of
      Right (JSFunction _ (JSIdentName _ "foo") _ params _ _ _) ->
        length (fromCommaList params) `shouldBe` 0
      _ -> expectationFailure "Expected function without parameters"

  it "parses function with parameters" $ do
    case parseStatement "function add(a, b) { return a + b; }" of
      Right (JSFunction _ (JSIdentName _ "add") _ params _ _ _) ->
        length (fromCommaList params) `shouldBe` 2
      _ -> expectationFailure "Expected function with parameters"

  it "parses function with default parameters" $ do
    case parseStatement "function greet(name = 'World') {}" of
      Right (JSFunction _ (JSIdentName _ "greet") _ params _ _ _) -> do
        length (fromCommaList params) `shouldBe` 1
        case fromCommaList params of
          [JSAssignExpression (JSIdentifier _ "name") (JSAssign _) _] -> pure ()
          _ -> expectationFailure "Expected default parameter"
      _ -> expectationFailure "Expected function with default parameter"

-- | Test class declarations
testClassDeclarations :: Spec
testClassDeclarations = describe "class declarations" $ do
  it "parses class without superclass" $ do
    case parseStatement "class Foo {}" of
      Right (JSClass _ (JSIdentName _ "Foo") JSExtendsNone _ _ _ _) ->
        pure ()
      _ -> expectationFailure "Expected class without superclass"

  it "parses class with superclass" $ do
    case parseStatement "class Foo extends Bar {}" of
      Right (JSClass _ (JSIdentName _ "Foo") (JSExtends _ _) _ _ _ _) ->
        pure ()
      _ -> expectationFailure "Expected class with superclass"

  it "parses class with constructor" $ do
    case parseStatement "class Foo { constructor() {} }" of
      Right (JSClass _ (JSIdentName _ "Foo") _ _ methods _ _) -> do
        length methods `shouldBe` 1
        case methods of
          [JSClassInstanceMethod _] -> pure ()
          _ -> expectationFailure "Expected constructor method"
      _ -> expectationFailure "Expected class with constructor"

  it "parses class with methods" $ do
    case parseStatement "class Foo { bar() {} baz() {} }" of
      Right (JSClass _ (JSIdentName _ "Foo") _ _ methods _ _) ->
        length methods `shouldBe` 2
      _ -> expectationFailure "Expected class with methods"

-- | Test try statements
testTryStatements :: Spec
testTryStatements = describe "try statements" $ do
  it "parses try with catch" $ do
    case parseStatement "try {} catch (e) {}" of
      Right (JSTry _ _ [_] JSNoFinally) ->
        pure ()
      _ -> expectationFailure "Expected try with catch"

  it "parses try with finally" $ do
    case parseStatement "try {} finally {}" of
      Right (JSTry _ _ [] (JSFinally _ _)) ->
        pure ()
      _ -> expectationFailure "Expected try with finally"

  it "parses try with catch and finally" $ do
    case parseStatement "try {} catch (e) {} finally {}" of
      Right (JSTry _ _ [_] (JSFinally _ _)) ->
        pure ()
      _ -> expectationFailure "Expected try with catch and finally"

-- | Test error conditions and edge cases
testErrorConditions :: Spec
testErrorConditions = describe "error handling" $ do
  it "handles incomplete statements gracefully" $ do
    case parseStatement "if" of
      Left _ -> pure ()  -- Expected failure
      Right _ -> expectationFailure "Should fail on incomplete if statement"

  it "handles invalid syntax gracefully" $ do
    case parseStatement "var 123 = x;" of
      Left _ -> pure ()  -- Expected failure
      Right _ -> expectationFailure "Should fail on invalid variable name"

  it "handles unexpected tokens gracefully" $ do
    case parseStatement "function function" of
      Left _ -> pure ()  -- Expected failure
      Right _ -> expectationFailure "Should fail on unexpected tokens"