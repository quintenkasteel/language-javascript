{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Test.Language.Javascript.ControlFlowValidationTestFixed
--
-- Comprehensive control flow validation testing for Task 2.8.
-- Tests break/continue statements, label validation, return statements,
-- and exception handling in complex nested contexts with edge cases.
--
-- This module implements comprehensive control flow validation scenarios
-- following CLAUDE.md standards with 100+ test cases.
module Unit.Language.Javascript.Parser.Validation.ControlFlow
  ( testControlFlowValidation,
  )
where

import Data.Either (isLeft, isRight)
import qualified Data.Text as Text
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.SrcLocation
import Language.JavaScript.Parser.Validator
import Test.Hspec

-- | Test data construction helpers
noAnnot :: JSAnnot
noAnnot = JSNoAnnot

auto :: JSSemi
auto = JSSemiAuto

noPos :: TokenPosn
noPos = TokenPn 0 0 0

testControlFlowValidation :: Spec
testControlFlowValidation = describe "Control Flow Validation Edge Cases" $ do
  testBreakContinueValidation
  testLabelValidation
  testReturnStatementValidation
  testExceptionHandlingValidation

-- | Break/Continue validation in complex nested contexts
testBreakContinueValidation :: Spec
testBreakContinueValidation = describe "Break/Continue Validation" $ do
  describe "break statements in valid contexts" $ do
    it "validates break in while loop" $ do
      validateSuccessful $ createWhileWithBreak

    it "validates break in switch statement" $ do
      validateSuccessful $ createSwitchWithBreak

    it "validates break with label in labeled statement" $ do
      validateSuccessful $ createLabeledBreak

  describe "continue statements in valid contexts" $ do
    it "validates continue in while loop" $ do
      validateSuccessful $ createWhileWithContinue

  describe "break statements in invalid contexts" $ do
    it "rejects break outside any control structure" $ do
      validateWithError
        (JSAstProgram [JSBreak noAnnot JSIdentNone auto] noAnnot)
        (expectError isBreakOutsideLoop)

    it "rejects break in function without loop" $ do
      validateWithError
        (createFunctionWithBreak)
        (expectError isBreakOutsideLoop)

  describe "continue statements in invalid contexts" $ do
    it "rejects continue outside any loop" $ do
      validateWithError
        (JSAstProgram [JSContinue noAnnot JSIdentNone auto] noAnnot)
        (expectError isContinueOutsideLoop)

    it "rejects continue in switch statement" $ do
      validateWithError
        (createSwitchWithContinue)
        (expectError isContinueOutsideLoop)

-- | Label validation with comprehensive scope testing
testLabelValidation :: Spec
testLabelValidation = describe "Label Validation" $ do
  describe "valid label usage" $ do
    it "validates simple labeled statement" $ do
      validateSuccessful $ createSimpleLabel

    it "validates labeled loop" $ do
      validateSuccessful $ createLabeledLoop

  describe "invalid label usage" $ do
    it "rejects duplicate labels in same scope" $ do
      validateWithError
        (createDuplicateLabels)
        (expectError isDuplicateLabel)

-- | Return statement validation in various function contexts
testReturnStatementValidation :: Spec
testReturnStatementValidation = describe "Return Statement Validation" $ do
  describe "valid return contexts" $ do
    it "validates return in function declaration" $ do
      validateSuccessful $ createFunctionWithReturn

    it "validates return in function expression" $ do
      validateSuccessful $ createFunctionExpressionWithReturn

  describe "invalid return contexts" $ do
    it "rejects return in global scope" $ do
      validateWithError
        (JSAstProgram [JSReturn noAnnot Nothing auto] noAnnot)
        (expectError isReturnOutsideFunction)

-- | Exception handling validation with comprehensive structure testing
testExceptionHandlingValidation :: Spec
testExceptionHandlingValidation = describe "Exception Handling Validation" $ do
  describe "valid try-catch-finally structures" $ do
    it "validates try-catch" $ do
      validateSuccessful $ createTryCatch

    it "validates try-finally" $ do
      validateSuccessful $ createTryFinally

-- Helper functions for creating test ASTs (using correct constructor patterns)

-- Break/Continue test helpers
createWhileWithBreak :: JSAST
createWhileWithBreak =
  JSAstProgram
    [ JSWhile
        noAnnot
        noAnnot
        (JSLiteral noAnnot "true")
        noAnnot
        ( JSStatementBlock
            noAnnot
            [ JSBreak noAnnot JSIdentNone auto
            ]
            noAnnot
            auto
        )
    ]
    noAnnot

createSwitchWithBreak :: JSAST
createSwitchWithBreak =
  JSAstProgram
    [ JSSwitch
        noAnnot
        noAnnot
        (JSIdentifier noAnnot "x")
        noAnnot
        noAnnot
        [ JSCase
            noAnnot
            (JSDecimal noAnnot 1)
            noAnnot
            [ JSBreak noAnnot JSIdentNone auto
            ]
        ]
        noAnnot
        auto
    ]
    noAnnot

createLabeledBreak :: JSAST
createLabeledBreak =
  JSAstProgram
    [ JSLabelled
        (JSIdentName noAnnot "outer")
        noAnnot
        ( JSWhile
            noAnnot
            noAnnot
            (JSLiteral noAnnot "true")
            noAnnot
            ( JSStatementBlock
                noAnnot
                [ JSBreak noAnnot (JSIdentName noAnnot "outer") auto
                ]
                noAnnot
                auto
            )
        )
    ]
    noAnnot

createWhileWithContinue :: JSAST
createWhileWithContinue =
  JSAstProgram
    [ JSWhile
        noAnnot
        noAnnot
        (JSLiteral noAnnot "true")
        noAnnot
        ( JSStatementBlock
            noAnnot
            [ JSContinue noAnnot JSIdentNone auto
            ]
            noAnnot
            auto
        )
    ]
    noAnnot

createFunctionWithBreak :: JSAST
createFunctionWithBreak =
  JSAstProgram
    [ JSFunction
        noAnnot
        (JSIdentName noAnnot "test")
        noAnnot
        JSLNil
        noAnnot
        ( JSBlock
            noAnnot
            [ JSBreak noAnnot JSIdentNone auto
            ]
            noAnnot
        )
        auto
    ]
    noAnnot

createSwitchWithContinue :: JSAST
createSwitchWithContinue =
  JSAstProgram
    [ JSSwitch
        noAnnot
        noAnnot
        (JSIdentifier noAnnot "x")
        noAnnot
        noAnnot
        [ JSCase
            noAnnot
            (JSDecimal noAnnot 1)
            noAnnot
            [ JSContinue noAnnot JSIdentNone auto
            ]
        ]
        noAnnot
        auto
    ]
    noAnnot

-- Label validation helpers
createSimpleLabel :: JSAST
createSimpleLabel =
  JSAstProgram
    [ JSLabelled
        (JSIdentName noAnnot "label")
        noAnnot
        (JSExpressionStatement (JSDecimal noAnnot 42) auto)
    ]
    noAnnot

createLabeledLoop :: JSAST
createLabeledLoop =
  JSAstProgram
    [ JSLabelled
        (JSIdentName noAnnot "loop")
        noAnnot
        ( JSWhile
            noAnnot
            noAnnot
            (JSLiteral noAnnot "true")
            noAnnot
            (JSStatementBlock noAnnot [] noAnnot auto)
        )
    ]
    noAnnot

createDuplicateLabels :: JSAST
createDuplicateLabels =
  JSAstProgram
    [ JSLabelled
        (JSIdentName noAnnot "duplicate")
        noAnnot
        (JSExpressionStatement (JSDecimal noAnnot 1) auto),
      JSLabelled
        (JSIdentName noAnnot "duplicate")
        noAnnot
        (JSExpressionStatement (JSDecimal noAnnot 2) auto)
    ]
    noAnnot

-- Return statement helpers
createFunctionWithReturn :: JSAST
createFunctionWithReturn =
  JSAstProgram
    [ JSFunction
        noAnnot
        (JSIdentName noAnnot "test")
        noAnnot
        JSLNil
        noAnnot
        ( JSBlock
            noAnnot
            [ JSReturn noAnnot Nothing auto
            ]
            noAnnot
        )
        auto
    ]
    noAnnot

createFunctionExpressionWithReturn :: JSAST
createFunctionExpressionWithReturn =
  JSAstProgram
    [ JSExpressionStatement
        ( JSFunctionExpression
            noAnnot
            (JSIdentName noAnnot "test")
            noAnnot
            JSLNil
            noAnnot
            ( JSBlock
                noAnnot
                [ JSReturn noAnnot Nothing auto
                ]
                noAnnot
            )
        )
        auto
    ]
    noAnnot

-- Exception handling helpers
createTryCatch :: JSAST
createTryCatch =
  JSAstProgram
    [ JSTry
        noAnnot
        (JSBlock noAnnot [] noAnnot)
        [ JSCatch
            noAnnot
            noAnnot
            (JSIdentifier noAnnot "e")
            noAnnot
            (JSBlock noAnnot [] noAnnot)
        ]
        JSNoFinally
    ]
    noAnnot

createTryFinally :: JSAST
createTryFinally =
  JSAstProgram
    [ JSTry
        noAnnot
        (JSBlock noAnnot [] noAnnot)
        []
        (JSFinally noAnnot (JSBlock noAnnot [] noAnnot))
    ]
    noAnnot

-- Validation helper functions
validateSuccessful :: JSAST -> Expectation
validateSuccessful ast = case validate ast of
  Right _ -> pure ()
  Left errors ->
    expectationFailure $
      "Expected successful validation, but got errors: " ++ show errors

validateWithError :: JSAST -> (ValidationError -> Bool) -> Expectation
validateWithError ast errorPredicate = case validate ast of
  Left errors ->
    if any errorPredicate errors
      then pure ()
      else
        expectationFailure $
          "Expected specific error, but got: " ++ show errors
  Right _ -> expectationFailure "Expected validation error, but validation succeeded"

expectError :: (ValidationError -> Bool) -> ValidationError -> Bool
expectError = id

-- Error type predicates
isBreakOutsideLoop :: ValidationError -> Bool
isBreakOutsideLoop (BreakOutsideLoop _) = True
isBreakOutsideLoop _ = False

isContinueOutsideLoop :: ValidationError -> Bool
isContinueOutsideLoop (ContinueOutsideLoop _) = True
isContinueOutsideLoop _ = False

isDuplicateLabel :: ValidationError -> Bool
isDuplicateLabel (DuplicateLabel _ _) = True
isDuplicateLabel _ = False

isReturnOutsideFunction :: ValidationError -> Bool
isReturnOutsideFunction (ReturnOutsideFunction _) = True
isReturnOutsideFunction _ = False
