{-# LANGUAGE OverloadedStrings #-}

module Test.Language.Javascript.Validator
    ( testValidator
    ) where

import Test.Hspec
import Data.Either (isLeft, isRight)

import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.Validator
import Language.JavaScript.Parser.SrcLocation (TokenPosn(..), tokenPosnEmpty)

-- Test data construction helpers
noPos :: TokenPosn
noPos = tokenPosnEmpty

noAnnot :: JSAnnot
noAnnot = JSNoAnnot

auto :: JSSemi
auto = JSSemiAuto

testValidator :: Spec
testValidator = describe "AST Validator Tests" $ do
  
  describe "strongly typed error messages" $ do
    it "provides specific error types for break outside loop" $ do
      let invalidProgram = JSAstProgram
            [ JSBreak noAnnot JSIdentNone auto
            ]
            noAnnot
      case validate invalidProgram of
        Left [BreakOutsideLoop _] -> pure ()
        _ -> expectationFailure "Expected BreakOutsideLoop error"
    
    it "provides specific error types for return outside function" $ do
      let invalidProgram = JSAstProgram
            [ JSReturn noAnnot (Just (JSDecimal noAnnot "42")) auto
            ]
            noAnnot
      case validate invalidProgram of
        Left [ReturnOutsideFunction _] -> pure ()
        _ -> expectationFailure "Expected ReturnOutsideFunction error"
    
    it "provides specific error types for await outside async" $ do
      let invalidProgram = JSAstProgram
            [ JSExpressionStatement
                (JSAwaitExpression noAnnot (JSCallExpression
                  (JSIdentifier noAnnot "fetch")
                  noAnnot
                  (JSLOne (JSStringLiteral noAnnot "url"))
                  noAnnot))
                auto
            ]
            noAnnot
      case validate invalidProgram of
        Left [AwaitOutsideAsync _] -> pure ()
        _ -> expectationFailure "Expected AwaitOutsideAsync error"
    
    it "provides specific error types for yield outside generator" $ do
      let invalidProgram = JSAstProgram
            [ JSExpressionStatement
                (JSYieldExpression noAnnot (Just (JSDecimal noAnnot "42")))
                auto
            ]
            noAnnot
      case validate invalidProgram of
        Left [YieldOutsideGenerator _] -> pure ()
        _ -> expectationFailure "Expected YieldOutsideGenerator error"
    
    it "provides specific error types for const without initializer" $ do
      let invalidProgram = JSAstProgram
            [ JSConstant noAnnot
                (JSLOne (JSVarInitExpression
                  (JSIdentifier noAnnot "x")
                  JSVarInitNone))
                auto
            ]
            noAnnot
      case validate invalidProgram of
        Left [ConstWithoutInitializer "x" _] -> pure ()
        _ -> expectationFailure "Expected ConstWithoutInitializer error"

    it "provides specific error types for invalid assignment targets" $ do
      let invalidProgram = JSAstProgram
            [ JSAssignStatement
                (JSDecimal noAnnot "42")
                (JSAssign noAnnot)
                (JSDecimal noAnnot "24")
                auto
            ]
            noAnnot
      case validate invalidProgram of
        Left [InvalidAssignmentTarget _ _] -> pure ()
        _ -> expectationFailure "Expected InvalidAssignmentTarget error"

  describe "toString functions work correctly" $ do
    it "converts BreakOutsideLoop to readable string" $ do
      let err = BreakOutsideLoop (TokenPn 0 1 1)
      errorToString err `shouldContain` "Break statement must be inside a loop"
      errorToString err `shouldContain` "at line 1, column 1"
    
    it "converts multiple errors to readable string" $ do
      let errors = [ BreakOutsideLoop (TokenPn 0 1 1)
                   , ReturnOutsideFunction (TokenPn 0 2 5)
                   ]
      let result = errorsToString errors
      result `shouldContain` "2 error(s)"
      result `shouldContain` "Break statement"
      result `shouldContain` "Return statement"
    
    it "handles complex error types with context" $ do
      let err = DuplicateParameter "param" (TokenPn 0 1 10)
      errorToString err `shouldContain` "Duplicate parameter name 'param'"
      errorToString err `shouldContain` "at line 1, column 10"

  describe "valid programs" $ do
    it "validates simple program" $ do
      let validProgram = JSAstProgram 
            [ JSVariable noAnnot 
                (JSLOne (JSVarInitExpression 
                  (JSIdentifier noAnnot "x") 
                  (JSVarInit noAnnot (JSDecimal noAnnot "42"))))
                auto
            ] 
            noAnnot
      validate validProgram `shouldSatisfy` isRight
    
    it "validates function declaration" $ do
      let funcProgram = JSAstProgram 
            [ JSFunction noAnnot 
                (JSIdentName noAnnot "test")
                noAnnot
                (JSLOne (JSIdentifier noAnnot "param"))
                noAnnot
                (JSBlock noAnnot 
                  [ JSReturn noAnnot 
                      (Just (JSIdentifier noAnnot "param"))
                      auto
                  ]
                  noAnnot)
                auto
            ]
            noAnnot
      validate funcProgram `shouldSatisfy` isRight
    
    it "validates loop with break" $ do
      let loopProgram = JSAstProgram
            [ JSWhile noAnnot noAnnot
                (JSLiteral noAnnot "true")
                noAnnot
                (JSStatementBlock noAnnot
                  [ JSBreak noAnnot JSIdentNone auto
                  ]
                  noAnnot auto)
            ]
            noAnnot
      validate loopProgram `shouldSatisfy` isRight
    
    it "validates switch with break" $ do
      let switchProgram = JSAstProgram
            [ JSSwitch noAnnot noAnnot
                (JSIdentifier noAnnot "x")
                noAnnot noAnnot
                [ JSCase noAnnot 
                    (JSDecimal noAnnot "1")
                    noAnnot
                    [ JSBreak noAnnot JSIdentNone auto
                    ]
                ]
                noAnnot auto
            ]
            noAnnot
      validate switchProgram `shouldSatisfy` isRight

    it "validates async function with await" $ do
      let asyncProgram = JSAstProgram
            [ JSAsyncFunction noAnnot noAnnot
                (JSIdentName noAnnot "test")
                noAnnot
                JSLNil
                noAnnot
                (JSBlock noAnnot
                  [ JSReturn noAnnot
                      (Just (JSAwaitExpression noAnnot 
                        (JSCallExpression 
                          (JSIdentifier noAnnot "fetch")
                          noAnnot
                          (JSLOne (JSStringLiteral noAnnot "url"))
                          noAnnot)))
                      auto
                  ]
                  noAnnot)
                auto
            ]
            noAnnot
      validate asyncProgram `shouldSatisfy` isRight
    
    it "validates generator function with yield" $ do
      let genProgram = JSAstProgram
            [ JSGenerator noAnnot noAnnot
                (JSIdentName noAnnot "test")
                noAnnot
                JSLNil
                noAnnot
                (JSBlock noAnnot
                  [ JSExpressionStatement
                      (JSYieldExpression noAnnot 
                        (Just (JSDecimal noAnnot "42")))
                      auto
                  ]
                  noAnnot)
                auto
            ]
            noAnnot
      validate genProgram `shouldSatisfy` isRight

    it "validates const declaration with initializer" $ do
      let constProgram = JSAstProgram
            [ JSConstant noAnnot
                (JSLOne (JSVarInitExpression
                  (JSIdentifier noAnnot "x")
                  (JSVarInit noAnnot (JSDecimal noAnnot "42"))))
                auto
            ]
            noAnnot
      validate constProgram `shouldSatisfy` isRight

  describe "invalid programs with specific error types" $ do
    it "rejects break outside loop with specific error" $ do
      let invalidProgram = JSAstProgram
            [ JSBreak noAnnot JSIdentNone auto
            ]
            noAnnot
      case validate invalidProgram of
        Left [BreakOutsideLoop _] -> pure ()
        other -> expectationFailure $ "Expected BreakOutsideLoop but got: " ++ show other
    
    it "rejects continue outside loop with specific error" $ do
      let invalidProgram = JSAstProgram
            [ JSContinue noAnnot JSIdentNone auto
            ]
            noAnnot
      case validate invalidProgram of
        Left [ContinueOutsideLoop _] -> pure ()
        other -> expectationFailure $ "Expected ContinueOutsideLoop but got: " ++ show other
    
    it "rejects return outside function with specific error" $ do
      let invalidProgram = JSAstProgram
            [ JSReturn noAnnot 
                (Just (JSDecimal noAnnot "42"))
                auto
            ]
            noAnnot
      case validate invalidProgram of
        Left [ReturnOutsideFunction _] -> pure ()
        other -> expectationFailure $ "Expected ReturnOutsideFunction but got: " ++ show other

  describe "strict mode validation" $ do
    it "validates strict mode is detected from 'use strict' directive" $ do
      let strictProgram = JSAstProgram
            [ JSExpressionStatement (JSStringLiteral noAnnot "use strict") auto
            , JSVariable noAnnot
                (JSLOne (JSVarInitExpression
                  (JSIdentifier noAnnot "x")
                  (JSVarInit noAnnot (JSDecimal noAnnot "42"))))
                auto
            ]
            noAnnot
      validate strictProgram `shouldSatisfy` isRight
    
    it "validates strict mode in modules" $ do
      let moduleAST = JSAstModule
            [ JSModuleStatementListItem
                (JSVariable noAnnot
                  (JSLOne (JSVarInitExpression
                    (JSIdentifier noAnnot "x")
                    (JSVarInit noAnnot (JSDecimal noAnnot "42"))))
                  auto)
            ]
            noAnnot
      validate moduleAST `shouldSatisfy` isRight
    
    it "rejects with statement in strict mode" $ do
      let strictWithProgram = JSAstProgram
            [ JSExpressionStatement (JSStringLiteral noAnnot "use strict") auto
            , JSWith noAnnot noAnnot
                (JSIdentifier noAnnot "obj")
                noAnnot
                (JSEmptyStatement noAnnot)
                auto
            ]
            noAnnot
      case validate strictWithProgram of
        Left errors -> 
          any (\err -> case err of
            WithStatementInStrict _ -> True
            _ -> False) errors `shouldBe` True
        _ -> expectationFailure "Expected with statement error in strict mode"

  describe "expression validation edge cases" $ do
    it "validates valid assignment targets" $ do
      let validTargets = 
            [ JSIdentifier noAnnot "x"
            , JSMemberDot (JSIdentifier noAnnot "obj") noAnnot (JSIdentifier noAnnot "prop")
            , JSMemberSquare (JSIdentifier noAnnot "arr") noAnnot (JSDecimal noAnnot "0") noAnnot
            , JSArrayLiteral noAnnot [] noAnnot  -- Destructuring
            , JSObjectLiteral noAnnot (JSCTLNone JSLNil) noAnnot  -- Destructuring
            ]
      
      mapM_ (\target -> validateAssignmentTarget target `shouldBe` []) validTargets
    
    it "rejects invalid assignment targets with specific errors" $ do
      let invalidLiteral = JSDecimal noAnnot "42"
      case validateAssignmentTarget invalidLiteral of
        [InvalidAssignmentTarget _ _] -> pure ()
        other -> expectationFailure $ "Expected InvalidAssignmentTarget but got: " ++ show other
      
      let invalidString = JSStringLiteral noAnnot "hello"
      case validateAssignmentTarget invalidString of
        [InvalidAssignmentTarget _ _] -> pure ()
        other -> expectationFailure $ "Expected InvalidAssignmentTarget but got: " ++ show other

  describe "JavaScript edge cases and corner cases" $ do
    it "validates nested function contexts" $ do
      let nestedProgram = JSAstProgram
            [ JSFunction noAnnot
                (JSIdentName noAnnot "outer")
                noAnnot
                JSLNil
                noAnnot
                (JSBlock noAnnot
                  [ JSFunction noAnnot
                      (JSIdentName noAnnot "inner")
                      noAnnot
                      JSLNil
                      noAnnot
                      (JSBlock noAnnot
                        [ JSReturn noAnnot
                            (Just (JSDecimal noAnnot "42"))
                            auto
                        ]
                        noAnnot)
                      auto
                  , JSReturn noAnnot
                      (Just (JSCallExpression
                        (JSIdentifier noAnnot "inner")
                        noAnnot
                        JSLNil
                        noAnnot))
                      auto
                  ]
                  noAnnot)
                auto
            ]
            noAnnot
      validate nestedProgram `shouldSatisfy` isRight

    it "validates nested loop contexts" $ do
      let nestedLoop = JSAstProgram
            [ JSWhile noAnnot noAnnot
                (JSLiteral noAnnot "true")
                noAnnot
                (JSStatementBlock noAnnot
                  [ JSFor noAnnot noAnnot
                      JSLNil noAnnot
                      (JSLOne (JSLiteral noAnnot "true"))
                      noAnnot JSLNil noAnnot
                      (JSStatementBlock noAnnot
                        [ JSBreak noAnnot JSIdentNone auto
                        , JSContinue noAnnot JSIdentNone auto
                        ]
                        noAnnot auto)
                  ]
                  noAnnot auto)
            ]
            noAnnot
      validate nestedLoop `shouldSatisfy` isRight

    it "validates class with methods" $ do
      let classProgram = JSAstProgram
            [ JSClass noAnnot
                (JSIdentName noAnnot "TestClass")
                JSExtendsNone
                noAnnot
                [ JSClassInstanceMethod 
                    (JSMethodDefinition 
                      (JSPropertyIdent noAnnot "method")
                      noAnnot
                      JSLNil
                      noAnnot
                      (JSBlock noAnnot
                        [ JSReturn noAnnot
                            (Just (JSLiteral noAnnot "this"))
                            auto
                        ]
                        noAnnot))
                ]
                noAnnot
                auto
            ]
            noAnnot
      validate classProgram `shouldSatisfy` isRight
    
    it "handles for-in and for-of loop validation" $ do
      let forInProgram = JSAstProgram
            [ JSForIn noAnnot noAnnot
                (JSIdentifier noAnnot "key")
                (JSBinOpIn noAnnot)
                (JSIdentifier noAnnot "obj")
                noAnnot
                (JSEmptyStatement noAnnot)
            ]
            noAnnot
      validate forInProgram `shouldSatisfy` isRight
      
      let forOfProgram = JSAstProgram
            [ JSForOf noAnnot noAnnot
                (JSIdentifier noAnnot "item")
                (JSBinOpOf noAnnot)
                (JSIdentifier noAnnot "array")
                noAnnot
                (JSEmptyStatement noAnnot)
            ]
            noAnnot
      validate forOfProgram `shouldSatisfy` isRight
    
    it "validates template literals" $ do
      let templateProgram = JSAstProgram
            [ JSExpressionStatement
                (JSTemplateLiteral Nothing noAnnot "hello"
                  [ JSTemplatePart (JSIdentifier noAnnot "name") noAnnot " world"
                  ])
                auto
            ]
            noAnnot
      validate templateProgram `shouldSatisfy` isRight
    
    it "validates arrow functions with different parameter forms" $ do
      let arrowProgram = JSAstProgram
            [ JSVariable noAnnot
                (JSLOne (JSVarInitExpression
                  (JSIdentifier noAnnot "arrow1")
                  (JSVarInit noAnnot
                    (JSArrowExpression
                      (JSUnparenthesizedArrowParameter (JSIdentName noAnnot "x"))
                      noAnnot
                      (JSConciseExpressionBody (JSIdentifier noAnnot "x"))))))
                auto
            , JSVariable noAnnot
                (JSLOne (JSVarInitExpression
                  (JSIdentifier noAnnot "arrow2")
                  (JSVarInit noAnnot
                    (JSArrowExpression
                      (JSParenthesizedArrowParameterList noAnnot
                        (JSLCons 
                          (JSLOne (JSIdentifier noAnnot "a"))
                          noAnnot
                          (JSIdentifier noAnnot "b"))
                        noAnnot)
                      noAnnot
                      (JSConciseFunctionBody
                        (JSBlock noAnnot
                          [ JSReturn noAnnot
                              (Just (JSExpressionBinary
                                (JSIdentifier noAnnot "a")
                                (JSBinOpPlus noAnnot)
                                (JSIdentifier noAnnot "b")))
                              auto
                          ]
                          noAnnot))))))
                auto
            ]
            noAnnot
      validate arrowProgram `shouldSatisfy` isRight

  describe "comprehensive control flow validation" $ do
    it "validates try-catch-finally" $ do
      let tryProgram = JSAstProgram
            [ JSTry noAnnot
                (JSBlock noAnnot
                  [ JSThrow noAnnot (JSStringLiteral noAnnot "error") auto
                  ]
                  noAnnot)
                [ JSCatch noAnnot noAnnot
                    (JSIdentifier noAnnot "e")
                    noAnnot
                    (JSBlock noAnnot
                      [ JSExpressionStatement
                          (JSCallExpression
                            (JSMemberDot 
                              (JSIdentifier noAnnot "console")
                              noAnnot
                              (JSIdentifier noAnnot "log"))
                            noAnnot
                            (JSLOne (JSIdentifier noAnnot "e"))
                            noAnnot)
                          auto
                      ]
                      noAnnot)
                ]
                (JSFinally noAnnot
                  (JSBlock noAnnot
                    [ JSExpressionStatement
                        (JSCallExpression
                          (JSMemberDot 
                            (JSIdentifier noAnnot "console")
                            noAnnot
                            (JSIdentifier noAnnot "log"))
                          noAnnot
                          (JSLOne (JSStringLiteral noAnnot "cleanup"))
                          noAnnot)
                        auto
                    ]
                    noAnnot))
            ]
            noAnnot
      validate tryProgram `shouldSatisfy` isRight

  describe "module validation edge cases" $ do
    it "validates module with imports and exports" $ do
      let moduleAST = JSAstModule
            [ JSModuleImportDeclaration noAnnot
                (JSImportDeclaration
                  (JSImportClauseDefault (JSIdentName noAnnot "React"))
                  (JSFromClause noAnnot noAnnot "react")
                  auto)
            , JSModuleStatementListItem
                (JSFunction noAnnot
                  (JSIdentName noAnnot "component")
                  noAnnot
                  JSLNil
                  noAnnot
                  (JSBlock noAnnot
                    [ JSReturn noAnnot
                        (Just (JSLiteral noAnnot "null"))
                        auto
                    ]
                    noAnnot)
                  auto)
            ]
            noAnnot
      validate moduleAST `shouldSatisfy` isRight
    
    it "rejects import outside module" $ do
      let validProgram = JSAstProgram
            [ JSVariable noAnnot
                (JSLOne (JSVarInitExpression
                  (JSIdentifier noAnnot "x")
                  (JSVarInit noAnnot (JSDecimal noAnnot "42"))))
                auto
            ]
            noAnnot
      -- This should be valid as it's just a statement, not an import
      validate validProgram `shouldSatisfy` isRight

  describe "edge cases and boundary conditions" $ do
    it "validates empty program" $ do
      let emptyProgram = JSAstProgram [] noAnnot
      validate emptyProgram `shouldSatisfy` isRight
    
    it "validates empty module" $ do
      let emptyModule = JSAstModule [] noAnnot
      validate emptyModule `shouldSatisfy` isRight
    
    it "validates single expression" $ do
      let exprAST = JSAstExpression (JSDecimal noAnnot "42") noAnnot
      validate exprAST `shouldSatisfy` isRight
    
    it "validates single statement" $ do
      let stmtAST = JSAstStatement (JSEmptyStatement noAnnot) noAnnot
      validate stmtAST `shouldSatisfy` isRight
    
    it "handles complex nested expressions" $ do
      let complexExpr = JSAstExpression
            (JSExpressionTernary
              (JSExpressionBinary
                (JSIdentifier noAnnot "x")
                (JSBinOpGt noAnnot)
                (JSDecimal noAnnot "0"))
              noAnnot
              (JSCallExpression
                (JSMemberDot
                  (JSIdentifier noAnnot "console")
                  noAnnot
                  (JSIdentifier noAnnot "log"))
                noAnnot
                (JSLOne (JSStringLiteral noAnnot "positive"))
                noAnnot)
              noAnnot
              (JSCallExpression
                (JSMemberDot
                  (JSIdentifier noAnnot "console")
                  noAnnot
                  (JSIdentifier noAnnot "log"))
                noAnnot
                (JSLOne (JSStringLiteral noAnnot "not positive"))
                noAnnot))
            noAnnot
      validate complexExpr `shouldSatisfy` isRight

  describe "literal validation edge cases" $ do
    it "validates various numeric literals" $ do
      let numericProgram = JSAstProgram
            [ JSExpressionStatement (JSDecimal noAnnot "42") auto
            , JSExpressionStatement (JSDecimal noAnnot "3.14") auto
            , JSExpressionStatement (JSDecimal noAnnot "1e10") auto
            , JSExpressionStatement (JSHexInteger noAnnot "0xFF") auto
            , JSExpressionStatement (JSBigIntLiteral noAnnot "123n") auto
            ]
            noAnnot
      validate numericProgram `shouldSatisfy` isRight
    
    it "validates string literals with various content" $ do
      let stringProgram = JSAstProgram
            [ JSExpressionStatement (JSStringLiteral noAnnot "simple") auto
            , JSExpressionStatement (JSStringLiteral noAnnot "with\nneWlines") auto
            , JSExpressionStatement (JSStringLiteral noAnnot "with \"quotes\"") auto
            , JSExpressionStatement (JSStringLiteral noAnnot "unicode: ü") auto
            ]
            noAnnot
      validate stringProgram `shouldSatisfy` isRight
    
    it "validates regex literals" $ do
      let regexProgram = JSAstProgram
            [ JSExpressionStatement (JSRegEx noAnnot "/pattern/g") auto
            , JSExpressionStatement (JSRegEx noAnnot "/[a-z]+/i") auto
            ]
            noAnnot
      validate regexProgram `shouldSatisfy` isRight