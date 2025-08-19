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
                  Nothing
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

  describe "control flow context validation (HIGH priority)" $ do
    describe "yield in parameter defaults" $ do
      it "rejects yield in function parameter defaults" $ do
        let invalidProgram = JSAstProgram
              [ JSFunction noAnnot
                  (JSIdentName noAnnot "test")
                  noAnnot
                  (JSLOne (JSVarInitExpression
                    (JSIdentifier noAnnot "x")
                    (JSVarInit noAnnot (JSYieldExpression noAnnot (Just (JSDecimal noAnnot "1"))))))
                  noAnnot
                  (JSBlock noAnnot [] noAnnot)
                  auto
              ]
              noAnnot
        case validate invalidProgram of
          Left errors -> 
            any (\err -> case err of
              YieldInParameterDefault _ -> True
              _ -> False) errors `shouldBe` True
          _ -> expectationFailure "Expected YieldInParameterDefault error"
      
      it "rejects yield in generator parameter defaults" $ do
        let invalidProgram = JSAstProgram
              [ JSGenerator noAnnot noAnnot
                  (JSIdentName noAnnot "test")
                  noAnnot
                  (JSLOne (JSVarInitExpression
                    (JSIdentifier noAnnot "x")
                    (JSVarInit noAnnot (JSYieldExpression noAnnot (Just (JSDecimal noAnnot "1"))))))
                  noAnnot
                  (JSBlock noAnnot [] noAnnot)
                  auto
              ]
              noAnnot
        case validate invalidProgram of
          Left errors ->
            any (\err -> case err of
              YieldInParameterDefault _ -> True
              _ -> False) errors `shouldBe` True
          _ -> expectationFailure "Expected YieldInParameterDefault error"

    describe "await in parameter defaults" $ do
      it "rejects await in function parameter defaults" $ do
        let invalidProgram = JSAstProgram
              [ JSFunction noAnnot
                  (JSIdentName noAnnot "test")
                  noAnnot
                  (JSLOne (JSVarInitExpression
                    (JSIdentifier noAnnot "x")
                    (JSVarInit noAnnot (JSAwaitExpression noAnnot (JSCallExpression
                      (JSIdentifier noAnnot "fetch")
                      noAnnot
                      (JSLOne (JSStringLiteral noAnnot "url"))
                      noAnnot)))))
                  noAnnot
                  (JSBlock noAnnot [] noAnnot)
                  auto
              ]
              noAnnot
        case validate invalidProgram of
          Left errors ->
            any (\err -> case err of
              AwaitInParameterDefault _ -> True
              _ -> False) errors `shouldBe` True
          _ -> expectationFailure "Expected AwaitInParameterDefault error"
      
      it "rejects await in async function parameter defaults" $ do
        let invalidProgram = JSAstProgram
              [ JSAsyncFunction noAnnot noAnnot
                  (JSIdentName noAnnot "test")
                  noAnnot
                  (JSLOne (JSVarInitExpression
                    (JSIdentifier noAnnot "x")
                    (JSVarInit noAnnot (JSAwaitExpression noAnnot (JSCallExpression
                      (JSIdentifier noAnnot "fetch")
                      noAnnot
                      (JSLOne (JSStringLiteral noAnnot "url"))
                      noAnnot)))))
                  noAnnot
                  (JSBlock noAnnot [] noAnnot)
                  auto
              ]
              noAnnot
        case validate invalidProgram of
          Left errors ->
            any (\err -> case err of
              AwaitInParameterDefault _ -> True
              _ -> False) errors `shouldBe` True
          _ -> expectationFailure "Expected AwaitInParameterDefault error"

    describe "labeled break validation" $ do
      it "rejects break with non-existent label" $ do
        let invalidProgram = JSAstProgram
              [ JSWhile noAnnot noAnnot
                  (JSLiteral noAnnot "true")
                  noAnnot
                  (JSStatementBlock noAnnot
                    [ JSBreak noAnnot (JSIdentName noAnnot "nonexistent") auto
                    ]
                    noAnnot auto)
              ]
              noAnnot
        case validate invalidProgram of
          Left errors ->
            any (\err -> case err of
              LabelNotFound "nonexistent" _ -> True
              _ -> False) errors `shouldBe` True
          _ -> expectationFailure "Expected LabelNotFound error"
      
      it "accepts break with valid label to labeled statement" $ do
        let validProgram = JSAstProgram
              [ JSLabelled (JSIdentName noAnnot "outer") noAnnot
                  (JSWhile noAnnot noAnnot 
                    (JSLiteral noAnnot "true") 
                    noAnnot
                    (JSStatementBlock noAnnot
                      [ JSBreak noAnnot (JSIdentName noAnnot "outer") auto
                      ]
                      noAnnot auto))
              ]
              noAnnot
        validate validProgram `shouldSatisfy` isRight
      
      it "rejects break outside switch context with label" $ do
        let invalidProgram = JSAstProgram
              [ JSLabelled (JSIdentName noAnnot "label") noAnnot
                  (JSExpressionStatement (JSDecimal noAnnot "42") auto)
              , JSBreak noAnnot (JSIdentName noAnnot "label") auto
              ]
              noAnnot
        case validate invalidProgram of
          Left errors ->
            any (\err -> case err of
              BreakOutsideSwitch _ -> True
              LabelNotFound _ _ -> True
              _ -> False) errors `shouldBe` True
          _ -> expectationFailure "Expected BreakOutsideSwitch or LabelNotFound error"

    describe "labeled continue validation" $ do
      it "rejects continue with non-existent label" $ do
        let invalidProgram = JSAstProgram
              [ JSWhile noAnnot noAnnot
                  (JSLiteral noAnnot "true")
                  noAnnot
                  (JSStatementBlock noAnnot
                    [ JSContinue noAnnot (JSIdentName noAnnot "nonexistent") auto
                    ]
                    noAnnot auto)
              ]
              noAnnot
        case validate invalidProgram of
          Left errors ->
            any (\err -> case err of
              LabelNotFound "nonexistent" _ -> True
              _ -> False) errors `shouldBe` True
          _ -> expectationFailure "Expected LabelNotFound error"
      
      it "accepts continue with valid label to labeled loop" $ do
        let validProgram = JSAstProgram
              [ JSLabelled (JSIdentName noAnnot "outer") noAnnot
                  (JSWhile noAnnot noAnnot 
                    (JSLiteral noAnnot "true") 
                    noAnnot
                    (JSStatementBlock noAnnot
                      [ JSContinue noAnnot (JSIdentName noAnnot "outer") auto
                      ]
                      noAnnot auto))
              ]
              noAnnot
        validate validProgram `shouldSatisfy` isRight

    describe "duplicate label validation" $ do
      it "rejects duplicate labels" $ do
        let invalidProgram = JSAstProgram
              [ JSLabelled (JSIdentName noAnnot "label") noAnnot
                  (JSLabelled (JSIdentName noAnnot "label") noAnnot
                    (JSExpressionStatement (JSDecimal noAnnot "42") auto))
              ]
              noAnnot
        case validate invalidProgram of
          Left errors ->
            any (\err -> case err of
              DuplicateLabel "label" _ -> True
              _ -> False) errors `shouldBe` True
          _ -> expectationFailure "Expected DuplicateLabel error"

  describe "assignment target validation (HIGH priority)" $ do
    describe "invalid destructuring targets" $ do
      it "rejects literal as destructuring array target" $ do
        let invalidProgram = JSAstProgram
              [ JSAssignStatement
                  (JSDecimal noAnnot "42")
                  (JSAssign noAnnot)
                  (JSArrayLiteral noAnnot 
                    [ JSArrayElement (JSIdentifier noAnnot "a")
                    , JSArrayComma noAnnot
                    , JSArrayElement (JSIdentifier noAnnot "b")
                    ] noAnnot)
                  auto
              ]
              noAnnot
        case validate invalidProgram of
          Left errors ->
            any (\err -> case err of
              InvalidDestructuringTarget _ _ -> True
              InvalidAssignmentTarget _ _ -> True
              _ -> False) errors `shouldBe` True
          _ -> expectationFailure "Expected InvalidDestructuringTarget or InvalidAssignmentTarget error"
      
      it "rejects literal as destructuring object target" $ do
        let invalidProgram = JSAstProgram
              [ JSAssignStatement
                  (JSStringLiteral noAnnot "hello")
                  (JSAssign noAnnot)
                  (JSObjectLiteral noAnnot 
                    (JSCTLNone (JSLOne (JSPropertyNameandValue 
                      (JSPropertyIdent noAnnot "x") 
                      noAnnot 
                      [JSIdentifier noAnnot "value"])))
                    noAnnot)
                  auto
              ]
              noAnnot
        case validate invalidProgram of
          Left errors ->
            any (\err -> case err of
              InvalidDestructuringTarget _ _ -> True
              InvalidAssignmentTarget _ _ -> True
              _ -> False) errors `shouldBe` True
          _ -> expectationFailure "Expected InvalidDestructuringTarget or InvalidAssignmentTarget error"

    describe "for-in loop LHS validation" $ do
      it "rejects literal in for-in LHS" $ do
        let invalidProgram = JSAstProgram
              [ JSForIn noAnnot noAnnot
                  (JSDecimal noAnnot "42")
                  (JSBinOpIn noAnnot)
                  (JSIdentifier noAnnot "obj")
                  noAnnot
                  (JSEmptyStatement noAnnot)
              ]
              noAnnot
        case validate invalidProgram of
          Left errors ->
            any (\err -> case err of
              InvalidLHSInForIn _ _ -> True
              InvalidAssignmentTarget _ _ -> True
              _ -> False) errors `shouldBe` True
          _ -> expectationFailure "Expected InvalidLHSInForIn or InvalidAssignmentTarget error"
      
      it "rejects string literal in for-in LHS" $ do
        let invalidProgram = JSAstProgram
              [ JSForIn noAnnot noAnnot
                  (JSStringLiteral noAnnot "invalid")
                  (JSBinOpIn noAnnot)
                  (JSIdentifier noAnnot "obj")
                  noAnnot
                  (JSEmptyStatement noAnnot)
              ]
              noAnnot
        case validate invalidProgram of
          Left errors ->
            any (\err -> case err of
              InvalidLHSInForIn _ _ -> True
              InvalidAssignmentTarget _ _ -> True
              _ -> False) errors `shouldBe` True
          _ -> expectationFailure "Expected InvalidLHSInForIn or InvalidAssignmentTarget error"
      
      it "accepts valid identifier in for-in LHS" $ do
        let validProgram = JSAstProgram
              [ JSForIn noAnnot noAnnot
                  (JSIdentifier noAnnot "key")
                  (JSBinOpIn noAnnot)
                  (JSIdentifier noAnnot "obj")
                  noAnnot
                  (JSEmptyStatement noAnnot)
              ]
              noAnnot
        validate validProgram `shouldSatisfy` isRight

    describe "for-of loop LHS validation" $ do
      it "rejects literal in for-of LHS" $ do
        let invalidProgram = JSAstProgram
              [ JSForOf noAnnot noAnnot
                  (JSDecimal noAnnot "42")
                  (JSBinOpOf noAnnot)
                  (JSIdentifier noAnnot "array")
                  noAnnot
                  (JSEmptyStatement noAnnot)
              ]
              noAnnot
        case validate invalidProgram of
          Left errors ->
            any (\err -> case err of
              InvalidLHSInForOf _ _ -> True
              InvalidAssignmentTarget _ _ -> True
              _ -> False) errors `shouldBe` True
          _ -> expectationFailure "Expected InvalidLHSInForOf or InvalidAssignmentTarget error"
      
      it "rejects function call in for-of LHS" $ do
        let invalidProgram = JSAstProgram
              [ JSForOf noAnnot noAnnot
                  (JSCallExpression
                    (JSIdentifier noAnnot "func")
                    noAnnot
                    JSLNil
                    noAnnot)
                  (JSBinOpOf noAnnot)
                  (JSIdentifier noAnnot "array")
                  noAnnot
                  (JSEmptyStatement noAnnot)
              ]
              noAnnot
        case validate invalidProgram of
          Left errors ->
            any (\err -> case err of
              InvalidLHSInForOf _ _ -> True
              InvalidAssignmentTarget _ _ -> True
              _ -> False) errors `shouldBe` True
          _ -> expectationFailure "Expected InvalidLHSInForOf or InvalidAssignmentTarget error"
      
      it "accepts valid identifier in for-of LHS" $ do
        let validProgram = JSAstProgram
              [ JSForOf noAnnot noAnnot
                  (JSIdentifier noAnnot "item")
                  (JSBinOpOf noAnnot)
                  (JSIdentifier noAnnot "array")
                  noAnnot
                  (JSEmptyStatement noAnnot)
              ]
              noAnnot
        validate validProgram `shouldSatisfy` isRight

    describe "complex destructuring validation" $ do
      it "validates simple array destructuring patterns" $ do
        let validProgram = JSAstProgram
              [ JSVariable noAnnot
                  (JSLOne (JSVarInitExpression
                    (JSArrayLiteral noAnnot 
                      [ JSArrayElement (JSIdentifier noAnnot "a")
                      , JSArrayComma noAnnot
                      , JSArrayElement (JSIdentifier noAnnot "b")
                      ] noAnnot)
                    (JSVarInit noAnnot (JSArrayLiteral noAnnot 
                      [ JSArrayElement (JSDecimal noAnnot "1")
                      , JSArrayComma noAnnot
                      , JSArrayElement (JSDecimal noAnnot "2")
                      ] noAnnot))))
                  auto
              ]
              noAnnot
        validate validProgram `shouldSatisfy` isRight
      
      it "validates simple object destructuring patterns" $ do
        let validProgram = JSAstProgram
              [ JSVariable noAnnot
                  (JSLOne (JSVarInitExpression
                    (JSObjectLiteral noAnnot 
                      (JSCTLNone (JSLOne (JSPropertyNameandValue
                        (JSPropertyIdent noAnnot "x")
                        noAnnot
                        [JSIdentifier noAnnot "a"])))
                      noAnnot)
                    (JSVarInit noAnnot (JSObjectLiteral noAnnot
                      (JSCTLNone (JSLOne (JSPropertyNameandValue
                        (JSPropertyIdent noAnnot "x")
                        noAnnot
                        [JSDecimal noAnnot "1"])))
                      noAnnot))))
                  auto
              ]
              noAnnot
        validate validProgram `shouldSatisfy` isRight

  describe "class constructor validation (HIGH priority)" $ do
    describe "multiple constructor errors" $ do
      it "rejects class with multiple constructors" $ do
        let invalidProgram = JSAstProgram
              [ JSClass noAnnot
                  (JSIdentName noAnnot "TestClass")
                  JSExtendsNone
                  noAnnot
                  [ JSClassInstanceMethod 
                      (JSMethodDefinition 
                        (JSPropertyIdent noAnnot "constructor")
                        noAnnot
                        JSLNil
                        noAnnot
                        (JSBlock noAnnot [] noAnnot))
                  , JSClassInstanceMethod 
                      (JSMethodDefinition 
                        (JSPropertyIdent noAnnot "constructor")
                        noAnnot
                        JSLNil
                        noAnnot
                        (JSBlock noAnnot [] noAnnot))
                  ]
                  noAnnot
                  auto
              ]
              noAnnot
        case validate invalidProgram of
          Left errors ->
            any (\err -> case err of
              MultipleConstructors _ -> True
              DuplicateMethodName "constructor" _ -> True
              _ -> False) errors `shouldBe` True
          _ -> expectationFailure "Expected MultipleConstructors or DuplicateMethodName error"
      
      it "accepts class with single constructor" $ do
        let validProgram = JSAstProgram
              [ JSClass noAnnot
                  (JSIdentName noAnnot "TestClass")
                  JSExtendsNone
                  noAnnot
                  [ JSClassInstanceMethod 
                      (JSMethodDefinition 
                        (JSPropertyIdent noAnnot "constructor")
                        noAnnot
                        JSLNil
                        noAnnot
                        (JSBlock noAnnot [] noAnnot))
                  ]
                  noAnnot
                  auto
              ]
              noAnnot
        validate validProgram `shouldSatisfy` isRight

    describe "constructor generator errors" $ do
      it "rejects generator constructor" $ do
        let invalidProgram = JSAstProgram
              [ JSClass noAnnot
                  (JSIdentName noAnnot "TestClass")
                  JSExtendsNone
                  noAnnot
                  [ JSClassInstanceMethod 
                      (JSGeneratorMethodDefinition 
                        noAnnot
                        (JSPropertyIdent noAnnot "constructor")
                        noAnnot
                        JSLNil
                        noAnnot
                        (JSBlock noAnnot [] noAnnot))
                  ]
                  noAnnot
                  auto
              ]
              noAnnot
        case validate invalidProgram of
          Left errors ->
            any (\err -> case err of
              ConstructorWithGenerator _ -> True
              _ -> False) errors `shouldBe` True
          _ -> expectationFailure "Expected ConstructorWithGenerator error"
      
      it "accepts regular generator method (not constructor)" $ do
        let validProgram = JSAstProgram
              [ JSClass noAnnot
                  (JSIdentName noAnnot "TestClass")
                  JSExtendsNone
                  noAnnot
                  [ JSClassInstanceMethod 
                      (JSGeneratorMethodDefinition 
                        noAnnot
                        (JSPropertyIdent noAnnot "method")
                        noAnnot
                        JSLNil
                        noAnnot
                        (JSBlock noAnnot [] noAnnot))
                  ]
                  noAnnot
                  auto
              ]
              noAnnot
        validate validProgram `shouldSatisfy` isRight

    describe "static constructor errors" $ do
      it "rejects static constructor" $ do
        let invalidProgram = JSAstProgram
              [ JSClass noAnnot
                  (JSIdentName noAnnot "TestClass")
                  JSExtendsNone
                  noAnnot
                  [ JSClassStaticMethod noAnnot
                      (JSMethodDefinition 
                        (JSPropertyIdent noAnnot "constructor")
                        noAnnot
                        JSLNil
                        noAnnot
                        (JSBlock noAnnot [] noAnnot))
                  ]
                  noAnnot
                  auto
              ]
              noAnnot
        case validate invalidProgram of
          Left errors ->
            any (\err -> case err of
              StaticConstructor _ -> True
              _ -> False) errors `shouldBe` True
          _ -> expectationFailure "Expected StaticConstructor error"
      
      it "accepts static method (not constructor)" $ do
        let validProgram = JSAstProgram
              [ JSClass noAnnot
                  (JSIdentName noAnnot "TestClass")
                  JSExtendsNone
                  noAnnot
                  [ JSClassStaticMethod noAnnot
                      (JSMethodDefinition 
                        (JSPropertyIdent noAnnot "method")
                        noAnnot
                        JSLNil
                        noAnnot
                        (JSBlock noAnnot [] noAnnot))
                  ]
                  noAnnot
                  auto
              ]
              noAnnot
        validate validProgram `shouldSatisfy` isRight

    describe "duplicate method name validation" $ do
      it "rejects class with duplicate method names" $ do
        let invalidProgram = JSAstProgram
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
                        (JSBlock noAnnot [] noAnnot))
                  , JSClassInstanceMethod 
                      (JSMethodDefinition 
                        (JSPropertyIdent noAnnot "method")
                        noAnnot
                        JSLNil
                        noAnnot
                        (JSBlock noAnnot [] noAnnot))
                  ]
                  noAnnot
                  auto
              ]
              noAnnot
        case validate invalidProgram of
          Left errors ->
            any (\err -> case err of
              DuplicateMethodName "method" _ -> True
              _ -> False) errors `shouldBe` True
          _ -> expectationFailure "Expected DuplicateMethodName error"
      
      it "accepts class with different method names" $ do
        let validProgram = JSAstProgram
              [ JSClass noAnnot
                  (JSIdentName noAnnot "TestClass")
                  JSExtendsNone
                  noAnnot
                  [ JSClassInstanceMethod 
                      (JSMethodDefinition 
                        (JSPropertyIdent noAnnot "method1")
                        noAnnot
                        JSLNil
                        noAnnot
                        (JSBlock noAnnot [] noAnnot))
                  , JSClassInstanceMethod 
                      (JSMethodDefinition 
                        (JSPropertyIdent noAnnot "method2")
                        noAnnot
                        JSLNil
                        noAnnot
                        (JSBlock noAnnot [] noAnnot))
                  ]
                  noAnnot
                  auto
              ]
              noAnnot
        validate validProgram `shouldSatisfy` isRight

    describe "getter and setter validation" $ do
      it "rejects getter with parameters" $ do
        let invalidProgram = JSAstProgram
              [ JSClass noAnnot
                  (JSIdentName noAnnot "TestClass")
                  JSExtendsNone
                  noAnnot
                  [ JSClassInstanceMethod 
                      (JSPropertyAccessor 
                        (JSAccessorGet noAnnot)
                        (JSPropertyIdent noAnnot "prop")
                        noAnnot
                        (JSLOne (JSIdentifier noAnnot "x"))
                        noAnnot
                        (JSBlock noAnnot [] noAnnot))
                  ]
                  noAnnot
                  auto
              ]
              noAnnot
        case validate invalidProgram of
          Left errors ->
            any (\err -> case err of
              GetterWithParameters _ -> True
              _ -> False) errors `shouldBe` True
          _ -> expectationFailure "Expected GetterWithParameters error"
      
      it "rejects setter without parameters" $ do
        let invalidProgram = JSAstProgram
              [ JSClass noAnnot
                  (JSIdentName noAnnot "TestClass")
                  JSExtendsNone
                  noAnnot
                  [ JSClassInstanceMethod 
                      (JSPropertyAccessor 
                        (JSAccessorSet noAnnot)
                        (JSPropertyIdent noAnnot "prop")
                        noAnnot
                        JSLNil
                        noAnnot
                        (JSBlock noAnnot [] noAnnot))
                  ]
                  noAnnot
                  auto
              ]
              noAnnot
        case validate invalidProgram of
          Left errors ->
            any (\err -> case err of
              SetterWithoutParameter _ -> True
              _ -> False) errors `shouldBe` True
          _ -> expectationFailure "Expected SetterWithoutParameter error"
      
      it "rejects setter with multiple parameters" $ do
        let invalidProgram = JSAstProgram
              [ JSClass noAnnot
                  (JSIdentName noAnnot "TestClass")
                  JSExtendsNone
                  noAnnot
                  [ JSClassInstanceMethod 
                      (JSPropertyAccessor 
                        (JSAccessorSet noAnnot)
                        (JSPropertyIdent noAnnot "prop")
                        noAnnot
                        (JSLCons
                          (JSLOne (JSIdentifier noAnnot "x"))
                          noAnnot
                          (JSIdentifier noAnnot "y"))
                        noAnnot
                        (JSBlock noAnnot [] noAnnot))
                  ]
                  noAnnot
                  auto
              ]
              noAnnot
        case validate invalidProgram of
          Left errors ->
            any (\err -> case err of
              SetterWithMultipleParameters _ -> True
              _ -> False) errors `shouldBe` True
          _ -> expectationFailure "Expected SetterWithMultipleParameters error"
      
      it "accepts valid getter and setter" $ do
        let validProgram = JSAstProgram
              [ JSClass noAnnot
                  (JSIdentName noAnnot "TestClass")
                  JSExtendsNone
                  noAnnot
                  [ JSClassInstanceMethod 
                      (JSPropertyAccessor 
                        (JSAccessorGet noAnnot)
                        (JSPropertyIdent noAnnot "x")
                        noAnnot
                        JSLNil
                        noAnnot
                        (JSBlock noAnnot [] noAnnot))
                  , JSClassInstanceMethod 
                      (JSPropertyAccessor 
                        (JSAccessorSet noAnnot)
                        (JSPropertyIdent noAnnot "y")
                        noAnnot
                        (JSLOne (JSIdentifier noAnnot "value"))
                        noAnnot
                        (JSBlock noAnnot [] noAnnot))
                  ]
                  noAnnot
                  auto
              ]
              noAnnot
        validate validProgram `shouldSatisfy` isRight

  describe "strict mode validation (HIGH priority)" $ do
    describe "octal literal errors" $ do
      it "rejects octal literals in strict mode" $ do
        let invalidProgram = JSAstProgram
              [ JSExpressionStatement (JSStringLiteral noAnnot "use strict") auto
              , JSExpressionStatement (JSOctal noAnnot "0123") auto
              ]
              noAnnot
        case validate invalidProgram of
          Left errors ->
            any (\err -> case err of
              InvalidOctalInStrict _ _ -> True
              _ -> False) errors `shouldBe` True
          _ -> expectationFailure "Expected InvalidOctalInStrict error"
      
      it "accepts octal literals outside strict mode" $ do
        let validProgram = JSAstProgram
              [ JSExpressionStatement (JSOctal noAnnot "0123") auto
              ]
              noAnnot
        validate validProgram `shouldSatisfy` isRight
    
    describe "delete identifier errors" $ do  
      it "rejects delete of unqualified identifier in strict mode" $ do
        let invalidProgram = JSAstProgram
              [ JSExpressionStatement (JSStringLiteral noAnnot "use strict") auto
              , JSExpressionStatement 
                  (JSUnaryExpression (JSUnaryOpDelete noAnnot) (JSIdentifier noAnnot "x"))
                  auto
              ]
              noAnnot
        case validate invalidProgram of
          Left errors ->
            any (\err -> case err of
              DeleteOfUnqualifiedInStrict _ -> True
              _ -> False) errors `shouldBe` True
          _ -> expectationFailure "Expected DeleteOfUnqualifiedInStrict error"
      
      it "accepts delete of property in strict mode" $ do
        let validProgram = JSAstProgram
              [ JSExpressionStatement (JSStringLiteral noAnnot "use strict") auto
              , JSExpressionStatement 
                  (JSUnaryExpression (JSUnaryOpDelete noAnnot) 
                    (JSMemberDot (JSIdentifier noAnnot "obj") noAnnot (JSIdentifier noAnnot "prop")))
                  auto
              ]
              noAnnot
        validate validProgram `shouldSatisfy` isRight
    
    describe "duplicate object property errors" $ do
      it "rejects duplicate object properties in strict mode" $ do
        let invalidProgram = JSAstProgram
              [ JSExpressionStatement (JSStringLiteral noAnnot "use strict") auto
              , JSExpressionStatement 
                  (JSObjectLiteral noAnnot
                    (JSCTLNone (JSLCons
                      (JSLOne (JSPropertyNameandValue 
                        (JSPropertyIdent noAnnot "prop")
                        noAnnot
                        [JSDecimal noAnnot "1"]))
                      noAnnot
                      (JSPropertyNameandValue 
                        (JSPropertyIdent noAnnot "prop")
                        noAnnot
                        [JSDecimal noAnnot "2"])))
                    noAnnot)
                  auto
              ]
              noAnnot
        case validate invalidProgram of
          Left errors ->
            any (\err -> case err of
              DuplicatePropertyInStrict _ _ -> True
              _ -> False) errors `shouldBe` True
          _ -> expectationFailure "Expected DuplicatePropertyInStrict error"
      
      it "accepts unique object properties in strict mode" $ do
        let validProgram = JSAstProgram
              [ JSExpressionStatement (JSStringLiteral noAnnot "use strict") auto
              , JSExpressionStatement 
                  (JSObjectLiteral noAnnot
                    (JSCTLNone (JSLCons
                      (JSLOne (JSPropertyNameandValue 
                        (JSPropertyIdent noAnnot "prop1")
                        noAnnot
                        [JSDecimal noAnnot "1"]))
                      noAnnot
                      (JSPropertyNameandValue 
                        (JSPropertyIdent noAnnot "prop2")
                        noAnnot
                        [JSDecimal noAnnot "2"])))
                    noAnnot)
                  auto
              ]
              noAnnot
        validate validProgram `shouldSatisfy` isRight
    
    describe "reserved word errors" $ do
      it "rejects 'arguments' as identifier in strict mode" $ do
        let invalidProgram = JSAstProgram
              [ JSExpressionStatement (JSStringLiteral noAnnot "use strict") auto
              , JSVariable noAnnot
                  (JSLOne (JSVarInitExpression
                    (JSIdentifier noAnnot "arguments")
                    (JSVarInit noAnnot (JSDecimal noAnnot "42"))))
                  auto
              ]
              noAnnot
        case validate invalidProgram of
          Left errors ->
            any (\err -> case err of
              ReservedWordAsIdentifier "arguments" _ -> True
              _ -> False) errors `shouldBe` True
          _ -> expectationFailure "Expected ReservedWordAsIdentifier error"
      
      it "rejects 'eval' as identifier in strict mode" $ do
        let invalidProgram = JSAstProgram
              [ JSExpressionStatement (JSStringLiteral noAnnot "use strict") auto
              , JSVariable noAnnot
                  (JSLOne (JSVarInitExpression
                    (JSIdentifier noAnnot "eval")
                    (JSVarInit noAnnot (JSDecimal noAnnot "42"))))
                  auto
              ]
              noAnnot
        case validate invalidProgram of
          Left errors ->
            any (\err -> case err of
              ReservedWordAsIdentifier "eval" _ -> True
              _ -> False) errors `shouldBe` True
          _ -> expectationFailure "Expected ReservedWordAsIdentifier error"
      
      it "rejects future reserved words in strict mode" $ do
        let invalidProgram = JSAstProgram
              [ JSExpressionStatement (JSStringLiteral noAnnot "use strict") auto
              , JSVariable noAnnot
                  (JSLOne (JSVarInitExpression
                    (JSIdentifier noAnnot "implements")
                    (JSVarInit noAnnot (JSDecimal noAnnot "42"))))
                  auto
              ]
              noAnnot
        case validate invalidProgram of
          Left errors ->
            any (\err -> case err of
              FutureReservedWord "implements" _ -> True
              _ -> False) errors `shouldBe` True
          _ -> expectationFailure "Expected FutureReservedWord error"
      
      it "accepts standard identifiers in strict mode" $ do
        let validProgram = JSAstProgram
              [ JSExpressionStatement (JSStringLiteral noAnnot "use strict") auto
              , JSVariable noAnnot
                  (JSLOne (JSVarInitExpression
                    (JSIdentifier noAnnot "validName")
                    (JSVarInit noAnnot (JSDecimal noAnnot "42"))))
                  auto
              ]
              noAnnot
        validate validProgram `shouldSatisfy` isRight
    
    describe "duplicate parameter errors" $ do
      it "rejects duplicate function parameters in strict mode" $ do
        let invalidProgram = JSAstProgram
              [ JSFunction noAnnot
                  (JSIdentName noAnnot "test")
                  noAnnot
                  (JSLCons
                    (JSLOne (JSIdentifier noAnnot "x"))
                    noAnnot
                    (JSIdentifier noAnnot "x"))
                  noAnnot
                  (JSBlock noAnnot 
                    [ JSExpressionStatement (JSStringLiteral noAnnot "use strict") auto
                    ]
                    noAnnot)
                  auto
              ]
              noAnnot
        case validate invalidProgram of
          Left errors ->
            any (\err -> case err of
              DuplicateParameter "x" _ -> True
              _ -> False) errors `shouldBe` True
          _ -> expectationFailure "Expected DuplicateParameter error"
      
      it "accepts unique function parameters in strict mode" $ do
        let validProgram = JSAstProgram
              [ JSFunction noAnnot
                  (JSIdentName noAnnot "test")
                  noAnnot
                  (JSLCons
                    (JSLOne (JSIdentifier noAnnot "x"))
                    noAnnot
                    (JSIdentifier noAnnot "y"))
                  noAnnot
                  (JSBlock noAnnot 
                    [ JSExpressionStatement (JSStringLiteral noAnnot "use strict") auto
                    ]
                    noAnnot)
                  auto
              ]
              noAnnot
        validate validProgram `shouldSatisfy` isRight
    
    describe "module strict mode" $ do
      it "treats ES6 modules as automatically strict" $ do
        let moduleProgram = JSAstModule
              [ JSModuleExportDeclaration noAnnot
                  (JSExportFrom 
                    (JSExportClause noAnnot JSLNil noAnnot)
                    (JSFromClause noAnnot noAnnot "./module")
                    auto)
              , JSModuleStatementListItem 
                  (JSExpressionStatement (JSOctal noAnnot "0123") auto)
              ]
              noAnnot
        case validate moduleProgram of
          Left errors ->
            any (\err -> case err of
              InvalidOctalInStrict _ _ -> True
              _ -> False) errors `shouldBe` True
          _ -> expectationFailure "Expected InvalidOctalInStrict error in module"
      
      it "validates import/export in module context" $ do
        let validModule = JSAstModule
              [ JSModuleImportDeclaration noAnnot
                  (JSImportDeclarationBare
                    noAnnot
                    "react"
                    Nothing
                    auto)
              , JSModuleExportDeclaration noAnnot
                  (JSExportFrom 
                    (JSExportClause noAnnot JSLNil noAnnot)
                    (JSFromClause noAnnot noAnnot "./module")
                    auto)
              ]
              noAnnot
        validate validModule `shouldSatisfy` isRight