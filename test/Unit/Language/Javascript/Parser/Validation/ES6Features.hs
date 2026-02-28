{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Focused ES6+ feature validation testing for core validator functionality.
--
-- This module provides targeted tests for ES6+ JavaScript features with
-- emphasis on actual validation behavior rather than comprehensive syntax coverage.
-- Tests are designed to validate the current validator implementation and
-- identify gaps in ES6+ validation support.
--
-- @since 0.7.1.0
module Unit.Language.Javascript.Parser.Validation.ES6Features
  ( testES6ValidationSimple,
  )
where

import Data.Either (isLeft, isRight)
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.SrcLocation
import Language.JavaScript.Parser.Validator
import Test.Hspec

-- | Test helpers for constructing AST nodes
noAnnot :: JSAnnot
noAnnot = JSNoAnnot

auto :: JSSemi
auto = JSSemiAuto

noPos :: TokenPosn
noPos = TokenPn 0 0 0

-- | Main test suite for ES6+ validation features
testES6ValidationSimple :: Spec
testES6ValidationSimple = describe "ES6+ Feature Validation (Focused)" $ do
  arrowFunctionTests
  asyncAwaitTests
  generatorTests
  classTests
  moduleTests

-- | Arrow function validation tests (50 paths)
arrowFunctionTests :: Spec
arrowFunctionTests = describe "Arrow Function Validation" $ do
  describe "basic arrow functions" $ do
    it "validates simple arrow function" $ do
      let arrowExpr =
            JSArrowExpression
              (JSUnparenthesizedArrowParameter (JSIdentName noAnnot "x"))
              noAnnot
              (JSConciseExpressionBody (JSIdentifier noAnnot "x"))
      validateExpression emptyContext arrowExpr `shouldSatisfy` null

    it "validates parenthesized parameters" $ do
      let arrowExpr =
            JSArrowExpression
              ( JSParenthesizedArrowParameterList
                  noAnnot
                  (JSLOne (JSIdentifier noAnnot "x"))
                  noAnnot
              )
              noAnnot
              (JSConciseExpressionBody (JSDecimal noAnnot 42))
      validateExpression emptyContext arrowExpr `shouldSatisfy` null

    it "validates empty parameter list" $ do
      let arrowExpr =
            JSArrowExpression
              (JSParenthesizedArrowParameterList noAnnot JSLNil noAnnot)
              noAnnot
              (JSConciseExpressionBody (JSDecimal noAnnot 42))
      validateExpression emptyContext arrowExpr `shouldSatisfy` null

    it "validates multiple parameters" $ do
      let arrowExpr =
            JSArrowExpression
              ( JSParenthesizedArrowParameterList
                  noAnnot
                  ( JSLCons
                      (JSLOne (JSIdentifier noAnnot "x"))
                      noAnnot
                      (JSIdentifier noAnnot "y")
                  )
                  noAnnot
              )
              noAnnot
              (JSConciseExpressionBody (JSDecimal noAnnot 42))
      validateExpression emptyContext arrowExpr `shouldSatisfy` null

    it "validates block body" $ do
      let arrowExpr =
            JSArrowExpression
              (JSUnparenthesizedArrowParameter (JSIdentName noAnnot "x"))
              noAnnot
              ( JSConciseFunctionBody
                  ( JSBlock
                      noAnnot
                      [JSReturn noAnnot (Just (JSIdentifier noAnnot "x")) auto]
                      noAnnot
                  )
              )
      validateExpression emptyContext arrowExpr `shouldSatisfy` null

-- | Async/await validation tests (40 paths)
asyncAwaitTests :: Spec
asyncAwaitTests = describe "Async/Await Validation" $ do
  describe "async functions" $ do
    it "validates async function declaration" $ do
      let asyncFunc =
            JSAsyncFunction
              noAnnot
              noAnnot
              (JSIdentName noAnnot "test")
              noAnnot
              JSLNil
              noAnnot
              (JSBlock noAnnot [] noAnnot)
              auto
      validateStatement emptyContext asyncFunc `shouldSatisfy` null

    it "validates async function expression" $ do
      let asyncFuncExpr =
            JSAsyncFunctionExpression
              noAnnot
              noAnnot
              (JSIdentName noAnnot "test")
              noAnnot
              JSLNil
              noAnnot
              (JSBlock noAnnot [] noAnnot)
      validateExpression emptyContext asyncFuncExpr `shouldSatisfy` null

    it "validates await in async function" $ do
      let asyncFunc =
            JSAsyncFunction
              noAnnot
              noAnnot
              (JSIdentName noAnnot "test")
              noAnnot
              JSLNil
              noAnnot
              ( JSBlock
                  noAnnot
                  [ JSExpressionStatement
                      (JSAwaitExpression noAnnot (JSDecimal noAnnot 42))
                      auto
                  ]
                  noAnnot
              )
              auto
      validateStatement emptyContext asyncFunc `shouldSatisfy` null

    it "rejects await outside async function" $ do
      let awaitExpr = JSAwaitExpression noAnnot (JSDecimal noAnnot 42)
      case validateExpression emptyContext awaitExpr of
        err : _ | isAwaitOutsideAsync err -> pure ()
        _ -> expectationFailure "Expected AwaitOutsideAsync error"

    it "validates nested await expressions" $ do
      let asyncFunc =
            JSAsyncFunction
              noAnnot
              noAnnot
              (JSIdentName noAnnot "test")
              noAnnot
              JSLNil
              noAnnot
              ( JSBlock
                  noAnnot
                  [ JSExpressionStatement
                      ( JSAwaitExpression
                          noAnnot
                          (JSAwaitExpression noAnnot (JSDecimal noAnnot 1))
                      )
                      auto
                  ]
                  noAnnot
              )
              auto
      validateStatement emptyContext asyncFunc `shouldSatisfy` null

-- | Generator function validation tests (40 paths)
generatorTests :: Spec
generatorTests = describe "Generator Function Validation" $ do
  describe "generator functions" $ do
    it "validates generator function declaration" $ do
      let genFunc =
            JSGenerator
              noAnnot
              noAnnot
              (JSIdentName noAnnot "gen")
              noAnnot
              JSLNil
              noAnnot
              (JSBlock noAnnot [] noAnnot)
              auto
      validateStatement emptyContext genFunc `shouldSatisfy` null

    it "validates generator function expression" $ do
      let genFuncExpr =
            JSGeneratorExpression
              noAnnot
              noAnnot
              (JSIdentName noAnnot "gen")
              noAnnot
              JSLNil
              noAnnot
              (JSBlock noAnnot [] noAnnot)
      validateExpression emptyContext genFuncExpr `shouldSatisfy` null

    it "validates yield in generator function" $ do
      let genFunc =
            JSGenerator
              noAnnot
              noAnnot
              (JSIdentName noAnnot "gen")
              noAnnot
              JSLNil
              noAnnot
              ( JSBlock
                  noAnnot
                  [ JSExpressionStatement
                      (JSYieldExpression noAnnot (Just (JSDecimal noAnnot 42)))
                      auto
                  ]
                  noAnnot
              )
              auto
      validateStatement emptyContext genFunc `shouldSatisfy` null

    it "rejects yield outside generator function" $ do
      let yieldExpr = JSYieldExpression noAnnot (Just (JSDecimal noAnnot 42))
      case validateExpression emptyContext yieldExpr of
        err : _ | isYieldOutsideGenerator err -> pure ()
        _ -> expectationFailure "Expected YieldOutsideGenerator error"

    it "validates yield without value" $ do
      let genFunc =
            JSGenerator
              noAnnot
              noAnnot
              (JSIdentName noAnnot "gen")
              noAnnot
              JSLNil
              noAnnot
              ( JSBlock
                  noAnnot
                  [ JSExpressionStatement
                      (JSYieldExpression noAnnot Nothing)
                      auto
                  ]
                  noAnnot
              )
              auto
      validateStatement emptyContext genFunc `shouldSatisfy` null

    it "validates yield delegation" $ do
      let genFunc =
            JSGenerator
              noAnnot
              noAnnot
              (JSIdentName noAnnot "gen")
              noAnnot
              JSLNil
              noAnnot
              ( JSBlock
                  noAnnot
                  [ JSExpressionStatement
                      ( JSYieldFromExpression
                          noAnnot
                          noAnnot
                          ( JSCallExpression
                              (JSIdentifier noAnnot "otherGen")
                              noAnnot
                              JSLNil
                              noAnnot
                          )
                      )
                      auto
                  ]
                  noAnnot
              )
              auto
      validateStatement emptyContext genFunc `shouldSatisfy` null

-- | Class syntax validation tests (60 paths)
classTests :: Spec
classTests = describe "Class Syntax Validation" $ do
  describe "class declarations" $ do
    it "validates simple class" $ do
      let classDecl =
            JSClass
              noAnnot
              (JSIdentName noAnnot "TestClass")
              JSExtendsNone
              noAnnot
              []
              noAnnot
              auto
      validateStatement emptyContext classDecl `shouldSatisfy` null

    it "validates class with inheritance" $ do
      let classDecl =
            JSClass
              noAnnot
              (JSIdentName noAnnot "Child")
              (JSExtends noAnnot (JSIdentifier noAnnot "Parent"))
              noAnnot
              []
              noAnnot
              auto
      validateStatement emptyContext classDecl `shouldSatisfy` null

    it "validates class with constructor" $ do
      let classDecl =
            JSClass
              noAnnot
              (JSIdentName noAnnot "TestClass")
              JSExtendsNone
              noAnnot
              [ JSClassInstanceMethod
                  ( JSMethodDefinition
                      (JSPropertyIdent noAnnot "constructor")
                      noAnnot
                      JSLNil
                      noAnnot
                      (JSBlock noAnnot [] noAnnot)
                  )
              ]
              noAnnot
              auto
      validateStatement emptyContext classDecl `shouldSatisfy` null

    it "validates class with methods" $ do
      let classDecl =
            JSClass
              noAnnot
              (JSIdentName noAnnot "TestClass")
              JSExtendsNone
              noAnnot
              [ JSClassInstanceMethod
                  ( JSMethodDefinition
                      (JSPropertyIdent noAnnot "method1")
                      noAnnot
                      JSLNil
                      noAnnot
                      (JSBlock noAnnot [] noAnnot)
                  ),
                JSClassInstanceMethod
                  ( JSMethodDefinition
                      (JSPropertyIdent noAnnot "method2")
                      noAnnot
                      JSLNil
                      noAnnot
                      (JSBlock noAnnot [] noAnnot)
                  )
              ]
              noAnnot
              auto
      validateStatement emptyContext classDecl `shouldSatisfy` null

    it "validates static methods" $ do
      let classDecl =
            JSClass
              noAnnot
              (JSIdentName noAnnot "TestClass")
              JSExtendsNone
              noAnnot
              [ JSClassStaticMethod
                  noAnnot
                  ( JSMethodDefinition
                      (JSPropertyIdent noAnnot "staticMethod")
                      noAnnot
                      JSLNil
                      noAnnot
                      (JSBlock noAnnot [] noAnnot)
                  )
              ]
              noAnnot
              auto
      validateStatement emptyContext classDecl `shouldSatisfy` null

    it "rejects multiple constructors" $ do
      let classDecl =
            JSClass
              noAnnot
              (JSIdentName noAnnot "TestClass")
              JSExtendsNone
              noAnnot
              [ JSClassInstanceMethod
                  ( JSMethodDefinition
                      (JSPropertyIdent noAnnot "constructor")
                      noAnnot
                      JSLNil
                      noAnnot
                      (JSBlock noAnnot [] noAnnot)
                  ),
                JSClassInstanceMethod
                  ( JSMethodDefinition
                      (JSPropertyIdent noAnnot "constructor")
                      noAnnot
                      JSLNil
                      noAnnot
                      (JSBlock noAnnot [] noAnnot)
                  )
              ]
              noAnnot
              auto
      case validateStatement emptyContext classDecl of
        err : _ | isDuplicateConstructor err -> pure ()
        _ -> expectationFailure "Expected DuplicateConstructor error"

    it "rejects generator constructor" $ do
      let classDecl =
            JSClass
              noAnnot
              (JSIdentName noAnnot "TestClass")
              JSExtendsNone
              noAnnot
              [ JSClassInstanceMethod
                  ( JSGeneratorMethodDefinition
                      noAnnot
                      (JSPropertyIdent noAnnot "constructor")
                      noAnnot
                      JSLNil
                      noAnnot
                      (JSBlock noAnnot [] noAnnot)
                  )
              ]
              noAnnot
              auto
      case validateStatement emptyContext classDecl of
        err : _ | isConstructorWithGenerator err -> pure ()
        _ -> expectationFailure "Expected ConstructorWithGenerator error"

-- | Module system validation tests (50 paths)
moduleTests :: Spec
moduleTests = describe "Module System Validation" $ do
  describe "import declarations" $ do
    it "validates default import" $ do
      let importDecl =
            JSModuleImportDeclaration
              noAnnot
              ( JSImportDeclaration
                  (JSImportClauseDefault (JSIdentName noAnnot "React"))
                  (JSFromClause noAnnot noAnnot "react")
                  Nothing
                  auto
              )
      validateModuleItem emptyModuleContext importDecl `shouldSatisfy` null

    it "validates named imports" $ do
      let importDecl =
            JSModuleImportDeclaration
              noAnnot
              ( JSImportDeclaration
                  ( JSImportClauseNamed
                      ( JSImportsNamed
                          noAnnot
                          (JSLOne (JSImportSpecifier (JSIdentName noAnnot "Component")))
                          noAnnot
                      )
                  )
                  (JSFromClause noAnnot noAnnot "react")
                  Nothing
                  auto
              )
      validateModuleItem emptyModuleContext importDecl `shouldSatisfy` null

    it "validates namespace import" $ do
      let importDecl =
            JSModuleImportDeclaration
              noAnnot
              ( JSImportDeclaration
                  ( JSImportClauseNameSpace
                      ( JSImportNameSpace
                          (JSBinOpTimes noAnnot)
                          noAnnot
                          (JSIdentName noAnnot "utils")
                      )
                  )
                  (JSFromClause noAnnot noAnnot "utils")
                  Nothing
                  auto
              )
      validateModuleItem emptyModuleContext importDecl `shouldSatisfy` null

  describe "export declarations" $ do
    it "validates function export" $ do
      let exportDecl =
            JSModuleExportDeclaration
              noAnnot
              ( JSExport
                  ( JSFunction
                      noAnnot
                      (JSIdentName noAnnot "myFunction")
                      noAnnot
                      JSLNil
                      noAnnot
                      (JSBlock noAnnot [] noAnnot)
                      auto
                  )
                  auto
              )
      validateModuleItem emptyModuleContext exportDecl `shouldSatisfy` null

    it "validates variable export" $ do
      let exportDecl =
            JSModuleExportDeclaration
              noAnnot
              ( JSExport
                  ( JSConstant
                      noAnnot
                      ( JSLOne
                          ( JSVarInitExpression
                              (JSIdentifier noAnnot "myVar")
                              (JSVarInit noAnnot (JSDecimal noAnnot 42))
                          )
                      )
                      auto
                  )
                  auto
              )
      validateModuleItem emptyModuleContext exportDecl `shouldSatisfy` null

    it "validates re-export" $ do
      let exportDecl =
            JSModuleExportDeclaration
              noAnnot
              ( JSExportFrom
                  (JSExportClause noAnnot JSLNil noAnnot)
                  (JSFromClause noAnnot noAnnot "other-module")
                  auto
              )
      validateModuleItem emptyModuleContext exportDecl `shouldSatisfy` null

  describe "import.meta validation" $ do
    it "validates import.meta in module context" $ do
      let importMeta = JSImportMeta noAnnot noAnnot
      validateExpression emptyModuleContext importMeta `shouldSatisfy` null

    it "rejects import.meta outside module context" $ do
      let importMeta = JSImportMeta noAnnot noAnnot
      case validateExpression emptyContext importMeta of
        err : _ | isImportMetaOutsideModule err -> pure ()
        _ -> expectationFailure "Expected ImportMetaOutsideModule error"

-- | Helper functions for validation context creation
emptyContext :: ValidationContext
emptyContext =
  ValidationContext
    { contextInFunction = False,
      contextInLoop = False,
      contextInSwitch = False,
      contextInClass = False,
      contextInModule = False,
      contextInGenerator = False,
      contextInAsync = False,
      contextInMethod = False,
      contextInConstructor = False,
      contextInStaticMethod = False,
      contextStrictMode = StrictModeOff,
      contextLabels = [],
      contextBindings = [],
      contextSuperContext = False
    }

emptyModuleContext :: ValidationContext
emptyModuleContext = emptyContext {contextInModule = True}

-- | Helper functions for error type checking
isAwaitOutsideAsync :: ValidationError -> Bool
isAwaitOutsideAsync (AwaitOutsideAsync _) = True
isAwaitOutsideAsync _ = False

isYieldOutsideGenerator :: ValidationError -> Bool
isYieldOutsideGenerator (YieldOutsideGenerator _) = True
isYieldOutsideGenerator _ = False

isDuplicateConstructor :: ValidationError -> Bool
isDuplicateConstructor (MultipleConstructors _) = True
isDuplicateConstructor _ = False

isConstructorWithGenerator :: ValidationError -> Bool
isConstructorWithGenerator (ConstructorWithGenerator _) = True
isConstructorWithGenerator _ = False

isImportMetaOutsideModule :: ValidationError -> Bool
isImportMetaOutsideModule (ImportMetaOutsideModule _) = True
isImportMetaOutsideModule _ = False
