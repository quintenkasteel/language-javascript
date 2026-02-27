{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for modern JavaScript features parser.
--
-- This module contains comprehensive tests for all modern JavaScript language
-- features implemented in Phase 4 of the flatparse migration:
--
--   * Async/await functions and expressions
--   * Generator functions and yield expressions
--   * ES6 module system (import/export)
--   * Destructuring patterns (arrays and objects)
--   * Spread syntax and rest parameters
--   * Private class fields and methods
--
-- The tests verify both successful parsing and proper error handling,
-- ensuring compliance with ECMAScript specifications.

module Test.Unit.Language.Javascript.Parser.Flatparse.ModernTest
  ( tests )
where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.Flatparse.Modern
import Language.JavaScript.Parser.Flatparse.Parser
import Language.JavaScript.Parser.Flatparse.Pos

-- | All modern JavaScript feature tests.
tests :: Spec
tests = describe "Modern JavaScript Features" $ do
  describe "Async/Await" asyncAwaitTests
  describe "Generators" generatorTests
  describe "Module System" moduleSystemTests
  describe "Destructuring" destructuringTests
  describe "Spread Syntax" spreadTests
  describe "Private Fields" privateFieldTests

-- ---------------------------------------------------------------------
-- Async/Await Tests
-- ---------------------------------------------------------------------

asyncAwaitTests :: Spec
asyncAwaitTests = do
  describe "async function declarations" $ do
    it "parses simple async function" $ do
      let input = "async function fetchData() { return await fetch('/api'); }"
      case parseAsyncFunction input of
        Right (JSAsyncFunctionDeclaration _ name params _) -> do
          name `shouldBe` "fetchData"
          Vector.length params `shouldBe` 0
        Left err -> expectationFailure $ "Parse failed: " ++ show err

    it "parses async function with parameters" $ do
      let input = "async function getData(url, options) { return await fetch(url, options); }"
      case parseAsyncFunction input of
        Right (JSAsyncFunctionDeclaration _ name params _) -> do
          name `shouldBe` "getData"
          Vector.length params `shouldBe` 2
        Left err -> expectationFailure $ "Parse failed: " ++ show err

  describe "async function expressions" $ do
    it "parses async function expression" $ do
      let input = "async function() { return await Promise.resolve(42); }"
      case parseAsyncFunctionExpression input of
        Right (JSAsyncFunctionExpression _ name params _) -> do
          name `shouldBe` Nothing
          Vector.length params `shouldBe` 0
        Left err -> expectationFailure $ "Parse failed: " ++ show err

    it "parses named async function expression" $ do
      let input = "async function helper() { return await doWork(); }"
      case parseAsyncFunctionExpression input of
        Right (JSAsyncFunctionExpression _ name params _) -> do
          name `shouldBe` Just "helper"
          Vector.length params `shouldBe` 0
        Left err -> expectationFailure $ "Parse failed: " ++ show err

  describe "await expressions" $ do
    it "parses simple await expression" $ do
      let input = "await promise"
      case parseAwaitExpression input of
        Right (JSAwaitExpression _ expr) ->
          case expr of
            JSIdentifier _ name -> name `shouldBe` "promise"
            _ -> expectationFailure "Expected identifier"
        Left err -> expectationFailure $ "Parse failed: " ++ show err

    it "parses await with function call" $ do
      let input = "await fetch('/api/data')"
      case parseAwaitExpression input of
        Right (JSAwaitExpression _ expr) ->
          case expr of
            JSCallExpression _ func args -> do
              case func of
                JSIdentifier _ name -> name `shouldBe` "fetch"
                _ -> expectationFailure "Expected identifier"
              Vector.length args `shouldBe` 1
            _ -> expectationFailure "Expected function call"
        Left err -> expectationFailure $ "Parse failed: " ++ show err

-- ---------------------------------------------------------------------
-- Generator Tests
-- ---------------------------------------------------------------------

generatorTests :: Spec
generatorTests = do
  describe "generator function declarations" $ do
    it "parses simple generator function" $ do
      let input = "function* numbers() { yield 1; yield 2; }"
      case parseGeneratorFunction input of
        Right (JSGeneratorFunctionDeclaration _ name params _) -> do
          name `shouldBe` "numbers"
          Vector.length params `shouldBe` 0
        Left err -> expectationFailure $ "Parse failed: " ++ show err

    it "parses generator with parameters" $ do
      let input = "function* fibonacci(start, count) { yield start; }"
      case parseGeneratorFunction input of
        Right (JSGeneratorFunctionDeclaration _ name params _) -> do
          name `shouldBe` "fibonacci"
          Vector.length params `shouldBe` 2
        Left err -> expectationFailure $ "Parse failed: " ++ show err

  describe "generator function expressions" $ do
    it "parses anonymous generator expression" $ do
      let input = "function*() { yield 42; }"
      case parseGeneratorFunctionExpression input of
        Right (JSGeneratorFunctionExpression _ name params _) -> do
          name `shouldBe` Nothing
          Vector.length params `shouldBe` 0
        Left err -> expectationFailure $ "Parse failed: " ++ show err

  describe "yield expressions" $ do
    it "parses simple yield" $ do
      let input = "yield value"
      case parseYieldExpression input of
        Right (JSYieldExpression _ expr) ->
          case expr of
            Just (JSIdentifier _ name) -> name `shouldBe` "value"
            _ -> expectationFailure "Expected identifier"
        Left err -> expectationFailure $ "Parse failed: " ++ show err

    it "parses yield delegation" $ do
      let input = "yield* otherGenerator()"
      case parseYieldExpression input of
        Right (JSYieldDelegation _ expr) ->
          case expr of
            JSCallExpression _ func _ ->
              case func of
                JSIdentifier _ name -> name `shouldBe` "otherGenerator"
                _ -> expectationFailure "Expected identifier"
            _ -> expectationFailure "Expected function call"
        Left err -> expectationFailure $ "Parse failed: " ++ show err

-- ---------------------------------------------------------------------
-- Module System Tests
-- ---------------------------------------------------------------------

moduleSystemTests :: Spec
moduleSystemTests = do
  describe "import declarations" $ do
    it "parses default import" $ do
      let input = "import React from 'react';"
      case parseImportDeclaration input of
        Right (JSModuleStatement (JSModuleImportDeclaration (JSImportDeclaration _ specs source))) -> do
          source `shouldBe` "react"
          Vector.length specs `shouldBe` 1
          case Vector.head specs of
            JSImportDefault name -> name `shouldBe` "React"
            _ -> expectationFailure "Expected default import"
        Left err -> expectationFailure $ "Parse failed: " ++ show err

    it "parses named imports" $ do
      let input = "import { useState, useEffect } from 'react';"
      case parseImportDeclaration input of
        Right (JSModuleStatement (JSModuleImportDeclaration (JSImportDeclaration _ specs source))) -> do
          source `shouldBe` "react"
          Vector.length specs `shouldBe` 2
        Left err -> expectationFailure $ "Parse failed: " ++ show err

    it "parses namespace import" $ do
      let input = "import * as React from 'react';"
      case parseImportDeclaration input of
        Right (JSModuleStatement (JSModuleImportDeclaration (JSImportDeclaration _ specs source))) -> do
          source `shouldBe` "react"
          Vector.length specs `shouldBe` 1
          case Vector.head specs of
            JSImportNamespace name -> name `shouldBe` "React"
            _ -> expectationFailure "Expected namespace import"
        Left err -> expectationFailure $ "Parse failed: " ++ show err

  describe "export declarations" $ do
    it "parses default export" $ do
      let input = "export default MyComponent;"
      case parseExportDeclaration input of
        Right (JSModuleStatement (JSModuleExportDeclaration (JSExportDefault _ expr))) ->
          case expr of
            JSIdentifier _ name -> name `shouldBe` "MyComponent"
            _ -> expectationFailure "Expected identifier"
        Left err -> expectationFailure $ "Parse failed: " ++ show err

    it "parses named exports" $ do
      let input = "export { foo, bar };"
      case parseExportDeclaration input of
        Right (JSModuleStatement (JSModuleExportDeclaration (JSExportNamed _ specs source))) -> do
          Vector.length specs `shouldBe` 2
          source `shouldBe` Nothing
        Left err -> expectationFailure $ "Parse failed: " ++ show err

-- ---------------------------------------------------------------------
-- Destructuring Tests
-- ---------------------------------------------------------------------

destructuringTests :: Spec
destructuringTests = do
  describe "array destructuring" $ do
    it "parses simple array destructuring" $ do
      let input = "[a, b, c]"
      case parseArrayDestructuring input of
        Right (JSArrayPattern _ elements) ->
          Vector.length elements `shouldBe` 3
        Left err -> expectationFailure $ "Parse failed: " ++ show err

    it "parses array destructuring with rest" $ do
      let input = "[first, ...rest]"
      case parseArrayDestructuring input of
        Right (JSArrayPattern _ elements) -> do
          Vector.length elements `shouldBe` 2
          case Vector.last elements of
            JSDestructuringRest name -> name `shouldBe` "rest"
            _ -> expectationFailure "Expected rest element"
        Left err -> expectationFailure $ "Parse failed: " ++ show err

    it "parses array destructuring with defaults" $ do
      let input = "[a = 1, b = 2]"
      case parseArrayDestructuring input of
        Right (JSArrayPattern _ elements) -> do
          Vector.length elements `shouldBe` 2
          case Vector.head elements of
            JSDestructuringDefault name _ -> name `shouldBe` "a"
            _ -> expectationFailure "Expected default element"
        Left err -> expectationFailure $ "Parse failed: " ++ show err

  describe "object destructuring" $ do
    it "parses simple object destructuring" $ do
      let input = "{x, y}"
      case parseObjectDestructuring input of
        Right (JSObjectPattern _ properties) ->
          Vector.length properties `shouldBe` 2
        Left err -> expectationFailure $ "Parse failed: " ++ show err

    it "parses object destructuring with aliases" $ do
      let input = "{x: newX, y: newY}"
      case parseObjectDestructuring input of
        Right (JSObjectPattern _ properties) -> do
          Vector.length properties `shouldBe` 2
          case Vector.head properties of
            JSObjectPatternProp key local -> do
              key `shouldBe` "x"
              local `shouldBe` "newX"
            _ -> expectationFailure "Expected property with alias"
        Left err -> expectationFailure $ "Parse failed: " ++ show err

-- ---------------------------------------------------------------------
-- Spread Syntax Tests
-- ---------------------------------------------------------------------

spreadTests :: Spec
spreadTests = do
  describe "spread in arrays" $ do
    it "parses spread element in array" $ do
      let input = "[1, ...items, 2]"
      case parseArrayWithSpread input of
        Right (JSArrayLiteral _ elements) -> do
          Vector.length elements `shouldBe` 3
          case elements Vector.! 1 of
            JSArraySpread expr ->
              case expr of
                JSIdentifier _ name -> name `shouldBe` "items"
                _ -> expectationFailure "Expected identifier"
            _ -> expectationFailure "Expected spread element"
        Left err -> expectationFailure $ "Parse failed: " ++ show err

  describe "rest parameters" $ do
    it "parses rest parameter in function" $ do
      let input = "(a, b, ...rest)"
      case parseFunctionParameters input of
        Right params -> do
          Vector.length params `shouldBe` 3
          case Vector.last params of
            JSParameterRest _ name -> name `shouldBe` "rest"
            _ -> expectationFailure "Expected rest parameter"
        Left err -> expectationFailure $ "Parse failed: " ++ show err

-- ---------------------------------------------------------------------
-- Private Fields Tests
-- ---------------------------------------------------------------------

privateFieldTests :: Spec
privateFieldTests = do
  describe "private fields" $ do
    it "parses private field declaration" $ do
      let input = "#privateField = 42"
      case parsePrivateField input of
        Right (JSPrivateField _ name initializer) -> do
          name `shouldBe` "privateField"
          case initializer of
            Just (JSLiteral _ (JSNumericLiteral "42")) -> pure ()
            _ -> expectationFailure "Expected numeric literal"
        Left err -> expectationFailure $ "Parse failed: " ++ show err

    it "parses private method" $ do
      let input = "#privateMethod() { return this.#privateField; }"
      case parsePrivateMethod input of
        Right (JSPrivateMethod _ name params _) -> do
          name `shouldBe` "privateMethod"
          Vector.length params `shouldBe` 0
        Left err -> expectationFailure $ "Parse failed: " ++ show err

-- ---------------------------------------------------------------------
-- Helper Functions for Testing
-- ---------------------------------------------------------------------

-- | Parse async function for testing.
parseAsyncFunction :: Text -> Either String JSStatement
parseAsyncFunction input =
  case parseExpression input of
    ParseSuccess (ParseSuccess result _ _) ->
      case result of
        stmt@(JSAsyncFunctionDeclaration {}) -> Right stmt
        _ -> Left "Not an async function declaration"
    ParseFailure failure -> Left (show failure)

-- | Parse async function expression for testing.
parseAsyncFunctionExpression :: Text -> Either String JSExpression
parseAsyncFunctionExpression input =
  case parseExpression input of
    ParseSuccess (ParseSuccess result _ _) ->
      case result of
        expr@(JSAsyncFunctionExpression {}) -> Right expr
        _ -> Left "Not an async function expression"
    ParseFailure failure -> Left (show failure)

-- | Parse await expression for testing.
parseAwaitExpression :: Text -> Either String JSExpression
parseAwaitExpression input =
  case parseExpression input of
    ParseSuccess (ParseSuccess result _ _) ->
      case result of
        expr@(JSAwaitExpression {}) -> Right expr
        _ -> Left "Not an await expression"
    ParseFailure failure -> Left (show failure)

-- | Parse generator function for testing.
parseGeneratorFunction :: Text -> Either String JSStatement
parseGeneratorFunction input =
  case parseExpression input of
    ParseSuccess (ParseSuccess result _ _) ->
      case result of
        stmt@(JSGeneratorFunctionDeclaration {}) -> Right stmt
        _ -> Left "Not a generator function declaration"
    ParseFailure failure -> Left (show failure)

-- | Parse generator function expression for testing.
parseGeneratorFunctionExpression :: Text -> Either String JSExpression
parseGeneratorFunctionExpression input =
  case parseExpression input of
    ParseSuccess (ParseSuccess result _ _) ->
      case result of
        expr@(JSGeneratorFunctionExpression {}) -> Right expr
        _ -> Left "Not a generator function expression"
    ParseFailure failure -> Left (show failure)

-- | Parse yield expression for testing.
parseYieldExpression :: Text -> Either String JSExpression
parseYieldExpression input =
  case parseExpression input of
    ParseSuccess (ParseSuccess result _ _) ->
      case result of
        expr@(JSYieldExpression {}) -> Right expr
        expr@(JSYieldDelegation {}) -> Right expr
        _ -> Left "Not a yield expression"
    ParseFailure failure -> Left (show failure)

-- | Parse import declaration for testing.
parseImportDeclaration :: Text -> Either String JSStatement
parseImportDeclaration input =
  case parseExpression input of
    ParseSuccess (ParseSuccess result _ _) ->
      case result of
        stmt@(JSModuleStatement {}) -> Right stmt
        _ -> Left "Not a module statement"
    ParseFailure failure -> Left (show failure)

-- | Parse export declaration for testing.
parseExportDeclaration :: Text -> Either String JSStatement
parseExportDeclaration input =
  case parseExpression input of
    ParseSuccess (ParseSuccess result _ _) ->
      case result of
        stmt@(JSModuleStatement {}) -> Right stmt
        _ -> Left "Not a module statement"
    ParseFailure failure -> Left (show failure)

-- | Parse array destructuring for testing.
parseArrayDestructuring :: Text -> Either String JSDestructuringPattern
parseArrayDestructuring input =
  case parseExpression input of
    ParseSuccess (ParseSuccess result _ _) ->
      case result of
        JSDestructuringAssignment _ pattern _ -> Right pattern
        _ -> Left "Not a destructuring assignment"
    ParseFailure failure -> Left (show failure)

-- | Parse object destructuring for testing.
parseObjectDestructuring :: Text -> Either String JSDestructuringPattern
parseObjectDestructuring input =
  case parseExpression input of
    ParseSuccess (ParseSuccess result _ _) ->
      case result of
        JSDestructuringAssignment _ pattern _ -> Right pattern
        _ -> Left "Not a destructuring assignment"
    ParseFailure failure -> Left (show failure)

-- | Parse array with spread for testing.
parseArrayWithSpread :: Text -> Either String JSExpression
parseArrayWithSpread input =
  case parseExpression input of
    ParseSuccess (ParseSuccess result _ _) ->
      case result of
        expr@(JSArrayLiteral {}) -> Right expr
        _ -> Left "Not an array literal"
    ParseFailure failure -> Left (show failure)

-- | Parse function parameters for testing.
parseFunctionParameters :: Text -> Either String (Vector JSParameter)
parseFunctionParameters input =
  case parseExpression input of
    ParseSuccess (ParseSuccess result _ _) ->
      case result of
        JSFunctionExpression _ _ params _ -> Right params
        _ -> Left "Not a function expression"
    ParseFailure failure -> Left (show failure)

-- | Parse private field for testing.
parsePrivateField :: Text -> Either String JSMethodDefinition
parsePrivateField input =
  case parseExpression input of
    ParseSuccess (ParseSuccess result _ _) ->
      case result of
        _ -> Left "Private field parsing not implemented"
    ParseFailure failure -> Left (show failure)

-- | Parse private method for testing.
parsePrivateMethod :: Text -> Either String JSMethodDefinition
parsePrivateMethod input =
  case parseExpression input of
    ParseSuccess (ParseSuccess result _ _) ->
      case result of
        _ -> Left "Private method parsing not implemented"
    ParseFailure failure -> Left (show failure)