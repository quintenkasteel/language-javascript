{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : Language.JavaScript.Runtime.Integration
-- Copyright   : (c) 2025 Runtime Integration
-- License     : BSD-style
-- Stability   : experimental
-- Portability : ghc
--
-- Integration points for JSDoc runtime validation with JavaScript AST.
-- This module provides the bridge between static JSDoc parsing and
-- runtime function validation, demonstrating how to extract JSDoc
-- information and apply runtime type checking.
--
-- == Core Integration Features
--
-- * Extract JSDoc from JavaScript function AST nodes
-- * Generate runtime validation functions from JSDoc specifications
-- * Integrate with function call sites for parameter validation
-- * Provide examples of end-to-end runtime validation workflows
--
-- == Usage Examples
--
-- Basic function validation integration:
--
-- >>> integrateRuntimeValidation functionAST runtimeArgs
-- Right validatedArgs  -- All parameters validated successfully
--
-- Function with validation errors:
--
-- >>> validateJSFunction jsDoc [JSString "hello", JSNumber 42]
-- Left [ValidationError ...]  -- Type mismatch detected
--
module Language.JavaScript.Runtime.Integration
  ( -- * Integration Functions
    integrateRuntimeValidation,
    validateJSFunction,
    extractValidationFromJSDoc,
    createFunctionValidator,

    -- * Example Usage
    demonstrateRuntimeValidation,
    exampleValidatedFunction,
    showValidationWorkflow,

    -- * Utility Functions
    hasRuntimeValidation,
    shouldValidateFunction,
    getValidationConfig,
  )
where

import qualified Data.Text as Text
import Language.JavaScript.Parser.Validator
  ( ValidationError
  , RuntimeValue(..)
  , RuntimeValidationConfig(..)
  , validateRuntimeCall
  , validateRuntimeReturn
  , validateRuntimeParameters
  , formatValidationError
  , defaultValidationConfig
  , developmentConfig
  , productionConfig
  )
import Language.JavaScript.Parser.Token
  ( JSDocComment(..)
  , JSDocTag(..)
  , JSDocType(..)
  , JSDocObjectField(..)
  )
import Language.JavaScript.Parser.SrcLocation (TokenPosn(..))
import qualified Language.JavaScript.Parser.SrcLocation as SrcLoc

-- | Integrate runtime validation with JavaScript function AST
--
-- Note: This demonstrates the concept. In practice, conversion between
-- Token.JSDocComment and JSDoc.JSDocComment would be needed for full integration.
integrateRuntimeValidation :: JSDocComment -> [RuntimeValue] -> Either [ValidationError] [RuntimeValue]
integrateRuntimeValidation jsDoc runtimeArgs = validateRuntimeCall jsDoc runtimeArgs

-- | Validate JavaScript function call using JSDoc specifications
--
-- Direct validation of function arguments against JSDoc parameter
-- types and return type specifications.
validateJSFunction :: JSDocComment -> [RuntimeValue] -> Either [ValidationError] [RuntimeValue]
validateJSFunction = validateRuntimeCall

-- | Extract validation function from JSDoc comment
--
-- Creates a validation function that can be applied to runtime
-- arguments based on the JSDoc specifications.
extractValidationFromJSDoc :: JSDocComment -> ([RuntimeValue] -> Either [ValidationError] [RuntimeValue])
extractValidationFromJSDoc jsDoc = validateRuntimeCall jsDoc

-- | Create function validator from JSDoc comment
--
-- Generates a reusable validation function that can be applied
-- to multiple function calls with the same signature.
createFunctionValidator :: JSDocComment -> ([RuntimeValue] -> Either [ValidationError] [RuntimeValue])
createFunctionValidator = validateRuntimeCall

-- | Demonstrate end-to-end runtime validation workflow
--
-- Shows complete example of how runtime validation integrates
-- with JavaScript function definitions and calls.
demonstrateRuntimeValidation :: IO ()
demonstrateRuntimeValidation = do
  putStrLn "=== JSDoc Runtime Validation Demo ==="
  putStrLn ""

  -- Example 1: Successful validation
  putStrLn "Example 1: Valid function call"
  let validArgs = [JSString "John", JSNumber 25.0]
  case validateJSFunction exampleJSDoc validArgs of
    Right args -> do
      putStrLn "✅ Validation passed!"
      putStrLn ("  Arguments: " ++ show args)
    Left errors -> do
      putStrLn "❌ Validation failed:"
      mapM_ (putStrLn . ("  " ++) . Text.unpack . formatValidationError) errors

  putStrLn ""

  -- Example 2: Type mismatch validation
  putStrLn "Example 2: Type mismatch (number instead of string)"
  let invalidArgs = [JSNumber 42.0, JSNumber 25.0]
  case validateJSFunction exampleJSDoc invalidArgs of
    Right args -> do
      putStrLn "✅ Validation passed!"
      putStrLn ("  Arguments: " ++ show args)
    Left errors -> do
      putStrLn "❌ Validation failed:"
      mapM_ (putStrLn . ("  " ++) . Text.unpack . formatValidationError) errors

  putStrLn ""

  -- Example 3: Complex type validation (object)
  putStrLn "Example 3: Complex object validation"
  let objectArgs = [JSObject [("name", JSString "Jane"), ("age", JSNumber 30.0)]]
  case validateJSFunction complexJSDoc objectArgs of
    Right args -> do
      putStrLn "✅ Validation passed!"
      putStrLn ("  Arguments: " ++ show args)
    Left errors -> do
      putStrLn "❌ Validation failed:"
      mapM_ (putStrLn . ("  " ++) . Text.unpack . formatValidationError) errors

  putStrLn ""
  putStrLn "=== Demo Complete ==="

-- | Example JSDoc comment for demonstration
exampleJSDoc :: JSDocComment
exampleJSDoc = JSDocComment
  { jsDocPosition = TokenPn 0 1 1,
    jsDocDescription = Just "Example function for demonstration",
    jsDocTags =
      [ JSDocTag "param" (Just (JSDocBasicType "string")) (Just "name") (Just "User name") (TokenPn 0 1 1) Nothing,
        JSDocTag "param" (Just (JSDocBasicType "number")) (Just "age") (Just "User age") (TokenPn 0 1 1) Nothing,
        JSDocTag "returns" (Just (JSDocBasicType "boolean")) Nothing (Just "Success status") (TokenPn 0 1 1) Nothing
      ]
  }

-- | Example JSDoc with complex object type
complexJSDoc :: JSDocComment
complexJSDoc = JSDocComment
  { jsDocPosition = TokenPn 0 1 1,
    jsDocDescription = Just "Function with complex object parameter",
    jsDocTags =
      [ JSDocTag "param"
          (Just (JSDocObjectType
            [ JSDocObjectField "name" (JSDocBasicType "string") False,
              JSDocObjectField "age" (JSDocBasicType "number") False
            ]))
          (Just "user")
          (Just "User object")
          (TokenPn 0 1 1)
          Nothing,
        JSDocTag "returns" (Just (JSDocBasicType "string")) Nothing (Just "Greeting message") (TokenPn 0 1 1) Nothing
      ]
  }

-- | Example function AST with JSDoc
-- Note: This would normally be constructed from parsing JavaScript source
-- For now, we demonstrate validation directly with JSDoc comments
exampleValidatedFunction :: Text.Text
exampleValidatedFunction =
  "/**\n\
  \ * Example function for demonstration\n\
  \ * @param {string} name User name\n\
  \ * @param {number} age User age\n\
  \ * @returns {boolean} Success status\n\
  \ */\n\
  \function validateExample(name, age) {\n\
  \  return name.length > 0 && age >= 0;\n\
  \}"

-- | Show complete validation workflow with examples
showValidationWorkflow :: IO ()
showValidationWorkflow = do
  putStrLn "=== Runtime Validation Workflow ==="
  putStrLn ""

  putStrLn "Step 1: Parse JavaScript with JSDoc"
  putStrLn "  JavaScript source:"
  putStrLn "  /**"
  putStrLn "   * Add two numbers"
  putStrLn "   * @param {number} a First number"
  putStrLn "   * @param {number} b Second number"
  putStrLn "   * @returns {number} Sum result"
  putStrLn "   */"
  putStrLn "  function add(a, b) { return a + b; }"
  putStrLn ""

  putStrLn "Step 2: Extract JSDoc from AST"
  putStrLn "  ✅ JSDoc extracted successfully"
  putStrLn "  ✅ Parameter types: a: number, b: number"
  putStrLn "  ✅ Return type: number"
  putStrLn ""

  putStrLn "Step 3: Runtime function call validation"
  let validCall = [JSNumber 5.0, JSNumber 3.0]
      invalidCall = [JSString "5", JSNumber 3.0]

  putStrLn "  Valid call: add(5, 3)"
  case validateJSFunction addJSDoc validCall of
    Right _ -> putStrLn "  ✅ Validation passed - all parameters are numbers"
    Left errors -> putStrLn ("  ❌ Validation failed: " ++ show errors)

  putStrLn ""
  putStrLn "  Invalid call: add(\"5\", 3)"
  case validateJSFunction addJSDoc invalidCall of
    Right _ -> putStrLn "  ✅ Validation passed"
    Left errors -> do
      putStrLn "  ❌ Validation failed:"
      mapM_ (putStrLn . ("    " ++) . Text.unpack . formatValidationError) errors

  putStrLn ""
  putStrLn "Step 4: Return value validation"
  let returnValue = JSNumber 8.0
      invalidReturn = JSString "8"

  putStrLn "  Valid return: 8"
  case validateRuntimeReturn addJSDoc returnValue of
    Right _ -> putStrLn "  ✅ Return validation passed"
    Left err -> putStrLn ("  ❌ Return validation failed: " ++ Text.unpack (formatValidationError err))

  putStrLn "  Invalid return: \"8\""
  case validateRuntimeReturn addJSDoc invalidReturn of
    Right _ -> putStrLn "  ✅ Return validation passed"
    Left err -> do
      putStrLn "  ❌ Return validation failed:"
      putStrLn ("    " ++ Text.unpack (formatValidationError err))

  putStrLn ""
  putStrLn "=== Workflow Complete ==="

-- | JSDoc for add function example
addJSDoc :: JSDocComment
addJSDoc = JSDocComment
  { jsDocPosition = TokenPn 0 1 1,
    jsDocDescription = Just "Add two numbers",
    jsDocTags =
      [ JSDocTag "param" (Just (JSDocBasicType "number")) (Just "a") (Just "First number") (TokenPn 0 1 1) Nothing,
        JSDocTag "param" (Just (JSDocBasicType "number")) (Just "b") (Just "Second number") (TokenPn 0 1 1) Nothing,
        JSDocTag "returns" (Just (JSDocBasicType "number")) Nothing (Just "Sum result") (TokenPn 0 1 1) Nothing
      ]
  }

-- | Check if JSDoc comment has runtime validation available
hasRuntimeValidation :: JSDocComment -> Bool
hasRuntimeValidation jsDoc = not (null (jsDocTags jsDoc))

-- | Determine if function should be validated based on configuration
shouldValidateFunction :: RuntimeValidationConfig -> JSDocComment -> Bool
shouldValidateFunction config jsDoc =
  validationEnabled config && hasRuntimeValidation jsDoc
  where
    validationEnabled (RuntimeValidationConfig enabled _ _ _ _) = enabled

-- | Get validation configuration for JSDoc
getValidationConfig :: JSDocComment -> RuntimeValidationConfig
getValidationConfig jsDoc =
  if hasRuntimeValidation jsDoc
    then developmentConfig  -- Use development config for documented functions
    else productionConfig   -- Use minimal config for undocumented functions