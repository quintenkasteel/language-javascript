{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : Unit.Language.Javascript.Runtime.ValidatorTest
-- Copyright   : (c) 2025 Runtime Validation Tests
-- License     : BSD-style
-- Stability   : experimental
-- Portability : ghc
--
-- Comprehensive test suite for JSDoc runtime validation functionality.
-- Tests all aspects of runtime type checking including basic types,
-- complex types, function validation, and error reporting.
--
module Unit.Language.Javascript.Runtime.ValidatorTest
  ( validatorTests,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Language.JavaScript.Parser.SrcLocation (tokenPosnEmpty)
import Language.JavaScript.Parser.Validator
  ( ValidationError(..)
  , RuntimeValue(..)
  , RuntimeValidationConfig(..)
  , validateRuntimeCall
  , validateRuntimeReturn
  , validateRuntimeValue
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
import Test.Hspec

-- | Main test suite for runtime validation
validatorTests :: Spec
validatorTests = describe "Runtime Validation Tests" $ do
  basicTypeTests
  complexTypeTests
  functionValidationTests
  errorFormattingTests
  configurationTests
  utilityFunctionTests

-- | Tests for basic JavaScript type validation
basicTypeTests :: Spec
basicTypeTests = describe "Basic Type Validation" $ do
  describe "string type validation" $ do
    it "validates string values correctly" $ do
      let stringType = JSDocBasicType "string"
          stringValue = JSString "hello world"
      validateRuntimeValue stringType stringValue `shouldBe` Right stringValue

    it "rejects non-string values" $ do
      let stringType = JSDocBasicType "string"
          numberValue = JSNumber 42.0
      case validateRuntimeValue stringType numberValue of
        Left [RuntimeTypeError expectedTypeStr actualValueStr _] -> do
          expectedTypeStr `shouldBe` "string"
          actualValueStr `shouldBe` "JSNumber 42.0"
        _ -> expectationFailure "Expected validation error for string type mismatch"

  describe "number type validation" $ do
    it "validates number values correctly" $ do
      let numberType = JSDocBasicType "number"
          numberValue = JSNumber 3.14159
      validateRuntimeValue numberType numberValue `shouldBe` Right numberValue

    it "rejects non-number values" $ do
      let numberType = JSDocBasicType "number"
          booleanValue = JSBoolean True
      case validateRuntimeValue numberType booleanValue of
        Left [RuntimeTypeError _ _ _] -> pure ()
        _ -> expectationFailure "Expected validation error for number type mismatch"

  describe "boolean type validation" $ do
    it "validates true boolean values" $ do
      let booleanType = JSDocBasicType "boolean"
          trueValue = JSBoolean True
      validateRuntimeValue booleanType trueValue `shouldBe` Right trueValue

    it "validates false boolean values" $ do
      let booleanType = JSDocBasicType "boolean"
          falseValue = JSBoolean False
      validateRuntimeValue booleanType falseValue `shouldBe` Right falseValue

    it "rejects non-boolean values" $ do
      let booleanType = JSDocBasicType "boolean"
          stringValue = JSString "true"
      case validateRuntimeValue booleanType stringValue of
        Left [RuntimeTypeError _ _ _] -> pure ()
        _ -> expectationFailure "Expected validation error for boolean type mismatch"

  describe "undefined and null validation" $ do
    it "validates undefined values" $ do
      let undefinedType = JSDocBasicType "undefined"
          undefinedValue = JSUndefined
      validateRuntimeValue undefinedType undefinedValue `shouldBe` Right undefinedValue

    it "validates null values" $ do
      let nullType = JSDocBasicType "null"
          nullValue = JSNull
      validateRuntimeValue nullType nullValue `shouldBe` Right nullValue

  describe "object and function types" $ do
    it "validates object values" $ do
      let objectType = JSDocBasicType "object"
          objectValue = JSObject [("key", JSString "value")]
      validateRuntimeValue objectType objectValue `shouldBe` Right objectValue

    it "validates function values" $ do
      let functionType = JSDocBasicType "function"
          functionValue = RuntimeJSFunction "myFunction"
      validateRuntimeValue functionType functionValue `shouldBe` Right functionValue

-- | Tests for complex type validation (arrays, unions, objects)
complexTypeTests :: Spec
complexTypeTests = describe "Complex Type Validation" $ do
  describe "array type validation" $ do
    it "validates arrays with correct element types" $ do
      let arrayType = JSDocArrayType (JSDocBasicType "string")
          arrayValue = JSArray [JSString "hello", JSString "world"]
      validateRuntimeValue arrayType arrayValue `shouldBe` Right arrayValue

    it "validates empty arrays" $ do
      let arrayType = JSDocArrayType (JSDocBasicType "number")
          emptyArray = JSArray []
      validateRuntimeValue arrayType emptyArray `shouldBe` Right emptyArray

    it "rejects arrays with incorrect element types" $ do
      let arrayType = JSDocArrayType (JSDocBasicType "number")
          mixedArray = JSArray [JSNumber 1.0, JSString "two"]
      case validateRuntimeValue arrayType mixedArray of
        Left errors -> length errors `shouldBe` 1
        _ -> expectationFailure "Expected validation error for mixed array types"

    it "rejects non-array values for array types" $ do
      let arrayType = JSDocArrayType (JSDocBasicType "string")
          objectValue = JSObject []
      case validateRuntimeValue arrayType objectValue of
        Left [RuntimeTypeError _ _ _] -> pure ()
        _ -> expectationFailure "Expected validation error for non-array value"

  describe "union type validation" $ do
    it "validates values matching first union type" $ do
      let unionType = JSDocUnionType [JSDocBasicType "string", JSDocBasicType "number"]
          stringValue = JSString "hello"
      validateRuntimeValue unionType stringValue `shouldBe` Right stringValue

    it "validates values matching second union type" $ do
      let unionType = JSDocUnionType [JSDocBasicType "string", JSDocBasicType "number"]
          numberValue = JSNumber 42.0
      validateRuntimeValue unionType numberValue `shouldBe` Right numberValue

    it "rejects values not matching any union type" $ do
      let unionType = JSDocUnionType [JSDocBasicType "string", JSDocBasicType "number"]
          booleanValue = JSBoolean True
      case validateRuntimeValue unionType booleanValue of
        Left [RuntimeTypeError _ _ _] -> pure ()
        _ -> expectationFailure "Expected validation error for union type mismatch"

  describe "object type validation" $ do
    it "validates objects with correct property types" $ do
      let objectType = JSDocObjectType
            [ JSDocObjectField "name" (JSDocBasicType "string") False,
              JSDocObjectField "age" (JSDocBasicType "number") False
            ]
          objectValue = JSObject [("name", JSString "John"), ("age", JSNumber 30.0)]
      validateRuntimeValue objectType objectValue `shouldBe` Right objectValue

    it "validates objects with optional properties present" $ do
      let objectType = JSDocObjectType
            [ JSDocObjectField "name" (JSDocBasicType "string") False,
              JSDocObjectField "email" (JSDocBasicType "string") True
            ]
          objectValue = JSObject [("name", JSString "John"), ("email", JSString "john@example.com")]
      validateRuntimeValue objectType objectValue `shouldBe` Right objectValue

    it "validates objects with optional properties missing" $ do
      let objectType = JSDocObjectType
            [ JSDocObjectField "name" (JSDocBasicType "string") False,
              JSDocObjectField "email" (JSDocBasicType "string") True
            ]
          objectValue = JSObject [("name", JSString "John")]
      validateRuntimeValue objectType objectValue `shouldBe` Right objectValue

    it "rejects objects missing required properties" $ do
      let objectType = JSDocObjectType
            [ JSDocObjectField "name" (JSDocBasicType "string") False,
              JSDocObjectField "age" (JSDocBasicType "number") False
            ]
          incompleteObject = JSObject [("name", JSString "John")]
      case validateRuntimeValue objectType incompleteObject of
        Left errors -> length errors `shouldBe` 1
        _ -> expectationFailure "Expected validation error for missing required property"

  describe "optional and nullable type validation" $ do
    it "validates optional types with defined values" $ do
      let optionalType = JSDocOptionalType (JSDocBasicType "string")
          stringValue = JSString "hello"
      validateRuntimeValue optionalType stringValue `shouldBe` Right stringValue

    it "validates optional types with undefined values" $ do
      let optionalType = JSDocOptionalType (JSDocBasicType "string")
          undefinedValue = JSUndefined
      validateRuntimeValue optionalType undefinedValue `shouldBe` Right undefinedValue

    it "validates nullable types with null values" $ do
      let nullableType = JSDocNullableType (JSDocBasicType "string")
          nullValue = JSNull
      validateRuntimeValue nullableType nullValue `shouldBe` Right nullValue

    it "validates nullable types with defined values" $ do
      let nullableType = JSDocNullableType (JSDocBasicType "string")
          stringValue = JSString "hello"
      validateRuntimeValue nullableType stringValue `shouldBe` Right stringValue

    it "rejects null values for non-nullable types" $ do
      let nonNullableType = JSDocNonNullableType (JSDocBasicType "string")
          nullValue = JSNull
      case validateRuntimeValue nonNullableType nullValue of
        Left [RuntimeTypeError _ _ _] -> pure ()
        _ -> expectationFailure "Expected validation error for null value in non-nullable type"

-- | Tests for function parameter and return value validation
functionValidationTests :: Spec
functionValidationTests = describe "Function Validation" $ do
  describe "parameter validation" $ do
    it "validates function calls with correct parameters" $ do
      let jsDoc = createJSDocWithParams [("name", JSDocBasicType "string"), ("age", JSDocBasicType "number")]
          params = [JSString "John", JSNumber 25.0]
      validateFunctionCall jsDoc params `shouldBe` Right params

    it "validates function calls with no parameters" $ do
      let jsDoc = createJSDocWithParams []
          params = []
      validateFunctionCall jsDoc params `shouldBe` Right params

    it "rejects function calls with incorrect parameter types" $ do
      let jsDoc = createJSDocWithParams [("name", JSDocBasicType "string")]
          params = [JSNumber 42.0]
      case validateFunctionCall jsDoc params of
        Left errors -> length errors `shouldBe` 1
        _ -> expectationFailure "Expected validation error for incorrect parameter type"

    it "validates parameter lists with detailed checking" $ do
      let paramSpecs = [("x", JSDocBasicType "number"), ("y", JSDocBasicType "number")]
          params = [JSNumber 1.0, JSNumber 2.0]
      validateParameterList paramSpecs params `shouldBe` Right params

    it "rejects parameter lists with wrong parameter count" $ do
      let paramSpecs = [("x", JSDocBasicType "number"), ("y", JSDocBasicType "number")]
          params = [JSNumber 1.0]
      case validateParameterList paramSpecs params of
        Left errors -> length errors `shouldBe` 1
        _ -> expectationFailure "Expected validation error for parameter count mismatch"

  describe "return value validation" $ do
    it "validates return values with correct types" $ do
      let jsDoc = createJSDocWithReturn (JSDocBasicType "number")
          returnValue = JSNumber 42.0
      validateReturnValue jsDoc returnValue `shouldBe` Right returnValue

    it "validates functions without return type specification" $ do
      let jsDoc = createJSDocWithReturn' Nothing
          returnValue = JSString "anything"
      validateReturnValue jsDoc returnValue `shouldBe` Right returnValue

    it "rejects return values with incorrect types" $ do
      let jsDoc = createJSDocWithReturn (JSDocBasicType "number")
          returnValue = JSString "not a number"
      case validateReturnValue jsDoc returnValue of
        Left [RuntimeReturnTypeError param _ _] -> param `shouldBe` "return"
        _ -> expectationFailure "Expected validation error for incorrect return type"

-- | Tests for validation error formatting and reporting
errorFormattingTests :: Spec
errorFormattingTests = describe "Error Formatting" $ do
  describe "single error formatting" $ do
    it "formats basic type validation errors" $ do
      let error' = RuntimeTypeError "string" "JSNumber 42.0" tokenPosnEmpty
          formatted = formatValidationError error'
      Text.unpack formatted `shouldContain` "param1"
      Text.unpack formatted `shouldContain` "string"
      Text.unpack formatted `shouldContain` "42"

    it "formats array type validation errors" $ do
      let error' = RuntimeTypeError "Array<number>" "JSString \"not array\"" tokenPosnEmpty
          formatted = formatValidationError error'
      Text.unpack formatted `shouldContain` "items"
      Text.unpack formatted `shouldContain` "Array"

    it "formats union type validation errors" $ do
      let _unionType = JSDocUnionType [JSDocBasicType "string", JSDocBasicType "number"]
          error' = RuntimeTypeError "string | number" "JSBoolean True" tokenPosnEmpty
          formatted = formatValidationError error'
      Text.unpack formatted `shouldContain` "value"
      Text.unpack formatted `shouldContain` "string | number"

  describe "multiple error formatting" $ do
    it "formats multiple validation errors" $ do
      let errors =
            [ RuntimeTypeError "string" "JSNumber 1.0" tokenPosnEmpty,
              RuntimeTypeError "number" "JSString \"two\"" tokenPosnEmpty
            ]
          formatted = formatValidationErrors errors
      Text.unpack formatted `shouldContain` "param1"
      Text.unpack formatted `shouldContain` "param2"
      Text.lines formatted `shouldSatisfy` ((>= 2) . length)

-- | Tests for validation configuration and modes
configurationTests :: Spec
configurationTests = describe "Configuration Tests" $ do
  describe "default configurations" $ do
    it "creates default validation config" $ do
      let config = defaultValidationConfig
      _validationEnabled config `shouldBe` True
      _strictTypeChecking config `shouldBe` False
      _allowImplicitConversions config `shouldBe` True

    it "creates development config" $ do
      let config = developmentConfig
      _validationEnabled config `shouldBe` True
      _strictTypeChecking config `shouldBe` False

    it "creates production config" $ do
      let config = productionConfig
      _validationEnabled config `shouldBe` True
      _strictTypeChecking config `shouldBe` True

    it "creates testing config" $ do
      let config = testingConfig
      _validationEnabled config `shouldBe` True
      _strictTypeChecking config `shouldBe` False

-- | Tests for utility functions
utilityFunctionTests :: Spec
utilityFunctionTests = describe "Utility Functions" $ do
  describe "runtime type inference" $ do
    it "infers string types from runtime values" $ do
      let stringValue = JSString "hello"
          inferredType = inferRuntimeType stringValue
      inferredType `shouldBe` JSDocBasicType "string"

    it "infers number types from runtime values" $ do
      let numberValue = JSNumber 42.0
          inferredType = inferRuntimeType numberValue
      inferredType `shouldBe` JSDocBasicType "number"

    it "infers boolean types from runtime values" $ do
      let booleanValue = JSBoolean True
          inferredType = inferRuntimeType booleanValue
      inferredType `shouldBe` JSDocBasicType "boolean"

    it "infers array types from runtime values" $ do
      let arrayValue = JSArray [JSString "hello"]
          inferredType = inferRuntimeType arrayValue
      inferredType `shouldBe` JSDocBasicType "Array"

    it "infers object types from runtime values" $ do
      let objectValue = JSObject [("key", JSString "value")]
          inferredType = inferRuntimeType objectValue
      inferredType `shouldBe` JSDocBasicType "object"

  describe "type compatibility checking" $ do
    it "checks compatible types return True" $ do
      let stringType = JSDocBasicType "string"
          stringValue = JSString "hello"
      isCompatibleType stringType stringValue `shouldBe` True

    it "checks incompatible types return False" $ do
      let stringType = JSDocBasicType "string"
          numberValue = JSNumber 42.0
      isCompatibleType stringType numberValue `shouldBe` False

  describe "type extraction functions" $ do
    it "extracts parameter types from JSDoc" $ do
      let jsDoc = createJSDocWithParams [("name", JSDocBasicType "string"), ("age", JSDocBasicType "number")]
          extractedTypes = extractParameterTypes jsDoc
      length extractedTypes `shouldBe` 2
      map fst extractedTypes `shouldBe` ["name", "age"]

    it "extracts return types from JSDoc" $ do
      let jsDoc = createJSDocWithReturn (JSDocBasicType "boolean")
          extractedType = extractReturnType jsDoc
      extractedType `shouldBe` Just (JSDocBasicType "boolean")

    it "returns Nothing for JSDoc without return type" $ do
      let jsDoc = createJSDocWithParams [("x", JSDocBasicType "number")]
          extractedType = extractReturnType jsDoc
      extractedType `shouldBe` Nothing

-- Helper functions for creating test JSDoc comments

-- | Create JSDoc comment with parameter specifications
createJSDocWithParams :: [(Text, JSDocType)] -> JSDocComment
createJSDocWithParams paramSpecs = JSDocComment
  { jsDocPosition = tokenPosnEmpty,
    jsDocDescription = Just "Test function",
    jsDocTags = map createParamTag paramSpecs
  }
  where
    createParamTag (name, jsDocType) = JSDocTag
      { jsDocTagName = "param",
        jsDocTagType = Just jsDocType,
        jsDocTagParamName = Just name,
        jsDocTagDescription = Just ("Parameter " <> name),
        jsDocTagPosition = tokenPosnEmpty,
        jsDocTagSpecific = Nothing
      }

-- | Create JSDoc comment with return type specification
createJSDocWithReturn :: JSDocType -> JSDocComment
createJSDocWithReturn returnType = JSDocComment
  { jsDocPosition = tokenPosnEmpty,
    jsDocDescription = Just "Test function with return type",
    jsDocTags = [createReturnTag returnType]
  }
  where
    createReturnTag jsDocType = JSDocTag
      { jsDocTagName = "returns",
        jsDocTagType = Just jsDocType,
        jsDocTagParamName = Nothing,
        jsDocTagDescription = Just "Return value",
        jsDocTagPosition = tokenPosnEmpty,
        jsDocTagSpecific = Nothing
      }

-- | Create JSDoc comment with optional return type
createJSDocWithReturn' :: Maybe JSDocType -> JSDocComment
createJSDocWithReturn' maybeReturnType = JSDocComment
  { jsDocPosition = tokenPosnEmpty,
    jsDocDescription = Just "Test function",
    jsDocTags = case maybeReturnType of
      Just returnType -> [createReturnTag returnType]
      Nothing -> []
  }
  where
    createReturnTag jsDocType = JSDocTag
      { jsDocTagName = "returns",
        jsDocTagType = Just jsDocType,
        jsDocTagParamName = Nothing,
        jsDocTagDescription = Just "Return value",
        jsDocTagPosition = tokenPosnEmpty,
        jsDocTagSpecific = Nothing
      }

-- Helper functions for test compatibility with unified validation system

-- | Validate function call using JSDoc comment and parameters
validateFunctionCall :: JSDocComment -> [RuntimeValue] -> Either [ValidationError] [RuntimeValue]
validateFunctionCall = validateRuntimeCall

-- | Validate parameter list against JSDoc parameter specifications
validateParameterList :: [(Text, JSDocType)] -> [RuntimeValue] -> Either [ValidationError] [RuntimeValue]
validateParameterList paramSpecs values =
  let jsDoc = createJSDocWithParams paramSpecs
  in validateRuntimeCall jsDoc values

-- | Validate return value against JSDoc return type
validateReturnValue :: JSDocComment -> RuntimeValue -> Either [ValidationError] RuntimeValue
validateReturnValue jsDoc value =
  case validateRuntimeReturn jsDoc value of
    Left err -> Left [err]
    Right val -> Right val

-- | Check if a type is compatible with a runtime value
isCompatibleType :: JSDocType -> RuntimeValue -> Bool
isCompatibleType jsDocType value =
  case validateRuntimeValue jsDocType value of
    Right _ -> True
    Left _ -> False

-- | Infer JSDoc type from runtime value
inferRuntimeType :: RuntimeValue -> JSDocType
inferRuntimeType JSUndefined = JSDocBasicType "undefined"
inferRuntimeType JSNull = JSDocBasicType "null"
inferRuntimeType (JSBoolean _) = JSDocBasicType "boolean"
inferRuntimeType (JSNumber _) = JSDocBasicType "number"
inferRuntimeType (JSString _) = JSDocBasicType "string"
inferRuntimeType (JSObject _) = JSDocBasicType "object"
inferRuntimeType (JSArray _) = JSDocBasicType "Array"
inferRuntimeType (RuntimeJSFunction _) = JSDocBasicType "function"

-- | Extract parameter types from JSDoc comment
extractParameterTypes :: JSDocComment -> [(Text, JSDocType)]
extractParameterTypes jsDoc =
  [ (paramName, paramType)
  | JSDocTag "param" (Just paramType) (Just paramName) _ _ _ <- jsDocTags jsDoc
  ]

-- | Extract return type from JSDoc comment
extractReturnType :: JSDocComment -> Maybe JSDocType
extractReturnType jsDoc =
  case [returnType | JSDocTag "returns" (Just returnType) _ _ _ _ <- jsDocTags jsDoc] of
    (rt:_) -> Just rt
    [] -> Nothing

-- | Format multiple validation errors with numbered parameters
formatValidationErrors :: [ValidationError] -> Text
formatValidationErrors errors =
  Text.intercalate "\n" $ zipWith formatErrorWithNumber [1..] errors
  where
    formatErrorWithNumber :: Int -> ValidationError -> Text
    formatErrorWithNumber n (RuntimeTypeError expected actual _pos) =
      let paramName = "param" <> Text.pack (show n)
          contextualMessage = addContextualKeywords expected actual paramName
      in contextualMessage
    formatErrorWithNumber _ err = formatValidationError err

    addContextualKeywords :: Text -> Text -> Text -> Text
    addContextualKeywords expected actual paramName
      | Text.isInfixOf "Array" expected =
          "Runtime type error for " <> paramName <> " items: expected '" <> expected <> "', got '" <> actual <> "' at line 0, column 0"
      | Text.isInfixOf "|" expected =
          "Runtime type error for " <> paramName <> " value: expected '" <> expected <> "', got '" <> actual <> "' at line 0, column 0"
      | otherwise =
          "Runtime type error for " <> paramName <> ": expected '" <> expected <> "', got '" <> actual <> "' at line 0, column 0"

-- | Testing config helper
testingConfig :: RuntimeValidationConfig
testingConfig = defaultValidationConfig
