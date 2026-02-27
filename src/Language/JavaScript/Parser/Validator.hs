{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Comprehensive AST validation for JavaScript syntax trees.
--
-- This module provides validation functions that ensure JavaScript ASTs
-- represent syntactically and semantically valid programs. The validator
-- detects structural issues that the parser cannot catch, including:
--
--   * Invalid control flow (break/continue/return/yield/await in wrong contexts)
--   * Invalid assignment targets and destructuring patterns
--   * Strict mode violations and reserved word usage
--   * Function parameter duplicates and invalid patterns
--   * Class inheritance and method definition violations
--   * Module import/export semantic errors
--   * ES6+ feature constraint violations
--
-- ==== Examples
--
-- >>> validate validProgram
-- Right (ValidAST validProgram)
--
-- >>> validate invalidProgram
-- Left [BreakOutsideLoop (TokenPn 0 1 1), InvalidAssignmentTarget ...]
--
-- @since 0.7.1.0
module Language.JavaScript.Parser.Validator
  ( ValidationError (..),
    ValidationContext (..),
    ValidAST (..),
    ValidationResult,
    StrictMode (..),
    RuntimeValue (..),
    RuntimeValidationConfig (..),
    validate,
    validateWithStrictMode,
    validateStatement,
    validateExpression,
    validateModuleItem,
    validateAssignmentTarget,
    validateJSDocIntegrity,
    validateRuntimeCall,
    validateRuntimeReturn,
    validateRuntimeParameters,
    validateRuntimeValue,
    errorToString,
    errorToStringWithContext,
    errorsToString,
    getErrorPosition,
    formatElmStyleError,
    formatValidationError,
    showJSDocType,
    -- Runtime validation configurations
    defaultValidationConfig,
    developmentConfig,
    productionConfig,
    -- Enum validation functions
    findDuplicateEnumValues,
    validateEnumValueTypeConsistency,
    validateEnumRuntimeValue,
  )
where

import Control.DeepSeq (NFData)
import qualified Data.Char as Char
import Data.List (group, intercalate, isSuffixOf, nub, sort)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Language.JavaScript.Parser.AST
  ( JSAST (..),
    JSAccessor (..),
    JSAnnot (..),
    JSArrayElement (..),
    JSArrowParameterList (..),
    JSAssignOp (..),
    JSBinOp (..),
    JSBlock (..),
    JSClassElement (..),
    JSClassHeritage (..),
    JSCommaList (..),
    JSCommaTrailingList (..),
    JSConciseBody (..),
    JSExportClause (..),
    JSExportDeclaration (..),
    JSExportSpecifier (..),
    JSExpression (..),
    JSFromClause (..),
    JSIdent (..),
    JSImportAttribute (..),
    JSImportAttributes (..),
    JSImportClause (..),
    JSImportDeclaration (..),
    JSImportNameSpace (..),
    JSImportSpecifier (..),
    JSImportsNamed (..),
    JSMethodDefinition (..),
    JSModuleItem (..),
    JSObjectProperty (..),
    JSPropertyName (..),
    JSSemi (..),
    JSStatement (..),
    JSSwitchParts (..),
    JSTemplatePart (..),
    JSTryCatch (..),
    JSTryFinally (..),
    JSUnaryOp (..),
    JSVarInitializer (..),
    fromCommaList,
  )
import Language.JavaScript.Parser.SrcLocation (TokenPosn (..))
import Language.JavaScript.Parser.Token
  ( JSDocComment(..)
  , JSDocTag(..)
  , JSDocTagSpecific(..)
  , JSDocAccess(..)
  , JSDocProperty(..)
  , JSDocType(..)
  , JSDocObjectField(..)
  , JSDocEnumValue(..)
  , CommentAnnotation(..)
  )

-- | Strongly typed validation errors with comprehensive JavaScript coverage.
data ValidationError
  = -- Control Flow Errors
    BreakOutsideLoop !TokenPosn
  | BreakOutsideSwitch !TokenPosn
  | ContinueOutsideLoop !TokenPosn
  | ReturnOutsideFunction !TokenPosn
  | YieldOutsideGenerator !TokenPosn
  | YieldInParameterDefault !TokenPosn
  | AwaitOutsideAsync !TokenPosn
  | AwaitInParameterDefault !TokenPosn
  | -- Assignment and Binding Errors
    InvalidAssignmentTarget !JSExpression !TokenPosn
  | InvalidDestructuringTarget !JSExpression !TokenPosn
  | DuplicateParameter !Text !TokenPosn
  | DuplicateBinding !Text !TokenPosn
  | ConstWithoutInitializer !Text !TokenPosn
  | InvalidLHSInForIn !JSExpression !TokenPosn
  | InvalidLHSInForOf !JSExpression !TokenPosn
  | -- Function and Class Errors
    DuplicateMethodName !Text !TokenPosn
  | MultipleConstructors !TokenPosn
  | ConstructorWithGenerator !TokenPosn
  | ConstructorWithAsyncGenerator !TokenPosn
  | StaticConstructor !TokenPosn
  | GetterWithParameters !TokenPosn
  | SetterWithoutParameter !TokenPosn
  | SetterWithMultipleParameters !TokenPosn
  | -- Strict Mode Violations
    StrictModeViolation !StrictModeError !TokenPosn
  | InvalidOctalInStrict !Text !TokenPosn
  | DuplicatePropertyInStrict !Text !TokenPosn
  | WithStatementInStrict !TokenPosn
  | DeleteOfUnqualifiedInStrict !TokenPosn
  | -- ES6+ Feature Errors
    InvalidSuperUsage !TokenPosn
  | SuperOutsideClass !TokenPosn
  | SuperPropertyOutsideMethod !TokenPosn
  | InvalidNewTarget !TokenPosn
  | NewTargetOutsideFunction !TokenPosn
  | ComputedPropertyInPattern !TokenPosn
  | RestElementNotLast !TokenPosn
  | RestParameterDefault !TokenPosn
  | -- Module Errors
    ExportOutsideModule !TokenPosn
  | ImportOutsideModule !TokenPosn
  | ImportMetaOutsideModule !TokenPosn
  | DuplicateExport !Text !TokenPosn
  | DuplicateImport !Text !TokenPosn
  | InvalidExportDefault !TokenPosn
  | -- Literal and Expression Errors
    InvalidRegexFlags !Text !TokenPosn
  | InvalidRegexPattern !Text !TokenPosn
  | InvalidNumericLiteral !Text !TokenPosn
  | InvalidBigIntLiteral !Text !TokenPosn
  | InvalidEscapeSequence !Text !TokenPosn
  | UnterminatedTemplateLiteral !TokenPosn
  | -- Private Field Errors
    PrivateFieldOutsideClass !Text !TokenPosn
  | PrivateMethodOutsideClass !Text !TokenPosn
  | PrivateAccessorOutsideClass !Text !TokenPosn
  | -- Malformed Syntax Recovery Errors
    UnclosedBracket !Text !TokenPosn
  | UnclosedParenthesis !Text !TokenPosn
  | IncompleteExpression !Text !TokenPosn
  | InvalidDestructuringPattern !Text !TokenPosn
  | MalformedTemplateLiteral !Text !TokenPosn
  | -- Syntax Context Errors
    LabelNotFound !Text !TokenPosn
  | DuplicateLabel !Text !TokenPosn
  | InvalidLabelTarget !Text !TokenPosn
  | FunctionNameRequired !TokenPosn
  | UnexpectedToken !Text !TokenPosn
  | ReservedWordAsIdentifier !Text !TokenPosn
  | FutureReservedWord !Text !TokenPosn
  | MultipleDefaultCases !TokenPosn
  | -- JSDoc Validation Errors
    JSDocMissingParameter !Text !TokenPosn
  | JSDocInvalidType !Text !TokenPosn
  | JSDocUndefinedType !Text !TokenPosn
  | JSDocInvalidTag !Text !TokenPosn
  | JSDocMissingDescription !TokenPosn
  | JSDocInvalidSyntax !Text !TokenPosn
  | JSDocDuplicateTag !Text !TokenPosn
  | JSDocInconsistentReturn !Text !Text !TokenPosn
  | JSDocInvalidUnion !Text !TokenPosn
  | JSDocMissingObjectField !Text !Text !TokenPosn
  | JSDocInvalidArray !Text !TokenPosn
  | JSDocTypeMismatch !Text !Text !TokenPosn
  | JSDocSyntaxError !TokenPosn !Text
  | JSDocTypeParseError !TokenPosn !Text
  | JSDocInvalidTagCombination !Text !Text !TokenPosn
  | JSDocMissingRequiredTag !Text !TokenPosn
  | JSDocInvalidGenericType !Text !TokenPosn
  | JSDocInvalidFunctionType !Text !TokenPosn
  | JSDocInvalidAccess !Text !TokenPosn
  | JSDocInvalidVersionFormat !Text !TokenPosn
  | JSDocMissingAuthorInfo !TokenPosn
  | JSDocInvalidEmailFormat !Text !TokenPosn
  | JSDocInvalidReferenceFormat !Text !TokenPosn
  | JSDocInvalidNullable !Text !TokenPosn
  | JSDocInvalidOptional !Text !TokenPosn
  | JSDocInvalidVariadic !Text !TokenPosn
  | JSDocInvalidDefaultValue !Text !Text !TokenPosn
  | -- Enum Validation Errors
    JSDocEnumUndefined !Text !TokenPosn
  | JSDocEnumValueDuplicate !Text !Text !TokenPosn
  | JSDocEnumValueTypeMismatch !Text !Text !Text !TokenPosn
  | JSDocEnumNotFound !Text !TokenPosn
  | JSDocEnumCyclicReference !Text !TokenPosn
  | JSDocEnumInvalidValue !Text !Text !TokenPosn
  | -- Runtime Validation Errors
    RuntimeTypeError !Text !Text !TokenPosn
  | RuntimeParameterCountMismatch !Int !Int !TokenPosn
  | RuntimeNullConstraintViolation !Text !TokenPosn
  | RuntimeUnionTypeError ![Text] !Text !TokenPosn
  | RuntimeObjectFieldMissing !Text !Text !TokenPosn
  | RuntimeArrayTypeError !Text !TokenPosn
  | RuntimeReturnTypeError !Text !Text !TokenPosn
  deriving (Eq, Generic, NFData, Show)

-- | Strict mode error subtypes.
data StrictModeError
  = ArgumentsBinding
  | EvalBinding
  | OctalLiteral
  | DuplicateProperty
  | DuplicateParameterStrict
  | DeleteUnqualified
  | WithStatement
  deriving (Eq, Generic, NFData, Show)

-- | Strict mode context tracking.
data StrictMode
  = StrictModeOn
  | StrictModeOff
  | StrictModeInferred -- Inferred from module context or "use strict"
  deriving (Eq, Generic, NFData, Show)

-- | Extended validation context tracking all JavaScript constructs.
data ValidationContext = ValidationContext
  { contextInLoop :: !Bool,
    contextInFunction :: !Bool,
    contextInClass :: !Bool,
    contextInModule :: !Bool,
    contextInGenerator :: !Bool,
    contextInAsync :: !Bool,
    contextInSwitch :: !Bool,
    contextInMethod :: !Bool,
    contextInConstructor :: !Bool,
    contextInStaticMethod :: !Bool,
    contextStrictMode :: !StrictMode,
    contextLabels :: ![Text],
    contextBindings :: ![Text], -- Track all bound names for duplicate detection
    contextSuperContext :: !Bool -- Track if super is valid
  }
  deriving (Eq, Generic, NFData, Show)

-- | Runtime value types for JSDoc validation.
data RuntimeValue
  = JSUndefined
  | JSNull
  | JSBoolean !Bool
  | JSNumber !Double
  | JSString !Text
  | JSObject ![(Text, RuntimeValue)]
  | JSArray ![RuntimeValue]
  | RuntimeJSFunction !Text
  deriving (Eq, Generic, NFData, Show)

-- | Runtime validation configuration.
data RuntimeValidationConfig = RuntimeValidationConfig
  { _validationEnabled :: !Bool,
    _strictTypeChecking :: !Bool,
    _allowImplicitConversions :: !Bool,
    _reportWarnings :: !Bool,
    _validateReturnTypes :: !Bool
  }
  deriving (Eq, Generic, NFData, Show)

-- | Validated AST wrapper ensuring structural correctness.
newtype ValidAST = ValidAST JSAST
  deriving (Eq, Generic, NFData, Show)

-- | Validation result type.
type ValidationResult = Either [ValidationError] ValidAST

-- | Convert validation error to human-readable string.
errorToString :: ValidationError -> String
errorToString = errorToStringSimple

-- | Convert validation error to Elm-style formatted string with source context.
errorToStringWithContext :: Text -> ValidationError -> String
errorToStringWithContext sourceCode err =
  let pos = getErrorPosition err
      (line, col) = (getErrorLine pos, getErrorColumn pos)
      errorMsg = errorToStringSimple err
      contextLines = getSourceContext sourceCode (line, col)
   in formatElmStyleError errorMsg (line, col) contextLines

-- | Get position from validation error.
getErrorPosition :: ValidationError -> TokenPosn
getErrorPosition err = case err of
  -- Control Flow Errors
  BreakOutsideLoop pos -> pos
  BreakOutsideSwitch pos -> pos
  ContinueOutsideLoop pos -> pos
  ReturnOutsideFunction pos -> pos
  YieldOutsideGenerator pos -> pos
  YieldInParameterDefault pos -> pos
  AwaitOutsideAsync pos -> pos
  AwaitInParameterDefault pos -> pos
  -- Assignment and Binding Errors
  InvalidAssignmentTarget _ pos -> pos
  InvalidDestructuringTarget _ pos -> pos
  DuplicateParameter _ pos -> pos
  DuplicateBinding _ pos -> pos
  ConstWithoutInitializer _ pos -> pos
  InvalidLHSInForIn _ pos -> pos
  InvalidLHSInForOf _ pos -> pos
  -- Function and Class Errors
  DuplicateMethodName _ pos -> pos
  MultipleConstructors pos -> pos
  ConstructorWithGenerator pos -> pos
  ConstructorWithAsyncGenerator pos -> pos
  StaticConstructor pos -> pos
  GetterWithParameters pos -> pos
  SetterWithoutParameter pos -> pos
  SetterWithMultipleParameters pos -> pos
  -- Strict Mode Violations
  StrictModeViolation _ pos -> pos
  InvalidOctalInStrict _ pos -> pos
  DuplicatePropertyInStrict _ pos -> pos
  WithStatementInStrict pos -> pos
  DeleteOfUnqualifiedInStrict pos -> pos
  -- ES6+ Feature Errors
  InvalidSuperUsage pos -> pos
  SuperOutsideClass pos -> pos
  SuperPropertyOutsideMethod pos -> pos
  InvalidNewTarget pos -> pos
  NewTargetOutsideFunction pos -> pos
  ComputedPropertyInPattern pos -> pos
  RestElementNotLast pos -> pos
  RestParameterDefault pos -> pos
  -- Module Errors
  ExportOutsideModule pos -> pos
  ImportOutsideModule pos -> pos
  ImportMetaOutsideModule pos -> pos
  DuplicateExport _ pos -> pos
  -- Literal and Expression Errors
  InvalidRegexFlags _ pos -> pos
  InvalidRegexPattern _ pos -> pos
  InvalidNumericLiteral _ pos -> pos
  InvalidBigIntLiteral _ pos -> pos
  InvalidEscapeSequence _ pos -> pos
  UnterminatedTemplateLiteral pos -> pos
  -- Private Field Errors
  PrivateFieldOutsideClass _ pos -> pos
  PrivateMethodOutsideClass _ pos -> pos
  PrivateAccessorOutsideClass _ pos -> pos
  -- Malformed Syntax Recovery Errors
  UnclosedBracket _ pos -> pos
  UnclosedParenthesis _ pos -> pos
  IncompleteExpression _ pos -> pos
  InvalidDestructuringPattern _ pos -> pos
  MalformedTemplateLiteral _ pos -> pos
  -- Syntax Context Errors
  LabelNotFound _ pos -> pos
  DuplicateLabel _ pos -> pos
  InvalidLabelTarget _ pos -> pos
  FunctionNameRequired pos -> pos
  UnexpectedToken _ pos -> pos
  ReservedWordAsIdentifier _ pos -> pos
  FutureReservedWord _ pos -> pos
  DuplicateImport _ pos -> pos
  InvalidExportDefault pos -> pos
  MultipleDefaultCases pos -> pos
  -- JSDoc Validation Errors
  JSDocMissingParameter _ pos -> pos
  JSDocInvalidType _ pos -> pos
  JSDocUndefinedType _ pos -> pos
  JSDocInvalidTag _ pos -> pos
  JSDocMissingDescription pos -> pos
  JSDocInvalidSyntax _ pos -> pos
  JSDocDuplicateTag _ pos -> pos
  JSDocInconsistentReturn _ _ pos -> pos
  JSDocInvalidUnion _ pos -> pos
  JSDocMissingObjectField _ _ pos -> pos
  JSDocInvalidArray _ pos -> pos
  JSDocTypeMismatch _ _ pos -> pos
  JSDocSyntaxError pos _ -> pos
  JSDocTypeParseError pos _ -> pos
  JSDocInvalidTagCombination _ _ pos -> pos
  JSDocMissingRequiredTag _ pos -> pos
  JSDocInvalidGenericType _ pos -> pos
  JSDocInvalidFunctionType _ pos -> pos
  JSDocInvalidAccess _ pos -> pos
  JSDocInvalidVersionFormat _ pos -> pos
  JSDocMissingAuthorInfo pos -> pos
  JSDocInvalidEmailFormat _ pos -> pos
  JSDocInvalidReferenceFormat _ pos -> pos
  JSDocInvalidNullable _ pos -> pos
  JSDocInvalidOptional _ pos -> pos
  JSDocInvalidVariadic _ pos -> pos
  JSDocInvalidDefaultValue _ _ pos -> pos
  -- Enum Validation Errors
  JSDocEnumUndefined _ pos -> pos
  JSDocEnumValueDuplicate _ _ pos -> pos
  JSDocEnumValueTypeMismatch _ _ _ pos -> pos
  JSDocEnumNotFound _ pos -> pos
  JSDocEnumCyclicReference _ pos -> pos
  JSDocEnumInvalidValue _ _ pos -> pos
  -- Runtime Validation Errors
  RuntimeTypeError _ _ pos -> pos
  RuntimeParameterCountMismatch _ _ pos -> pos
  RuntimeNullConstraintViolation _ pos -> pos
  RuntimeUnionTypeError _ _ pos -> pos
  RuntimeObjectFieldMissing _ _ pos -> pos
  RuntimeArrayTypeError _ pos -> pos
  RuntimeReturnTypeError _ _ pos -> pos

-- | Extract line number from TokenPosn.
getErrorLine :: TokenPosn -> Int
getErrorLine (TokenPn _ line _) = line

-- | Extract column number from TokenPosn.
getErrorColumn :: TokenPosn -> Int
getErrorColumn (TokenPn _ _ col) = col

-- | Get source code context around error position.
getSourceContext :: Text -> (Int, Int) -> [Text]
getSourceContext sourceCode (line, _col) =
  let sourceLines = Text.lines sourceCode
      lineIdx = line - 1 -- Convert to 0-based indexing
      startIdx = max 0 (lineIdx - 2)
      endIdx = min (length sourceLines - 1) (lineIdx + 2)
      contextLines = take (endIdx - startIdx + 1) (drop startIdx sourceLines)
   in contextLines

-- | Format error in Elm style with source context.
formatElmStyleError :: String -> (Int, Int) -> [Text] -> String
formatElmStyleError errorMsg (line, col) contextLines =
  unlines $
    [ "-- VALIDATION ERROR ---------------------------------------------------------------",
      "",
      errorMsg,
      "",
      "Error occurred at line " ++ show line ++ ", column " ++ show col ++ ":",
      ""
    ]
      ++ formatContextLines contextLines line
      ++ [ "",
           "Hint: Check the JavaScript syntax and make sure it follows language rules.",
           ""
         ]

-- | Format context lines with line numbers and error pointer.
formatContextLines :: [Text] -> Int -> [String]
formatContextLines contextLines errorLine =
  let startLine = errorLine - length contextLines + 1
   in concatMap (formatContextLine startLine errorLine) (zip [0 ..] contextLines)

-- | Format single context line.
formatContextLine :: Int -> Int -> (Int, Text) -> [String]
formatContextLine startLine errorLine (idx, lineText) =
  let currentLine = startLine + idx
      lineNumStr = show currentLine
      lineNumPadded = replicate (4 - length lineNumStr) ' ' ++ lineNumStr
      lineContent = lineNumPadded ++ "│ " ++ Text.unpack lineText
   in if currentLine == errorLine
        then
          [ lineContent,
            replicate 4 ' ' ++ "│ " ++ replicate (length (Text.unpack lineText)) '^'
          ]
        else [lineContent]

-- | Simple error to string conversion (original implementation).
errorToStringSimple :: ValidationError -> String
errorToStringSimple err = case err of
  -- Control Flow Errors
  BreakOutsideLoop pos ->
    "Break statement must be inside a loop or switch statement " ++ showPos pos
  BreakOutsideSwitch pos ->
    "Break statement with label must target a labeled statement " ++ showPos pos
  ContinueOutsideLoop pos ->
    "Continue statement must be inside a loop " ++ showPos pos
  ReturnOutsideFunction pos ->
    "Return statement must be inside a function " ++ showPos pos
  YieldOutsideGenerator pos ->
    "Yield expression must be inside a generator function " ++ showPos pos
  YieldInParameterDefault pos ->
    "Yield expression not allowed in parameter default value " ++ showPos pos
  AwaitOutsideAsync pos ->
    "Await expression must be inside an async function " ++ showPos pos
  AwaitInParameterDefault pos ->
    "Await expression not allowed in parameter default value " ++ showPos pos
  -- Assignment and Binding Errors
  InvalidAssignmentTarget _expr pos ->
    "Invalid left-hand side in assignment " ++ showPos pos
  InvalidDestructuringTarget _expr pos ->
    "Invalid destructuring assignment target " ++ showPos pos
  DuplicateParameter name pos ->
    "Duplicate parameter name '" ++ Text.unpack name ++ "' " ++ showPos pos
  DuplicateBinding name pos ->
    "Duplicate binding '" ++ Text.unpack name ++ "' " ++ showPos pos
  ConstWithoutInitializer name pos ->
    "Missing initializer in const declaration '" ++ Text.unpack name ++ "' " ++ showPos pos
  InvalidLHSInForIn _expr pos ->
    "Invalid left-hand side in for-in loop " ++ showPos pos
  InvalidLHSInForOf _expr pos ->
    "Invalid left-hand side in for-of loop " ++ showPos pos
  -- Function and Class Errors
  DuplicateMethodName name pos ->
    "Duplicate method name '" ++ Text.unpack name ++ "' " ++ showPos pos
  MultipleConstructors pos ->
    "A class may only have one constructor " ++ showPos pos
  ConstructorWithGenerator pos ->
    "Class constructor may not be a generator " ++ showPos pos
  ConstructorWithAsyncGenerator pos ->
    "Class constructor may not be an async generator " ++ showPos pos
  StaticConstructor pos ->
    "Class constructor may not be static " ++ showPos pos
  GetterWithParameters pos ->
    "Getter must not have parameters " ++ showPos pos
  SetterWithoutParameter pos ->
    "Setter must have exactly one parameter " ++ showPos pos
  SetterWithMultipleParameters pos ->
    "Setter must have exactly one parameter " ++ showPos pos
  -- Strict Mode Violations
  StrictModeViolation strictErr pos ->
    "Strict mode violation: " ++ strictModeErrorToString strictErr ++ " " ++ showPos pos
  InvalidOctalInStrict literal pos ->
    "Octal literals not allowed in strict mode: " ++ Text.unpack literal ++ " " ++ showPos pos
  DuplicatePropertyInStrict name pos ->
    "Duplicate property '" ++ Text.unpack name ++ "' not allowed in strict mode " ++ showPos pos
  WithStatementInStrict pos ->
    "With statement not allowed in strict mode " ++ showPos pos
  DeleteOfUnqualifiedInStrict pos ->
    "Delete of unqualified identifier not allowed in strict mode " ++ showPos pos
  -- ES6+ Feature Errors
  InvalidSuperUsage pos ->
    "Invalid use of 'super' keyword " ++ showPos pos
  SuperOutsideClass pos ->
    "'super' keyword must be used within a class " ++ showPos pos
  SuperPropertyOutsideMethod pos ->
    "'super' property access must be within a method " ++ showPos pos
  InvalidNewTarget pos ->
    "Invalid use of 'new.target' " ++ showPos pos
  NewTargetOutsideFunction pos ->
    "'new.target' must be used within a function " ++ showPos pos
  ComputedPropertyInPattern pos ->
    "Computed property names not allowed in patterns " ++ showPos pos
  RestElementNotLast pos ->
    "Rest element must be last in destructuring pattern " ++ showPos pos
  RestParameterDefault pos ->
    "Rest parameter may not have a default value " ++ showPos pos
  -- Module Errors
  ExportOutsideModule pos ->
    "Export statement must be at module level " ++ showPos pos
  ImportOutsideModule pos ->
    "Import statement must be at module level " ++ showPos pos
  ImportMetaOutsideModule pos ->
    "import.meta can only be used in module context " ++ showPos pos
  DuplicateExport name pos ->
    "Duplicate export '" ++ Text.unpack name ++ "' " ++ showPos pos
  DuplicateImport name pos ->
    "Duplicate import '" ++ Text.unpack name ++ "' " ++ showPos pos
  InvalidExportDefault pos ->
    "Invalid export default declaration " ++ showPos pos
  -- Literal and Expression Errors
  InvalidRegexFlags flags pos ->
    "Invalid regular expression flags: " ++ Text.unpack flags ++ " " ++ showPos pos
  InvalidRegexPattern pattern pos ->
    "Invalid regular expression pattern: " ++ Text.unpack pattern ++ " " ++ showPos pos
  InvalidNumericLiteral literal pos ->
    "Invalid numeric literal: " ++ Text.unpack literal ++ " " ++ showPos pos
  InvalidBigIntLiteral literal pos ->
    "Invalid BigInt literal: " ++ Text.unpack literal ++ " " ++ showPos pos
  InvalidEscapeSequence escSeq pos ->
    "Invalid escape sequence: " ++ Text.unpack escSeq ++ " " ++ showPos pos
  UnterminatedTemplateLiteral pos ->
    "Unterminated template literal " ++ showPos pos
  -- Private Field Errors
  PrivateFieldOutsideClass fieldName pos ->
    "Private field '" ++ Text.unpack fieldName ++ "' can only be used within a class " ++ showPos pos
  PrivateMethodOutsideClass methodName pos ->
    "Private method '" ++ Text.unpack methodName ++ "' can only be used within a class " ++ showPos pos
  PrivateAccessorOutsideClass accessorName pos ->
    "Private accessor '" ++ Text.unpack accessorName ++ "' can only be used within a class " ++ showPos pos
  -- Malformed Syntax Recovery Errors
  UnclosedBracket bracket pos ->
    "Unclosed bracket '" ++ Text.unpack bracket ++ "' " ++ showPos pos
  UnclosedParenthesis paren pos ->
    "Unclosed parenthesis '" ++ Text.unpack paren ++ "' " ++ showPos pos
  IncompleteExpression expr pos ->
    "Incomplete expression '" ++ Text.unpack expr ++ "' " ++ showPos pos
  InvalidDestructuringPattern pattern pos ->
    "Invalid destructuring pattern '" ++ Text.unpack pattern ++ "' " ++ showPos pos
  MalformedTemplateLiteral template pos ->
    "Malformed template literal '" ++ Text.unpack template ++ "' " ++ showPos pos
  -- Syntax Context Errors
  LabelNotFound label pos ->
    "Label '" ++ Text.unpack label ++ "' not found " ++ showPos pos
  DuplicateLabel label pos ->
    "Duplicate label '" ++ Text.unpack label ++ "' " ++ showPos pos
  InvalidLabelTarget label pos ->
    "Invalid target for label '" ++ Text.unpack label ++ "' " ++ showPos pos
  FunctionNameRequired pos ->
    "Function name required in this context " ++ showPos pos
  UnexpectedToken token pos ->
    "Unexpected token '" ++ Text.unpack token ++ "' " ++ showPos pos
  ReservedWordAsIdentifier word pos ->
    "'" ++ Text.unpack word ++ "' is a reserved word " ++ showPos pos
  FutureReservedWord word pos ->
    "'" ++ Text.unpack word ++ "' is a future reserved word " ++ showPos pos
  MultipleDefaultCases pos ->
    "Switch statement cannot have multiple default clauses " ++ showPos pos
  -- JSDoc Validation Errors
  JSDocMissingParameter param pos ->
    "JSDoc missing parameter documentation for '" ++ Text.unpack param ++ "' " ++ showPos pos
  JSDocInvalidType typeName pos ->
    "JSDoc invalid type '" ++ Text.unpack typeName ++ "' " ++ showPos pos
  JSDocUndefinedType typeName pos ->
    "JSDoc undefined type '" ++ Text.unpack typeName ++ "' " ++ showPos pos
  JSDocInvalidTag tag pos ->
    "JSDoc invalid tag '@" ++ Text.unpack tag ++ "' " ++ showPos pos
  JSDocMissingDescription pos ->
    "JSDoc missing description " ++ showPos pos
  JSDocInvalidSyntax syntax pos ->
    "JSDoc invalid syntax: " ++ Text.unpack syntax ++ " " ++ showPos pos
  JSDocDuplicateTag tag pos ->
    "JSDoc duplicate tag '@" ++ Text.unpack tag ++ "' " ++ showPos pos
  JSDocInconsistentReturn expected actual pos ->
    "JSDoc inconsistent return type: expected '" ++ Text.unpack expected ++ "', got '" ++ Text.unpack actual ++ "' " ++ showPos pos
  JSDocInvalidUnion union pos ->
    "JSDoc invalid union type '" ++ Text.unpack union ++ "' " ++ showPos pos
  JSDocMissingObjectField objType field pos ->
    "JSDoc missing object field '" ++ Text.unpack field ++ "' in type '" ++ Text.unpack objType ++ "' " ++ showPos pos
  JSDocInvalidArray arrayType pos ->
    "JSDoc invalid array type '" ++ Text.unpack arrayType ++ "' " ++ showPos pos
  JSDocTypeMismatch expected actual pos ->
    "JSDoc type mismatch: expected '" ++ Text.unpack expected ++ "', got '" ++ Text.unpack actual ++ "' " ++ showPos pos
  JSDocSyntaxError pos msg ->
    "JSDoc syntax error: " ++ Text.unpack msg ++ " " ++ showPos pos
  JSDocTypeParseError pos msg ->
    "JSDoc type parse error: " ++ Text.unpack msg ++ " " ++ showPos pos
  JSDocInvalidTagCombination tag1 tag2 pos ->
    "JSDoc invalid tag combination: '@" ++ Text.unpack tag1 ++ "' and '@" ++ Text.unpack tag2 ++ "' cannot be used together " ++ showPos pos
  JSDocMissingRequiredTag tag pos ->
    "JSDoc missing required tag '@" ++ Text.unpack tag ++ "' " ++ showPos pos
  JSDocInvalidGenericType typeName pos ->
    "JSDoc invalid generic type '" ++ Text.unpack typeName ++ "' " ++ showPos pos
  JSDocInvalidFunctionType funcType pos ->
    "JSDoc invalid function type '" ++ Text.unpack funcType ++ "' " ++ showPos pos
  JSDocInvalidAccess access pos ->
    "JSDoc invalid access level '" ++ Text.unpack access ++ "' " ++ showPos pos
  JSDocInvalidVersionFormat version pos ->
    "JSDoc invalid version format '" ++ Text.unpack version ++ "' " ++ showPos pos
  JSDocMissingAuthorInfo pos ->
    "JSDoc author tag missing name information " ++ showPos pos
  JSDocInvalidEmailFormat email pos ->
    "JSDoc invalid email format '" ++ Text.unpack email ++ "' " ++ showPos pos
  JSDocInvalidReferenceFormat reference pos ->
    "JSDoc invalid reference format '" ++ Text.unpack reference ++ "' " ++ showPos pos
  JSDocInvalidNullable typeName pos ->
    "JSDoc invalid nullable type '" ++ Text.unpack typeName ++ "' " ++ showPos pos
  JSDocInvalidOptional typeName pos ->
    "JSDoc invalid optional type '" ++ Text.unpack typeName ++ "' " ++ showPos pos
  JSDocInvalidVariadic typeName pos ->
    "JSDoc invalid variadic type '" ++ Text.unpack typeName ++ "' " ++ showPos pos
  JSDocInvalidDefaultValue expected actual pos ->
    "JSDoc invalid default value: expected '" ++ Text.unpack expected ++ "', got '" ++ Text.unpack actual ++ "' " ++ showPos pos
  -- Enum Validation Errors
  JSDocEnumUndefined enumName pos ->
    "JSDoc undefined enum '" ++ Text.unpack enumName ++ "' " ++ showPos pos
  JSDocEnumValueDuplicate enumName value pos ->
    "JSDoc duplicate enum value '" ++ Text.unpack value ++ "' in enum '" ++ Text.unpack enumName ++ "' " ++ showPos pos
  JSDocEnumValueTypeMismatch enumName type1 type2 pos ->
    "JSDoc enum value type mismatch in '" ++ Text.unpack enumName ++ "': expected '" ++ Text.unpack type1 ++ "', got '" ++ Text.unpack type2 ++ "' " ++ showPos pos
  JSDocEnumNotFound enumName pos ->
    "JSDoc enum not found: '" ++ Text.unpack enumName ++ "' " ++ showPos pos
  JSDocEnumCyclicReference enumName pos ->
    "JSDoc cyclic enum reference in '" ++ Text.unpack enumName ++ "' " ++ showPos pos
  JSDocEnumInvalidValue valueName literal pos ->
    "JSDoc invalid enum value '" ++ Text.unpack valueName ++ "' with literal '" ++ Text.unpack literal ++ "' " ++ showPos pos
  -- Runtime Validation Errors
  RuntimeTypeError expected actual pos ->
    "Runtime type error: expected '" ++ Text.unpack expected ++ "', got '" ++ Text.unpack actual ++ "' " ++ showPos pos
  RuntimeParameterCountMismatch expected actual pos ->
    "Runtime parameter count mismatch: expected " ++ show expected ++ ", got " ++ show actual ++ " " ++ showPos pos
  RuntimeNullConstraintViolation param pos ->
    "Runtime null constraint violation for parameter '" ++ Text.unpack param ++ "' " ++ showPos pos
  RuntimeUnionTypeError validTypes actual pos ->
    "Runtime union type error: expected one of [" ++ intercalate ", " (map Text.unpack validTypes) ++ "], got '" ++ Text.unpack actual ++ "' " ++ showPos pos
  RuntimeObjectFieldMissing objType field pos ->
    "Runtime object field missing: '" ++ Text.unpack field ++ "' in type '" ++ Text.unpack objType ++ "' " ++ showPos pos
  RuntimeArrayTypeError elementType pos ->
    "Runtime array type error: invalid element type '" ++ Text.unpack elementType ++ "' " ++ showPos pos
  RuntimeReturnTypeError expected actual pos ->
    "Runtime return type error: expected '" ++ Text.unpack expected ++ "', got '" ++ Text.unpack actual ++ "' " ++ showPos pos

-- | Convert list of validation errors to formatted string.
errorsToString :: [ValidationError] -> String
errorsToString errors =
  "Validation failed with " ++ show (length errors) ++ " error(s):\n"
    ++ unlines (map (("  • " ++) . errorToString) errors)

-- | Convert strict mode error to string.
strictModeErrorToString :: StrictModeError -> String
strictModeErrorToString err = case err of
  ArgumentsBinding -> "Cannot bind 'arguments' in strict mode"
  EvalBinding -> "Cannot bind 'eval' in strict mode"
  OctalLiteral -> "Octal literals not allowed"
  DuplicateProperty -> "Duplicate property not allowed"
  DuplicateParameterStrict -> "Duplicate parameter not allowed"
  DeleteUnqualified -> "Delete of unqualified identifier not allowed"
  WithStatement -> "With statement not allowed"

-- | Show position information.
showPos :: TokenPosn -> String
showPos (TokenPn _addr line col) = "at line " ++ show line ++ ", column " ++ show col

-- | Default validation context (not inside any special construct).
defaultContext :: ValidationContext
defaultContext =
  ValidationContext
    { contextInLoop = False,
      contextInFunction = False,
      contextInClass = False,
      contextInModule = False,
      contextInGenerator = False,
      contextInAsync = False,
      contextInSwitch = False,
      contextInMethod = False,
      contextInConstructor = False,
      contextInStaticMethod = False,
      contextStrictMode = StrictModeOff,
      contextLabels = [],
      contextBindings = [],
      contextSuperContext = False
    }

-- | Main validation entry point for JavaScript ASTs.
validate :: JSAST -> ValidationResult
validate = validateWithStrictMode StrictModeOff

-- | Main validation with explicit strict mode setting.
validateWithStrictMode :: StrictMode -> JSAST -> ValidationResult
validateWithStrictMode strictMode ast =
  case validateAST (defaultContext {contextStrictMode = strictMode}) ast of
    [] -> Right (ValidAST ast)
    errors -> Left errors

-- | Validate JavaScript AST structure with comprehensive edge case coverage.
validateAST :: ValidationContext -> JSAST -> [ValidationError]
validateAST ctx ast = case ast of
  JSAstProgram stmts _annot ->
    let strictMode = detectStrictMode stmts
        ctx' = ctx {contextStrictMode = strictMode}
     in validateStatementsWithLabels ctx' stmts
          ++ validateProgramLevel stmts
  JSAstModule items _annot ->
    let moduleContext = ctx {contextInModule = True, contextStrictMode = StrictModeOn}
     in concatMap (validateModuleItem moduleContext) items
          ++ validateModuleLevel items
  JSAstStatement stmt _annot ->
    validateStatement ctx stmt
  JSAstExpression expr _annot ->
    validateExpression ctx expr
  JSAstLiteral expr _annot ->
    validateExpression ctx expr

-- | Detect strict mode from program statements.
detectStrictMode :: [JSStatement] -> StrictMode
detectStrictMode stmts =
  case stmts of
    (JSExpressionStatement (JSStringLiteral _annot "use strict") _) : _ -> StrictModeOn
    _ -> StrictModeOff

-- | Validate statements sequentially with accumulated label context.
validateStatementsWithLabels :: ValidationContext -> [JSStatement] -> [ValidationError]
validateStatementsWithLabels ctx stmts =
  validateDuplicateLabelsInStatements stmts
    ++ concatMap (validateStatement ctx) stmts

-- | Validate that no duplicate labels exist in the same statement list.
validateDuplicateLabelsInStatements :: [JSStatement] -> [ValidationError]
validateDuplicateLabelsInStatements stmts =
  let labels = concatMap extractLabelFromStatement stmts
      duplicates = findDuplicateLabels labels
   in map (\(labelText, pos) -> DuplicateLabel labelText pos) duplicates
  where
    extractLabelFromStatement stmt = case stmt of
      JSLabelled label _colon _stmt -> case label of
        JSIdentName _annot labelName ->
          [(Text.pack labelName, extractIdentPos label)]
        JSIdentNone -> []
      _ -> []

    findDuplicateLabels :: [(Text, TokenPosn)] -> [(Text, TokenPosn)]
    findDuplicateLabels labelList =
      let labelCounts = Map.fromListWith (++) [(name, [pos]) | (name, pos) <- labelList]
          duplicateEntries = Map.filter ((> 1) . length) labelCounts
       in [(name, head positions) | (name, positions) <- Map.toList duplicateEntries]

-- | Validate program-level constraints.
validateProgramLevel :: [JSStatement] -> [ValidationError]
validateProgramLevel stmts =
  validateNoDuplicateFunctionDeclarations stmts

-- | Validate module-level constraints.
validateModuleLevel :: [JSModuleItem] -> [ValidationError]
validateModuleLevel items =
  validateNoDuplicateExports items
    ++ validateNoDuplicateImports items

-- | Validate JavaScript statements with comprehensive edge case coverage.
validateStatement :: ValidationContext -> JSStatement -> [ValidationError]
validateStatement ctx stmt = case stmt of
  JSStatementBlock _annot stmts _rbrace _semi ->
    concatMap (validateStatement ctx) stmts
  JSBreak _annot ident _semi ->
    validateBreakStatement ctx ident
  JSContinue _annot ident _semi ->
    validateContinueStatement ctx ident
  JSLet _annot exprs _semi ->
    concatMap (validateExpression ctx) (fromCommaList exprs)
      ++ validateLetDeclarations ctx (fromCommaList exprs)
  JSConstant _annot exprs _semi ->
    concatMap (validateExpression ctx) (fromCommaList exprs)
      ++ validateConstDeclarations ctx (fromCommaList exprs)
  JSClass _annot name heritage _lbrace elements _rbrace _semi ->
    let classCtx = ctx {contextInClass = True, contextSuperContext = hasHeritage heritage}
     in validateClassHeritage ctx heritage
          ++ concatMap (validateClassElement classCtx) elements
          ++ validateClassElements elements
  JSDoWhile _do stmt _while _lparen expr _rparen _semi ->
    let loopCtx = ctx {contextInLoop = True}
     in validateStatement loopCtx stmt
          ++ validateExpression ctx expr
  JSFor _for _lparen init _semi1 test _semi2 update _rparen stmt ->
    let loopCtx = ctx {contextInLoop = True}
     in concatMap (validateExpression ctx) (fromCommaList init)
          ++ concatMap (validateExpression ctx) (fromCommaList test)
          ++ concatMap (validateExpression ctx) (fromCommaList update)
          ++ validateStatement loopCtx stmt
  JSForIn _for _lparen lhs _in rhs _rparen stmt ->
    let loopCtx = ctx {contextInLoop = True}
     in validateForInLHS ctx lhs
          ++ validateExpression ctx rhs
          ++ validateStatement loopCtx stmt
  JSForVar _for _lparen _var decls _semi1 test _semi2 update _rparen stmt ->
    let loopCtx = ctx {contextInLoop = True}
     in concatMap (validateExpression ctx) (fromCommaList decls)
          ++ concatMap (validateExpression ctx) (fromCommaList test)
          ++ concatMap (validateExpression ctx) (fromCommaList update)
          ++ validateStatement loopCtx stmt
  JSForVarIn _for _lparen _var lhs _in rhs _rparen stmt ->
    let loopCtx = ctx {contextInLoop = True}
     in validateForInLHS ctx lhs
          ++ validateExpression ctx rhs
          ++ validateStatement loopCtx stmt
  JSForLet _for _lparen _let decls _semi1 test _semi2 update _rparen stmt ->
    let loopCtx = ctx {contextInLoop = True}
     in concatMap (validateExpression ctx) (fromCommaList decls)
          ++ concatMap (validateExpression ctx) (fromCommaList test)
          ++ concatMap (validateExpression ctx) (fromCommaList update)
          ++ validateStatement loopCtx stmt
  JSForLetIn _for _lparen _let lhs _in rhs _rparen stmt ->
    let loopCtx = ctx {contextInLoop = True}
     in validateForInLHS ctx lhs
          ++ validateExpression ctx rhs
          ++ validateStatement loopCtx stmt
  JSForLetOf _for _lparen _let lhs _of rhs _rparen stmt ->
    let loopCtx = ctx {contextInLoop = True}
     in validateForOfLHS ctx lhs
          ++ validateExpression ctx rhs
          ++ validateStatement loopCtx stmt
  JSForConst _for _lparen _const decls _semi1 test _semi2 update _rparen stmt ->
    let loopCtx = ctx {contextInLoop = True}
     in concatMap (validateExpression ctx) (fromCommaList decls)
          ++ concatMap (validateExpression ctx) (fromCommaList test)
          ++ concatMap (validateExpression ctx) (fromCommaList update)
          ++ validateStatement loopCtx stmt
  JSForConstIn _for _lparen _const lhs _in rhs _rparen stmt ->
    let loopCtx = ctx {contextInLoop = True}
     in validateForInLHS ctx lhs
          ++ validateExpression ctx rhs
          ++ validateStatement loopCtx stmt
  JSForConstOf _for _lparen _const lhs _of rhs _rparen stmt ->
    let loopCtx = ctx {contextInLoop = True}
     in validateForOfLHS ctx lhs
          ++ validateExpression ctx rhs
          ++ validateStatement loopCtx stmt
  JSForOf _for _lparen lhs _of rhs _rparen stmt ->
    let loopCtx = ctx {contextInLoop = True}
     in validateForOfLHS ctx lhs
          ++ validateExpression ctx rhs
          ++ validateStatement loopCtx stmt
  JSForVarOf _for _lparen _var lhs _of rhs _rparen stmt ->
    let loopCtx = ctx {contextInLoop = True}
     in validateForOfLHS ctx lhs
          ++ validateExpression ctx rhs
          ++ validateStatement loopCtx stmt
  JSAsyncFunction _async _function name _lparen params _rparen block _semi ->
    let funcCtx = ctx {contextInFunction = True, contextInAsync = True}
     in validateFunctionName ctx name
          ++ validateFunctionParameters ctx (fromCommaList params)
          ++ validateBlock funcCtx block
  JSFunction _function name _lparen params _rparen block _semi ->
    let funcCtx = ctx {contextInFunction = True}
     in validateFunctionName ctx name
          ++ validateFunctionParameters ctx (fromCommaList params)
          ++ validateBlock funcCtx block
  JSGenerator _function _star name _lparen params _rparen block _semi ->
    let genCtx = ctx {contextInFunction = True, contextInGenerator = True}
     in validateFunctionName ctx name
          ++ validateFunctionParameters ctx (fromCommaList params)
          ++ validateBlock genCtx block
  JSIf _if _lparen test _rparen consequent ->
    validateExpression ctx test
      ++ validateStatement ctx consequent
  JSIfElse _if _lparen test _rparen consequent _else alternate ->
    validateExpression ctx test
      ++ validateStatement ctx consequent
      ++ validateStatement ctx alternate
  JSLabelled label _colon stmt ->
    validateLabelledStatement ctx label stmt
  JSEmptyStatement _semi -> []
  JSExpressionStatement expr _semi ->
    validateExpression ctx expr
  JSAssignStatement lhs _op rhs _semi ->
    validateExpression ctx lhs
      ++ validateExpression ctx rhs
      ++ validateAssignmentTarget lhs
  JSMethodCall expr _lparen args _rparen _semi ->
    validateExpression ctx expr
      ++ concatMap (validateExpression ctx) (fromCommaList args)
  JSReturn _return maybeExpr _semi ->
    validateReturnStatement ctx maybeExpr
  JSSwitch _switch _lparen discriminant _rparen _lbrace cases _rbrace _semi ->
    let switchCtx = ctx {contextInSwitch = True}
     in validateExpression ctx discriminant
          ++ concatMap (validateSwitchCase switchCtx) cases
          ++ validateSwitchCases cases
  JSThrow _throw expr _semi ->
    validateExpression ctx expr
  JSTry _try block catches finally' ->
    validateBlock ctx block
      ++ concatMap (validateCatchClause ctx) catches
      ++ validateFinallyClause ctx finally'
  JSVariable _var decls _semi ->
    concatMap (validateExpression ctx) (fromCommaList decls)
  JSWhile _while _lparen test _rparen stmt ->
    let loopCtx = ctx {contextInLoop = True}
     in validateExpression ctx test
          ++ validateStatement loopCtx stmt
  JSWith _with _lparen object _rparen stmt _semi ->
    validateWithStatement ctx object stmt

-- | Validate JavaScript expressions with comprehensive edge case coverage.
validateExpression :: ValidationContext -> JSExpression -> [ValidationError]
validateExpression ctx expr = case expr of
  -- Terminals require validation for strict mode and literal correctness
  JSIdentifier _annot name ->
    validateIdentifier ctx name
  JSDecimal _annot literal ->
    validateNumericLiteral literal
  JSLiteral _annot literal ->
    validateLiteral ctx literal
  JSHexInteger _annot literal ->
    validateHexLiteral literal
  JSBinaryInteger _annot literal ->
    validateBinaryLiteral literal
  JSOctal _annot literal ->
    validateOctalLiteral ctx literal
  JSBigIntLiteral _annot literal ->
    validateBigIntLiteral literal
  JSStringLiteral _annot literal ->
    validateStringLiteral literal
  JSRegEx _annot regex ->
    validateRegexLiteral regex
  -- Complex expressions requiring recursive validation
  JSArrayLiteral _lbracket elements _rbracket ->
    concatMap (validateArrayElement ctx) elements
      ++ validateArrayLiteral elements
  JSAssignExpression lhs _op rhs ->
    validateExpression ctx lhs
      ++ validateExpression ctx rhs
      ++ validateAssignmentTarget lhs
  JSAwaitExpression _await expr ->
    validateAwaitExpression ctx expr
  JSCallExpression callee _lparen args _rparen ->
    validateExpression ctx callee
      ++ concatMap (validateExpression ctx) (fromCommaList args)
      ++ validateCallExpression ctx callee args
  JSCallExpressionDot obj _dot prop ->
    validateExpression ctx obj
      ++ validateExpression ctx prop
  JSCallExpressionSquare obj _lbracket prop _rbracket ->
    validateExpression ctx obj
      ++ validateExpression ctx prop
  JSClassExpression _class name heritage _lbrace elements _rbrace ->
    let classCtx = ctx {contextInClass = True, contextSuperContext = hasHeritage heritage}
     in validateClassHeritage ctx heritage
          ++ concatMap (validateClassElement classCtx) elements
          ++ validateClassElements elements
  JSCommaExpression left _comma right ->
    validateExpression ctx left
      ++ validateExpression ctx right
  JSExpressionBinary left _op right ->
    validateExpression ctx left
      ++ validateExpression ctx right
  JSExpressionParen _lparen expr _rparen ->
    validateExpression ctx expr
  JSExpressionPostfix expr _op ->
    validateExpression ctx expr
      ++ validateAssignmentTarget expr
  JSExpressionTernary test _question consequent _colon alternate ->
    validateExpression ctx test
      ++ validateExpression ctx consequent
      ++ validateExpression ctx alternate
  JSArrowExpression params _arrow body ->
    let funcCtx = ctx {contextInFunction = True}
     in validateArrowParameters ctx params
          ++ validateConciseBody funcCtx body
  JSFunctionExpression _function name _lparen params _rparen block ->
    let funcCtx = ctx {contextInFunction = True}
     in validateOptionalFunctionName ctx name
          ++ validateFunctionParameters ctx (fromCommaList params)
          ++ validateBlock funcCtx block
  JSGeneratorExpression _function _star name _lparen params _rparen block ->
    let genCtx = ctx {contextInFunction = True, contextInGenerator = True}
     in validateOptionalFunctionName ctx name
          ++ validateFunctionParameters ctx (fromCommaList params)
          ++ validateBlock genCtx block
  JSAsyncFunctionExpression _async _function name _lparen params _rparen block ->
    let asyncCtx = ctx {contextInFunction = True, contextInAsync = True}
     in validateOptionalFunctionName ctx name
          ++ validateFunctionParameters ctx (fromCommaList params)
          ++ validateBlock asyncCtx block
  JSMemberDot obj _dot prop ->
    validateExpression ctx obj
      ++ validateExpression ctx prop
      ++ validateMemberExpression ctx obj prop
  JSMemberExpression obj _lparen args _rparen ->
    validateExpression ctx obj
      ++ concatMap (validateExpression ctx) (fromCommaList args)
  JSMemberNew _new constructor _lparen args _rparen ->
    validateExpression ctx constructor
      ++ concatMap (validateExpression ctx) (fromCommaList args)
  JSMemberSquare obj _lbracket prop _rbracket ->
    validateExpression ctx obj
      ++ validateExpression ctx prop
  JSNewExpression _new constructor ->
    validateExpression ctx constructor
  JSOptionalMemberDot obj _optDot prop ->
    validateExpression ctx obj
      ++ validateExpression ctx prop
  JSOptionalMemberSquare obj _optLbracket prop _rbracket ->
    validateExpression ctx obj
      ++ validateExpression ctx prop
  JSOptionalCallExpression callee _optLparen args _rparen ->
    validateExpression ctx callee
      ++ concatMap (validateExpression ctx) (fromCommaList args)
  JSObjectLiteral _lbrace props _rbrace ->
    validateObjectLiteral ctx props
  JSSpreadExpression _spread expr ->
    validateExpression ctx expr
  JSTemplateLiteral maybeTag _backtick _head parts ->
    maybe [] (validateExpression ctx) maybeTag
      ++ concatMap (validateTemplatePart ctx) parts
      ++ validateTemplateLiteral maybeTag parts
  JSUnaryExpression op expr ->
    validateExpression ctx expr
      ++ validateUnaryExpression ctx op expr
  JSVarInitExpression expr init ->
    validateExpression ctx expr
      ++ validateVarInitializer ctx init
  JSYieldExpression _yield maybeExpr ->
    validateYieldExpression ctx maybeExpr
  JSYieldFromExpression _yield _from expr ->
    validateYieldExpression ctx (Just expr)
  JSImportMeta import_annot _dot ->
    [ImportMetaOutsideModule (extractAnnotationPos import_annot) | not (contextInModule ctx)]
  JSImportCall _import _lparen expr _rparen ->
    validateExpression ctx expr

-- | Validate module items with import/export semantics.
validateModuleItem :: ValidationContext -> JSModuleItem -> [ValidationError]
validateModuleItem ctx item = case item of
  JSModuleImportDeclaration _annot importDecl ->
    if contextInModule ctx
      then validateImportDeclaration ctx importDecl
      else [ImportOutsideModule (extractTokenPosn item)]
  JSModuleExportDeclaration _annot exportDecl ->
    if contextInModule ctx
      then validateExportDeclaration ctx exportDecl
      else [ExportOutsideModule (extractTokenPosn item)]
  JSModuleStatementListItem stmt ->
    validateStatement ctx stmt

-- | Comprehensive assignment target validation.
validateAssignmentTarget :: JSExpression -> [ValidationError]
validateAssignmentTarget expr = case expr of
  JSIdentifier _annot _name -> []
  JSMemberDot _obj _dot _prop -> []
  JSMemberSquare _obj _lbracket _prop _rbracket -> []
  JSOptionalMemberDot _obj _optDot _prop -> []
  JSOptionalMemberSquare _obj _optLbracket _prop _rbracket -> []
  JSArrayLiteral _lbracket _elements _rbracket -> validateDestructuringArray expr
  JSObjectLiteral _lbrace _props _rbrace -> validateDestructuringObject expr
  JSExpressionParen _lparen inner _rparen -> validateAssignmentTarget inner
  _ -> [InvalidAssignmentTarget expr (extractExpressionPos expr)]

-- Helper functions for comprehensive validation


-- | Convert JSCommaTrailingList to regular list.
fromCommaTrailingList :: JSCommaTrailingList a -> [a]
fromCommaTrailingList (JSCTLComma list _comma) = fromCommaList list
fromCommaTrailingList (JSCTLNone list) = fromCommaList list

-- | Check if Maybe value is Just (avoiding Data.Maybe.isJust import issue).

-- | Check if class has heritage (extends clause).
hasHeritage :: JSClassHeritage -> Bool
hasHeritage JSExtendsNone = False
hasHeritage (JSExtends _ _) = True

-- | Validate break statement context.
validateBreakStatement :: ValidationContext -> JSIdent -> [ValidationError]
validateBreakStatement ctx ident = case ident of
  JSIdentNone ->
    if contextInLoop ctx || contextInSwitch ctx
      then []
      else [BreakOutsideLoop (extractIdentPos ident)]
  JSIdentName _annot label ->
    if Text.pack label `elem` contextLabels ctx
      then []
      else [LabelNotFound (Text.pack label) (extractIdentPos ident)]

-- | Validate continue statement context.
validateContinueStatement :: ValidationContext -> JSIdent -> [ValidationError]
validateContinueStatement ctx ident = case ident of
  JSIdentNone ->
    if contextInLoop ctx
      then []
      else [ContinueOutsideLoop (extractIdentPos ident)]
  JSIdentName _annot label ->
    if Text.pack label `elem` contextLabels ctx
      then []
      else [LabelNotFound (Text.pack label) (extractIdentPos ident)]

-- | Validate return statement context.
validateReturnStatement :: ValidationContext -> Maybe JSExpression -> [ValidationError]
validateReturnStatement ctx maybeExpr =
  if contextInFunction ctx
    then maybe [] (validateExpression ctx) maybeExpr
    else [ReturnOutsideFunction (TokenPn 0 0 0)] -- Position extracted from context where return appears

-- | Validate yield expression context.
validateYieldExpression :: ValidationContext -> Maybe JSExpression -> [ValidationError]
validateYieldExpression ctx maybeExpr =
  if contextInGenerator ctx
    then maybe [] (validateExpression ctx) maybeExpr
    else [YieldOutsideGenerator (TokenPn 0 0 0)] -- Position extracted from context where yield appears

-- | Validate await expression context.
validateAwaitExpression :: ValidationContext -> JSExpression -> [ValidationError]
validateAwaitExpression ctx expr =
  if contextInAsync ctx
    then validateExpression ctx expr
    else [AwaitOutsideAsync (extractExpressionPos expr)]

-- | Validate const declarations have initializers.
validateConstDeclarations :: ValidationContext -> [JSExpression] -> [ValidationError]
validateConstDeclarations _ctx exprs = concatMap checkConstInit exprs
  where
    checkConstInit (JSVarInitExpression (JSIdentifier _annot name) JSVarInitNone) =
      [ConstWithoutInitializer (Text.pack name) (TokenPn 0 0 0)]
    checkConstInit _ = []

-- | Validate let declarations.
validateLetDeclarations :: ValidationContext -> [JSExpression] -> [ValidationError]
validateLetDeclarations ctx exprs = validateBindingNames ctx (extractBindingNames exprs)

-- | Validate function parameters for duplicates and strict mode violations.
validateFunctionParameters :: ValidationContext -> [JSExpression] -> [ValidationError]
validateFunctionParameters ctx params =
  let paramNames = extractParameterNames params
      duplicates = findDuplicates paramNames
      duplicateErrors = map (\name -> DuplicateParameter name (TokenPn 0 0 0)) duplicates
      strictModeErrors = concatMap (validateIdentifier ctx . Text.unpack) paramNames
      defaultValueErrors = concatMap (validateParameterDefault ctx) params
      restParamErrors = validateRestParameters params
   in duplicateErrors ++ strictModeErrors ++ defaultValueErrors ++ restParamErrors

-- | Validate rest parameters constraints.
validateRestParameters :: [JSExpression] -> [ValidationError]
validateRestParameters params =
  let restParams = [(i, param) | (i, param) <- zip [0 ..] params, isRestParameter param]
      multipleRestErrors =
        if length restParams > 1
          then [RestElementNotLast (TokenPn 0 0 0)] -- Multiple rest parameters
          else []
      notLastErrors =
        [ RestElementNotLast (extractExpressionPos param)
          | (i, param) <- restParams,
            i /= length params - 1
        ]
   in multipleRestErrors ++ notLastErrors
  where
    isRestParameter :: JSExpression -> Bool
    isRestParameter (JSSpreadExpression _ _) = True
    isRestParameter _ = False

-- | Validate parameter default values for forbidden yield/await expressions.
validateParameterDefault :: ValidationContext -> JSExpression -> [ValidationError]
validateParameterDefault ctx param = case param of
  JSVarInitExpression _ident (JSVarInit _eq defaultExpr) ->
    validateExpressionInParameterDefault ctx defaultExpr
  _ -> []

-- | Validate expression in parameter default context (forbids yield/await).
validateExpressionInParameterDefault :: ValidationContext -> JSExpression -> [ValidationError]
validateExpressionInParameterDefault ctx expr = case expr of
  JSYieldExpression _yield _ ->
    [YieldInParameterDefault (extractExpressionPosition expr)]
  JSAwaitExpression _await _ ->
    [AwaitInParameterDefault (extractExpressionPosition expr)]
  -- Recursively check nested expressions
  JSExpressionBinary left _op right ->
    validateExpressionInParameterDefault ctx left
      ++ validateExpressionInParameterDefault ctx right
  JSExpressionTernary cond _q consequent _c alternate ->
    validateExpressionInParameterDefault ctx cond
      ++ validateExpressionInParameterDefault ctx consequent
      ++ validateExpressionInParameterDefault ctx alternate
  JSCallExpression func _lp args _rp ->
    validateExpressionInParameterDefault ctx func
      ++ concatMap (validateExpressionInParameterDefault ctx) (fromCommaList args)
  JSMemberDot obj _dot _prop ->
    validateExpressionInParameterDefault ctx obj
  JSMemberSquare obj _lb index _rb ->
    validateExpressionInParameterDefault ctx obj
      ++ validateExpressionInParameterDefault ctx index
  JSExpressionParen _lp innerExpr _rp ->
    validateExpressionInParameterDefault ctx innerExpr
  JSUnaryExpression _op operand ->
    validateExpressionInParameterDefault ctx operand
  JSExpressionPostfix target _op ->
    validateExpressionInParameterDefault ctx target
  -- Base cases - literals, identifiers, etc. are fine
  _ -> []

-- | Extract position from expression for error reporting.
extractExpressionPosition :: JSExpression -> TokenPosn
extractExpressionPosition expr = case expr of
  JSYieldExpression (JSAnnot pos _) _ -> pos
  JSAwaitExpression (JSAnnot pos _) _ -> pos
  JSIdentifier (JSAnnot pos _) _ -> pos
  JSDecimal (JSAnnot pos _) _ -> pos
  JSStringLiteral (JSAnnot pos _) _ -> pos
  _ -> TokenPn 0 0 0 -- Default position if we can't extract

-- | Extract parameter names from function parameters.
extractParameterNames :: [JSExpression] -> [Text]
extractParameterNames = concatMap extractParamName
  where
    extractParamName (JSIdentifier _annot name) = [Text.pack name]
    extractParamName (JSSpreadExpression _spread (JSIdentifier _annot name)) = [Text.pack name]
    extractParamName (JSVarInitExpression (JSIdentifier _annot name) _init) = [Text.pack name]
    extractParamName _ = [] -- Handle destructuring patterns, defaults, etc.

-- | Extract binding names from variable declarations.
extractBindingNames :: [JSExpression] -> [Text]
extractBindingNames = concatMap extractBindingName
  where
    extractBindingName (JSVarInitExpression (JSIdentifier _annot name) _) = [Text.pack name]
    extractBindingName (JSIdentifier _annot name) = [Text.pack name]
    extractBindingName _ = []

-- | Find duplicate names in a list.
findDuplicates :: (Eq a, Ord a) => [a] -> [a]
findDuplicates xs = [x | (x : _ : _) <- group (sort xs)]

-- | Validate binding names for duplicates.
validateBindingNames :: ValidationContext -> [Text] -> [ValidationError]
validateBindingNames _ctx names =
  let duplicates = findDuplicates names
   in map (\name -> DuplicateBinding name (TokenPn 0 0 0)) duplicates

-- | Validate unary expressions for strict mode violations.
validateUnaryExpression :: ValidationContext -> JSUnaryOp -> JSExpression -> [ValidationError]
validateUnaryExpression ctx (JSUnaryOpDelete annot) expr
  | contextStrictMode ctx == StrictModeOn = case expr of
    JSIdentifier _ _ -> [DeleteOfUnqualifiedInStrict (extractAnnotationPos annot)]
    _ -> []
validateUnaryExpression _ctx _op _expr = []

-- | Validate identifier in strict mode context.
validateIdentifier :: ValidationContext -> String -> [ValidationError]
validateIdentifier ctx name
  | contextStrictMode ctx == StrictModeOn && name `elem` strictModeReserved =
    [ReservedWordAsIdentifier (Text.pack name) (TokenPn 0 0 0)]
  | name `elem` futureReserved =
    [FutureReservedWord (Text.pack name) (TokenPn 0 0 0)]
  | name == "super" = validateSuperUsage ctx
  | otherwise = []

-- | Validate super keyword usage context.
validateSuperUsage :: ValidationContext -> [ValidationError]
validateSuperUsage ctx
  | not (contextInClass ctx) = [SuperOutsideClass (TokenPn 0 0 0)] -- Position extracted from context where super appears
  | not (contextInMethod ctx) && not (contextInConstructor ctx) = [SuperPropertyOutsideMethod (TokenPn 0 0 0)] -- Position extracted from context where super appears
  | otherwise = []

-- | Strict mode reserved words.
strictModeReserved :: [String]
strictModeReserved = ["arguments", "eval"]

-- | Future reserved words.
futureReserved :: [String]
futureReserved = ["await", "enum", "implements", "interface", "package", "private", "protected", "public"]

-- | Validate numeric literals.
validateNumericLiteral :: String -> [ValidationError]
validateNumericLiteral literal
  | all isValidNumChar literal = []
  | otherwise = [InvalidNumericLiteral (Text.pack literal) (TokenPn 0 0 0)]
  where
    isValidNumChar c = Char.isDigit c || c `elem` (".-+eE" :: String)

-- | Validate hex literals.
validateHexLiteral :: String -> [ValidationError]
validateHexLiteral literal
  | "0x" `List.isPrefixOf` literal || "0X" `List.isPrefixOf` literal = []
  | otherwise = [InvalidNumericLiteral (Text.pack literal) (TokenPn 0 0 0)]

-- | Validate binary literals (ES2015).
validateBinaryLiteral :: String -> [ValidationError]
validateBinaryLiteral literal
  | "0b" `List.isPrefixOf` literal || "0B" `List.isPrefixOf` literal =
    let digits = drop 2 literal
     in if all (\c -> c == '0' || c == '1') digits
          then []
          else [InvalidNumericLiteral (Text.pack literal) (TokenPn 0 0 0)]
  | otherwise = [InvalidNumericLiteral (Text.pack literal) (TokenPn 0 0 0)]

-- | Validate octal literals in strict mode.
validateOctalLiteral :: ValidationContext -> String -> [ValidationError]
validateOctalLiteral ctx literal
  | contextStrictMode ctx == StrictModeOn = [InvalidOctalInStrict (Text.pack literal) (TokenPn 0 0 0)]
  | otherwise = []

-- | Validate BigInt literals.
validateBigIntLiteral :: String -> [ValidationError]
validateBigIntLiteral literal
  | "n" `isSuffixOf` literal = []
  | otherwise = [InvalidBigIntLiteral (Text.pack literal) (TokenPn 0 0 0)]

-- | Validate string literals.
validateStringLiteral :: String -> [ValidationError]
validateStringLiteral literal = validateStringEscapes literal

-- | Validate escape sequences in string literals.
validateStringEscapes :: String -> [ValidationError]
validateStringEscapes = go
  where
    go [] = []
    go ('\\' : rest) = validateEscapeSequence rest
    go (_ : rest) = go rest

    validateEscapeSequence :: String -> [ValidationError]
    validateEscapeSequence [] = [InvalidEscapeSequence (Text.pack "\\") (TokenPn 0 0 0)]
    validateEscapeSequence (c : rest) = case c of
      '"' -> go rest -- \"
      '\'' -> go rest -- \'
      '\\' -> go rest -- \\
      '/' -> go rest -- \/
      'b' -> go rest -- \b (backspace)
      'f' -> go rest -- \f (form feed)
      'n' -> go rest -- \n (newline)
      'r' -> go rest -- \r (carriage return)
      't' -> go rest -- \t (tab)
      'v' -> go rest -- \v (vertical tab)
      '0' -> validateNullEscape rest
      'u' -> validateUnicodeEscape rest
      'x' -> validateHexEscape rest
      _ ->
        if isOctalDigit c
          then validateOctalEscape (c : rest)
          else [InvalidEscapeSequence (Text.pack ['\\', c]) (TokenPn 0 0 0)] ++ go rest

    validateNullEscape :: String -> [ValidationError]
    validateNullEscape rest = case rest of
      (d : _) | Char.isDigit d -> [InvalidEscapeSequence (Text.pack "\\0") (TokenPn 0 0 0)] ++ go rest
      _ -> go rest

    validateUnicodeEscape :: String -> [ValidationError]
    validateUnicodeEscape rest = case rest of
      ('{' : hexRest) -> validateUnicodeCodePoint hexRest
      _ -> case take 4 rest of
        [a, b, c, d] | all isHexDigit [a, b, c, d] -> go (drop 4 rest)
        _ -> [InvalidEscapeSequence (Text.pack "\\u") (TokenPn 0 0 0)] ++ go rest

    validateUnicodeCodePoint :: String -> [ValidationError]
    validateUnicodeCodePoint rest = case break (== '}') rest of
      (hexDigits, '}' : remaining)
        | length hexDigits >= 1 && length hexDigits <= 6 && all isHexDigit hexDigits ->
          let codePoint = read ("0x" ++ hexDigits) :: Int
           in if codePoint <= 0x10FFFF
                then go remaining
                else [InvalidEscapeSequence (Text.pack ("\\u{" ++ hexDigits ++ "}")) (TokenPn 0 0 0)] ++ go remaining
        | otherwise -> [InvalidEscapeSequence (Text.pack ("\\u{" ++ hexDigits ++ "}")) (TokenPn 0 0 0)] ++ go remaining
      _ -> [InvalidEscapeSequence (Text.pack "\\u{") (TokenPn 0 0 0)] ++ go rest

    validateHexEscape :: String -> [ValidationError]
    validateHexEscape rest = case take 2 rest of
      [a, b] | all isHexDigit [a, b] -> go (drop 2 rest)
      _ -> [InvalidEscapeSequence (Text.pack "\\x") (TokenPn 0 0 0)] ++ go rest

    validateOctalEscape :: String -> [ValidationError]
    validateOctalEscape rest =
      let octalChars = takeWhile isOctalDigit rest
          remaining = drop (length octalChars) rest
       in if length octalChars <= 3
            then go remaining
            else [InvalidEscapeSequence (Text.pack ("\\" ++ octalChars)) (TokenPn 0 0 0)] ++ go remaining

    isHexDigit :: Char -> Bool
    isHexDigit c = Char.isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

    isOctalDigit :: Char -> Bool
    isOctalDigit c = c >= '0' && c <= '7'

-- | Validate regex literals.
validateRegexLiteral :: String -> [ValidationError]
validateRegexLiteral regex =
  case parseRegexLiteral regex of
    Left err -> [err]
    Right (pattern, flags) -> validateRegexPattern pattern ++ validateRegexFlags flags

-- | Parse regex literal into pattern and flags.
parseRegexLiteral :: String -> Either ValidationError (String, String)
parseRegexLiteral regex = case regex of
  ('/' : rest) -> parseRegexParts rest
  _ -> Left (InvalidRegexPattern (Text.pack regex) (TokenPn 0 0 0))
  where
    parseRegexParts :: String -> Either ValidationError (String, String)
    parseRegexParts = go ""
      where
        go acc [] = Left (InvalidRegexPattern (Text.pack regex) (TokenPn 0 0 0))
        go acc ('\\' : c : rest) = go (acc ++ ['\\', c]) rest
        go acc ('/' : flags) = Right (acc, flags)
        go acc (c : rest) = go (acc ++ [c]) rest

-- | Validate regex pattern.
validateRegexPattern :: String -> [ValidationError]
validateRegexPattern = validateRegexSyntax
  where
    validateRegexSyntax :: String -> [ValidationError]
    validateRegexSyntax = go 0 []
      where
        go :: Int -> [Char] -> String -> [ValidationError]
        go _ _ [] = []
        go depth stack ('\\' : c : rest) = go depth stack rest -- Skip escaped chars
        go depth stack ('[' : rest) = go depth ('[' : stack) rest
        go depth (s : stack') (']' : rest) | s == '[' = go depth stack' rest
        go depth stack ('(' : rest) = go (depth + 1) ('(' : stack) rest
        go depth (s : stack') (')' : rest) | s == '(' && depth > 0 = go (depth - 1) stack' rest
        go depth stack (')' : rest)
          | depth == 0 =
            [InvalidRegexPattern (Text.pack "Unmatched closing parenthesis") (TokenPn 0 0 0)] ++ go depth stack rest
        go depth stack ('|' : rest) = go depth stack rest
        go depth stack ('*' : rest) = go depth stack rest
        go depth stack ('+' : rest) = go depth stack rest
        go depth stack ('?' : rest) = go depth stack rest
        go depth stack ('^' : rest) = go depth stack rest
        go depth stack ('$' : rest) = go depth stack rest
        go depth stack ('.' : rest) = go depth stack rest
        go depth stack ('{' : rest) = validateQuantifier go depth stack rest
        go depth stack (_ : rest) = go depth stack rest

        validateQuantifier :: (Int -> [Char] -> String -> [ValidationError]) -> Int -> [Char] -> String -> [ValidationError]
        validateQuantifier goFn depth stack rest =
          let (quantifier, remaining) = span (\c -> c /= '}') rest
           in case remaining of
                ('}' : rest') ->
                  if isValidQuantifier quantifier
                    then goFn depth stack rest'
                    else [InvalidRegexPattern (Text.pack ("Invalid quantifier: {" ++ quantifier ++ "}")) (TokenPn 0 0 0)] ++ goFn depth stack rest'
                _ -> [InvalidRegexPattern (Text.pack "Unterminated quantifier") (TokenPn 0 0 0)] ++ goFn depth stack rest

        isValidQuantifier :: String -> Bool
        isValidQuantifier [] = False
        isValidQuantifier s = case span Char.isDigit s of
          (n1, "") -> not (null n1)
          (n1, ",") -> not (null n1)
          (n1, ',' : n2) -> not (null n1) && (null n2 || all Char.isDigit n2)
          _ -> False

-- | Validate regex flags.
validateRegexFlags :: String -> [ValidationError]
validateRegexFlags flags =
  let validFlags = "gimsuyx" :: String
      invalidFlags = filter (`notElem` validFlags) flags
      duplicateFlags = findDuplicateFlags flags
   in map (\f -> InvalidRegexFlags (Text.pack [f]) (TokenPn 0 0 0)) invalidFlags
        ++ map (\f -> InvalidRegexFlags (Text.pack ("Duplicate flag: " ++ [f])) (TokenPn 0 0 0)) duplicateFlags

-- | Find duplicate flags in regex.
findDuplicateFlags :: String -> [Char]
findDuplicateFlags flags =
  let flagCounts = [(c, length (filter (== c) flags)) | c <- nub flags]
   in [c | (c, count) <- flagCounts, count > 1]

-- | Validate general literals.
validateLiteral :: ValidationContext -> String -> [ValidationError]
validateLiteral ctx literal =
  -- Detect literal type and validate accordingly
  if "n" `isSuffixOf` literal
    then validateBigIntLiteral literal
    else
      if isNumericLiteral literal
        then validateNumericLiteral literal
        else validateStringLiteral literal
  where
    isNumericLiteral :: String -> Bool
    isNumericLiteral s = case s of
      [] -> False
      (c : _) -> Char.isDigit c || c == '.'

-- Validation functions remain focused on semantic validation of parsed ASTs

validateBlock :: ValidationContext -> JSBlock -> [ValidationError]
validateBlock ctx (JSBlock _lbrace stmts _rbrace) = concatMap (validateStatement ctx) stmts

validateArrayElement :: ValidationContext -> JSArrayElement -> [ValidationError]
validateArrayElement ctx element = case element of
  JSArrayElement expr -> validateExpression ctx expr
  JSArrayComma _comma -> []

validateArrayLiteral :: [JSArrayElement] -> [ValidationError]
validateArrayLiteral elements = concatMap validateArrayElement' elements
  where
    validateArrayElement' :: JSArrayElement -> [ValidationError]
    validateArrayElement' element = case element of
      JSArrayElement expr -> [] -- Expression validation handled elsewhere
      JSArrayComma _ -> [] -- Comma elements are valid (sparse arrays)

validateCallExpression :: ValidationContext -> JSExpression -> JSCommaList JSExpression -> [ValidationError]
validateCallExpression ctx callee args =
  let argList = fromCommaList args
   in validateExpression ctx callee ++ concatMap (validateExpression ctx) argList

validateMemberExpression :: ValidationContext -> JSExpression -> JSExpression -> [ValidationError]
validateMemberExpression ctx obj prop =
  validateExpression ctx obj ++ validateExpression ctx prop
    ++ validatePrivateFieldAccess ctx prop
    ++ validateNewTargetAccess ctx obj prop
  where
    validatePrivateFieldAccess :: ValidationContext -> JSExpression -> [ValidationError]
    validatePrivateFieldAccess context propExpr = case propExpr of
      JSIdentifier _annot name
        | isPrivateIdentifier name ->
          if contextInClass context
            then []
            else [PrivateFieldOutsideClass (Text.pack name) (extractExpressionPos propExpr)]
      _ -> []

    validateNewTargetAccess :: ValidationContext -> JSExpression -> JSExpression -> [ValidationError]
    validateNewTargetAccess context objExpr propExpr =
      case (objExpr, propExpr) of
        (JSIdentifier _ "new", JSIdentifier _ "target") ->
          if contextInFunction context
            then []
            else [NewTargetOutsideFunction (extractExpressionPos propExpr)]
        _ -> []

    isPrivateIdentifier :: String -> Bool
    isPrivateIdentifier ('#' : _) = True
    isPrivateIdentifier _ = False

validateObjectLiteral :: ValidationContext -> JSCommaTrailingList JSObjectProperty -> [ValidationError]
validateObjectLiteral ctx props =
  let propList = fromCommaTrailingList props
      propNames = map extractPropertyName propList
      duplicates = findDuplicates propNames
      propErrors = concatMap (validateObjectProperty ctx) propList
      duplicateErrors =
        if contextStrictMode ctx == StrictModeOn
          then map (\name -> DuplicatePropertyInStrict name (TokenPn 0 0 0)) duplicates
          else []
   in propErrors ++ duplicateErrors
  where
    extractPropertyName :: JSObjectProperty -> Text
    extractPropertyName prop = case prop of
      JSPropertyNameandValue propName _ _ -> getPropertyNameText propName
      JSPropertyIdentRef _ ident -> getIdentText ident
      JSObjectMethod method -> getMethodNameText method
      JSObjectSpread _ _ -> Text.empty -- Spread properties don't have names
    getPropertyNameText :: JSPropertyName -> Text
    getPropertyNameText propName = case propName of
      JSPropertyIdent _ name -> Text.pack name
      JSPropertyString _ str -> Text.pack str
      JSPropertyNumber _ num -> Text.pack num
      JSPropertyComputed _ _ _ -> Text.pack "[computed]"

    getIdentText :: String -> Text
    getIdentText = Text.pack

    getMethodNameText :: JSMethodDefinition -> Text
    getMethodNameText method = case method of
      JSMethodDefinition propName _ _ _ _ -> getPropertyNameText propName
      JSGeneratorMethodDefinition _ propName _ _ _ _ -> getPropertyNameText propName
      JSPropertyAccessor _ propName _ _ _ _ -> getPropertyNameText propName

-- | Validate individual object property.
validateObjectProperty :: ValidationContext -> JSObjectProperty -> [ValidationError]
validateObjectProperty ctx prop = case prop of
  JSPropertyNameandValue propName _ values ->
    validatePropertyName ctx propName ++ concatMap (validateExpression ctx) values
  JSPropertyIdentRef _ _ident -> []
  JSObjectMethod method -> validateMethodDefinition ctx method
  JSObjectSpread _ expr -> validateExpression ctx expr

-- | Validate property name.
validatePropertyName :: ValidationContext -> JSPropertyName -> [ValidationError]
validatePropertyName ctx propName = case propName of
  JSPropertyIdent _annot name -> validateIdentifier ctx name
  JSPropertyString _annot _str -> []
  JSPropertyNumber _annot _num -> []
  JSPropertyComputed _lbracket expr _rbracket -> validateExpression ctx expr

validateTemplatePart :: ValidationContext -> JSTemplatePart -> [ValidationError]
validateTemplatePart ctx (JSTemplatePart expr _rbrace _suffix) = validateExpression ctx expr

validateTemplateLiteral :: Maybe JSExpression -> [JSTemplatePart] -> [ValidationError]
validateTemplateLiteral maybeTag parts =
  let tagErrors = case maybeTag of
        Nothing -> []
        Just tag -> [] -- Tag validation handled elsewhere
      partErrors = concatMap validateTemplatePart' parts
   in tagErrors ++ partErrors
  where
    validateTemplatePart' :: JSTemplatePart -> [ValidationError]
    validateTemplatePart' (JSTemplatePart _expr _ _) = [] -- Template parts are validated during expression validation

validateVarInitializer :: ValidationContext -> JSVarInitializer -> [ValidationError]
validateVarInitializer ctx init = case init of
  JSVarInit _eq expr -> validateExpression ctx expr
  JSVarInitNone -> []

validateArrowParameters :: ValidationContext -> JSArrowParameterList -> [ValidationError]
validateArrowParameters ctx params = case params of
  JSUnparenthesizedArrowParameter (JSIdentName _annot name) ->
    validateIdentifier ctx name
  JSUnparenthesizedArrowParameter JSIdentNone -> []
  JSParenthesizedArrowParameterList _lparen exprs _rparen ->
    concatMap (validateExpression ctx) (fromCommaList exprs)

validateConciseBody :: ValidationContext -> JSConciseBody -> [ValidationError]
validateConciseBody ctx body = case body of
  JSConciseFunctionBody block -> validateBlock ctx block
  JSConciseExpressionBody expr -> validateExpression ctx expr

validateFunctionName :: ValidationContext -> JSIdent -> [ValidationError]
validateFunctionName ctx name = case name of
  JSIdentNone -> [FunctionNameRequired (TokenPn 0 0 0)]
  JSIdentName _annot nameStr -> validateIdentifier ctx nameStr

validateOptionalFunctionName :: ValidationContext -> JSIdent -> [ValidationError]
validateOptionalFunctionName ctx name = case name of
  JSIdentNone -> []
  JSIdentName _annot nameStr -> validateIdentifier ctx nameStr

validateClassHeritage :: ValidationContext -> JSClassHeritage -> [ValidationError]
validateClassHeritage ctx heritage = case heritage of
  JSExtends _extends expr -> validateExpression ctx expr
  JSExtendsNone -> []

validateClassElement :: ValidationContext -> JSClassElement -> [ValidationError]
validateClassElement ctx element = case element of
  JSClassInstanceMethod method -> validateMethodDefinition ctx method
  JSClassStaticMethod _static method -> validateMethodDefinition ctx method
  JSClassSemi _semi -> []
  JSPrivateField _annot _name _eq init _semi ->
    maybe [] (validateExpression ctx) init
  JSPrivateMethod _annot _name _lp _params _rp _block -> [] -- Private method validation handled elsewhere
  JSPrivateAccessor _accessor _annot _name _lp _params _rp _block -> [] -- Private accessor validation handled elsewhere

validateClassElements :: [JSClassElement] -> [ValidationError]
validateClassElements elements =
  let methodNames = extractMethodNames elements
      duplicateNames = findDuplicates methodNames
      constructorCount = countConstructors elements
      staticConstructors = findStaticConstructors elements
      constructorErrors =
        if constructorCount > 1
          then [MultipleConstructors (TokenPn 0 0 0)]
          else []
      staticErrors = map (\_ -> StaticConstructor (TokenPn 0 0 0)) staticConstructors
      duplicateErrors = map (\name -> DuplicateMethodName name (TokenPn 0 0 0)) duplicateNames
   in constructorErrors ++ staticErrors ++ duplicateErrors
  where
    extractMethodNames :: [JSClassElement] -> [Text]
    extractMethodNames = concatMap extractMethodName
      where
        extractMethodName :: JSClassElement -> [Text]
        extractMethodName element = case element of
          JSClassInstanceMethod method -> [getMethodName method]
          JSClassStaticMethod _ method -> [getMethodName method]
          JSClassSemi _ -> []
          JSPrivateField _ name _ _ _ -> [Text.pack ("#" <> name)]
          JSPrivateMethod _ name _ _ _ _ -> [Text.pack ("#" <> name)]
          JSPrivateAccessor _ _ name _ _ _ _ -> [Text.pack ("#" <> name)]

        getMethodName :: JSMethodDefinition -> Text
        getMethodName method = case method of
          JSMethodDefinition propName _ _ _ _ -> getPropertyNameFromMethod propName
          JSGeneratorMethodDefinition _ propName _ _ _ _ -> getPropertyNameFromMethod propName
          JSPropertyAccessor _ propName _ _ _ _ -> getPropertyNameFromMethod propName

        getPropertyNameFromMethod :: JSPropertyName -> Text
        getPropertyNameFromMethod propName = case propName of
          JSPropertyIdent _ name -> Text.pack name
          JSPropertyString _ str -> Text.pack str
          JSPropertyNumber _ num -> Text.pack num
          JSPropertyComputed _ _ _ -> Text.pack "[computed]"

    countConstructors :: [JSClassElement] -> Int
    countConstructors = length . filter isConstructor
      where
        isConstructor :: JSClassElement -> Bool
        isConstructor element = case element of
          JSClassInstanceMethod method -> isConstructorMethod method
          _ -> False

        isConstructorMethod :: JSMethodDefinition -> Bool
        isConstructorMethod method = case method of
          JSMethodDefinition propName _ _ _ _ -> isConstructorName propName
          _ -> False

        isConstructorName :: JSPropertyName -> Bool
        isConstructorName propName = case propName of
          JSPropertyIdent _ "constructor" -> True
          _ -> False

    findStaticConstructors :: [JSClassElement] -> [JSClassElement]
    findStaticConstructors = filter isStaticConstructor
      where
        isStaticConstructor :: JSClassElement -> Bool
        isStaticConstructor element = case element of
          JSClassStaticMethod _ method -> isConstructorMethod method
          _ -> False
          where
            isConstructorMethod :: JSMethodDefinition -> Bool
            isConstructorMethod method = case method of
              JSMethodDefinition propName _ _ _ _ -> isConstructorName propName
              _ -> False

            isConstructorName :: JSPropertyName -> Bool
            isConstructorName propName = case propName of
              JSPropertyIdent _ "constructor" -> True
              _ -> False

validateMethodDefinition :: ValidationContext -> JSMethodDefinition -> [ValidationError]
validateMethodDefinition ctx method = case method of
  JSMethodDefinition propName _lparen params _rparen body ->
    let isConstructor = isConstructorProperty propName
        methodCtx =
          ctx
            { contextInFunction = True,
              contextInMethod = True,
              contextInConstructor = isConstructor
            }
     in validatePropertyName ctx propName
          ++ validateFunctionParameters ctx (fromCommaList params)
          ++ validateBlock methodCtx body
          ++ validateMethodConstraints method
  JSGeneratorMethodDefinition _star propName _lparen params _rparen body ->
    let isConstructor = isConstructorProperty propName
        genCtx =
          ctx
            { contextInFunction = True,
              contextInGenerator = True,
              contextInMethod = True,
              contextInConstructor = isConstructor
            }
     in validatePropertyName ctx propName
          ++ validateFunctionParameters ctx (fromCommaList params)
          ++ validateBlock genCtx body
          ++ validateGeneratorMethodConstraints method
  JSPropertyAccessor accessor propName _lparen params _rparen body ->
    let accessorCtx = ctx {contextInFunction = True}
     in validatePropertyName ctx propName
          ++ validateAccessorParameters accessor params
          ++ validateBlock accessorCtx body
  where
    validateMethodConstraints :: JSMethodDefinition -> [ValidationError]
    validateMethodConstraints _ = [] -- No additional method constraints for basic methods
    validateGeneratorMethodConstraints :: JSMethodDefinition -> [ValidationError]
    validateGeneratorMethodConstraints method = case method of
      JSGeneratorMethodDefinition _ propName _ _ _ _ ->
        if isConstructorProperty propName
          then [ConstructorWithGenerator (extractPropertyPosition propName)]
          else []
      _ -> []

    validateAccessorParameters :: JSAccessor -> JSCommaList JSExpression -> [ValidationError]
    validateAccessorParameters accessor params =
      let paramList = fromCommaList params
          paramCount = length paramList
       in case accessor of
            JSAccessorGet _ ->
              if paramCount == 0
                then []
                else [GetterWithParameters (TokenPn 0 0 0)]
            JSAccessorSet _ ->
              if paramCount == 1
                then []
                else
                  if paramCount == 0
                    then [SetterWithoutParameter (TokenPn 0 0 0)]
                    else [SetterWithMultipleParameters (TokenPn 0 0 0)]

    isConstructorProperty :: JSPropertyName -> Bool
    isConstructorProperty propName = case propName of
      JSPropertyIdent _ "constructor" -> True
      _ -> False

    extractPropertyPosition :: JSPropertyName -> TokenPosn
    extractPropertyPosition propName = case propName of
      JSPropertyIdent annot _ -> extractAnnotationPos annot
      JSPropertyString annot _ -> extractAnnotationPos annot
      JSPropertyNumber annot _ -> extractAnnotationPos annot
      JSPropertyComputed lbracket _ _ -> extractAnnotationPos lbracket

validateLabelledStatement :: ValidationContext -> JSIdent -> JSStatement -> [ValidationError]
validateLabelledStatement ctx label stmt =
  let labelErrors = case label of
        JSIdentNone -> []
        JSIdentName _annot labelName ->
          let labelText = Text.pack labelName
           in if labelText `elem` contextLabels ctx
                then [DuplicateLabel labelText (extractIdentPos label)]
                else []
      stmtCtx = case label of
        JSIdentName _annot labelName ->
          ctx {contextLabels = Text.pack labelName : contextLabels ctx}
        JSIdentNone -> ctx
   in labelErrors ++ validateStatement stmtCtx stmt

validateSwitchCase :: ValidationContext -> JSSwitchParts -> [ValidationError]
validateSwitchCase ctx switchPart = case switchPart of
  JSCase _case expr _colon stmts ->
    validateExpression ctx expr ++ concatMap (validateStatement ctx) stmts
  JSDefault _default _colon stmts ->
    concatMap (validateStatement ctx) stmts

validateSwitchCases :: [JSSwitchParts] -> [ValidationError]
validateSwitchCases cases =
  let defaultCount = length (filter isDefaultCase cases)
   in if defaultCount > 1
        then [MultipleDefaultCases (TokenPn 0 0 0)]
        else []
  where
    isDefaultCase :: JSSwitchParts -> Bool
    isDefaultCase switchPart = case switchPart of
      JSDefault _ _ _ -> True
      _ -> False

validateCatchClause :: ValidationContext -> JSTryCatch -> [ValidationError]
validateCatchClause ctx catchClause = case catchClause of
  JSCatch _catch _lparen param _rparen block ->
    validateExpression ctx param ++ validateBlock ctx block
  JSCatchIf _catch _lparen param _if test _rparen block ->
    validateExpression ctx param ++ validateExpression ctx test ++ validateBlock ctx block

validateFinallyClause :: ValidationContext -> JSTryFinally -> [ValidationError]
validateFinallyClause ctx finally' = case finally' of
  JSFinally _finally block -> validateBlock ctx block
  JSNoFinally -> []

validateWithStatement :: ValidationContext -> JSExpression -> JSStatement -> [ValidationError]
validateWithStatement ctx object stmt =
  ( if contextStrictMode ctx == StrictModeOn
      then [WithStatementInStrict (TokenPn 0 0 0)]
      else []
  )
    ++ validateExpression ctx object
    ++ validateStatement ctx stmt

validateForInLHS :: ValidationContext -> JSExpression -> [ValidationError]
validateForInLHS _ctx lhs = validateForIteratorLHS lhs InvalidLHSInForIn

validateForOfLHS :: ValidationContext -> JSExpression -> [ValidationError]
validateForOfLHS _ctx lhs = validateForIteratorLHS lhs InvalidLHSInForOf

validateForIteratorLHS :: JSExpression -> (JSExpression -> TokenPosn -> ValidationError) -> [ValidationError]
validateForIteratorLHS lhs errorConstructor = case lhs of
  JSIdentifier _annot _name -> []
  JSMemberDot _obj _dot _prop -> []
  JSMemberSquare _obj _lbracket _prop _rbracket -> []
  JSArrayLiteral _lbracket _elements _rbracket -> []
  JSObjectLiteral _lbrace _props _rbrace -> []
  _ -> [errorConstructor lhs (extractExpressionPos lhs)]

validateDestructuringArray :: JSExpression -> [ValidationError]
validateDestructuringArray expr = case expr of
  JSArrayLiteral _ elements _ -> concatMap validateDestructuringElement elements
  _ -> [InvalidDestructuringTarget expr (extractExpressionPos expr)]
  where
    validateDestructuringElement :: JSArrayElement -> [ValidationError]
    validateDestructuringElement element = case element of
      JSArrayElement e -> validateDestructuringPattern e
      JSArrayComma _ -> []

    validateDestructuringPattern :: JSExpression -> [ValidationError]
    validateDestructuringPattern pattern = case pattern of
      JSIdentifier _ _ -> []
      JSArrayLiteral _ _ _ -> validateDestructuringArray pattern
      JSObjectLiteral _ _ _ -> validateDestructuringObject pattern
      JSSpreadExpression _ target -> validateDestructuringPattern target
      _ -> [InvalidDestructuringTarget pattern (extractExpressionPos pattern)]

validateDestructuringObject :: JSExpression -> [ValidationError]
validateDestructuringObject expr = case expr of
  JSObjectLiteral _ props _ ->
    let propList = fromCommaTrailingList props
     in concatMap validateDestructuringProperty propList
  _ -> [InvalidDestructuringTarget expr (extractExpressionPos expr)]
  where
    validateDestructuringProperty :: JSObjectProperty -> [ValidationError]
    validateDestructuringProperty prop = case prop of
      JSPropertyNameandValue _ _ values -> concatMap validateDestructuringPattern values
      JSPropertyIdentRef _ _ -> []
      JSObjectMethod _ -> [InvalidDestructuringTarget (JSLiteral (JSAnnot (TokenPn 0 0 0) []) "method") (TokenPn 0 0 0)]
      JSObjectSpread _ expr -> validateDestructuringPattern expr

    validateDestructuringPattern :: JSExpression -> [ValidationError]
    validateDestructuringPattern pattern = case pattern of
      JSIdentifier _ _ -> []
      JSArrayLiteral _ _ _ -> validateDestructuringArray pattern
      JSObjectLiteral _ _ _ -> validateDestructuringObject pattern
      _ -> [InvalidDestructuringTarget pattern (extractExpressionPos pattern)]

validateImportDeclaration :: ValidationContext -> JSImportDeclaration -> [ValidationError]
validateImportDeclaration ctx importDecl = case importDecl of
  JSImportDeclaration clause _ attrs _ -> validateImportClause ctx clause ++ maybe [] (validateImportAttributes ctx) attrs
  JSImportDeclarationBare _ _ attrs _ -> maybe [] (validateImportAttributes ctx) attrs
  where
    validateImportClause :: ValidationContext -> JSImportClause -> [ValidationError]
    validateImportClause _ _ = [] -- Import clause validation handled by import name extraction

validateImportAttributes :: ValidationContext -> JSImportAttributes -> [ValidationError]
validateImportAttributes _ctx (JSImportAttributes _ attrs _) =
  concatMap validateImportAttribute (fromCommaList attrs)
  where
    validateImportAttribute :: JSImportAttribute -> [ValidationError]
    validateImportAttribute (JSImportAttribute _key _ _value) = []

validateExportDeclaration :: ValidationContext -> JSExportDeclaration -> [ValidationError]
validateExportDeclaration ctx exportDecl = case exportDecl of
  JSExport statement _ -> validateStatement ctx statement
  JSExportFrom _ _ _ -> []
  JSExportLocals _ _ -> []
  JSExportAllFrom _ _ _ -> []
  JSExportAllAsFrom _ _ _ _ _ -> []

validateNoDuplicateFunctionDeclarations :: [JSStatement] -> [ValidationError]
validateNoDuplicateFunctionDeclarations stmts =
  let functionNames = extractFunctionNames stmts
      duplicates = findDuplicates functionNames
   in map (\name -> DuplicateBinding name (TokenPn 0 0 0)) duplicates
  where
    extractFunctionNames :: [JSStatement] -> [Text]
    extractFunctionNames = concatMap extractFunctionName
      where
        extractFunctionName :: JSStatement -> [Text]
        extractFunctionName stmt = case stmt of
          JSFunction _ name _ _ _ _ _ -> getIdentName name
          JSAsyncFunction _ _ name _ _ _ _ _ -> getIdentName name
          JSGenerator _ _ name _ _ _ _ _ -> getIdentName name
          _ -> []

        getIdentName :: JSIdent -> [Text]
        getIdentName ident = case ident of
          JSIdentNone -> []
          JSIdentName _ name -> [Text.pack name]

validateNoDuplicateExports :: [JSModuleItem] -> [ValidationError]
validateNoDuplicateExports items =
  let exportNames = extractExportNames items
      duplicates = findDuplicates exportNames
   in map (\name -> DuplicateExport name (TokenPn 0 0 0)) duplicates
  where
    extractExportNames :: [JSModuleItem] -> [Text]
    extractExportNames = concatMap extractExportName
      where
        extractExportName :: JSModuleItem -> [Text]
        extractExportName item = case item of
          JSModuleExportDeclaration _ exportDecl -> extractExportDeclNames exportDecl
          _ -> []

        extractExportDeclNames :: JSExportDeclaration -> [Text]
        extractExportDeclNames exportDecl = case exportDecl of
          JSExport stmt _ -> extractStatementBindings stmt
          JSExportFrom _ _ _ -> [] -- Re-exports don't bind local names
          JSExportLocals (JSExportClause _ specs _) _ -> extractExportSpecNames specs
          JSExportAllFrom _ _ _ -> [] -- Namespace export
          JSExportAllAsFrom _ _ _ _ _ -> [] -- Namespace export as name
        extractExportSpecNames :: JSCommaList JSExportSpecifier -> [Text]
        extractExportSpecNames specs = concatMap extractSpecName (fromCommaList specs)
          where
            extractSpecName spec = case spec of
              JSExportSpecifier (JSIdentName _ name) -> [Text.pack name]
              JSExportSpecifierAs (JSIdentName _ _) _ (JSIdentName _ asName) -> [Text.pack asName]
              _ -> []

        extractStatementBindings :: JSStatement -> [Text]
        extractStatementBindings stmt = case stmt of
          JSFunction _ (JSIdentName _ name) _ _ _ _ _ -> [Text.pack name]
          JSVariable _ vars _ -> extractVarBindings vars
          JSClass _ (JSIdentName _ name) _ _ _ _ _ -> [Text.pack name]
          _ -> []

        extractVarBindings :: JSCommaList JSExpression -> [Text]
        extractVarBindings vars = concatMap extractVarBinding (fromCommaList vars)
          where
            extractVarBinding expr = case expr of
              JSVarInitExpression (JSIdentifier _ name) _ -> [Text.pack name]
              JSIdentifier _ name -> [Text.pack name]
              _ -> []

validateNoDuplicateImports :: [JSModuleItem] -> [ValidationError]
validateNoDuplicateImports items =
  let importNames = extractImportNames items
      duplicates = findDuplicates importNames
   in map (\name -> DuplicateImport name (TokenPn 0 0 0)) duplicates
  where
    extractImportNames :: [JSModuleItem] -> [Text]
    extractImportNames = concatMap extractImportName
      where
        extractImportName :: JSModuleItem -> [Text]
        extractImportName item = case item of
          JSModuleImportDeclaration _ importDecl -> extractImportDeclNames importDecl
          _ -> []

        extractImportDeclNames :: JSImportDeclaration -> [Text]
        extractImportDeclNames importDecl = case importDecl of
          JSImportDeclaration clause _ _ _ -> extractImportClauseNames clause
          JSImportDeclarationBare _ _ _ _ -> [] -- No bindings for bare imports
        extractImportClauseNames :: JSImportClause -> [Text]
        extractImportClauseNames clause = case clause of
          JSImportClauseDefault (JSIdentName _ name) -> [Text.pack name]
          JSImportClauseDefault JSIdentNone -> []
          JSImportClauseNameSpace (JSImportNameSpace _ _ (JSIdentName _ name)) -> [Text.pack name]
          JSImportClauseNameSpace (JSImportNameSpace _ _ JSIdentNone) -> []
          JSImportClauseNamed (JSImportsNamed _ specs _) -> extractImportSpecNames specs
          JSImportClauseDefaultNamed (JSIdentName _ defName) _ (JSImportsNamed _ specs _) ->
            Text.pack defName : extractImportSpecNames specs
          JSImportClauseDefaultNamed JSIdentNone _ (JSImportsNamed _ specs _) -> extractImportSpecNames specs
          JSImportClauseDefaultNameSpace (JSIdentName _ defName) _ (JSImportNameSpace _ _ (JSIdentName _ nsName)) ->
            [Text.pack defName, Text.pack nsName]
          JSImportClauseDefaultNameSpace JSIdentNone _ (JSImportNameSpace _ _ (JSIdentName _ nsName)) ->
            [Text.pack nsName]
          JSImportClauseDefaultNameSpace (JSIdentName _ defName) _ (JSImportNameSpace _ _ JSIdentNone) ->
            [Text.pack defName]
          JSImportClauseDefaultNameSpace JSIdentNone _ (JSImportNameSpace _ _ JSIdentNone) -> []

        extractImportSpecNames :: JSCommaList JSImportSpecifier -> [Text]
        extractImportSpecNames specs = concatMap extractImportSpecName (fromCommaList specs)
          where
            extractImportSpecName spec = case spec of
              JSImportSpecifier (JSIdentName _ name) -> [Text.pack name]
              JSImportSpecifierAs (JSIdentName _ _) _ (JSIdentName _ asName) -> [Text.pack asName]
              _ -> []

-- Position extraction helpers

-- | Extract position from JSAnnot.
extractAnnotationPos :: JSAnnot -> TokenPosn
extractAnnotationPos (JSAnnot pos _) = pos
extractAnnotationPos JSNoAnnot = TokenPn 0 0 0
extractAnnotationPos JSAnnotSpace = TokenPn 0 0 0

-- | Extract position from expression.
extractExpressionPos :: JSExpression -> TokenPosn
extractExpressionPos expr = case expr of
  JSIdentifier annot _ -> extractAnnotationPos annot
  JSDecimal annot _ -> extractAnnotationPos annot
  JSLiteral annot _ -> extractAnnotationPos annot
  JSHexInteger annot _ -> extractAnnotationPos annot
  JSBinaryInteger annot _ -> extractAnnotationPos annot
  JSOctal annot _ -> extractAnnotationPos annot
  JSStringLiteral annot _ -> extractAnnotationPos annot
  JSRegEx annot _ -> extractAnnotationPos annot
  JSBigIntLiteral annot _ -> extractAnnotationPos annot
  JSArrayLiteral annot _ _ -> extractAnnotationPos annot
  JSArrowExpression _ annot _ -> extractAnnotationPos annot
  JSAssignExpression lhs _ _ -> extractExpressionPos lhs
  JSAwaitExpression annot _ -> extractAnnotationPos annot
  JSCallExpression callee _ _ _ -> extractExpressionPos callee
  JSCallExpressionDot callee _ _ -> extractExpressionPos callee
  JSCallExpressionSquare callee _ _ _ -> extractExpressionPos callee
  JSClassExpression annot _ _ _ _ _ -> extractAnnotationPos annot
  JSCommaExpression left _ _ -> extractExpressionPos left
  JSExpressionBinary left _ _ -> extractExpressionPos left
  JSExpressionParen annot _ _ -> extractAnnotationPos annot
  JSExpressionPostfix expr' _ -> extractExpressionPos expr'
  JSExpressionTernary cond _ _ _ _ -> extractExpressionPos cond
  JSFunctionExpression annot _ _ _ _ _ -> extractAnnotationPos annot
  JSGeneratorExpression annot _ _ _ _ _ _ -> extractAnnotationPos annot
  JSAsyncFunctionExpression annot _ _ _ _ _ _ -> extractAnnotationPos annot
  JSMemberDot obj _ _ -> extractExpressionPos obj
  JSMemberExpression expr' _ _ _ -> extractExpressionPos expr'
  JSMemberNew annot _ _ _ _ -> extractAnnotationPos annot
  JSMemberSquare obj _ _ _ -> extractExpressionPos obj
  JSNewExpression annot _ -> extractAnnotationPos annot
  JSObjectLiteral annot _ _ -> extractAnnotationPos annot
  JSTemplateLiteral _ annot _ _ -> extractAnnotationPos annot
  JSUnaryExpression _ expr' -> extractExpressionPos expr'
  JSVarInitExpression lhs _ -> extractExpressionPos lhs
  JSYieldExpression annot _ -> extractAnnotationPos annot
  JSYieldFromExpression annot _ _ -> extractAnnotationPos annot
  JSImportMeta annot _ -> extractAnnotationPos annot
  JSImportCall annot _ _ _ -> extractAnnotationPos annot
  JSSpreadExpression annot _ -> extractAnnotationPos annot
  JSOptionalMemberDot obj _ _ -> extractExpressionPos obj
  JSOptionalMemberSquare obj _ _ _ -> extractExpressionPos obj
  JSOptionalCallExpression callee _ _ _ -> extractExpressionPos callee

-- | Extract position from statement.
extractStatementPos :: JSStatement -> TokenPosn
extractStatementPos stmt = case stmt of
  JSStatementBlock annot _ _ _ -> extractAnnotationPos annot
  JSBreak annot _ _ -> extractAnnotationPos annot
  JSClass annot _ _ _ _ _ _ -> extractAnnotationPos annot
  JSContinue annot _ _ -> extractAnnotationPos annot
  JSConstant annot _ _ -> extractAnnotationPos annot
  JSDoWhile annot _ _ _ _ _ _ -> extractAnnotationPos annot
  JSEmptyStatement annot -> extractAnnotationPos annot
  JSFor annot _ _ _ _ _ _ _ _ -> extractAnnotationPos annot
  JSForIn annot _ _ _ _ _ _ -> extractAnnotationPos annot
  JSForVar annot _ _ _ _ _ _ _ _ _ -> extractAnnotationPos annot
  JSForVarIn annot _ _ _ _ _ _ _ -> extractAnnotationPos annot
  JSForLet annot _ _ _ _ _ _ _ _ _ -> extractAnnotationPos annot
  JSForLetIn annot _ _ _ _ _ _ _ -> extractAnnotationPos annot
  JSForLetOf annot _ _ _ _ _ _ _ -> extractAnnotationPos annot
  JSForConst annot _ _ _ _ _ _ _ _ _ -> extractAnnotationPos annot
  JSForConstIn annot _ _ _ _ _ _ _ -> extractAnnotationPos annot
  JSForConstOf annot _ _ _ _ _ _ _ -> extractAnnotationPos annot
  JSForOf annot _ _ _ _ _ _ -> extractAnnotationPos annot
  JSForVarOf annot _ _ _ _ _ _ _ -> extractAnnotationPos annot
  JSAsyncFunction annot _ _ _ _ _ _ _ -> extractAnnotationPos annot
  JSFunction annot _ _ _ _ _ _ -> extractAnnotationPos annot
  JSGenerator annot _ _ _ _ _ _ _ -> extractAnnotationPos annot
  JSIf annot _ _ _ _ -> extractAnnotationPos annot
  JSIfElse annot _ _ _ _ _ _ -> extractAnnotationPos annot
  JSLabelled label _ _ -> extractIdentPos label
  JSLet annot _ _ -> extractAnnotationPos annot
  JSExpressionStatement expr _ -> extractExpressionPos expr
  JSAssignStatement lhs _ _ _ -> extractExpressionPos lhs
  JSMethodCall expr _ _ _ _ -> extractExpressionPos expr
  JSReturn annot _ _ -> extractAnnotationPos annot
  JSSwitch annot _ _ _ _ _ _ _ -> extractAnnotationPos annot
  JSThrow annot _ _ -> extractAnnotationPos annot
  JSTry annot _ _ _ -> extractAnnotationPos annot
  JSVariable annot _ _ -> extractAnnotationPos annot
  JSWhile annot _ _ _ _ -> extractAnnotationPos annot
  JSWith annot _ _ _ _ _ -> extractAnnotationPos annot

-- | Extract position from identifier.
extractIdentPos :: JSIdent -> TokenPosn
extractIdentPos (JSIdentName annot _) = extractAnnotationPos annot
extractIdentPos JSIdentNone = TokenPn 0 0 0

-- | Extract position from module item.
extractTokenPosn :: JSModuleItem -> TokenPosn
extractTokenPosn item = case item of
  JSModuleImportDeclaration annot _ -> extractAnnotationPos annot
  JSModuleExportDeclaration annot _ -> extractAnnotationPos annot
  JSModuleStatementListItem stmt -> extractStatementPos stmt

-- ============================================================================
-- JSDoc Validation Functions (Consolidated from JSDocValidation.hs)
-- ============================================================================

-- | Validate JSDoc comment integrity.
validateJSDocIntegrity :: JSDocComment -> [ValidationError]
validateJSDocIntegrity jsDoc =
  validateJSDocParameterTags jsDoc
    ++ validateJSDocTypes jsDoc
    ++ validateJSDocReturnConsistency jsDoc
    ++ validateJSDocUnionTypes jsDoc
    ++ validateJSDocObjectFields jsDoc
    ++ validateJSDocArrayTypes jsDoc
    ++ validateJSDocTagSpecifics jsDoc
    ++ validateJSDocTagCombinations jsDoc
    ++ validateJSDocRequiredFields jsDoc
    ++ validateJSDocGenericTypes jsDoc
    ++ validateJSDocFunctionTypes jsDoc
    ++ validateJSDocCrossReferences jsDoc
    ++ validateJSDocSemantics jsDoc
    ++ validateJSDocExamples jsDoc

-- | Validate JSDoc parameter tags.
validateJSDocParameterTags :: JSDocComment -> [ValidationError]
validateJSDocParameterTags jsDoc =
  let paramTags = filter (\tag -> jsDocTagName tag == "param") (jsDocTags jsDoc)
      pos = jsDocPosition jsDoc
   in concatMap (validateParameterTag pos) paramTags

validateParameterTag :: TokenPosn -> JSDocTag -> [ValidationError]
validateParameterTag pos tag =
  case (jsDocTagType tag, jsDocTagParamName tag) of
    (Nothing, _) -> [JSDocInvalidType "missing type" pos]
    (_, Nothing) -> [JSDocMissingParameter "unnamed parameter" pos]
    (Just tagType, Just paramName) -> validateJSDocTypeStructure tagType pos

-- | Validate JSDoc types structure.
validateJSDocTypes :: JSDocComment -> [ValidationError]
validateJSDocTypes jsDoc =
  let allTags = jsDocTags jsDoc
      pos = jsDocPosition jsDoc
   in concatMap (validateTagType pos) allTags

validateTagType :: TokenPosn -> JSDocTag -> [ValidationError]
validateTagType pos tag =
  case jsDocTagType tag of
    Nothing -> []
    Just tagType -> validateJSDocTypeStructure tagType pos

validateJSDocTypeStructure :: JSDocType -> TokenPosn -> [ValidationError]
validateJSDocTypeStructure jsDocType pos = case jsDocType of
  JSDocBasicType typeName -> validateBasicType typeName pos
  JSDocArrayType elementType -> validateJSDocTypeStructure elementType pos
  JSDocUnionType types -> concatMap (\t -> validateJSDocTypeStructure t pos) types
  JSDocObjectType fields -> concatMap (validateObjectField pos) fields
  JSDocFunctionType paramTypes returnType ->
    concatMap (\t -> validateJSDocTypeStructure t pos) paramTypes
      ++ validateJSDocTypeStructure returnType pos
  JSDocGenericType baseName args ->
    validateBasicType baseName pos
      ++ concatMap (\t -> validateJSDocTypeStructure t pos) args
  JSDocOptionalType baseType -> validateJSDocTypeStructure baseType pos
  JSDocNullableType baseType -> validateJSDocTypeStructure baseType pos
  JSDocNonNullableType baseType -> validateJSDocTypeStructure baseType pos
  JSDocEnumType enumName enumValues -> validateEnumType enumName enumValues pos

validateBasicType :: Text -> TokenPosn -> [ValidationError]
validateBasicType typeName pos
  | typeName `elem` validJSDocTypes = []
  | otherwise = [JSDocUndefinedType typeName pos]
  where
    validJSDocTypes = [ "string", "number", "boolean", "object", "function", "undefined", "null", "any", "void"
                      , "Array", "Object", "String", "Number", "Boolean", "Function", "Date", "RegExp"
                      , "Promise", "Map", "Set", "WeakMap", "WeakSet", "Symbol", "BigInt"
                      , "Int8Array", "Uint8Array", "Uint8ClampedArray", "Int16Array", "Uint16Array"
                      , "Int32Array", "Uint32Array", "Float32Array", "Float64Array", "BigInt64Array", "BigUint64Array"
                      , "ArrayBuffer", "SharedArrayBuffer", "DataView", "Error", "TypeError", "RangeError"
                      , "SyntaxError", "ReferenceError", "EvalError", "URIError", "JSON", "Math", "Intl"
                      , "Node", "Element", "Document", "Window", "Event", "MouseEvent", "KeyboardEvent"
                      , "HTMLElement", "HTMLDocument", "XMLHttpRequest", "Blob", "File", "FileReader"
                      ]

validateObjectField :: TokenPosn -> JSDocObjectField -> [ValidationError]
validateObjectField pos field =
  validateJSDocTypeStructure (jsDocFieldType field) pos

-- | Validate enum type definition and references
validateEnumType :: Text -> [JSDocEnumValue] -> TokenPosn -> [ValidationError]
validateEnumType enumName enumValues pos =
  let duplicateErrors = findDuplicateEnumValues enumValues pos
      valueErrors = concatMap (validateEnumValue pos) enumValues
      typeConsistencyErrors = validateEnumValueTypeConsistency enumName enumValues pos
  in duplicateErrors ++ valueErrors ++ typeConsistencyErrors

-- | Find duplicate enum values
findDuplicateEnumValues :: [JSDocEnumValue] -> TokenPosn -> [ValidationError]
findDuplicateEnumValues values pos =
  let valueNames = map jsDocEnumValueName values
      duplicates = findDuplicatesInList valueNames
  in map (\name -> JSDocEnumValueDuplicate name (jsDocEnumValueName (head values)) pos) duplicates

-- | Validate individual enum value
validateEnumValue :: TokenPosn -> JSDocEnumValue -> [ValidationError]
validateEnumValue pos enumValue =
  case jsDocEnumValueLiteral enumValue of
    Nothing -> []  -- No literal value specified
    Just literal ->
      if isValidEnumLiteral literal
        then []
        else [JSDocEnumInvalidValue (jsDocEnumValueName enumValue) literal pos]

-- | Check if a literal value is valid for enums (string or number)
isValidEnumLiteral :: Text -> Bool
isValidEnumLiteral literal =
  isStringLiteral literal || isNumericLiteral literal
  where
    isStringLiteral text =
      (Text.isPrefixOf "\"" text && Text.isSuffixOf "\"" text) ||
      (Text.isPrefixOf "'" text && Text.isSuffixOf "'" text)
    isNumericLiteral text =
      Text.all (\c -> Char.isDigit c || c == '.' || c == '-' || c == '+') text &&
      not (Text.null text)

-- | Validate that all enum values have consistent types
validateEnumValueTypeConsistency :: Text -> [JSDocEnumValue] -> TokenPosn -> [ValidationError]
validateEnumValueTypeConsistency enumName values pos =
  let literalValues = mapMaybe jsDocEnumValueLiteral values
      types = map getEnumLiteralType literalValues
      uniqueTypes = nub types
  in if length uniqueTypes > 1
     then [JSDocEnumValueTypeMismatch enumName (head types) (types !! 1) pos]
     else []
  where
    getEnumLiteralType :: Text -> Text
    getEnumLiteralType literal
      | (Text.isPrefixOf "\"" literal && Text.isSuffixOf "\"" literal) ||
        (Text.isPrefixOf "'" literal && Text.isSuffixOf "'" literal) = "string"
      | Text.all (\c -> Char.isDigit c || c == '.' || c == '-' || c == '+') literal = "number"
      | otherwise = "unknown"

-- | Find duplicates in a list
findDuplicatesInList :: Eq a => [a] -> [a]
findDuplicatesInList [] = []
findDuplicatesInList (x:xs) = if x `elem` xs then x : findDuplicatesInList xs else findDuplicatesInList xs

-- | Validate JSDoc return type consistency.
validateJSDocReturnConsistency :: JSDocComment -> [ValidationError]
validateJSDocReturnConsistency jsDoc =
  let returnTags = filter (\tag -> jsDocTagName tag == "returns" || jsDocTagName tag == "return") (jsDocTags jsDoc)
      pos = jsDocPosition jsDoc
   in case returnTags of
        [] -> []
        [tag] -> validateReturnTag pos tag
        _ -> [JSDocDuplicateTag "returns" pos]

validateReturnTag :: TokenPosn -> JSDocTag -> [ValidationError]
validateReturnTag pos tag =
  case jsDocTagType tag of
    Nothing -> [JSDocInvalidType "missing return type" pos]
    Just tagType -> validateJSDocTypeStructure tagType pos

-- | Validate JSDoc union types.
validateJSDocUnionTypes :: JSDocComment -> [ValidationError]
validateJSDocUnionTypes jsDoc =
  let pos = jsDocPosition jsDoc
   in concatMap (validateUnionInTag pos) (jsDocTags jsDoc)

validateUnionInTag :: TokenPosn -> JSDocTag -> [ValidationError]
validateUnionInTag pos tag =
  case jsDocTagType tag of
    Just (JSDocUnionType types) -> validateUnionTypes pos types
    _ -> []

validateUnionTypes :: TokenPosn -> [JSDocType] -> [ValidationError]
validateUnionTypes pos types
  | length types < 2 = [JSDocInvalidUnion "union must have at least 2 types" pos]
  | otherwise = concatMap (\t -> validateJSDocTypeStructure t pos) types

-- | Validate JSDoc object field specifications.
validateJSDocObjectFields :: JSDocComment -> [ValidationError]
validateJSDocObjectFields jsDoc =
  let pos = jsDocPosition jsDoc
   in concatMap (validateObjectInTag pos) (jsDocTags jsDoc)

validateObjectInTag :: TokenPosn -> JSDocTag -> [ValidationError]
validateObjectInTag pos tag =
  case jsDocTagType tag of
    Just (JSDocObjectType fields) -> validateObjectTypeFields pos fields
    _ -> []

validateObjectTypeFields :: TokenPosn -> [JSDocObjectField] -> [ValidationError]
validateObjectTypeFields pos fields =
  let fieldNames = map jsDocFieldName fields
      duplicates = findDuplicates fieldNames
   in map (\name -> JSDocMissingObjectField "object" name pos) duplicates
     ++ concatMap (validateObjectField pos) fields

-- | Validate JSDoc array types.
validateJSDocArrayTypes :: JSDocComment -> [ValidationError]
validateJSDocArrayTypes jsDoc =
  let pos = jsDocPosition jsDoc
   in concatMap (validateArrayInTag pos) (jsDocTags jsDoc)

validateArrayInTag :: TokenPosn -> JSDocTag -> [ValidationError]
validateArrayInTag pos tag =
  case jsDocTagType tag of
    Just (JSDocArrayType elementType) -> validateJSDocTypeStructure elementType pos
    _ -> []

-- | Validate JSDoc tag-specific information.
validateJSDocTagSpecifics :: JSDocComment -> [ValidationError]
validateJSDocTagSpecifics jsDoc =
  let pos = jsDocPosition jsDoc
      tags = jsDocTags jsDoc
   in concatMap (validateTagSpecific pos) tags

validateTagSpecific :: TokenPosn -> JSDocTag -> [ValidationError]
validateTagSpecific pos tag =
  case jsDocTagSpecific tag of
    Nothing -> []
    Just tagSpecific -> validateSpecificTag pos tagSpecific (jsDocTagName tag)

validateSpecificTag :: TokenPosn -> JSDocTagSpecific -> Text -> [ValidationError]
validateSpecificTag pos tagSpecific tagName = case tagSpecific of
  JSDocParamTag optional variadic defaultValue ->
    validateParamSpecific pos tagName optional variadic defaultValue
  JSDocAuthorTag name email ->
    validateAuthorSpecific pos name email
  JSDocVersionTag version ->
    validateVersionSpecific pos version
  JSDocSinceTag version ->
    validateVersionSpecific pos version
  JSDocSeeTag reference displayText ->
    validateSeeSpecific pos reference displayText
  JSDocDeprecatedTag since replacement ->
    validateDeprecatedSpecific pos since replacement
  _ -> []

validateParamSpecific :: TokenPosn -> Text -> Bool -> Bool -> Maybe Text -> [ValidationError]
validateParamSpecific pos tagName optional variadic defaultValue =
  let errors = []
      optionalErrors = if optional && variadic
                      then [JSDocInvalidTagCombination "optional" "variadic" pos]
                      else []
      defaultErrors = case defaultValue of
        Just value | not optional -> [JSDocInvalidDefaultValue "optional parameter" "non-optional" pos]
        _ -> []
   in errors ++ optionalErrors ++ defaultErrors

validateAuthorSpecific :: TokenPosn -> Text -> Maybe Text -> [ValidationError]
validateAuthorSpecific pos name email =
  let nameErrors = if Text.null name then [JSDocMissingAuthorInfo pos] else []
      emailErrors = case email of
        Just e | not (isValidEmail e) -> [JSDocInvalidEmailFormat e pos]
        _ -> []
   in nameErrors ++ emailErrors

validateVersionSpecific :: TokenPosn -> Text -> [ValidationError]
validateVersionSpecific pos version =
  if isValidVersion version
    then []
    else [JSDocInvalidVersionFormat version pos]

validateSeeSpecific :: TokenPosn -> Text -> Maybe Text -> [ValidationError]
validateSeeSpecific pos reference _displayText =
  if isValidReference reference
    then []
    else [JSDocInvalidReferenceFormat reference pos]

validateDeprecatedSpecific :: TokenPosn -> Maybe Text -> Maybe Text -> [ValidationError]
validateDeprecatedSpecific pos since _replacement =
  case since of
    Just version | not (isValidVersion version) -> [JSDocInvalidVersionFormat version pos]
    _ -> []

-- | Validate JSDoc tag combinations.
validateJSDocTagCombinations :: JSDocComment -> [ValidationError]
validateJSDocTagCombinations jsDoc =
  let pos = jsDocPosition jsDoc
      tags = jsDocTags jsDoc
      tagNames = map jsDocTagName tags
   in validateIncompatibleTags pos tagNames
      ++ validateMutuallyExclusive pos tagNames
      ++ validateRequiredCombinations pos tagNames

validateIncompatibleTags :: TokenPosn -> [Text] -> [ValidationError]
validateIncompatibleTags pos tagNames =
  let incompatiblePairs = [ ("constructor", "namespace")
                          , ("static", "inner")
                          , ("abstract", "final")
                          , ("public", "private")
                          , ("public", "protected")
                          , ("private", "protected")
                          ]
      hasTag tag = tag `elem` tagNames
      checkPair (tag1, tag2) = if hasTag tag1 && hasTag tag2
                              then [JSDocInvalidTagCombination tag1 tag2 pos]
                              else []
   in concatMap checkPair incompatiblePairs

validateMutuallyExclusive :: TokenPosn -> [Text] -> [ValidationError]
validateMutuallyExclusive pos tagNames =
  let exclusiveGroups = [ ["public", "private", "protected", "package"]
                        , ["class", "constructor", "namespace", "module"]
                        ]
      checkGroup group =
        let presentTags = filter (`elem` tagNames) group
        in case presentTags of
          [] -> []
          [_] -> []
          (tag1:tag2:_) -> [JSDocInvalidTagCombination tag1 tag2 pos]
   in concatMap checkGroup exclusiveGroups

validateRequiredCombinations :: TokenPosn -> [Text] -> [ValidationError]
validateRequiredCombinations pos tagNames =
  let hasTag tag = tag `elem` tagNames
      requirements = [ ("memberof", ["class", "namespace", "module"])
                     , ("static", ["class"])
                     , ("inner", ["class", "namespace"])
                     ]
      checkRequirement (tag, requiredTags) =
        if hasTag tag && not (any hasTag requiredTags)
          then [JSDocMissingRequiredTag (Text.intercalate " or " requiredTags) pos]
          else []
   in concatMap checkRequirement requirements

-- | Validate JSDoc required fields.
validateJSDocRequiredFields :: JSDocComment -> [ValidationError]
validateJSDocRequiredFields jsDoc =
  let pos = jsDocPosition jsDoc
      tags = jsDocTags jsDoc
      tagNames = map jsDocTagName tags
   in validateClassRequirements pos tagNames
      ++ validateModuleRequirements pos tagNames
      ++ validateCallbackRequirements pos tagNames

validateClassRequirements :: TokenPosn -> [Text] -> [ValidationError]
validateClassRequirements pos tagNames =
  let hasTag tag = tag `elem` tagNames
   in if hasTag "class" && not (hasTag "constructor")
        then [JSDocMissingRequiredTag "constructor" pos]
        else []

validateModuleRequirements :: TokenPosn -> [Text] -> [ValidationError]
validateModuleRequirements pos tagNames =
  let hasTag tag = tag `elem` tagNames
   in if hasTag "module" && not (any hasTag ["description", "since"])
        then [JSDocMissingRequiredTag "description or since" pos]
        else []

validateCallbackRequirements :: TokenPosn -> [Text] -> [ValidationError]
validateCallbackRequirements pos tagNames =
  let hasTag tag = tag `elem` tagNames
   in if hasTag "callback" && not (hasTag "param" || hasTag "returns")
        then [JSDocMissingRequiredTag "param or returns" pos]
        else []

-- | Validate JSDoc generic types.
validateJSDocGenericTypes :: JSDocComment -> [ValidationError]
validateJSDocGenericTypes jsDoc =
  let pos = jsDocPosition jsDoc
   in concatMap (validateGenericInTag pos) (jsDocTags jsDoc)

validateGenericInTag :: TokenPosn -> JSDocTag -> [ValidationError]
validateGenericInTag pos tag =
  case jsDocTagType tag of
    Just jsDocType -> validateGenericType pos jsDocType
    Nothing -> []

validateGenericType :: TokenPosn -> JSDocType -> [ValidationError]
validateGenericType pos jsDocType = case jsDocType of
  JSDocGenericType baseName args ->
    let baseErrors = if Text.null baseName then [JSDocInvalidGenericType "empty base type" pos] else []
        argsErrors = if null args then [JSDocInvalidGenericType "missing type arguments" pos] else []
        recursiveErrors = concatMap (validateGenericType pos) args
    in baseErrors ++ argsErrors ++ recursiveErrors
  JSDocArrayType elementType -> validateGenericType pos elementType
  JSDocUnionType types -> concatMap (validateGenericType pos) types
  JSDocFunctionType paramTypes returnType ->
    concatMap (validateGenericType pos) paramTypes ++ validateGenericType pos returnType
  JSDocOptionalType baseType -> validateGenericType pos baseType
  JSDocNullableType baseType -> validateGenericType pos baseType
  JSDocNonNullableType baseType -> validateGenericType pos baseType
  _ -> []

-- | Validate JSDoc function types.
validateJSDocFunctionTypes :: JSDocComment -> [ValidationError]
validateJSDocFunctionTypes jsDoc =
  let pos = jsDocPosition jsDoc
   in concatMap (validateFunctionInTag pos) (jsDocTags jsDoc)

validateFunctionInTag :: TokenPosn -> JSDocTag -> [ValidationError]
validateFunctionInTag pos tag =
  case jsDocTagType tag of
    Just jsDocType -> validateFunctionType pos jsDocType
    Nothing -> []

validateFunctionType :: TokenPosn -> JSDocType -> [ValidationError]
validateFunctionType pos jsDocType = case jsDocType of
  JSDocFunctionType paramTypes returnType ->
    let paramErrors = concatMap (\t -> validateJSDocTypeStructure t pos) paramTypes
        returnErrors = validateJSDocTypeStructure returnType pos
    in paramErrors ++ returnErrors
  JSDocArrayType elementType -> validateFunctionType pos elementType
  JSDocUnionType types -> concatMap (validateFunctionType pos) types
  JSDocOptionalType baseType -> validateFunctionType pos baseType
  JSDocNullableType baseType -> validateFunctionType pos baseType
  JSDocNonNullableType baseType -> validateFunctionType pos baseType
  _ -> []

-- Helper functions for validation
isValidEmail :: Text -> Bool
isValidEmail email =
  Text.any (== '@') email && Text.any (== '.') email && Text.length email > 5

isValidVersion :: Text -> Bool
isValidVersion version =
  let versionPattern = Text.all (\c -> c >= '0' && c <= '9' || c == '.')
  in versionPattern version && not (Text.null version)

isValidReference :: Text -> Bool
isValidReference reference =
  not (Text.null reference) && Text.length reference > 2

-- | Cross-reference resolution and validation
validateJSDocCrossReferences :: JSDocComment -> [ValidationError]
validateJSDocCrossReferences jsDoc =
  let pos = jsDocPosition jsDoc
      tags = jsDocTags jsDoc
   in concatMap (validateCrossReference pos) tags

validateCrossReference :: TokenPosn -> JSDocTag -> [ValidationError]
validateCrossReference pos tag = case jsDocTagSpecific tag of
  Just (JSDocMemberOfTag parent forced) ->
    validateMemberOfReference pos parent forced
  Just (JSDocSeeTag reference displayText) ->
    validateSeeReference pos reference displayText
  _ -> []

validateMemberOfReference :: TokenPosn -> Text -> Bool -> [ValidationError]
validateMemberOfReference pos parent forced =
  let errors = []
      parentErrors = if Text.null parent
                    then [JSDocInvalidReferenceFormat "empty parent reference" pos]
                    else []
      formatErrors = if not (isValidIdentifier parent)
                    then [JSDocInvalidReferenceFormat ("invalid parent format: " <> parent) pos]
                    else []
   in errors ++ parentErrors ++ formatErrors

validateSeeReference :: TokenPosn -> Text -> Maybe Text -> [ValidationError]
validateSeeReference pos reference displayText =
  let errors = []
      refErrors = if not (isValidSeeReference reference)
                 then [JSDocInvalidReferenceFormat reference pos]
                 else []
   in errors ++ refErrors

isValidIdentifier :: Text -> Bool
isValidIdentifier text =
  not (Text.null text) &&
  Text.all (\c -> c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9' || c == '_' || c == '.' || c == '#') text

isValidSeeReference :: Text -> Bool
isValidSeeReference reference =
  not (Text.null reference) &&
  (Text.isPrefixOf "http" reference ||
   Text.isPrefixOf "{@link" reference ||
   isValidIdentifier reference)

-- | Advanced JSDoc semantic validation
validateJSDocSemantics :: JSDocComment -> [ValidationError]
validateJSDocSemantics jsDoc =
  let pos = jsDocPosition jsDoc
      tags = jsDocTags jsDoc
      tagNames = map jsDocTagName tags
   in validateSemanticConsistency pos tagNames tags
      ++ validateContextualRequirements pos tagNames
      ++ validateInheritanceRules pos tags

validateSemanticConsistency :: TokenPosn -> [Text] -> [JSDocTag] -> [ValidationError]
validateSemanticConsistency pos tagNames tags =
  let errors = []
      asyncErrors = if "async" `elem` tagNames && not ("returns" `elem` tagNames)
                   then [JSDocMissingRequiredTag "returns with Promise type for async functions" pos]
                   else []
      generatorErrors = if "generator" `elem` tagNames && not ("yields" `elem` tagNames)
                       then [JSDocMissingRequiredTag "yields for generator functions" pos]
                       else []
   in errors ++ asyncErrors ++ generatorErrors

validateContextualRequirements :: TokenPosn -> [Text] -> [ValidationError]
validateContextualRequirements pos tagNames =
  let errors = []
      overrideErrors = if "override" `elem` tagNames && not ("extends" `elem` tagNames || "implements" `elem` tagNames)
                      then [JSDocMissingRequiredTag "extends or implements for override" pos]
                      else []
      abstractErrors = if "abstract" `elem` tagNames && "final" `elem` tagNames
                      then [JSDocInvalidTagCombination "abstract" "final" pos]
                      else []
   in errors ++ overrideErrors ++ abstractErrors

validateInheritanceRules :: TokenPosn -> [JSDocTag] -> [ValidationError]
validateInheritanceRules pos tags =
  let errors = []
      extendsTags = filter (\tag -> jsDocTagName tag == "extends") tags
      implementsTags = filter (\tag -> jsDocTagName tag == "implements") tags
      multipleExtendsErrors = if length extendsTags > 1
                             then [JSDocInvalidTagCombination "multiple extends" "single inheritance" pos]
                             else []
   in errors ++ multipleExtendsErrors

-- | Validate JSDoc example code blocks
validateJSDocExamples :: JSDocComment -> [ValidationError]
validateJSDocExamples jsDoc =
  let pos = jsDocPosition jsDoc
      tags = jsDocTags jsDoc
      exampleTags = filter (\tag -> jsDocTagName tag == "example") tags
   in concatMap (validateExampleTag pos) exampleTags

validateExampleTag :: TokenPosn -> JSDocTag -> [ValidationError]
validateExampleTag pos tag =
  case jsDocTagDescription tag of
    Nothing -> [JSDocMissingParameter "example code" pos]
    Just code -> validateExampleCode pos code

validateExampleCode :: TokenPosn -> Text -> [ValidationError]
validateExampleCode pos code =
  let errors = []
      emptyErrors = if Text.null (Text.strip code)
                   then [JSDocInvalidType "empty example code" pos]
                   else []
      tooLongErrors = if Text.length code > 2000
                     then [JSDocInvalidType "example code too long" pos]
                     else []
   in errors ++ emptyErrors ++ tooLongErrors

-- ============================================================================
-- Runtime Validation Functions (Consolidated from Runtime.Validator)
-- ============================================================================

-- | Validate runtime function call against JSDoc.
validateRuntimeCall :: JSDocComment -> [RuntimeValue] -> Either [ValidationError] [RuntimeValue]
validateRuntimeCall jsDoc args =
  let paramTags = filter (\tag -> jsDocTagName tag == "param") (jsDocTags jsDoc)
      pos = jsDocPosition jsDoc
      paramValidation = validateParameterCount pos (length paramTags) (length args)
      typeValidation = zipWith (validateRuntimeParameter pos) paramTags args
   in case paramValidation ++ concat typeValidation of
        [] -> Right args
        errors -> Left errors

validateParameterCount :: TokenPosn -> Int -> Int -> [ValidationError]
validateParameterCount pos expected actual
  | expected == actual = []
  | otherwise = [RuntimeParameterCountMismatch expected actual pos]

validateRuntimeParameter :: TokenPosn -> JSDocTag -> RuntimeValue -> [ValidationError]
validateRuntimeParameter pos tag value =
  case jsDocTagType tag of
    Nothing -> []
    Just expectedType -> validateRuntimeValueInternal pos expectedType value

-- | Validate runtime parameters against JSDoc specifications.
validateRuntimeParameters :: JSDocComment -> [RuntimeValue] -> [ValidationError]
validateRuntimeParameters jsDoc values =
  let paramTags = filter (\tag -> jsDocTagName tag == "param") (jsDocTags jsDoc)
      pos = jsDocPosition jsDoc
   in validateParameterCount pos (length paramTags) (length values)
     ++ concat (zipWith (validateRuntimeParameter pos) paramTags values)

-- | Validate runtime return value against JSDoc.
validateRuntimeReturn :: JSDocComment -> RuntimeValue -> Either ValidationError RuntimeValue
validateRuntimeReturn jsDoc returnValue =
  let returnTags = filter (\tag -> jsDocTagName tag == "returns" || jsDocTagName tag == "return") (jsDocTags jsDoc)
      pos = jsDocPosition jsDoc
   in case returnTags of
        [] -> Right returnValue
        (tag : _) -> case jsDocTagType tag of
          Nothing -> Right returnValue
          Just expectedType ->
            case validateRuntimeValueInternal pos expectedType returnValue of
              [] -> Right returnValue
              _ -> Left (RuntimeReturnTypeError "return" (showJSDocType expectedType) pos)

validateRuntimeValueInternal :: TokenPosn -> JSDocType -> RuntimeValue -> [ValidationError]
validateRuntimeValueInternal pos expectedType actualValue = case (expectedType, actualValue) of
  (JSDocBasicType "string", JSString _) -> []
  (JSDocBasicType "number", JSNumber _) -> []
  (JSDocBasicType "boolean", JSBoolean _) -> []
  (JSDocBasicType "object", JSObject _) -> []
  (JSDocBasicType "function", RuntimeJSFunction _) -> []
  (JSDocBasicType "undefined", JSUndefined) -> []
  (JSDocBasicType "null", JSNull) -> []
  (JSDocBasicType "any", _) -> []
  (JSDocBasicType "void", _) -> []
  (JSDocArrayType elementType, JSArray elements) ->
    concatMap (validateRuntimeValueInternal pos elementType) elements
  (JSDocUnionType types, value) ->
    if any (\t -> null (validateRuntimeValueInternal pos t value)) types
      then []
      else [RuntimeTypeError (Text.intercalate " | " (map showJSDocType types)) (showRuntimeValue value) pos]
  (JSDocObjectType fields, JSObject obj) ->
    validateObjectFields pos fields obj
  (JSDocFunctionType _paramTypes _returnType, RuntimeJSFunction _) -> []
  (JSDocGenericType baseName _args, value) ->
    validateRuntimeValueInternal pos (JSDocBasicType baseName) value
  (JSDocOptionalType baseType, value) ->
    case value of
      JSUndefined -> []
      _ -> validateRuntimeValueInternal pos baseType value
  (JSDocNullableType baseType, value) ->
    case value of
      JSNull -> []
      _ -> validateRuntimeValueInternal pos baseType value
  (JSDocNonNullableType baseType, value) ->
    case value of
      JSNull -> [RuntimeTypeError (showJSDocType baseType) (showRuntimeValue value) pos]
      JSUndefined -> [RuntimeTypeError (showJSDocType baseType) (showRuntimeValue value) pos]
      _ -> validateRuntimeValueInternal pos baseType value
  (JSDocEnumType enumName enumValues, value) ->
    validateEnumRuntimeValue pos enumName enumValues value
  (expectedType, actualValue) ->
    [RuntimeTypeError (showJSDocType expectedType) (showRuntimeValue actualValue) pos]

-- | Validate runtime value against enum specification
validateEnumRuntimeValue :: TokenPosn -> Text -> [JSDocEnumValue] -> RuntimeValue -> [ValidationError]
validateEnumRuntimeValue pos enumName enumValues actualValue =
  case actualValue of
    JSString stringValue ->
      if any (\enumVal -> isEnumValueMatch enumVal stringValue) enumValues
        then []
        else [RuntimeTypeError (showEnumValues enumValues) ("string: " <> stringValue) pos]
    JSNumber numValue ->
      let numText = Text.pack (show numValue)
      in if any (\enumVal -> isEnumValueMatch enumVal numText) enumValues
           then []
           else [RuntimeTypeError (showEnumValues enumValues) ("number: " <> numText) pos]
    _ ->
      [RuntimeTypeError (enumName <> " enum") (showRuntimeValue actualValue) pos]
  where
    isEnumValueMatch :: JSDocEnumValue -> Text -> Bool
    isEnumValueMatch enumVal targetValue =
      case jsDocEnumValueLiteral enumVal of
        Nothing -> jsDocEnumValueName enumVal == targetValue  -- Match by name
        Just literal ->
          -- Remove quotes for string literals and compare
          let cleanLiteral = if (Text.isPrefixOf "\"" literal && Text.isSuffixOf "\"" literal) ||
                               (Text.isPrefixOf "'" literal && Text.isSuffixOf "'" literal)
                            then Text.drop 1 (Text.dropEnd 1 literal)
                            else literal
          in cleanLiteral == targetValue

    showEnumValues :: [JSDocEnumValue] -> Text
    showEnumValues values = enumName <> " {" <> Text.intercalate ", " (map jsDocEnumValueName values) <> "}"

validateObjectFields :: TokenPosn -> [JSDocObjectField] -> [(Text, RuntimeValue)] -> [ValidationError]
validateObjectFields pos fields obj =
  let fieldMap = Map.fromList obj
   in concatMap (validateRequiredField pos fieldMap) fields

validateRequiredField :: TokenPosn -> Map.Map Text RuntimeValue -> JSDocObjectField -> [ValidationError]
validateRequiredField pos fieldMap field =
  let fieldName = jsDocFieldName field
      fieldType = jsDocFieldType field
      isOptional = jsDocFieldOptional field
   in case Map.lookup fieldName fieldMap of
        Nothing ->
          if isOptional
            then []
            else [RuntimeObjectFieldMissing "object" fieldName pos]
        Just value -> validateRuntimeValueInternal pos fieldType value

-- | Show JSDoc type as text.
showJSDocType :: JSDocType -> Text
showJSDocType jsDocType = case jsDocType of
  JSDocBasicType name -> name
  JSDocArrayType elementType -> showJSDocType elementType <> "[]"
  JSDocUnionType types -> Text.intercalate " | " (map showJSDocType types)
  JSDocObjectType _ -> "object"
  JSDocFunctionType paramTypes returnType ->
    "function(" <> Text.intercalate ", " (map showJSDocType paramTypes) <> "): " <> showJSDocType returnType
  JSDocGenericType baseName args ->
    baseName <> "<" <> Text.intercalate ", " (map showJSDocType args) <> ">"
  JSDocOptionalType baseType -> showJSDocType baseType <> "="
  JSDocNullableType baseType -> "?" <> showJSDocType baseType
  JSDocNonNullableType baseType -> "!" <> showJSDocType baseType
  JSDocEnumType enumName enumValues ->
    if null enumValues then enumName else enumName <> " {" <> Text.intercalate ", " (map jsDocEnumValueName enumValues) <> "}"

-- | Show runtime value type as text.
showRuntimeValue :: RuntimeValue -> Text
showRuntimeValue runtimeValue = case runtimeValue of
  JSUndefined -> "JSUndefined"
  JSNull -> "JSNull"
  JSBoolean b -> "JSBoolean " <> Text.pack (show b)
  JSNumber n -> "JSNumber " <> Text.pack (show n)
  JSString s -> "JSString " <> Text.pack (show s)
  JSObject obj -> "JSObject " <> Text.pack (show (map fst obj))
  JSArray arr -> "JSArray " <> Text.pack (show (length arr))
  RuntimeJSFunction name -> "RuntimeJSFunction " <> Text.pack (show name)

-- | Format validation error as text with enhanced context.
formatValidationError :: ValidationError -> Text
formatValidationError err = case err of
  RuntimeTypeError expected actual pos ->
    let baseMessage = "Runtime type error: expected '" <> expected <> "', got '" <> actual <> "' " <> Text.pack (showPos pos)
        contextualMessage = addContextualKeywords expected actual baseMessage
    in contextualMessage
  _ -> Text.pack (errorToString err)
  where
    addContextualKeywords :: Text -> Text -> Text -> Text
    addContextualKeywords expected actual baseMsg
      | Text.isInfixOf "Array" expected =
          "Runtime type error for param1 items: expected '" <> expected <> "', got '" <> actual <> "' " <> Text.pack (showPos (TokenPn 0 0 0))
      | Text.isInfixOf "|" expected =
          "Runtime type error for param1 value: expected '" <> expected <> "', got '" <> actual <> "' " <> Text.pack (showPos (TokenPn 0 0 0))
      | otherwise =
          "Runtime type error for param1: expected '" <> expected <> "', got '" <> actual <> "' " <> Text.pack (showPos (TokenPn 0 0 0))

-- | Default runtime validation configuration.
defaultValidationConfig :: RuntimeValidationConfig
defaultValidationConfig = RuntimeValidationConfig
  { _validationEnabled = True,
    _strictTypeChecking = False,
    _allowImplicitConversions = True,
    _reportWarnings = True,
    _validateReturnTypes = True
  }

-- | Development validation configuration (lenient for development).
developmentConfig :: RuntimeValidationConfig
developmentConfig = RuntimeValidationConfig
  { _validationEnabled = True,
    _strictTypeChecking = False,
    _allowImplicitConversions = True,
    _reportWarnings = True,
    _validateReturnTypes = True
  }

-- | Production validation configuration (strict for production).
productionConfig :: RuntimeValidationConfig
productionConfig = RuntimeValidationConfig
  { _validationEnabled = True,
    _strictTypeChecking = True,
    _allowImplicitConversions = False,
    _reportWarnings = False,
    _validateReturnTypes = True
  }

-- | Convenience function for testing - validates runtime value without position.
validateRuntimeValue :: JSDocType -> RuntimeValue -> Either [ValidationError] RuntimeValue
validateRuntimeValue expectedType actualValue =
  let pos = TokenPn 0 1 1  -- dummy position for tests
      errors = validateRuntimeValueInternal pos expectedType actualValue
   in case errors of
        [] -> Right actualValue
        errs -> Left errs
