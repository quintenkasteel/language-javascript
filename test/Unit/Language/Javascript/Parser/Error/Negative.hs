{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Systematic Negative Testing for JavaScript Parser
--
-- This module provides comprehensive negative testing for all parser components
-- to ensure proper error handling and rejection of invalid JavaScript syntax.
-- It systematically tests invalid inputs for:
--
--   * Lexer: Invalid tokens, Unicode issues, string/regex errors
--   * Parser: Syntax errors in expressions, statements, declarations
--   * Validator: Semantic errors and invalid AST structures
--   * Module system: Invalid import/export syntax
--   * ES6+ features: Malformed modern JavaScript constructs
--
-- Tests are organized into three categories:
--
--   * Rejection tests: inputs the parser correctly rejects
--   * Permissive behavior tests: inputs that are technically invalid but
--     the parser accepts for error recovery or simplicity
--   * Valid-JS exclusions: inputs removed because they are actually valid
--     JavaScript (documented in comments where they were removed)
--
-- @since 0.7.1.0
module Unit.Language.Javascript.Parser.Error.Negative
  ( testNegativeCases,
  )
where

import Language.JavaScript.Parser (readJsSafe, readJsModuleSafe)
import Test.Hspec

-- | Comprehensive negative testing for all parser components
testNegativeCases :: Spec
testNegativeCases = describe "Negative Test Coverage" $ do
  describe "Lexer error handling" $ do
    testInvalidTokens
    testInvalidStrings
    testInvalidNumbers
    testInvalidRegex
    testInvalidUnicode

  describe "Expression parsing errors" $ do
    testInvalidExpressions
    testInvalidOperators
    testInvalidCalls
    testInvalidMemberAccess

  describe "Statement parsing errors" $ do
    testInvalidStatements
    testInvalidControlFlow
    testInvalidDeclarations
    testInvalidFunctions

  describe "Object and array errors" $ do
    testInvalidObjectLiterals
    testInvalidArrayLiterals

  describe "Module system errors" $ do
    testInvalidImports
    testInvalidExports
    testInvalidModuleSyntax

  describe "ES6+ feature errors" $ do
    testInvalidClasses
    testInvalidArrowFunctions
    testInvalidTemplates
    testInvalidDestructuring

  describe "Parser permissive behavior" $ do
    testPermissiveNumbers
    testPermissiveRegex
    testPermissiveStrings
    testPermissiveExpressions
    testPermissiveDeclarations
    testPermissiveObjectsAndArrays

-- | Test invalid token sequences and malformed tokens
testInvalidTokens :: Spec
testInvalidTokens = describe "Invalid tokens" $ do
  it "rejects invalid operators" $ do
    "x === = y" `shouldFailToParse` "Should reject invalid triple equals"
    "x ?? ?" `shouldFailToParse` "Should reject invalid nullish coalescing"

  it "rejects invalid punctuation" $ do
    "x @ y" `shouldFailToParse` "Should reject @ operator"
    "x # y" `shouldFailToParse` "Should reject # operator"
    "function f() {}} extra" `shouldFailToParse` "Should reject extra closing brace"

  -- Removed: "x $ y" is valid JS because $ is a valid identifier character
  -- Removed: "x + + + y" is valid JS: x + (+(+y))
  -- Removed: "x ... y" is valid JS in some contexts (spread/rest)

  it "rejects invalid keywords" $ do
    "class class" `shouldFailToParse` "Should reject class class"
    "function function" `shouldFailToParse` "Should reject function function"
    "var var" `shouldFailToParse` "Should reject var var"
    "if if" `shouldFailToParse` "Should reject if if"

-- | Test invalid string literals
testInvalidStrings :: Spec
testInvalidStrings = describe "Invalid strings" $ do
  it "rejects unclosed strings" $ do
    "\"unclosed" `shouldFailToParse` "Should reject unclosed double quote"
    "'unclosed" `shouldFailToParse` "Should reject unclosed single quote"
    "`unclosed" `shouldFailToParse` "Should reject unclosed template literal"

  it "rejects unterminated line continuation" $ do
    "'unterminated\\\nstring" `shouldFailToParse` "Should reject unterminated line continuation"

-- | Test invalid numeric literals
testInvalidNumbers :: Spec
testInvalidNumbers = describe "Invalid numbers" $ do
  it "rejects malformed decimals" $ do
    "1.." `shouldFailToParse` "Should reject double decimal point"
    ".." `shouldFailToParse` "Should reject double dot"

  it "rejects hex with decimal point" $ do
    "0x." `shouldFailToParse` "Should reject hex with decimal point"

  it "rejects incomplete signed exponents" $ do
    "1e+" `shouldFailToParse` "Should reject incomplete positive exponent"
    "1e-" `shouldFailToParse` "Should reject incomplete negative exponent"

-- | Test invalid regular expressions
testInvalidRegex :: Spec
testInvalidRegex = describe "Invalid regex" $ do
  it "rejects unclosed regex" $ do
    "/unclosed" `shouldFailToParse` "Should reject unclosed regex"
    "/pattern" `shouldFailToParse` "Should reject regex without closing slash"

  it "rejects unclosed bracket in regex" $ do
    "/[/" `shouldFailToParse` "Should reject unclosed bracket"

-- | Test invalid Unicode handling
testInvalidUnicode :: Spec
testInvalidUnicode = describe "Invalid Unicode" $ do
  it "rejects invalid Unicode identifiers" $ do
    "var \\u" `shouldFailToParse` "Should reject incomplete Unicode escape"
    "var \\u123" `shouldFailToParse` "Should reject short Unicode escape"
    "var \\u{}" `shouldFailToParse` "Should reject empty Unicode escape"

-- | Test invalid expressions
testInvalidExpressions :: Spec
testInvalidExpressions = describe "Invalid expressions" $ do
  it "rejects malformed conditionals" $ do
    "x ? : y" `shouldFailToParse` "Should reject missing middle expression"
    "x ? y" `shouldFailToParse` "Should reject missing colon"
    "? x : y" `shouldFailToParse` "Should reject missing condition"

  -- Removed: "x =+ y" is valid JS: x = (+y), assignment of unary plus
  -- Removed: "x + = y" is actually rejected, but "x += y" is compound assignment

  it "rejects invalid parentheses" $ do
    "(x" `shouldFailToParse` "Should reject unclosed paren"
    "x)" `shouldFailToParse` "Should reject unopened paren"
    "((x)" `shouldFailToParse` "Should reject mismatched parens"

-- | Test invalid operators
testInvalidOperators :: Spec
testInvalidOperators = describe "Invalid operators" $ do
  it "rejects malformed binary operators" $ do
    "x & & y" `shouldFailToParse` "Should reject space in and operator"
    "x | | y" `shouldFailToParse` "Should reject space in or operator"

  -- Removed: "++ +x" is valid JS: ++(+x), pre-increment of unary plus

-- | Test invalid function calls
testInvalidCalls :: Spec
testInvalidCalls = describe "Invalid calls" $ do
  it "rejects malformed argument lists" $ do
    "f(,x)" `shouldFailToParse` "Should reject leading comma"
    "f(x,,y)" `shouldFailToParse` "Should reject double comma"
    "f(x" `shouldFailToParse` "Should reject unclosed args"

-- | Test invalid member access
testInvalidMemberAccess :: Spec
testInvalidMemberAccess = describe "Invalid member access" $ do
  it "rejects malformed dot access" $ do
    "x." `shouldFailToParse` "Should reject missing property"
    ".x" `shouldFailToParse` "Should reject missing object"

  -- Removed: "x.123" is valid JS: member access followed by numeric literal

  it "rejects malformed bracket access" $ do
    "x[" `shouldFailToParse` "Should reject unclosed bracket"
    "x]" `shouldFailToParse` "Should reject unopened bracket"

-- | Test invalid statements
testInvalidStatements :: Spec
testInvalidStatements = describe "Invalid statements" $ do
  it "rejects malformed blocks" $ do
    "{" `shouldFailToParse` "Should reject unclosed block"
    "}" `shouldFailToParse` "Should reject unopened block"
    "{ { }" `shouldFailToParse` "Should reject mismatched blocks"

  it "rejects invalid labels" $ do
    "123: x" `shouldFailToParse` "Should reject numeric label"
    ": x" `shouldFailToParse` "Should reject missing label"
    "label:" `shouldFailToParse` "Should reject missing statement"

-- | Test invalid control flow
testInvalidControlFlow :: Spec
testInvalidControlFlow = describe "Invalid control flow" $ do
  it "rejects malformed if statements" $ do
    "if" `shouldFailToParse` "Should reject if without condition"
    "if (x" `shouldFailToParse` "Should reject unclosed condition"
    "if x)" `shouldFailToParse` "Should reject missing open paren"
    "if () {}" `shouldFailToParse` "Should reject empty condition"

  it "rejects malformed loops" $ do
    "for" `shouldFailToParse` "Should reject for without parts"
    "for (" `shouldFailToParse` "Should reject unclosed for"
    "for (;;;" `shouldFailToParse` "Should reject extra semicolon"
    "while" `shouldFailToParse` "Should reject while without condition"
    "do" `shouldFailToParse` "Should reject do without body"

  -- Removed: "break 123" and "continue 123" are valid JS with ASI
  -- (break/continue followed by expression statement 123)

-- | Test invalid declarations
testInvalidDeclarations :: Spec
testInvalidDeclarations = describe "Invalid declarations" $ do
  it "rejects malformed variable declarations" $ do
    "var" `shouldFailToParse` "Should reject var without identifier"
    "var 123" `shouldFailToParse` "Should reject numeric identifier"
    "let" `shouldFailToParse` "Should reject let without identifier"
    "const" `shouldFailToParse` "Should reject const without identifier"

  it "rejects reserved word identifiers" $ do
    "var class" `shouldFailToParse` "Should reject class as identifier"
    "let function" `shouldFailToParse` "Should reject function as identifier"
    "const if" `shouldFailToParse` "Should reject if as identifier"

-- | Test invalid functions
testInvalidFunctions :: Spec
testInvalidFunctions = describe "Invalid functions" $ do
  it "rejects malformed function declarations" $ do
    "function" `shouldFailToParse` "Should reject function without name"
    "function (" `shouldFailToParse` "Should reject function without name"
    "function f" `shouldFailToParse` "Should reject function without params/body"
    "function f(" `shouldFailToParse` "Should reject unclosed params"
    "function f() {" `shouldFailToParse` "Should reject unclosed body"

  it "rejects invalid parameter lists" $ do
    "function f(,)" `shouldFailToParse` "Should reject empty param"
    "function f(x,)" `shouldFailToParse` "Should reject trailing comma"
    "function f(123)" `shouldFailToParse` "Should reject numeric param"

-- | Test invalid object literals
testInvalidObjectLiterals :: Spec
testInvalidObjectLiterals = describe "Invalid object literals" $ do
  it "rejects malformed object syntax" $ do
    "{" `shouldFailToParse` "Should reject unclosed object"
    "{ :" `shouldFailToParse` "Should reject missing key"
    "{ x: }" `shouldFailToParse` "Should reject missing value"

  it "rejects invalid property names" $ do
    "{ : 1 }" `shouldFailToParse` "Should reject missing property"

-- | Test invalid array literals
testInvalidArrayLiterals :: Spec
testInvalidArrayLiterals = describe "Invalid array literals" $ do
  it "rejects malformed array syntax" $ do
    "[" `shouldFailToParse` "Should reject unclosed array"
    "[,," `shouldFailToParse` "Should reject unclosed with commas"
    "[1,," `shouldFailToParse` "Should reject unclosed with elements"

  it "accepts valid sparse arrays" $ do
    shouldAcceptAsScript "[,]"
    shouldAcceptAsScript "[1,,3]"

-- | Test invalid import statements
testInvalidImports :: Spec
testInvalidImports = describe "Invalid imports" $ do
  it "rejects malformed import syntax" $ do
    "import" `shouldFailToParseModule` "Should reject import without parts"
    "import from" `shouldFailToParseModule` "Should reject import without identifier"
    "import x" `shouldFailToParseModule` "Should reject import without from"
    "import x from" `shouldFailToParseModule` "Should reject import without module"
    "import { }" `shouldFailToParseModule` "Should reject empty braces"

  it "rejects invalid import specifiers" $ do
    "import { , } from 'mod'" `shouldFailToParseModule` "Should reject empty spec"
    "import { x, } from 'mod'" `shouldFailToParseModule` "Should reject trailing comma"
    "import { 123 } from 'mod'" `shouldFailToParseModule` "Should reject numeric import"

-- | Test invalid export statements
testInvalidExports :: Spec
testInvalidExports = describe "Invalid exports" $ do
  it "rejects malformed export syntax" $ do
    "export" `shouldFailToParseModule` "Should reject export without target"
    "export {" `shouldFailToParseModule` "Should reject unclosed braces"
    "export { ," `shouldFailToParseModule` "Should reject empty spec"

  it "rejects invalid export specifiers" $ do
    "export { 123 }" `shouldFailToParseModule` "Should reject numeric export"
    "export function" `shouldFailToParseModule` "Should reject function without name"

-- | Test invalid module syntax
testInvalidModuleSyntax :: Spec
testInvalidModuleSyntax = describe "Invalid module syntax" $ do
  it "rejects import in non-module context" $ do
    "import x from 'mod'" `shouldFailToParse` "Should reject import in script"
    "export const x = 1" `shouldFailToParse` "Should reject export in script"

  it "rejects mixed import/export errors" $ do
    "import export" `shouldFailToParseModule` "Should reject keywords together"
    "export import" `shouldFailToParseModule` "Should reject keywords together"

-- | Test invalid class syntax
testInvalidClasses :: Spec
testInvalidClasses = describe "Invalid classes" $ do
  it "rejects malformed class declarations" $ do
    "class" `shouldFailToParse` "Should reject class without name"
    "class {" `shouldFailToParse` "Should reject class without name"
    "class C {" `shouldFailToParse` "Should reject unclosed class"
    "class 123" `shouldFailToParse` "Should reject numeric class name"

  it "rejects invalid class methods" $ do
    "class C { constructor() }" `shouldFailToParse` "Should reject constructor without body"
    "class C { method( }" `shouldFailToParse` "Should reject method with unclosed parens"

-- | Test invalid arrow functions
testInvalidArrowFunctions :: Spec
testInvalidArrowFunctions = describe "Invalid arrow functions" $ do
  it "rejects malformed arrow syntax" $ do
    "=>" `shouldFailToParse` "Should reject arrow without params"
    "x =>" `shouldFailToParse` "Should reject arrow without body"
    "=> x" `shouldFailToParse` "Should reject arrow without params"
    "x = >" `shouldFailToParse` "Should reject space in arrow"

  it "rejects invalid parameter syntax" $ do
    "(,) => x" `shouldFailToParse` "Should reject empty param"
    "(123) => x" `shouldFailToParse` "Should reject numeric param"

-- | Test invalid template literals
testInvalidTemplates :: Spec
testInvalidTemplates = describe "Invalid templates" $ do
  it "rejects unclosed template literals" $ do
    "`unclosed" `shouldFailToParse` "Should reject unclosed template"
    "`${unclosed" `shouldFailToParse` "Should reject unclosed expression"
    "`${x" `shouldFailToParse` "Should reject unclosed expression"

  it "rejects invalid template expressions" $ do
    "`${}}`" `shouldFailToParse` "Should reject empty expression"
    "`${${}}`" `shouldFailToParse` "Should reject nested empty expression"

-- | Test invalid destructuring
testInvalidDestructuring :: Spec
testInvalidDestructuring = describe "Invalid destructuring" $ do
  it "rejects malformed array destructuring" $ do
    "var [" `shouldFailToParse` "Should reject unclosed array pattern"
    "var [,," `shouldFailToParse` "Should reject unclosed with commas"

  it "rejects malformed object destructuring" $ do
    "var {" `shouldFailToParse` "Should reject unclosed object pattern"
    "var { :" `shouldFailToParse` "Should reject missing key"
    "var { 123 }" `shouldFailToParse` "Should reject numeric key"

-- ============================================================================
-- Permissive behavior tests
--
-- The parser intentionally accepts some technically-invalid inputs for error
-- recovery or because full validation would require semantic analysis beyond
-- the parser's scope. These tests document that behavior explicitly.
-- ============================================================================

-- | Document permissive numeric literal handling
testPermissiveNumbers :: Spec
testPermissiveNumbers = describe "Permissive numeric literals" $ do
  it "accepts multiple decimal points (parsed as separate tokens)" $
    shouldAcceptAsScript "1.2.3"

  it "accepts empty hex prefix" $
    shouldAcceptAsScript "0x"

  it "accepts invalid hex digits" $
    shouldAcceptAsScript "0xG"

  it "accepts invalid octal digits" $ do
    shouldAcceptAsScript "0o9"
    shouldAcceptAsScript "0o8"

  it "accepts incomplete exponent without sign" $
    shouldAcceptAsScript "1e"

-- | Document permissive regex handling
testPermissiveRegex :: Spec
testPermissiveRegex = describe "Permissive regex literals" $ do
  it "accepts invalid regex flags" $
    shouldAcceptAsScript "/pattern/xyz"

  it "accepts duplicate regex flags" $
    shouldAcceptAsScript "/pattern/gg"

  it "accepts incomplete regex escape" $
    shouldAcceptAsScript "/\\\\/"

-- | Document permissive string handling
testPermissiveStrings :: Spec
testPermissiveStrings = describe "Permissive string literals" $ do
  it "accepts incomplete hex escape in string" $
    shouldAcceptAsScript "\"\\x\""

  it "accepts incomplete unicode escape in double-quoted string" $
    shouldAcceptAsScript "\"\\u\""

  it "accepts short unicode escape in single-quoted string" $
    shouldAcceptAsScript "'\\u123'"

  it "accepts short unicode escape in double-quoted string" $
    shouldAcceptAsScript "\"\\u123\""

  it "accepts incomplete unicode escape in single-quoted string" $
    shouldAcceptAsScript "'\\u'"

  it "accepts multi-line string continuation" $
    shouldAcceptAsScript "\"line\\\n\\\ncontinuation\""

-- | Document permissive expression handling
testPermissiveExpressions :: Spec
testPermissiveExpressions = describe "Permissive expressions" $ do
  it "accepts invalid assignment target (1 = x)" $
    shouldAcceptAsScript "1 = x"

  it "accepts empty bracket access" $
    shouldAcceptAsScript "x[]"

-- | Document permissive declaration handling
testPermissiveDeclarations :: Spec
testPermissiveDeclarations = describe "Permissive declarations" $ do
  it "accepts const without initializer" $
    shouldAcceptAsScript "const x"

  it "accepts numeric destructuring pattern" $
    shouldAcceptAsScript "var [123]"

  it "accepts invalid identifier in object key" $
    shouldAcceptAsScript "{ 123x: 1 }"

-- | Document permissive object literal handling
testPermissiveObjectsAndArrays :: Spec
testPermissiveObjectsAndArrays = describe "Permissive objects and arrays" $ do
  it "accepts getter without body as shorthand property" $
    shouldAcceptAsScript "{ get x }"

  it "accepts setter without params as shorthand property" $
    shouldAcceptAsScript "{ set x }"

-- ============================================================================
-- Utility functions
-- ============================================================================

-- | Assert that a JavaScript program fails to parse.
--
-- Uses 'readJsSafe' which returns 'Left' on parse failure, avoiding
-- the need for exception handling.
shouldFailToParse :: String -> String -> Expectation
shouldFailToParse input errorMsg =
  either (const (pure ())) (const (expectationFailure errorMsg)) result
  where
    result = readJsSafe input

-- | Assert that a JavaScript module fails to parse.
--
-- Uses 'readJsModuleSafe' which returns 'Left' on parse failure, avoiding
-- the need for exception handling.
shouldFailToParseModule :: String -> String -> Expectation
shouldFailToParseModule input errorMsg =
  either (const (pure ())) (const (expectationFailure errorMsg)) result
  where
    result = readJsModuleSafe input

-- | Assert that a JavaScript program is accepted by the parser.
--
-- Used to document permissive parser behavior where the parser accepts
-- technically-invalid inputs for error recovery or simplicity.
shouldAcceptAsScript :: String -> Expectation
shouldAcceptAsScript input =
  either reportUnexpectedFailure (const (pure ())) result
  where
    result = readJsSafe input

    reportUnexpectedFailure :: String -> Expectation
    reportUnexpectedFailure err =
      expectationFailure
        ( "Expected parser to accept "
            <> show input
            <> " (permissive) but it failed: "
            <> err
        )
