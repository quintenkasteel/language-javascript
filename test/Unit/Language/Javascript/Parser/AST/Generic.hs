{-# LANGUAGE BangPatterns #-}

module Unit.Language.Javascript.Parser.AST.Generic
  ( testGenericNFData,
  )
where

import Control.DeepSeq (rnf)
import qualified Data.ByteString.Char8 as BS8
import GHC.Generics (from, to)
import qualified Language.JavaScript.Parser.AST as AST
import Language.JavaScript.Parser.Grammar7
import Language.JavaScript.Parser.Parser
import Test.Hspec

testGenericNFData :: Spec
testGenericNFData = describe "Generic and NFData instances" $ do
  describe "NFData instances" $ do
    it "can deep evaluate simple expressions" $ do
      case parseUsing parseExpression "42" "test" of
        Right ast -> do
          -- Test that NFData deep evaluation completes without exception
          let !evaluated = rnf ast `seq` ast
          -- Verify the AST structure is preserved after deep evaluation
          case evaluated of
            AST.JSAstExpression (AST.JSDecimal _ val) _ | val == "42" -> pure ()
            _ -> expectationFailure "NFData evaluation altered AST structure"
        Left _ -> expectationFailure "Parse failed"

    it "can deep evaluate complex expressions" $ do
      case parseUsing parseExpression "foo.bar[baz](arg1, arg2)" "test" of
        Right ast -> do
          -- Test that NFData handles complex nested structures
          let !evaluated = rnf ast `seq` ast
          -- Verify complex expression maintains structure (any valid expression)
          case evaluated of
            AST.JSAstExpression _ _ -> pure ()
            _ -> expectationFailure "NFData failed to preserve expression structure"
        Left _ -> expectationFailure "Parse failed"

    it "can deep evaluate object literals" $ do
      case parseUsing parseExpression "{a: 1, b: 2, ...obj}" "test" of
        Right ast -> do
          -- Test NFData with object literal containing spread syntax
          let !evaluated = rnf ast `seq` ast
          -- Verify object literal structure is preserved
          case evaluated of
            AST.JSAstExpression (AST.JSObjectLiteral {}) _ -> pure ()
            _ -> expectationFailure "NFData failed to preserve object literal structure"
        Left _ -> expectationFailure "Parse failed"

    it "can deep evaluate arrow functions" $ do
      case parseUsing parseExpression "(x, y) => x + y" "test" of
        Right ast -> do
          -- Test NFData with arrow function expressions
          let !evaluated = rnf ast `seq` ast
          -- Verify arrow function structure is maintained
          case evaluated of
            AST.JSAstExpression (AST.JSArrowExpression {}) _ -> pure ()
            _ -> expectationFailure "NFData failed to preserve arrow function structure"
        Left _ -> expectationFailure "Parse failed"

    it "can deep evaluate statements" $ do
      case parseUsing parseStatement "function foo(x) { return x * 2; }" "test" of
        Right ast -> do
          -- Test NFData with function declaration statements
          let !evaluated = rnf ast `seq` ast
          -- Verify function statement structure is preserved
          case evaluated of
            AST.JSAstStatement (AST.JSFunction {}) _ -> pure ()
            _ -> expectationFailure "NFData failed to preserve function statement structure"
        Left _ -> expectationFailure "Parse failed"

    it "can deep evaluate complete programs" $ do
      case parseUsing parseProgram "var x = 42; function add(a, b) { return a + b; }" "test" of
        Right ast -> do
          -- Test NFData with complete program ASTs
          let !evaluated = rnf ast `seq` ast
          -- Verify program structure contains expected elements
          case evaluated of
            AST.JSAstProgram stmts _ -> do
              length stmts `shouldSatisfy` (>= 2)
            _ -> expectationFailure "NFData failed to preserve program structure"
        Left _ -> expectationFailure "Parse failed"

    it "can deep evaluate AST components" $ do
      let annotation = AST.JSNoAnnot
      let identifier = AST.JSIdentifier annotation "test"
      let literal = AST.JSDecimal annotation "42"
      -- Test NFData on individual AST components
      let !evalAnnot = rnf annotation `seq` annotation
      let !evalIdent = rnf identifier `seq` identifier
      let !evalLiteral = rnf literal `seq` literal
      -- Verify components maintain their values after evaluation
      case (evalAnnot, evalIdent, evalLiteral) of
        (AST.JSNoAnnot, AST.JSIdentifier _ testVal, AST.JSDecimal _ val42) | testVal == "test" && val42 == "42" -> pure ()
        _ -> expectationFailure "NFData evaluation altered AST component values"

  describe "Generic instances" $ do
    it "supports generic operations on expressions" $ do
      let expr = AST.JSIdentifier AST.JSNoAnnot "test"
      let generic = from expr
      let reconstructed = to generic
      reconstructed `shouldBe` expr

    it "supports generic operations on statements" $ do
      let stmt = AST.JSExpressionStatement (AST.JSIdentifier AST.JSNoAnnot "x") AST.JSSemiAuto
      let generic = from stmt
      let reconstructed = to generic
      reconstructed `shouldBe` stmt

    it "supports generic operations on annotations" $ do
      let annot = AST.JSNoAnnot
      let generic = from annot
      let reconstructed = to generic
      reconstructed `shouldBe` annot

    it "generic instances compile correctly" $ do
      -- Test that Generic instances are well-formed and functional
      let expr = AST.JSDecimal AST.JSNoAnnot "123"
      let generic = from expr
      let reconstructed = to generic
      -- Verify Generic round-trip preserves exact structure
      case (expr, reconstructed) of
        (AST.JSDecimal _ val1, AST.JSDecimal _ val2) | val1 == "123" && val2 == "123" -> pure ()
        _ -> expectationFailure "Generic round-trip failed to preserve structure"
      -- Verify Generic representation is meaningful (non-empty and contains structure)
      let genericStr = show generic
      case genericStr of
        s | length s > 5 -> pure ()
        _ -> expectationFailure ("Generic representation too simple: " ++ genericStr)

  describe "NFData performance benefits" $ do
    it "enables complete evaluation for benchmarking" $ do
      case parseUsing parseProgram complexJavaScript "test" of
        Right ast -> do
          -- Test that NFData enables complete evaluation for performance testing
          let !evaluated = rnf ast `seq` ast
          -- Verify the complex AST maintains its essential structure
          case evaluated of
            AST.JSAstProgram stmts _ -> do
              -- Should contain class, const, and function declarations
              length stmts `shouldSatisfy` (> 5)
            _ -> expectationFailure "NFData failed to preserve complex program structure"
        Left _ -> expectationFailure "Parse failed"

    it "prevents space leaks in large ASTs" $ do
      case parseUsing parseProgram largeJavaScript "test" of
        Right ast -> do
          -- Test that NFData prevents space leaks in large, nested ASTs
          let !evaluated = rnf ast `seq` ast
          -- Verify large nested object structure is preserved
          case evaluated of
            AST.JSAstProgram [AST.JSVariable {}] _ -> pure ()
            AST.JSAstProgram [AST.JSLet {}] _ -> pure ()
            AST.JSAstProgram [AST.JSConstant {}] _ -> pure ()
            _ -> expectationFailure "NFData failed to preserve large AST structure"
        Left _ -> expectationFailure "Parse failed"

-- Test data for complex JavaScript
complexJavaScript :: String
complexJavaScript =
  unlines
    [ "class Calculator {",
      "  constructor(name) {",
      "    this.name = name;",
      "  }",
      "",
      "  add(a, b) {",
      "    return a + b;",
      "  }",
      "",
      "  multiply(a, b) {",
      "    return a * b;",
      "  }",
      "}",
      "",
      "const calc = new Calculator('MyCalc');",
      "const result = calc.add(calc.multiply(2, 3), 4);",
      "",
      "function processArray(arr) {",
      "  return arr",
      "    .filter(x => x > 0)",
      "    .map(x => x * 2)",
      "    .reduce((a, b) => a + b, 0);",
      "}",
      "",
      "const numbers = [1, -2, 3, -4, 5];",
      "const processed = processArray(numbers);"
    ]

-- Test data for large JavaScript (nested structures)
largeJavaScript :: String
largeJavaScript =
  unlines
    [ "const config = {",
      "  database: {",
      "    host: 'localhost',",
      "    port: 5432,",
      "    credentials: {",
      "      username: 'admin',",
      "      password: 'secret'",
      "    },",
      "    options: {",
      "      ssl: true,",
      "      timeout: 30000,",
      "      retries: 3",
      "    }",
      "  },",
      "  api: {",
      "    endpoints: {",
      "      users: '/api/users',",
      "      posts: '/api/posts',",
      "      comments: '/api/comments'",
      "    },",
      "    middleware: [",
      "      'cors',",
      "      'auth',",
      "      'validation'",
      "    ]",
      "  },",
      "  features: {",
      "    experimental: {",
      "      newParser: true,",
      "      betaUI: false",
      "    }",
      "  }",
      "};"
    ]
