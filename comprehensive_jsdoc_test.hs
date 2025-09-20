{-# LANGUAGE OverloadedStrings #-}

import Language.JavaScript.Parser.Token
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser
import Language.JavaScript.Parser.SrcLocation
import Data.Text (Text)
import qualified Data.Text as Text

-- Comprehensive JSDoc parsing tests
main :: IO ()
main = do
  putStrLn "=== Comprehensive JSDoc Research & Testing ==="
  putStrLn ""

  -- Test 1: Basic JSDoc detection
  putStrLn "=== Test 1: JSDoc Comment Detection ==="
  testJSDocDetection
  putStrLn ""

  -- Test 2: JSDoc tag parsing
  putStrLn "=== Test 2: JSDoc Tag Parsing ==="
  testJSDocTagParsing
  putStrLn ""

  -- Test 3: Complex JSDoc patterns
  putStrLn "=== Test 3: Complex JSDoc Patterns ==="
  testComplexJSDocPatterns
  putStrLn ""

  -- Test 4: Edge cases
  putStrLn "=== Test 4: Edge Cases ==="
  testJSDocEdgeCases
  putStrLn ""

  -- Test 5: AST Integration
  putStrLn "=== Test 5: AST Integration ==="
  testASTIntegration
  putStrLn ""

  -- Test 6: Parser integration
  putStrLn "=== Test 6: Parser Integration ==="
  testParserIntegration

-- Test JSDoc comment detection
testJSDocDetection :: IO ()
testJSDocDetection = do
  let testCases =
        [ ("/** Simple JSDoc */", True)
        , ("/* Regular comment */", False)
        , ("/***/", True)
        , ("/** Multi\n * line\n * comment */", True)
        , ("// Single line comment", False)
        , ("/**\n * @param {string} name\n */", True)
        ]

  mapM_ testDetection testCases
  where
    testDetection (comment, expected) = do
      let result = isJSDocComment comment
      putStrLn $ "Comment: " ++ take 30 comment ++ "..."
      putStrLn $ "Expected: " ++ show expected ++ ", Got: " ++ show result
      putStrLn $ "Status: " ++ if result == expected then "✓ PASS" else "✗ FAIL"
      putStrLn ""

-- Test JSDoc tag parsing
testJSDocTagParsing :: IO ()
testJSDocTagParsing = do
  let testCases =
        [ "/** @param {string} name The user's name */"
        , "/** @returns {boolean} True if valid */"
        , "/** @param {number} age @returns {Object} */"
        , "/**\n * @param {string} name\n * @param {number} age\n * @returns {User}\n */"
        , "/** @deprecated Use newFunction() instead */"
        , "/** @throws {Error} When invalid input */"
        ]

  mapM_ testTagParsing testCases
  where
    testTagParsing comment = do
      putStrLn $ "Testing: " ++ take 50 comment ++ "..."
      case parseJSDocFromComment tokenPosnEmpty comment of
        Just jsDoc -> do
          putStrLn $ "✓ Parsed successfully"
          putStrLn $ "  Description: " ++ show (jsDocDescription jsDoc)
          putStrLn $ "  Tags count: " ++ show (length (jsDocTags jsDoc))
          mapM_ printTag (jsDocTags jsDoc)
        Nothing -> putStrLn "✗ Failed to parse"
      putStrLn ""

-- Test complex JSDoc patterns
testComplexJSDocPatterns :: IO ()
testComplexJSDocPatterns = do
  let complexCases =
        [ "/** @param {Array<string>} items List of items */"
        , "/** @param {Object.<string, number>} mapping Key-value pairs */"
        , "/** @param {function(string): boolean} predicate Filter function */"
        , "/** @param {string|number|null} value Mixed type value */"
        , "/** @param {...string} args Variable arguments */"
        , "/** @param {Promise<User>} userPromise Async user data */"
        ]

  mapM_ testComplexPattern complexCases
  where
    testComplexPattern comment = do
      putStrLn $ "Complex pattern: " ++ take 60 comment ++ "..."
      case parseJSDocFromComment tokenPosnEmpty comment of
        Just jsDoc -> do
          putStrLn "✓ Parsed"
          mapM_ printDetailedTag (jsDocTags jsDoc)
        Nothing -> putStrLn "✗ Failed"
      putStrLn ""

-- Test edge cases
testJSDocEdgeCases :: IO ()
testJSDocEdgeCases = do
  let edgeCases =
        [ "/**/"  -- Empty JSDoc
        , "/** */"  -- JSDoc with just space
        , "/** \n */"  -- JSDoc with newline
        , "/** @param */"  -- Incomplete tag
        , "/** @param {} */"  -- Empty type
        , "/** @param {string */"  -- Malformed type
        , "/** @unknown-tag test */"  -- Unknown tag
        ]

  mapM_ testEdgeCase edgeCases
  where
    testEdgeCase comment = do
      putStrLn $ "Edge case: " ++ show comment
      case parseJSDocFromComment tokenPosnEmpty comment of
        Just jsDoc -> do
          putStrLn "✓ Parsed (may be empty)"
          putStrLn $ "  Tags: " ++ show (length (jsDocTags jsDoc))
        Nothing -> putStrLn "✗ Failed to parse"
      putStrLn ""

-- Test AST integration
testASTIntegration :: IO ()
testASTIntegration = do
  let jsCode = Text.pack $ unlines
        [ "/**"
        , " * Calculate the sum of two numbers"
        , " * @param {number} a First number"
        , " * @param {number} b Second number"
        , " * @returns {number} The sum"
        , " */"
        , "function add(a, b) {"
        , "  return a + b;"
        , "}"
        ]

  putStrLn "Testing AST integration with JSDoc..."
  case parseModule jsCode "" of
    Right ast -> do
      putStrLn "✓ JavaScript parsed successfully"
      putStrLn "Searching for JSDoc in AST..."

      -- Try to extract JSDoc from the AST
      let jsDocFound = extractJSDocFromAST ast
      if null jsDocFound
        then putStrLn "✗ No JSDoc found in AST"
        else do
          putStrLn $ "✓ Found " ++ show (length jsDocFound) ++ " JSDoc comment(s)"
          mapM_ printFoundJSDoc jsDocFound
    Left err -> putStrLn $ "✗ Parse error: " ++ show err

-- Test parser integration
testParserIntegration :: IO ()
testParserIntegration = do
  putStrLn "Testing parser integration with various JS constructs..."

  let testCases =
        [ ("/** @class */ class User {}", "Class with JSDoc")
        , ("/** @module */ var module = {};", "Module with JSDoc")
        , ("var x = /** @type {number} */ 42;", "Inline JSDoc")
        , ("/** @namespace */ var NS = { /** @method */ foo: function() {} };", "Nested JSDoc")
        ]

  mapM_ testParserCase testCases
  where
    testParserCase (code, description) = do
      putStrLn $ "Testing: " ++ description
      case parseModule (Text.pack code) "" of
        Right _ast -> putStrLn "✓ Parsed successfully"
        Left err -> putStrLn $ "✗ Parse error: " ++ show err
      putStrLn ""

-- Helper functions
printTag :: JSDocTag -> IO ()
printTag tag = do
  putStrLn $ "    @" ++ Text.unpack (jsDocTagName tag)
  case jsDocTagType tag of
    Just tagType -> putStrLn $ "      Type: " ++ show tagType
    Nothing -> return ()
  case jsDocTagParamName tag of
    Just paramName -> putStrLn $ "      Param: " ++ Text.unpack paramName
    Nothing -> return ()
  case jsDocTagDescription tag of
    Just desc -> putStrLn $ "      Description: " ++ Text.unpack desc
    Nothing -> return ()

printDetailedTag :: JSDocTag -> IO ()
printDetailedTag tag = do
  putStrLn $ "  Tag: @" ++ Text.unpack (jsDocTagName tag)
  putStrLn $ "  Type: " ++ show (jsDocTagType tag)
  putStrLn $ "  Param: " ++ show (jsDocTagParamName tag)
  putStrLn $ "  Desc: " ++ show (jsDocTagDescription tag)

printFoundJSDoc :: JSDocComment -> IO ()
printFoundJSDoc jsDoc = do
  putStrLn $ "  JSDoc at position: " ++ show (jsDocPosition jsDoc)
  putStrLn $ "  Description: " ++ show (jsDocDescription jsDoc)
  putStrLn $ "  Tags: " ++ show (length (jsDocTags jsDoc))

-- Extract JSDoc from AST (simplified version)
extractJSDocFromAST :: JSAST -> [JSDocComment]
extractJSDocFromAST ast =
  case ast of
    JSAstProgram stmts _ -> concatMap extractFromStatement stmts
    JSAstModule items _ -> concatMap extractFromModuleItem items
    JSAstStatement stmt _ -> extractFromStatement stmt
    JSAstExpression expr _ -> extractFromExpression expr
    JSAstLiteral _ _ -> []

extractFromStatement :: JSStatement -> [JSDocComment]
extractFromStatement stmt =
  case extractJSDocFromStatement stmt of
    Just jsDoc -> [jsDoc]
    Nothing -> []

extractFromExpression :: JSExpression -> [JSDocComment]
extractFromExpression expr =
  case extractJSDocFromExpression expr of
    Just jsDoc -> [jsDoc]
    Nothing -> []

extractFromModuleItem :: JSModuleItem -> [JSDocComment]
extractFromModuleItem item =
  case item of
    JSModuleStatementListItem stmt -> extractFromStatement stmt
    _ -> []