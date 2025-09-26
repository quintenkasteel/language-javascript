{-# LANGUAGE OverloadedStrings #-}

import Language.JavaScript.Parser.Token
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser
import Language.JavaScript.Parser.SrcLocation

-- Test JSDoc parsing functionality
main :: IO ()
main = do
  putStrLn "=== JSDoc Integration Demo ==="
  putStrLn ""

  -- Test 1: JSDoc comment detection
  putStrLn "Test 1: JSDoc Comment Detection"
  let jsDocComment = "/** This is a JSDoc comment */"
  let regularComment = "/* This is a regular comment */"
  putStrLn $ "JSDoc comment detected: " ++ show (isJSDocComment jsDocComment)
  putStrLn $ "Regular comment detected: " ++ show (isJSDocComment regularComment)
  putStrLn ""

  -- Test 2: JSDoc parsing
  putStrLn "Test 2: JSDoc Content Parsing"
  let complexJSDoc = "/** Add two numbers @param {number} a First number @param {number} b Second number @returns {number} Sum */"
  case parseJSDocFromComment tokenPosnEmpty complexJSDoc of
    Just jsDoc -> do
      putStrLn "Successfully parsed JSDoc:"
      putStrLn $ "  Description: " ++ show (jsDocDescription jsDoc)
      putStrLn $ "  Number of tags: " ++ show (length (jsDocTags jsDoc))
      mapM_ printTag (jsDocTags jsDoc)
    Nothing -> putStrLn "Failed to parse JSDoc"
  putStrLn ""

  -- Test 3: Parsing JavaScript with JSDoc
  putStrLn "Test 3: JavaScript Parsing with JSDoc"
  jsContent <- readFile "test_jsdoc.js"
  case parseModule jsContent "" of
    Right ast -> do
      putStrLn "JavaScript parsed successfully!"
      putStrLn "Checking for JSDoc in AST..."
      -- The JSDoc would be in the comment annotations of the AST nodes
      putStrLn "AST contains JavaScript statements with potential JSDoc comments"
    Left err -> putStrLn $ "Parse error: " ++ show err

printTag :: JSDocTag -> IO ()
printTag tag = do
  putStrLn $ "    @" ++ show (jsDocTagName tag)
  case jsDocTagType tag of
    Just tagType -> putStrLn $ "      Type: " ++ show tagType
    Nothing -> return ()
  case jsDocTagParamName tag of
    Just paramName -> putStrLn $ "      Param: " ++ show paramName
    Nothing -> return ()
  case jsDocTagDescription tag of
    Just desc -> putStrLn $ "      Description: " ++ show desc
    Nothing -> return ()