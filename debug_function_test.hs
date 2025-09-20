#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import Language.JavaScript.Parser (parse)
import Language.JavaScript.Pretty.Printer (renderToString)
import Language.JavaScript.Process.TreeShake (treeShake, defaultOptions, analyzeUsage)
import Language.JavaScript.Process.TreeShake.Types
import Language.JavaScript.Parser.AST

main :: IO ()
main = do
  let source = "function used() { return 1; } function unused() { return 2; } used();"
  putStrLn "=== FUNCTION PRESERVATION TEST ==="
  putStrLn $ "Source: " ++ source
  
  case parse source "test" of
    Right ast -> do
      putStrLn "\n=== ORIGINAL AST ==="
      putStrLn $ renderToString ast
      
      putStrLn "\n=== USAGE ANALYSIS ==="
      let analysis = analyzeUsage ast
      let usageMap = _usageMap analysis
      
      putStrLn $ "Total identifiers: " ++ show (_totalIdentifiers analysis)
      putStrLn $ "Unused count: " ++ show (_unusedCount analysis)
      
      putStrLn "\n=== USAGE MAP DETAILS ==="
      Map.foldrWithKey (\name info acc -> do
        putStrLn $ Text.unpack name ++ ": used=" ++ show (_isUsed info) 
                                   ++ ", exported=" ++ show (_isExported info)
                                   ++ ", scope=" ++ show (_scopeDepth info)
                                   ++ ", refs=" ++ show (_directReferences info)
        acc) (pure ()) usageMap
      
      putStrLn "\n=== TREE SHAKING RESULT ==="
      let optimized = treeShake defaultOptions ast
      putStrLn $ renderToString optimized
      
      putStrLn "\n=== IDENTIFIER CHECK ==="
      putStrLn $ "Contains 'used': " ++ show (astContainsIdentifier optimized "used")
      putStrLn $ "Contains 'unused': " ++ show (astContainsIdentifier optimized "unused")
      
    Left err -> putStrLn $ "Parse failed: " ++ err

-- Helper function from test
astContainsIdentifier :: JSAST -> Text.Text -> Bool
astContainsIdentifier ast identifier = case ast of
  JSAstProgram statements _ -> 
    any (statementContainsIdentifier identifier) statements
  JSAstModule items _ -> 
    any (moduleItemContainsIdentifier identifier) items  
  JSAstStatement stmt _ ->
    statementContainsIdentifier identifier stmt
  JSAstExpression expr _ ->
    expressionContainsIdentifier identifier expr
  JSAstLiteral expr _ ->
    expressionContainsIdentifier identifier expr

-- Comprehensive checker functions that handle ALL expression types including function expressions
statementContainsIdentifier :: Text.Text -> JSStatement -> Bool
statementContainsIdentifier identifier stmt = case stmt of
  JSFunction _ ident _ _ _ body _ ->
    identifierMatches identifier ident || blockContainsIdentifier identifier body
  JSVariable _ decls _ ->
    any (expressionContainsIdentifier identifier) (fromCommaList decls)
  JSExpressionStatement expr _ ->
    expressionContainsIdentifier identifier expr
  _ -> False

expressionContainsIdentifier :: Text.Text -> JSExpression -> Bool  
expressionContainsIdentifier identifier expr = case expr of
  JSIdentifier _ name -> Text.pack name == identifier
  JSVarInitExpression lhs _ -> expressionContainsIdentifier identifier lhs
  JSCallExpression target _ args _ ->
    expressionContainsIdentifier identifier target ||
    any (expressionContainsIdentifier identifier) (fromCommaList args)
  JSFunctionExpression _ ident _ _ _ body ->
    identifierMatches identifier ident || blockContainsIdentifier identifier body
  _ -> False

blockContainsIdentifier :: Text.Text -> JSBlock -> Bool
blockContainsIdentifier identifier (JSBlock _ stmts _) =
  any (statementContainsIdentifier identifier) stmts

moduleItemContainsIdentifier :: Text.Text -> JSModuleItem -> Bool
moduleItemContainsIdentifier identifier item = case item of
  JSModuleStatementListItem stmt -> statementContainsIdentifier identifier stmt
  _ -> False

identifierMatches :: Text.Text -> JSIdent -> Bool
identifierMatches identifier (JSIdentName _ name) = Text.pack name == identifier
identifierMatches _ JSIdentNone = False

fromCommaList :: JSCommaList a -> [a]
fromCommaList JSLNil = []
fromCommaList (JSLOne x) = [x]
fromCommaList (JSLCons rest _ x) = x : fromCommaList rest