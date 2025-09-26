#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Control.Lens ((^.), (.~), (&))
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Process.TreeShake.Types
import Language.JavaScript.Pretty.Printer (renderToString)

main :: IO ()
main = do
  let source = "var used = 1, unused = 2; console.log(used);"
  putStrLn $ "Original source: " ++ source
  case parse source "test" of
    Right ast -> do
      putStrLn "\n=== ORIGINAL AST ==="
      print ast
      putStrLn "\n=== USAGE ANALYSIS ==="
      let analysis = analyzeUsage ast
      print analysis
      putStrLn "\n=== OPTIMIZED AST ==="
      let optimized = treeShake defaultOptions ast
      print optimized
      putStrLn "\n=== PRETTY PRINTED ORIGINAL ==="
      putStrLn $ renderToString ast
      putStrLn "\n=== PRETTY PRINTED OPTIMIZED ==="
      putStrLn $ renderToString optimized
      putStrLn "\n=== IDENTIFIER CHECK RESULTS ==="
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

statementContainsIdentifier :: Text.Text -> JSStatement -> Bool
statementContainsIdentifier identifier stmt = case stmt of
  JSFunction _ ident _ _ _ body _ ->
    identifierMatches identifier ident || blockContainsIdentifier identifier body
  JSVariable _ decls _ ->
    any (expressionContainsIdentifier identifier) (fromCommaList decls)
  JSLet _ decls _ ->
    any (expressionContainsIdentifier identifier) (fromCommaList decls)  
  JSConstant _ decls _ ->
    any (expressionContainsIdentifier identifier) (fromCommaList decls)
  JSClass _ ident _ _ _ _ _ ->
    identifierMatches identifier ident
  JSExpressionStatement expr _ ->
    expressionContainsIdentifier identifier expr
  JSStatementBlock _ stmts _ _ ->
    any (statementContainsIdentifier identifier) stmts
  JSReturn _ (Just expr) _ ->
    expressionContainsIdentifier identifier expr
  JSIf _ _ test _ thenStmt ->
    expressionContainsIdentifier identifier test || 
    statementContainsIdentifier identifier thenStmt
  JSIfElse _ _ test _ thenStmt _ elseStmt ->
    expressionContainsIdentifier identifier test || 
    statementContainsIdentifier identifier thenStmt ||
    statementContainsIdentifier identifier elseStmt
  _ -> False

expressionContainsIdentifier :: Text.Text -> JSExpression -> Bool
expressionContainsIdentifier identifier expr = case expr of
  JSIdentifier _ name -> Text.pack name == identifier
  JSVarInitExpression lhs _ -> expressionContainsIdentifier identifier lhs
  JSCallExpression func _ args _ ->
    expressionContainsIdentifier identifier func ||
    any (expressionContainsIdentifier identifier) (fromCommaList args)
  JSCallExpressionDot func _ prop ->
    expressionContainsIdentifier identifier func ||
    expressionContainsIdentifier identifier prop
  JSCallExpressionSquare func _ prop _ ->
    expressionContainsIdentifier identifier func ||
    expressionContainsIdentifier identifier prop
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