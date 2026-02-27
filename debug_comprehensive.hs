#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text, bytestring, flatparse
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified FlatParse.Basic as FP
import Data.ByteString (ByteString)

type JSParser = FP.Parser ByteString

-- Test fundamental parsing chain step by step
parseChar :: Char -> JSParser ()
parseChar c = do
  actual <- FP.anyChar
  if actual == c
    then pure ()
    else FP.empty

parseString :: String -> JSParser ()
parseString = mapM_ parseChar

-- Test whitespace
whitespace :: JSParser ()
whitespace = FP.skipMany (FP.satisfy (\c -> c `elem` [' ', '\t', '\n', '\r']))

-- Test identifier (simplified)
isIdentifierStart :: Char -> Bool
isIdentifierStart c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || c == '$'

isIdentifierContinue :: Char -> Bool
isIdentifierContinue c = isIdentifierStart c || (c >= '0' && c <= '9')

identifier :: JSParser String
identifier = do
  first <- FP.satisfy isIdentifierStart
  rest <- FP.many (FP.satisfy isIdentifierContinue)
  pure (first : rest)

-- Test numeric literal (simplified)
numericLiteral :: JSParser String
numericLiteral = FP.some (FP.satisfy (\c -> c >= '0' && c <= '9'))

-- Test expression (minimal)
expression :: JSParser String
expression = numericLiteral FP.<|> identifier

-- Test variable declarator
variableDeclarator :: JSParser (String, Maybe String)
variableDeclarator = do
  name <- identifier
  initializer <- FP.optional initializerExpression
  pure (name, initializer)
  where
    initializerExpression = do
      whitespace
      parseChar '='
      whitespace
      expr <- expression
      pure expr

-- Test variable declaration
variableDeclaration :: JSParser [(String, Maybe String)]
variableDeclaration = do
  parseString "var"
  whitespace
  declarators <- sepBy1 variableDeclarator (whitespace *> parseChar ',' *> whitespace)
  whitespace
  FP.optional (parseChar ';')
  pure declarators

sepBy1 :: JSParser a -> JSParser sep -> JSParser [a]
sepBy1 p sep = do
  first <- p
  rest <- FP.many (sep *> p)
  pure (first : rest)

-- Test the full parsing chain
testFullChain :: IO ()
testFullChain = do
  let input = "var x = 42;"
  let inputBS = Text.encodeUtf8 (Text.pack input)

  putStrLn $ "Testing full parsing chain for: " ++ input
  putStrLn ""

  -- Test each step
  putStrLn "Step 1: Test initial whitespace"
  case FP.runParser whitespace inputBS of
    FP.OK _ remaining -> putStrLn $ "✓ Whitespace: remaining = " ++ show remaining
    FP.Fail -> putStrLn "✗ Whitespace failed"
    FP.Err _ -> putStrLn "✗ Whitespace error"

  putStrLn "\nStep 2: Test 'var' keyword"
  case FP.runParser (parseString "var") inputBS of
    FP.OK _ remaining -> putStrLn $ "✓ 'var': remaining = " ++ show remaining
    FP.Fail -> putStrLn "✗ 'var' failed"
    FP.Err _ -> putStrLn "✗ 'var' error"

  putStrLn "\nStep 3: Test identifier 'x'"
  let afterVar = Text.encodeUtf8 "x = 42;"
  case FP.runParser identifier afterVar of
    FP.OK result remaining -> putStrLn $ "✓ Identifier: '" ++ result ++ "', remaining = " ++ show remaining
    FP.Fail -> putStrLn "✗ Identifier failed"
    FP.Err _ -> putStrLn "✗ Identifier error"

  putStrLn "\nStep 4: Test expression '42'"
  let exprInput = Text.encodeUtf8 "42;"
  case FP.runParser expression exprInput of
    FP.OK result remaining -> putStrLn $ "✓ Expression: '" ++ result ++ "', remaining = " ++ show remaining
    FP.Fail -> putStrLn "✗ Expression failed"
    FP.Err _ -> putStrLn "✗ Expression error"

  putStrLn "\nStep 5: Test variable declarator 'x = 42'"
  let declaratorInput = Text.encodeUtf8 "x = 42"
  case FP.runParser variableDeclarator declaratorInput of
    FP.OK (name, init) remaining -> putStrLn $ "✓ Declarator: name='" ++ name ++ "', init=" ++ show init ++ ", remaining = " ++ show remaining
    FP.Fail -> putStrLn "✗ Declarator failed"
    FP.Err _ -> putStrLn "✗ Declarator error"

  putStrLn "\nStep 6: Test full variable declaration"
  case FP.runParser variableDeclaration inputBS of
    FP.OK result remaining -> putStrLn $ "✓ Variable declaration: " ++ show result ++ ", remaining = " ++ show remaining
    FP.Fail -> putStrLn "✗ Variable declaration failed"
    FP.Err _ -> putStrLn "✗ Variable declaration error"

  putStrLn "\nStep 7: Test with EOF"
  case FP.runParser (variableDeclaration <* FP.eof) inputBS of
    FP.OK result remaining -> putStrLn $ "✓ With EOF: " ++ show result ++ ", remaining = " ++ show remaining
    FP.Fail -> putStrLn "✗ With EOF failed"
    FP.Err _ -> putStrLn "✗ With EOF error"

main :: IO ()
main = testFullChain