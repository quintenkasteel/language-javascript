-- | Re-export module for expression parsing.
--
-- All expression parsing logic now lives in "Language.JavaScript.Parser.Flatparse.Grammar"
-- to eliminate the circular dependency between expression and statement parsing.
-- This module re-exports the expression-related parsers for backward compatibility.
--
-- @since 0.8.0.0
module Language.JavaScript.Parser.Flatparse.Expression
  ( expression
  , assignmentExpression
  , primaryExpression
  , callMemberExpression
  , unaryExpression
  , binaryExpression
  , binaryOperator
  , unaryOperator
  , assignmentOperator
  , callExpression
  , literalExpression
  , identifierExpression
  , listToCommaList
  , listToCommaTrailingList
  , objectLiteral
  , arrayLiteral
  , argumentList
  , sepBy
  , sepBy1
  , parseChar
  , parseString
  , fpPosToAnnot
  , defaultAnnot
  , defaultSemi
  , classElement
  ) where

import Language.JavaScript.Parser.Flatparse.Grammar
  ( expression
  , assignmentExpression
  , primaryExpression
  , callMemberExpression
  , unaryExpression
  , binaryExpression
  , binaryOperator
  , unaryOperator
  , assignmentOperator
  , callExpression
  , literalExpression
  , identifierExpression
  , listToCommaList
  , listToCommaTrailingList
  , objectLiteral
  , arrayLiteral
  , argumentList
  , sepBy
  , sepBy1
  , parseChar
  , parseString
  , fpPosToAnnot
  , defaultAnnot
  , defaultSemi
  , classElement
  )
