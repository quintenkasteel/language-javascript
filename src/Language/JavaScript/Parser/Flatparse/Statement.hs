-- | Re-export module for statement parsing.
--
-- All statement parsing logic now lives in "Language.JavaScript.Parser.Flatparse.Grammar"
-- to eliminate the circular dependency between expression and statement parsing.
-- This module re-exports the statement-related parsers for backward compatibility.
--
-- @since 0.8.0.0
module Language.JavaScript.Parser.Flatparse.Statement
  ( statement
  , statementList
  , blockStatement
  , expressionStatement
  , emptyStatement
  , variableDeclaration
  , variableDeclarator
  , ifStatement
  , whileStatement
  , doWhileStatement
  , forStatement
  , forInit
  , switchStatement
  , caseClause
  , defaultClause
  , returnStatement
  , breakStatement
  , continueStatement
  , throwStatement
  , functionDeclaration
  , classDeclaration
  , classElement
  , regularMethodDefinition
  , tryStatement
  , catchClause
  , finallyClause
  , labeledStatement
  , withStatementPos
  , expectStatementEnd
  , isStatementKeyword
  -- Module items
  , moduleItem
  , moduleItemList
  ) where

import Language.JavaScript.Parser.Flatparse.Grammar
  ( statement
  , statementList
  , blockStatement
  , expressionStatement
  , emptyStatement
  , variableDeclaration
  , variableDeclarator
  , ifStatement
  , whileStatement
  , doWhileStatement
  , forStatement
  , forInit
  , switchStatement
  , caseClause
  , defaultClause
  , returnStatement
  , breakStatement
  , continueStatement
  , throwStatement
  , functionDeclaration
  , classDeclaration
  , classElement
  , regularMethodDefinition
  , tryStatement
  , catchClause
  , finallyClause
  , labeledStatement
  , withStatementPos
  , expectStatementEnd
  , isStatementKeyword
  , moduleItem
  , moduleItemList
  )
