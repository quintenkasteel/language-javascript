{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : Language.JavaScript.ParseError
-- Based on language-python version by Bernie Pope
-- Copyright   : (c) 2009 Bernie Pope
-- License     : BSD-style
-- Stability   : experimental
-- Portability : ghc
--
-- Enhanced error values for the lexer and parser with rich context information
-- and recovery suggestions for improved error reporting and recovery capabilities.
module Language.JavaScript.Parser.ParseError
  ( Error (..),
    ParseError (..),
    ParseContext (..),
    ErrorSeverity (..),
    RecoveryStrategy (..),
    renderParseError,
    getErrorPosition,
    isRecoverableError,
  )
where

--import Language.JavaScript.Parser.Pretty
-- import Control.Monad.Error.Class -- Control.Monad.Trans.Except
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
-- import Language.JavaScript.Parser.Lexer  -- No longer needed with flatparse
import Language.JavaScript.Parser.Token (Token, tokenSpan)
import Language.JavaScript.Parser.SrcLocation (TokenPosn)

-- | Parse context information for enhanced error reporting
data ParseContext
  = -- | At the top level of a program/module
    TopLevelContext
  | -- | Inside a function declaration or expression
    FunctionContext
  | -- | Inside a class declaration
    ClassContext
  | -- | Inside an expression
    ExpressionContext
  | -- | Inside a statement
    StatementContext
  | -- | Inside an object literal
    ObjectLiteralContext
  | -- | Inside an array literal
    ArrayLiteralContext
  | -- | Inside function parameters
    ParameterContext
  | -- | Inside import declaration
    ImportContext
  | -- | Inside export declaration
    ExportContext
  deriving (Eq, Generic, NFData, Show)

-- | Error severity levels for prioritizing error reporting
data ErrorSeverity
  = -- | Parse cannot continue
    CriticalError
  | -- | Significant syntax error but recovery possible
    MajorError
  | -- | Style or compatibility issue
    MinorError
  | -- | Potential issue but valid syntax
    Warning
  deriving (Eq, Generic, NFData, Show, Ord)

-- | Recovery strategies for panic mode error recovery
data RecoveryStrategy
  = -- | Skip to next semicolon
    SyncToSemicolon
  | -- | Skip to next closing brace
    SyncToCloseBrace
  | -- | Skip to next statement keyword
    SyncToKeyword
  | -- | Skip to end of file
    SyncToEOF
  | -- | Cannot recover from this error
    NoRecovery
  deriving (Eq, Generic, NFData, Show)

-- | Enhanced parse error types with rich context and recovery information
data ParseError
  = -- | Parser found unexpected token with context and suggestions
    UnexpectedToken
      { errorToken :: !Token,
        errorContext :: !ParseContext,
        expectedTokens :: ![String],
        errorSeverity :: !ErrorSeverity,
        recoveryStrategy :: !RecoveryStrategy
      }
  | -- | Lexer found unexpected character
    UnexpectedChar
      { errorChar :: !Char,
        errorPosition :: !TokenPosn,
        errorContext :: !ParseContext,
        errorSeverity :: !ErrorSeverity
      }
  | -- | General syntax error with suggestions
    SyntaxError
      { errorMessage :: !String,
        errorPosition :: !TokenPosn,
        errorContext :: !ParseContext,
        errorSeverity :: !ErrorSeverity,
        suggestions :: ![String]
      }
  | -- | Semantic validation error (e.g., invalid break/continue)
    SemanticError
      { errorMessage :: !String,
        errorPosition :: !TokenPosn,
        errorContext :: !ParseContext,
        errorDetails :: !String
      }
  | -- | Invalid numeric literal (e.g., 1.., 0x, 1e+)
    InvalidNumericLiteral
      { errorLiteral :: !String,
        errorPosition :: !TokenPosn,
        errorContext :: !ParseContext,
        suggestions :: ![String]
      }
  | -- | Invalid property access (e.g., x.123)
    InvalidPropertyAccess
      { errorProperty :: !String,
        errorPosition :: !TokenPosn,
        errorContext :: !ParseContext,
        suggestions :: ![String]
      }
  | -- | Invalid assignment target (e.g., 1 = x)
    InvalidAssignmentTarget
      { errorTarget :: !String,
        errorPosition :: !TokenPosn,
        errorContext :: !ParseContext,
        suggestions :: ![String]
      }
  | -- | Invalid control flow label (e.g., break 123)
    InvalidControlFlowLabel
      { errorLabel :: !String,
        errorPosition :: !TokenPosn,
        errorContext :: !ParseContext,
        suggestions :: ![String]
      }
  | -- | Const declaration without initializer
    MissingConstInitializer
      { errorIdentifier :: !String,
        errorPosition :: !TokenPosn,
        errorContext :: !ParseContext,
        suggestions :: ![String]
      }
  | -- | Invalid identifier (e.g., 123x)
    InvalidIdentifier
      { errorIdentifier :: !String,
        errorPosition :: !TokenPosn,
        errorContext :: !ParseContext,
        suggestions :: ![String]
      }
  | -- | Invalid arrow function parameter (e.g., (123) => x)
    InvalidArrowParameter
      { errorParameter :: !String,
        errorPosition :: !TokenPosn,
        errorContext :: !ParseContext,
        suggestions :: ![String]
      }
  | -- | Invalid escape sequence (e.g., \x, \u)
    InvalidEscapeSequence
      { errorSequence :: !String,
        errorPosition :: !TokenPosn,
        errorContext :: !ParseContext,
        suggestions :: ![String]
      }
  | -- | Invalid regex pattern or flags
    InvalidRegexPattern
      { errorPattern :: !String,
        errorPosition :: !TokenPosn,
        errorContext :: !ParseContext,
        suggestions :: ![String]
      }
  | -- | Invalid unicode escape sequence
    InvalidUnicodeSequence
      { errorSequence :: !String,
        errorPosition :: !TokenPosn,
        errorContext :: !ParseContext,
        suggestions :: ![String]
      }
  | -- | Legacy generic string error for backwards compatibility
    StrError String
  deriving (Eq, Generic, NFData, Show)

class Error a where
  -- | Creates an exception without a message.
  -- The default implementation is @'strMsg' \"\"@.
  noMsg :: a

  -- | Creates an exception with a message.
  -- The default implementation of @'strMsg' s@ is 'noMsg'.
  strMsg :: String -> a

instance Error ParseError where
  noMsg = StrError ""
  strMsg = StrError

-- | Render a parse error to a human-readable string with context
renderParseError :: ParseError -> String
renderParseError err = case err of
  UnexpectedToken token ctx expected severity _ ->
    let pos = show (tokenSpan token)
        tokenStr = show token
        contextStr = renderContext ctx
        expectedStr =
          if null expected
            then ""
            else "\n  Expected: " ++ unwords expected
        severityStr = "[" ++ show severity ++ "]"
     in severityStr ++ " Unexpected token " ++ tokenStr ++ " at " ++ pos
          ++ "\n  Context: "
          ++ contextStr
          ++ expectedStr
  UnexpectedChar char pos ctx severity ->
    let posStr = show pos
        contextStr = renderContext ctx
        severityStr = "[" ++ show severity ++ "]"
     in severityStr ++ " Unexpected character '" ++ [char] ++ "' at " ++ posStr
          ++ "\n  Context: "
          ++ contextStr
  SyntaxError msg pos ctx severity errSuggestions ->
    let posStr = show pos
        contextStr = renderContext ctx
        severityStr = "[" ++ show severity ++ "]"
        suggestStr =
          if null errSuggestions
            then ""
            else "\n  Suggestions: " ++ unlines (map ("    - " ++) errSuggestions)
     in severityStr ++ " Syntax error at " ++ posStr ++ ": " ++ msg
          ++ "\n  Context: "
          ++ contextStr
          ++ suggestStr
  SemanticError msg pos ctx details ->
    let posStr = show pos
        contextStr = renderContext ctx
     in "[Semantic Error] " ++ msg ++ " at " ++ posStr
          ++ "\n  Context: "
          ++ contextStr
          ++ "\n  Details: "
          ++ details
  InvalidNumericLiteral literal pos ctx errSuggestions ->
    let posStr = show pos
        contextStr = renderContext ctx
        suggestStr = renderSuggestions errSuggestions
     in "[Validation Error] Invalid numeric literal '" ++ literal ++ "' at " ++ posStr
          ++ "\n  Context: "
          ++ contextStr
          ++ suggestStr
  InvalidPropertyAccess prop pos ctx errSuggestions ->
    let posStr = show pos
        contextStr = renderContext ctx
        suggestStr = renderSuggestions errSuggestions
     in "[Validation Error] Invalid property access '." ++ prop ++ "' at " ++ posStr
          ++ "\n  Context: "
          ++ contextStr
          ++ suggestStr
  InvalidAssignmentTarget target pos ctx errSuggestions ->
    let posStr = show pos
        contextStr = renderContext ctx
        suggestStr = renderSuggestions errSuggestions
     in "[Validation Error] Invalid assignment target '" ++ target ++ "' at " ++ posStr
          ++ "\n  Context: "
          ++ contextStr
          ++ suggestStr
  InvalidControlFlowLabel label pos ctx errSuggestions ->
    let posStr = show pos
        contextStr = renderContext ctx
        suggestStr = renderSuggestions errSuggestions
     in "[Validation Error] Invalid control flow label '" ++ label ++ "' at " ++ posStr
          ++ "\n  Context: "
          ++ contextStr
          ++ suggestStr
  MissingConstInitializer ident pos ctx errSuggestions ->
    let posStr = show pos
        contextStr = renderContext ctx
        suggestStr = renderSuggestions errSuggestions
     in "[Validation Error] Missing const initializer for '" ++ ident ++ "' at " ++ posStr
          ++ "\n  Context: "
          ++ contextStr
          ++ suggestStr
  InvalidIdentifier ident pos ctx errSuggestions ->
    let posStr = show pos
        contextStr = renderContext ctx
        suggestStr = renderSuggestions errSuggestions
     in "[Validation Error] Invalid identifier '" ++ ident ++ "' at " ++ posStr
          ++ "\n  Context: "
          ++ contextStr
          ++ suggestStr
  InvalidArrowParameter param pos ctx errSuggestions ->
    let posStr = show pos
        contextStr = renderContext ctx
        suggestStr = renderSuggestions errSuggestions
     in "[Validation Error] Invalid arrow function parameter '" ++ param ++ "' at " ++ posStr
          ++ "\n  Context: "
          ++ contextStr
          ++ suggestStr
  InvalidEscapeSequence seq pos ctx errSuggestions ->
    let posStr = show pos
        contextStr = renderContext ctx
        suggestStr = renderSuggestions errSuggestions
     in "[Validation Error] Invalid escape sequence '" ++ seq ++ "' at " ++ posStr
          ++ "\n  Context: "
          ++ contextStr
          ++ suggestStr
  InvalidRegexPattern pattern pos ctx errSuggestions ->
    let posStr = show pos
        contextStr = renderContext ctx
        suggestStr = renderSuggestions errSuggestions
     in "[Validation Error] Invalid regex pattern '" ++ pattern ++ "' at " ++ posStr
          ++ "\n  Context: "
          ++ contextStr
          ++ suggestStr
  InvalidUnicodeSequence seq pos ctx errSuggestions ->
    let posStr = show pos
        contextStr = renderContext ctx
        suggestStr = renderSuggestions errSuggestions
     in "[Validation Error] Invalid unicode sequence '" ++ seq ++ "' at " ++ posStr
          ++ "\n  Context: "
          ++ contextStr
          ++ suggestStr
  StrError msg -> "Parse error: " ++ msg

-- | Helper function to render suggestions
renderSuggestions :: [String] -> String
renderSuggestions [] = ""
renderSuggestions errSuggestions =
  "\n  Suggestions: " ++ unlines (map ("    - " ++) errSuggestions)

-- | Render parse context to human-readable string
renderContext :: ParseContext -> String
renderContext ctx = case ctx of
  TopLevelContext -> "top level"
  FunctionContext -> "function body"
  ClassContext -> "class definition"
  ExpressionContext -> "expression"
  StatementContext -> "statement"
  ObjectLiteralContext -> "object literal"
  ArrayLiteralContext -> "array literal"
  ParameterContext -> "parameter list"
  ImportContext -> "import declaration"
  ExportContext -> "export declaration"

-- | Get the source position from any parse error
getErrorPosition :: ParseError -> Maybe TokenPosn
getErrorPosition err = case err of
  UnexpectedToken token _ _ _ _ -> Just (tokenSpan token)
  UnexpectedChar _ pos _ _ -> Just pos
  SyntaxError _ pos _ _ _ -> Just pos
  SemanticError _ pos _ _ -> Just pos
  InvalidNumericLiteral _ pos _ _ -> Just pos
  InvalidPropertyAccess _ pos _ _ -> Just pos
  InvalidAssignmentTarget _ pos _ _ -> Just pos
  InvalidControlFlowLabel _ pos _ _ -> Just pos
  MissingConstInitializer _ pos _ _ -> Just pos
  InvalidIdentifier _ pos _ _ -> Just pos
  InvalidArrowParameter _ pos _ _ -> Just pos
  InvalidEscapeSequence _ pos _ _ -> Just pos
  InvalidRegexPattern _ pos _ _ -> Just pos
  InvalidUnicodeSequence _ pos _ _ -> Just pos
  StrError _ -> Nothing

-- | Check if an error is recoverable using panic mode
isRecoverableError :: ParseError -> Bool
isRecoverableError err = case err of
  UnexpectedToken _ _ _ _ strategy -> strategy /= NoRecovery
  UnexpectedChar _ _ _ severity -> severity /= CriticalError
  SyntaxError _ _ _ severity _ -> severity /= CriticalError
  SemanticError _ _ _ _ -> True -- Semantic errors don't prevent parsing
  -- Validation errors are not recoverable - syntax must be correct
  InvalidNumericLiteral _ _ _ _ -> False
  InvalidPropertyAccess _ _ _ _ -> False
  InvalidAssignmentTarget _ _ _ _ -> False
  InvalidControlFlowLabel _ _ _ _ -> False
  MissingConstInitializer _ _ _ _ -> False
  InvalidIdentifier _ _ _ _ -> False
  InvalidArrowParameter _ _ _ _ -> False
  InvalidEscapeSequence _ _ _ _ -> False
  InvalidRegexPattern _ _ _ _ -> False
  InvalidUnicodeSequence _ _ _ _ -> False
  StrError _ -> False -- Legacy errors are not recoverable
