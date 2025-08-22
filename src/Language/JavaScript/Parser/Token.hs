{-# LANGUAGE CPP, DeriveDataTypeable, DeriveGeneric, DeriveAnyClass #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Python.Common.Token
-- Copyright   : (c) 2009 Bernie Pope
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Lexical tokens for the Python lexer. Contains the superset of tokens from
-- version 2 and version 3 of Python (they are mostly the same).
-----------------------------------------------------------------------------

module Language.JavaScript.Parser.Token
    (
      -- * The tokens
      Token (..)
    , CommentAnnotation (..)
    -- * String conversion
    , debugTokenString
    , tokenLiteralToString
    , commentAnnotationToString
    -- * Classification
    -- TokenClass (..),
    ) where

import Control.DeepSeq (NFData)
import Data.Data
import GHC.Generics (Generic)
import Language.JavaScript.Parser.SrcLocation
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8

data CommentAnnotation
    = CommentA TokenPosn ByteString
    | WhiteSpace TokenPosn ByteString
    | NoComment
    deriving (Eq, Generic, NFData, Show, Typeable, Data, Read)

-- | Lexical tokens.
-- Each may be annotated with any comment occurring between the prior token and this one
data Token
    -- Comment
    = CommentToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation] } -- ^ Single line comment.
    | WsToken      { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation] } -- ^ White space, for preservation.

    -- Identifiers
    | IdentifierToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }    -- ^ Identifier.
    | PrivateNameToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }    -- ^ Private identifier (#identifier).

    -- Javascript Literals

    | DecimalToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]   }
    -- ^ Literal: Decimal
    | HexIntegerToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]   }
    -- ^ Literal: Hexadecimal Integer
    | BinaryIntegerToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]   }
    -- ^ Literal: Binary Integer (ES2015)
    | OctalToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]   }
    -- ^ Literal: Octal Integer
    | StringToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    -- ^ Literal: string, delimited by either single or double quotes
    | RegExToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]   }
    -- ^ Literal: Regular Expression
    | BigIntToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation] }
    -- ^ Literal: BigInt Integer (e.g., 123n)

    -- Keywords
    | AsyncToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | AwaitToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | BreakToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | CaseToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | CatchToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | ClassToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | ConstToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | LetToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | ContinueToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | DebuggerToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | DefaultToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | DeleteToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | DoToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | ElseToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | EnumToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | ExtendsToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | FalseToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | FinallyToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | ForToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | FunctionToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | FromToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | IfToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | InToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | InstanceofToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | NewToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | NullToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | OfToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | ReturnToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | StaticToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | SuperToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | SwitchToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | ThisToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | ThrowToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | TrueToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | TryToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | TypeofToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | VarToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | VoidToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | WhileToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | YieldToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | ImportToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | WithToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | ExportToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    -- Future reserved words
    | FutureToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    -- Needed, not sure what they are though.
    | GetToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | SetToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }

    -- Delimiters
    -- Operators
    | AutoSemiToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | SemiColonToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | CommaToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | HookToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | ColonToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | OrToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | AndToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | BitwiseOrToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | BitwiseXorToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | BitwiseAndToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | StrictEqToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | EqToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | TimesAssignToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | DivideAssignToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | ModAssignToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | PlusAssignToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | MinusAssignToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | LshAssignToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | RshAssignToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | UrshAssignToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | AndAssignToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | XorAssignToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | OrAssignToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | LogicalAndAssignToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | LogicalOrAssignToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | NullishAssignToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | SimpleAssignToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | StrictNeToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | NeToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | LshToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | LeToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | LtToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | UrshToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | RshToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | GeToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | GtToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | IncrementToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | DecrementToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | PlusToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | MinusToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | MulToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | ExponentiationToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | DivToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | ModToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | NotToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | BitwiseNotToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | ArrowToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | SpreadToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | DotToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | OptionalChainingToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation] }
    -- ^ Optional chaining operator (?.)
    | OptionalBracketToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation] }
    -- ^ Optional bracket access (?.[)  
    | NullishCoalescingToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation] }
    -- ^ Nullish coalescing operator (??)
    | LeftBracketToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | RightBracketToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | LeftCurlyToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | RightCurlyToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | LeftParenToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | RightParenToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | CondcommentEndToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }

    -- Template literal lexical components
    | NoSubstitutionTemplateToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | TemplateHeadToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | TemplateMiddleToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | TemplateTailToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }

    -- Special cases
    | AsToken { tokenSpan :: !TokenPosn, tokenLiteral :: !ByteString, tokenComment :: ![CommentAnnotation]  }
    | TailToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  } -- ^ Stuff between last JS and EOF
    | EOFToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }  -- ^ End of file
    deriving (Eq, Generic, NFData, Show, Typeable)


-- | Produce a string from a token containing detailed information. Mainly intended for debugging.
debugTokenString :: Token -> String
debugTokenString = takeWhile (/= ' ') . show

-- | Convert token literal ByteString to String - single conversion point
tokenLiteralToString :: Token -> String
tokenLiteralToString = BS8.unpack . tokenLiteral

-- | Convert comment annotation ByteString to String
commentAnnotationToString :: CommentAnnotation -> String
commentAnnotationToString (CommentA _ bs) = BS8.unpack bs
commentAnnotationToString (WhiteSpace _ bs) = BS8.unpack bs
commentAnnotationToString NoComment = ""
