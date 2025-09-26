{-# OPTIONS  #-}
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : Language.JavaScript.ParserMonad
-- Copyright   : (c) 2012 Alan Zimmerman
-- License     : BSD-style
-- Stability   : experimental
-- Portability : ghc
--
-- Monad support for JavaScript parser and lexer.
module Language.JavaScript.Parser.ParserMonad
  ( AlexUserState (..),
    alexInitUserState,
  )
where

import Language.JavaScript.Parser.SrcLocation
import Language.JavaScript.Parser.Token

data AlexUserState = AlexUserState
  { -- | the previous token
    previousToken :: !Token,
    -- | the previous comment, if any
    comment :: [Token],
    -- | whether the parser is expecting template characters
    inTemplate :: Bool
  }

alexInitUserState :: AlexUserState
alexInitUserState =
  AlexUserState
    { previousToken = initToken,
      comment = [],
      inTemplate = False
    }

initToken :: Token
initToken = CommentToken tokenPosnEmpty "" []
