{-# OPTIONS  #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.JavaScript.ParserMonad
-- Copyright   : (c) 2012 Alan Zimmerman
-- License     : BSD-style
-- Stability   : experimental
-- Portability : ghc
--
-- Monad support for JavaScript parser and lexer.
-----------------------------------------------------------------------------

module Language.JavaScript.Parser.ParserMonad
    ( AlexUserState(..)
    , alexInitUserState
    ) where

import Language.JavaScript.Parser.Token
import Language.JavaScript.Parser.SrcLocation

data AlexUserState = AlexUserState
    { previousToken :: !Token   -- ^the previous token
    , comment :: [Token]        -- ^the previous comment, if any
    , inTemplate :: Bool        -- ^whether the parser is expecting template characters
    }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
    { previousToken = initToken
    , comment = []
    , inTemplate = False
    }

initToken :: Token
initToken = CommentToken tokenPosnEmpty "" []
