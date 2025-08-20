-----------------------------------------------------------------------------
-- |
-- Module      : Language.JavaScript.LexerUtils
-- Based on language-python version by Bernie Pope
-- Copyright   : (c) 2009 Bernie Pope
-- License     : BSD-style
-- Stability   : experimental
-- Portability : ghc
--
-- Various utilities to support the JavaScript lexer.
-----------------------------------------------------------------------------

module Language.JavaScript.Parser.LexerUtils
    ( StartCode
    , symbolToken
    , mkString
    , mkString'
    , commentToken
    , wsToken
    , regExToken
    , decimalToken
    , hexIntegerToken
    , binaryIntegerToken
    , octalToken
    , bigIntToken
    , stringToken
    ) where

import Language.JavaScript.Parser.Token as Token
import Language.JavaScript.Parser.SrcLocation
import Prelude hiding (span)

-- Functions for building tokens

type StartCode = Int

symbolToken :: Monad m => (TokenPosn -> [CommentAnnotation] -> Token) -> TokenPosn -> Int -> String -> m Token
symbolToken mkToken location _ _ = return (mkToken location [])

mkString :: (Monad m) => (TokenPosn -> String -> Token) -> TokenPosn -> Int -> String -> m Token
mkString toToken loc len str = return (toToken loc (take len str))

mkString' :: (Monad m) => (TokenPosn -> String -> [CommentAnnotation] -> Token) -> TokenPosn -> Int -> String -> m Token
mkString' toToken loc len str = return (toToken loc (take len str) [])

decimalToken :: TokenPosn -> String -> Token
decimalToken loc str = DecimalToken loc str []

hexIntegerToken :: TokenPosn -> String -> Token
hexIntegerToken loc str = HexIntegerToken loc str []

binaryIntegerToken :: TokenPosn -> String -> Token
binaryIntegerToken loc str = BinaryIntegerToken loc str []

octalToken :: TokenPosn -> String -> Token
octalToken loc str = OctalToken loc str []

bigIntToken :: TokenPosn -> String -> Token
bigIntToken loc str = BigIntToken loc str []

regExToken :: TokenPosn -> String -> Token
regExToken loc str = RegExToken loc str []

stringToken :: TokenPosn -> String -> Token
stringToken loc str = StringToken loc str []

commentToken :: TokenPosn -> String -> Token
commentToken loc str = CommentToken loc str []

wsToken :: TokenPosn -> String -> Token
wsToken loc str = WsToken loc str []
