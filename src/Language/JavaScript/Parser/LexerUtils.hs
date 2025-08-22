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
import Data.List (isInfixOf)
import Prelude hiding (span)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- Functions for building tokens

type StartCode = Int

-- Helper function for proper UTF-8 encoding of Haskell String to ByteString
stringToUtf8ByteString :: String -> ByteString
stringToUtf8ByteString = Text.encodeUtf8 . Text.pack

symbolToken :: Monad m => (TokenPosn -> [CommentAnnotation] -> Token) -> TokenPosn -> Int -> String -> m Token
symbolToken mkToken location _ _ = return (mkToken location [])

mkString :: (Monad m) => (TokenPosn -> String -> Token) -> TokenPosn -> Int -> String -> m Token
mkString toToken loc len str = return (toToken loc (take len str))

mkString' :: (Monad m) => (TokenPosn -> ByteString -> [CommentAnnotation] -> Token) -> TokenPosn -> Int -> String -> m Token
mkString' toToken loc len str = return (toToken loc (stringToUtf8ByteString (take len str)) [])

decimalToken :: TokenPosn -> String -> Token
decimalToken loc str 
  -- Validate decimal literal for edge cases
  | isValidDecimal str = DecimalToken loc (stringToUtf8ByteString str) []
  | otherwise = error ("Invalid decimal literal: " ++ str ++ " at " ++ show loc)
  where
    -- Check for invalid decimal patterns - very conservative
    isValidDecimal s
      -- Only reject clearly invalid patterns
      | ".." `isInfixOf` s = False  -- Reject incomplete decimals like "1.."
      | s `elem` [".", ".."] = False  -- Reject standalone dots  
      | otherwise = True  -- Accept everything else for now
    

hexIntegerToken :: TokenPosn -> String -> Token
hexIntegerToken loc str 
  -- Very conservative hex validation - only reject clearly incomplete patterns
  | isValidHex str = HexIntegerToken loc (stringToUtf8ByteString str) []
  | otherwise = error ("Invalid hex literal: " ++ str ++ " at " ++ show loc)
  where
    -- Check for invalid hex patterns
    isValidHex s
      -- Only reject incomplete hex prefixes like "0x" or "0X" with no digits
      | s `elem` ["0x", "0X"] = False
      | otherwise = True  -- Accept everything else
    
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

binaryIntegerToken :: TokenPosn -> String -> Token
binaryIntegerToken loc str
  -- Very conservative binary validation  
  | isValidBinary str = BinaryIntegerToken loc (stringToUtf8ByteString str) []
  | otherwise = error ("Invalid binary literal: " ++ str ++ " at " ++ show loc)
  where
    -- Check for invalid binary patterns
    isValidBinary s
      -- Only reject incomplete prefixes like "0b" or "0B" with no digits
      | s `elem` ["0b", "0B"] = False
      | otherwise = True  -- Accept everything else
    
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

octalToken :: TokenPosn -> String -> Token
octalToken loc str
  -- Very conservative octal validation
  | isValidOctal str = OctalToken loc (stringToUtf8ByteString str) []
  | otherwise = error ("Invalid octal literal: " ++ str ++ " at " ++ show loc)
  where
    -- Check for invalid octal patterns
    isValidOctal s
      -- Only reject incomplete prefixes like "0o" or "0O" with no digits
      | s `elem` ["0o", "0O"] = False
      | otherwise = True  -- Accept everything else
    
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

bigIntToken :: TokenPosn -> String -> Token
bigIntToken loc str = BigIntToken loc (stringToUtf8ByteString str) []

regExToken :: TokenPosn -> String -> Token
regExToken loc str = RegExToken loc (stringToUtf8ByteString str) []

stringToken :: TokenPosn -> String -> Token
stringToken loc str = StringToken loc (stringToUtf8ByteString str) []

commentToken :: TokenPosn -> String -> Token
commentToken loc str = CommentToken loc (stringToUtf8ByteString str) []

wsToken :: TokenPosn -> String -> Token
wsToken loc str = WsToken loc (stringToUtf8ByteString str) []
