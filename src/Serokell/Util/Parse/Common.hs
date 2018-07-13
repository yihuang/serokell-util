{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Parsing common helpers

module Serokell.Util.Parse.Common
       ( CharParser
       , countMinMax
       , limitedInt
       , byte
       , asciiAlphaNum
       , parseIntegralSafe
       ) where

import Universum hiding (fail)

import Control.Monad (fail)
import Prelude (read)
import Text.Megaparsec (ParsecT, Stream, option, Token, Tokens)
import Text.Megaparsec.Char (satisfy, digitChar)

type CharParser a = forall e s m. (Ord e, Stream s, Token s ~ Char, Tokens s ~ [Char]) => ParsecT e s m a

isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

isAsciiNum :: Char -> Bool
isAsciiNum c = c >= '0' && c <= '9'

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAsciiAlpha c || isAsciiNum c

asciiAlphaNum :: CharParser Char
asciiAlphaNum = satisfy isAsciiAlphaNum

countMinMax :: (Ord e, Stream s) => Int -> Int -> ParsecT e s m a -> ParsecT e s m [a]
countMinMax m x p
    | m > 0 = do
        f <- p
        end <- countMinMax (m - 1) (x - 1) p
        return $ f : end
    | x <= 0 = return []
    | otherwise = option [] $ do
        f <- p
        end <- countMinMax 0 (x - 1) p
        return $ f : end

limitedInt :: Int -> String -> CharParser Int
limitedInt x e = do
    b <- read <$> countMinMax 1 (intDigits x) digitChar
    if b > x
        then fail e
        else return b
  where
    intDigits = length . show @String

byte :: CharParser Word
byte = fromIntegral <$> limitedInt 255 "Value to large"

parseIntegralSafe :: Integral a => CharParser a
parseIntegralSafe = fromIntegerSafe . read =<< some digitChar
  where
    fromIntegerSafe :: Integral a => Integer -> CharParser a
    fromIntegerSafe x =
        let res = fromInteger x
        in  if fromIntegral res == x
            then return res
            else fail ("Number is too large: " ++ show x)
