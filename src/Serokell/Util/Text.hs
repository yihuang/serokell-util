-- | Utility functions to work with `text` and `text-format`. Feel free to add more if you need.
-- Some functions have two versions, `'` suffix means that function operates on strict Text.

module Serokell.Util.Text
       ( show
       , show'
       , FPFormat (..)
       , showFloat
       , showFloat'
       , showDecimal
       , showDecimal'
       , pairBuilder
       , listBuilder
       , listBuilderJSON
       , listBuilderJSONIndent
       , listBuilderCSV
       , mapBuilder
       , format
       , format'
       , formatSingle
       , formatSingle'
       , buildSingle
       , readFractional
       , readDouble
       , readDecimal
       , readUnsignedDecimal
       ) where

import qualified Data.Text                        as T
import           Data.Text.Buildable              (Buildable (build))
import qualified Data.Text.Format                 as F
import           Data.Text.Format.Params          (Params)
import qualified Data.Text.Lazy                   as LT
import qualified Data.Text.Lazy.Builder           as B
import qualified Data.Text.Lazy.Builder.Int       as B
import           Data.Text.Lazy.Builder.RealFloat (FPFormat (Exponent, Fixed, Generic))
import qualified Data.Text.Lazy.Builder.RealFloat as B
import qualified Data.Text.Read                   as T
import           Prelude                          hiding (show, showList)

show :: Buildable a
     => a -> LT.Text
show = B.toLazyText . build

show' :: Buildable a
      => a -> T.Text
show' = LT.toStrict . show

showFloat
  :: (RealFloat a)
  => FPFormat -> Maybe Int -> a -> LT.Text
showFloat f precision v = B.toLazyText $ B.formatRealFloat f precision v

showFloat'
  :: (RealFloat a)
  => FPFormat -> Maybe Int -> a -> T.Text
showFloat' f prec = LT.toStrict . showFloat f prec

showDecimal :: (Integral a)
             => a -> LT.Text
showDecimal = B.toLazyText . B.decimal

showDecimal' :: (Integral a)
              => a -> T.Text
showDecimal' = LT.toStrict . showDecimal

-- | Prints pair (a, b) like "(a, b)"
pairBuilder :: (Buildable a, Buildable b)
            => (a, b) -> B.Builder
pairBuilder = F.build "({}, {})"

-- | Generic list builder. Prints prefix, then values separated by delimiter and finally suffix
listBuilder
  :: (Buildable prefix, Buildable delimiter, Buildable suffix, Foldable t, Buildable a)
  => prefix -> delimiter -> suffix -> t a -> B.Builder
listBuilder prefix delimiter suffix as =
  mconcat [build prefix, mconcat builders, build suffix]
  where builders = foldr appendBuilder [] as
        appendBuilder a [] = [build a]
        appendBuilder a bs = build a : build delimiter : bs

-- | This function helps to deduce type arising from string literal
_listBuilder
  :: (Foldable t, Buildable a)
  => B.Builder -> B.Builder -> B.Builder -> t a -> B.Builder
_listBuilder = listBuilder

-- | Prints values in JSON-style (e. g. `[111, ololo, blablabla]`)
listBuilderJSON
  :: (Foldable t, Buildable a)
  => t a -> B.Builder
listBuilderJSON = _listBuilder "[" ", " "]"

-- | Like listBuilderJSON, but prints each value on a new line with indentation
listBuilderJSONIndent
  :: (Foldable t, Buildable a)
  => Word -> t a -> B.Builder
listBuilderJSONIndent _ as | null as = "[]"
listBuilderJSONIndent indent as
  | otherwise =
    listBuilder ("[\n" `LT.append` spaces)
                delimiter
                ("\n]" :: B.Builder)
                as
  where spaces =
          LT.replicate (fromIntegral indent)
                       " "
        delimiter = ",\n" `LT.append` spaces

-- | Prints comma separated values
listBuilderCSV
  :: (Foldable t, Buildable a)
  => t a -> B.Builder
listBuilderCSV = _listBuilder "" "," ""

-- | There is no appropriate type class for map, but all reasonable maps
-- provide something like `assocs` function.
-- Map may be printed prettier (e. g. using JSON style), it's future task.
-- Haing at least one such function is still good anyway.
mapBuilder
    :: (Traversable t, Buildable k, Buildable v)
    => t (k, v) -> B.Builder
mapBuilder = listBuilderJSON . fmap pairBuilder

-- | Re-export Data.Text.Format.format for convenience
format :: Params ps
       => F.Format -> ps -> LT.Text
format = F.format

-- | Version of Data.Text.Format.format which returns strict Text
format' :: Params ps
        => F.Format -> ps -> T.Text
format' f = LT.toStrict . F.format f

formatSingle :: Buildable a
             => F.Format -> a -> LT.Text
formatSingle f = format f . F.Only

formatSingle' :: Buildable a
              => F.Format -> a -> T.Text
formatSingle' f = LT.toStrict . formatSingle f

buildSingle :: Buildable a
            => F.Format -> a -> B.Builder
buildSingle f = F.build f . F.Only

-- | Read fractional number. Returns error (i. e. Left) if there is something else
readFractional :: Fractional a => T.Text -> Either String a
readFractional = _wrapReader T.rational

-- | Like readFractional, but much more efficient. It may be slightly less accurate
readDouble :: T.Text -> Either String Double
readDouble = _wrapReader T.double

-- | Read signed decimal number. Returns error (i. e. Left) if there is something else
-- WARNING: if input is negative and `a` is unsigned, overflow will occur
readDecimal :: Integral a => T.Text -> Either String a
readDecimal = _wrapReader $ T.signed T.decimal

-- | Read unsigned decimal number. Returns error (i. e. Left) if there is something else
readUnsignedDecimal :: Integral a => T.Text -> Either String a
readUnsignedDecimal = _wrapReader T.decimal

_wrapReader :: T.Reader a -> T.Text -> Either String a
_wrapReader reader t =
  case reader t of
    Left err -> Left $ mconcat [ "failed to parse '"
                               , T.unpack t
                               , "': "
                               , err
                               ]
    Right (res, "") -> Right res
    Right (_, remainder) ->
      Left $
      mconcat [ "failed to parse '"
              , T.unpack t
              , "', because there is a remainder: "
              , T.unpack remainder
              ]
