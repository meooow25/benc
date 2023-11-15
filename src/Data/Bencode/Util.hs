module Data.Bencode.Util
  ( readKnownNaturalAsInt
  , readKnownNaturalAsInt64
  , readKnownNaturalAsWord
  , readKnownNaturalAsWord64
  ) where

import Data.Bits
import Data.Int
import Data.Word
import qualified Data.ByteString as B

-- | The input string must be an unsigned decimal integer with no extraneous
-- leading zeros. Returns Nothing if the value is outside the bounds of an
-- @Int@.
readKnownNaturalAsInt :: Bool -> B.ByteString -> Maybe Int
readKnownNaturalAsInt = readInt maxIntLen
  where
    maxIntLen = case finiteBitSize (0 :: Int) of
      32 -> 10
      64 -> 19
      _  -> error "unsupported word size"

-- | Similar to 'readKnownNaturalAsInt', for 'Int64'.
readKnownNaturalAsInt64 :: Bool -> B.ByteString -> Maybe Int64
readKnownNaturalAsInt64 = readInt 19

readInt :: (Bounded a, Integral a) => Int -> Bool -> B.ByteString -> Maybe a
readInt maxLen neg s = case B.uncons sr of
  Nothing -> Just $! if neg then -n else n
  Just (d,sr')
    | B.null sr'
    , let d' = fromIntegral d - 48
    , n < iMaxDiv10 || n == iMaxDiv10 && d' <= (7 + fromIntegral (fromEnum neg))
                       -- last digit of maxBound = 7, minBound = 8
    -> Just $! if neg then -n * 10 - d' else n * 10 + d'
    | otherwise -> Nothing
  where
    (sl,sr) = B.splitAt (maxLen - 1) s
    n = B.foldl' (\acc d -> acc * 10 + fromIntegral d - 48) 0 sl
    iMaxDiv10 = maxBound `div` 10
{-# INLINE readInt #-}

-- | The input string must be an unsigned decimal integer with no extraneous
-- leading zeros. Returns Nothing if the value is outside the bounds of a
-- @Word@.
readKnownNaturalAsWord :: B.ByteString -> Maybe Word
readKnownNaturalAsWord = readWord maxWordLen
  where
    maxWordLen = case finiteBitSize (0 :: Word) of
      32 -> 10
      64 -> 20
      _  -> error "unsupported word size"

-- | Similar to 'readKnownNaturalAsWord', for 'Word64'.
readKnownNaturalAsWord64 :: B.ByteString -> Maybe Word64
readKnownNaturalAsWord64 = readWord 20

readWord :: (Bounded a, Integral a) => Int -> B.ByteString -> Maybe a
readWord maxLen s = case B.uncons sr of
  Nothing -> Just $! n
  Just (d,sr')
    | B.null sr'
    , let d' = fromIntegral d - 48
    , n < wMaxDiv10 || n == wMaxDiv10 && d' <= 5 -- last digit of maxBound is 5
    -> Just $! n * 10 + d'
    | otherwise -> Nothing
  where
    (sl,sr) = B.splitAt (maxLen - 1) s
    n = B.foldl' (\acc d -> acc * 10 + fromIntegral d - 48) 0 sl
    wMaxDiv10 = maxBound `div` 10
{-# INLINE readWord #-}
