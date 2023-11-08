module Data.Bencode.Util
  ( readKnownNaturalAsInt
  , readKnownNaturalAsWord
  ) where

import Data.Bits
import qualified Data.ByteString as B

-- | The input string must be an unsigned decimal integer with no extraneous
-- leading zeros. Returns Nothing if the value is outside the bounds of an
-- @Int@.
readKnownNaturalAsInt :: Bool -> B.ByteString -> Maybe Int
readKnownNaturalAsInt neg s = case B.uncons sr of
  Nothing -> Just $! if neg then -n else n
  Just (d,sr')
    | B.null sr'
    , let d' = fromIntegral d - 48
    , n < iMaxDiv10 || n == iMaxDiv10 && d' <= (7 + fromEnum neg)
                       -- last digit of maxBound = 7, minBound = 8
    -> Just $! if neg then -n * 10 - d' else n * 10 + d'
    | otherwise -> Nothing
  where
    (sl,sr) = B.splitAt (maxIntLen - 1) s
    n = B.foldl' (\acc d -> acc * 10 + fromIntegral d - 48) 0 sl

iMaxDiv10 :: Int
iMaxDiv10 = maxBound `div` 10

maxIntLen :: Int
maxIntLen = case finiteBitSize (0 :: Int) of
  32 -> 10
  64 -> 19
  _  -> error "unsupported word size"

-- | The input string must be an unsigned decimal integer with no extraneous
-- leading zeros. Returns Nothing if the value is outside the bounds of a
-- @Word@.
readKnownNaturalAsWord :: B.ByteString -> Maybe Word
readKnownNaturalAsWord s = case B.uncons sr of
  Nothing -> Just $! n
  Just (d,sr')
    | B.null sr'
    , let d' = fromIntegral d - 48
    , n < wMaxDiv10 || n == wMaxDiv10 && d' <= 5 -- last digit of maxBound is 5
    -> Just $! n * 10 + d'
    | otherwise -> Nothing
  where
    (sl,sr) = B.splitAt (maxWordLen - 1) s
    n = B.foldl' (\acc d -> acc * 10 + fromIntegral d - 48) 0 sl

wMaxDiv10 :: Word
wMaxDiv10 = maxBound `div` 10

maxWordLen :: Int
maxWordLen = case finiteBitSize (0 :: Word) of
  32 -> 10
  64 -> 20
  _  -> error "unsupported word size"
