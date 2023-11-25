{-# LANGUAGE BangPatterns #-}
module Data.Bencode.Util
  ( arrayFromRevListN
  , readKnownNaturalAsInt
  , readKnownNaturalAsInt64
  , readKnownNaturalAsWord
  , readKnownNaturalAsWord64
  ) where

import Data.Bits
import Data.Int
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Primitive.Array as A

-- | Create an array from a list in reverse order. The list length must be n.
arrayFromRevListN :: Int -> [a] -> A.Array a
arrayFromRevListN n xs = A.createArray n errorElement $ \a ->
  let f x k = \i ->
        if i == -1
        then pure ()
        else A.writeArray a i x *> k (i-1)
  in foldr f (\ !_ -> pure ()) xs (n-1)
{-# INLINE arrayFromRevListN #-}

errorElement :: a
errorElement = error "errorElement"

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
{-# INLINE readKnownNaturalAsInt #-}

-- | Similar to 'readKnownNaturalAsInt', for 'Int64'.
readKnownNaturalAsInt64 :: Bool -> B.ByteString -> Maybe Int64
readKnownNaturalAsInt64 = readInt 19
{-# INLINE readKnownNaturalAsInt64 #-}

readInt :: (Bounded a, Integral a) => Int -> Bool -> B.ByteString -> Maybe a
readInt maxLen neg s = if neg then fmap negate n else n
  where
    -- last digit of maxBound = 7, minBound = 8
    n = readWord maxLen (maxBound `div` 10) (7 + fromIntegral (fromEnum neg)) s
{-# INLINE readInt #-}

-- | The input string must be an unsigned decimal integer with no extraneous
-- leading zeros. Returns Nothing if the value is outside the bounds of a
-- @Word@.
readKnownNaturalAsWord :: B.ByteString -> Maybe Word
readKnownNaturalAsWord = readWord maxWordLen (maxBound `div` 10) 5
  where
    -- last digit of maxBound = 5
    maxWordLen = case finiteBitSize (0 :: Word) of
      32 -> 10
      64 -> 20
      _  -> error "unsupported word size"
{-# INLINE readKnownNaturalAsWord #-}

-- | Similar to 'readKnownNaturalAsWord', for 'Word64'.
readKnownNaturalAsWord64 :: B.ByteString -> Maybe Word64
readKnownNaturalAsWord64 = readWord 20 (maxBound `div` 10) 5
{-# INLINE readKnownNaturalAsWord64 #-}

-- maxLen must be > 0!
readWord :: (Bounded a, Integral a) => Int -> a -> a -> B.ByteString -> Maybe a
readWord maxLen maxValueDiv10 maxValueMod10 s =
  case compare (B.length s) maxLen of
    LT -> Just $! readFull' s
    EQ ->
      let n = readFull (B.unsafeInit s)
          d = digitToI (B.unsafeLast s)
      in if n < maxValueDiv10 || n == maxValueDiv10 && d <= maxValueMod10
         then Just $! n*10 + d
         else Nothing
    GT -> Nothing
{-# INLINE readWord #-}

readFull :: Integral a => B.ByteString -> a
readFull = B.foldl' (\acc c -> acc * 10 + digitToI c) 0
{-# INLINE readFull #-}

-- Same as readFull but avoids
-- https://gitlab.haskell.org/ghc/ghc/-/issues/24203
readFull' :: Integral a => B.ByteString -> a
readFull' s = case B.unsnoc s of
  Nothing     -> 0
  Just (s',c) -> readFull s' * 10 + digitToI c
{-# INLINE readFull' #-}

digitToI :: Integral a => Word8 -> a
digitToI c = fromIntegral c - 48
{-# INLINE digitToI #-}
