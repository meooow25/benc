{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Tasty.Bench

import Data.Bits
import Data.Int
import Data.Semigroup
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Data.Bencode.Decode as D
import qualified Data.Bencode.Encode as E

main :: IO ()
main =  defaultMain
  [ bgroup "Decode"
    [ envPure sData   $ bench "string" . whnf decString
    , envPure iData   $ bench "integer" . whnf decInteger
    , envPure lData   $ bench "list" . whnf decList
    , envPure dData   $ bench "dict" . whnf decDict
    , envPure sData   $ bench "text" . whnf decText
    , envPure lsData  $ bench "list string" . whnf decListString
    , envPure liData  $ bench "list int" . whnf decListInt
    , envPure llData  $ bench "list list" . whnf decListList
    , envPure ldData  $ bench "list dict" . whnf decListDict
    , envPure ldData2 $ bench "list fields" . whnf decListFields
    , envPure liData  $ bench "list word16" . whnf decListWord16
    ]
  , bgroup "Encode"
    [ bench "string"      $ whnf encManyString n
    , bench "integer"     $ whnf encManyInteger n
    , bench "list"        $ whnf encManyList n100
    , bench "list fusion" $ whnf encManyListFusion n
    , bench "dict"        $ whnf encManyDict n100
    , bench "text"        $ whnf encManyText n
    , bench "int"         $ whnf encManyInt n
    , bench "field"       $ whnf encManyField n10
    , bench "word16"      $ whnf encManyWord16 n
    ]
  ]
  where
    -- How was the test data and size selected?
    -- Pretty much arbitrarily. Sizes are chosen such that a benchmark
    -- takes <= ~200ms.
    n, n10, n100 :: Int
    !n = 1000000
    !n10 = n `div` 10
    !n100 = n `div` 100

    envPure = env . pure
    toBS = BL.toStrict . BB.toLazyByteString

    sData = BC.pack (show n) <> ":" <> stimes n "x"
    iData = "i1" <> stimes n "1" <> "e"
    lData = "l" <> stimes n "le" <> "e"
    dData = toBS $ "d" <> go n10 <> "e"
      where
        go i | i == 2*n10 = mempty
        go i = "6:" <> BB.intDec i <> "de" <> go (i+1)
    lsData = toBS $ "l" <> stimes n10 s <> "e"
      where
        s = "5:hello5:world13:one two three"
    liData = toBS $ "l" <> go n <> "e"
      where
        go 0 = mempty
        go i = "i" <> BB.intDec (i .&. 0xffff) <> "e" <> go (i-1)
    llData = toBS $ "l" <> stimes n "le" <> "e"
    ldData = toBS $ "l" <> stimes n "de" <> "e"
    ldData2 = toBS $ "l" <> stimes n10 d <> "e"
      where
        d = "d1:0de1:1de1:2de1:3de1:4de1:5de1:6de1:7de1:8de1:9dee"


-- All bench functions below are marked NOINLINE to make it easy to find
-- them by name in the GHC core output.

------------------------------
-- Decode
------------------------------

decString :: B.ByteString -> B.ByteString
decString = runP D.string
{-# NOINLINE decString #-}

decInteger :: B.ByteString -> Integer
decInteger = runP D.integer
{-# NOINLINE decInteger #-}

decList :: B.ByteString -> V.Vector (V.Vector ())
decList = runP (D.list (D.list (pure ())))
{-# NOINLINE decList #-}

decDict :: B.ByteString -> M.Map B.ByteString (M.Map B.ByteString ())
decDict = runP (D.dict (D.dict (pure ())))
{-# NOINLINE decDict #-}

decText :: B.ByteString -> T.Text
decText = runP D.text
{-# NOINLINE decText #-}

decListString :: B.ByteString -> V.Vector B.ByteString
decListString = runP (D.list D.string)
{-# NOINLINE decListString #-}

decListInt :: B.ByteString -> V.Vector Int
decListInt = runP (D.list D.int)
{-# NOINLINE decListInt #-}

decListList :: B.ByteString -> V.Vector (V.Vector ())
decListList = runP (D.list (D.list (pure ())))
{-# NOINLINE decListList #-}

decListDict :: B.ByteString -> V.Vector (M.Map B.ByteString ())
decListDict = runP (D.list (D.dict (pure ())))
{-# NOINLINE decListDict #-}

decListFields :: B.ByteString -> V.Vector ()
decListFields = runP (D.list foo)
  where
    foo = do
      D.field "0" (pure ())
      D.field "1" (pure ())
      D.field "2" (pure ())
      D.field "3" (pure ())
      D.field "4" (pure ())
      D.field "5" (pure ())
      D.field "6" (pure ())
      D.field "7" (pure ())
      D.field "8" (pure ())
      D.field "9" (pure ())
{-# NOINLINE decListFields #-}

decListWord16 :: B.ByteString -> V.Vector Word16
decListWord16 = runP (D.list D.word16)
{-# NOINLINE decListWord16 #-}

runP :: D.Parser a -> B.ByteString -> a
runP p = either error id . D.decode p
{-# INLINE runP #-}

------------------------------
-- Encode
------------------------------

encManyString :: Int -> Int64
encManyString = encMany E.string ("hello","world","one two three")
{-# NOINLINE encManyString #-}

encManyInteger :: Int -> Int64
encManyInteger = encMany E.integer (0,-100000,fromIntegral (maxBound :: Int))
{-# NOINLINE encManyInteger #-}

encManyList :: Int -> Int64
encManyList =
  encMany (E.list (E.list E.value))
          (V.empty, V.replicate 10 V.empty, V.replicate 90 V.empty)
{-# NOINLINE encManyList #-}

encManyListFusion :: Int -> Int64
encManyListFusion = getL . E.toBuilder . E.list E.int . flip V.generate id
{-# NOINLINE encManyListFusion #-}

encManyDict :: Int -> Int64
encManyDict =
  encMany (E.dict (E.dict E.value))
          (M.empty, M.singleton "a" M.empty, m99)
{-# NOINLINE encManyDict #-}

m99 :: M.Map B.ByteString (M.Map B.ByteString a)
m99 = M.fromList [(BC.pack (show i), M.empty) | i <- [1..99 :: Int]]

encManyText :: Int -> Int64
encManyText = encMany E.text ("hello","world","one two three")
{-# NOINLINE encManyText #-}

encManyInt :: Int -> Int64
encManyInt = encMany E.int (0,-100000,maxBound)
{-# NOINLINE encManyInt #-}

data ABC = A | B | C

encManyField :: Int -> Int64
encManyField = encMany foo (A,B,C)
  where
    foo A = e
    foo B = E.dict' $ E.field "foo" id e
    foo C = E.dict' $
         E.field "0" id e
      <> E.field "1" id e
      <> E.field "2" id e
      <> E.field "3" id e
      <> E.field "4" id e
      <> E.field "5" id e
      <> E.field "6" id e
      <> E.field "7" id e
      <> E.field "8" id e
      <> E.field "9" id e
    e = E.dict id M.empty
{-# NOINLINE encManyField #-}

encManyWord16 :: Int -> Int64
encManyWord16 = encMany E.word16 (0,1000,maxBound)
{-# NOINLINE encManyWord16 #-}

encMany :: (a -> E.Encoding) -> (a,a,a) -> Int -> Int64
encMany enc (x0',x1',x2') = getL . go x0' x1' x2'
  where
    go _  _  _  0 = mempty
    go x0 x1 x2 i = E.toBuilder (enc x0) <> go x1 x2 x0 (i-1)
{-# INLINE encMany #-}

getL :: BB.Builder -> Int64
getL = BL.length . BB.toLazyByteString
{-# INLINE getL #-}
