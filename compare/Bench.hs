{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PackageImports #-}
module Main (main) where

import Test.Tasty.Bench
import Test.Tasty.HUnit

import Control.Applicative
import Control.DeepSeq
import Control.Exception
import Data.Maybe
import System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import GHC.Generics

import qualified Data.AttoBencode as Atto
import qualified "bencode" Data.BEncode as Ben
import qualified Data.BEncode.Reader as Ben
import qualified "bencoding" Data.BEncode as Ing
import qualified Data.Bencode.Decode as D
import qualified Data.Bencode.Encode as E

main :: IO ()
main = do
  defaultMain $
    [ env (withFile testFile ReadMode B.hGetContents) $ \testData ->
      bgroup testName
        [ bgroup "Decode"
          [ bench    "benc"              $ nf decodeBenc testData
          , bench    "bencode"           $ nf decodeBencode testData
          , bench    "AttoBencode"       $ nf decodeAttoBencode testData
          , bench    "bencoding"         $ nf decodeBencoding testData
          , testCase "bencode match"     $ decodeBencode testData @?= decodeBenc testData
          , testCase "AttoBencode match" $ decodeAttoBencode testData @?= decodeBenc testData
          , testCase "bencoding match"   $ decodeBencoding testData @?= decodeBenc testData
          ]
        , env (evaluate (force (decodeBenc testData))) $ \decoded ->
          bgroup "Encode"
            [ bench    "benc"              $ nf encodeBenc decoded
            , bench    "bencode"           $ nf encodeBencode decoded
            , bench    "AttoBencode"       $ nf encodeAttoBencode decoded
            , bench    "bencoding"         $ nf encodeBencoding decoded
            , testCase "bencode match"     $ encodeBencode decoded @?= encodeBenc decoded
            , testCase "AttoBencode match" $ BL.fromStrict (encodeAttoBencode decoded) @?= encodeBenc decoded
            , testCase "bencoding match"   $ encodeBencoding decoded @?= encodeBenc decoded
            ]
        ]
    | (testName, testFile) <- testFiles ]

testFiles :: [(String, FilePath)]
testFiles =
  [ ("crossref", "data/April2023PublicDataFilefromCrossref.torrent")
  , ("ubuntu",   "data/ubuntu-22.04-desktop-amd64.iso.torrent")
  ]

-- | This represents a metainfo (.torrent) file.
data MetaInfo = MetaInfo
  { _announce     :: !T.Text
  , _announceList :: !(Maybe (V.Vector (V.Vector T.Text)))
  , _comment      :: !(Maybe T.Text)
  , _createdBy    :: !(Maybe T.Text)
  , _creationDate :: !(Maybe Int)
  , _info         :: !Info
  } deriving (Eq, Show, Generic, NFData)

data Info = Info
  { _singleOrMultiple :: !SingleOrMultiple
  , _name             :: !T.Text
  , _pieceLength      :: !Int
  , _pieces           :: !B.ByteString
  } deriving (Eq, Show, Generic, NFData)

data SingleOrMultiple
  = Single { _length :: !Int }
  | Multiple { _files :: !(V.Vector OneFileInfo) }
  deriving (Eq, Show, Generic, NFData)

data OneFileInfo = OneFileInfo
  { __length :: !Int
  , _path    :: !(V.Vector T.Text)
  } deriving (Eq, Show, Generic, NFData)

------------------------------
-- benc
------------------------------

decodeBenc :: B.ByteString -> MetaInfo
decodeBenc = either error id . D.decode metaInfoP

metaInfoP :: D.Parser MetaInfo
metaInfoP =
  MetaInfo
    <$> D.field "announce" D.text
    <*> optional (D.field "announce-list" (D.list (D.list D.text)))
    <*> optional (D.field "comment" D.text)
    <*> optional (D.field "created by" D.text)
    <*> optional (D.field "creation date" D.int)
    <*> D.field "info" infoP

infoP :: D.Parser Info
infoP =
  Info
    <$> (   Single   <$> D.field "length" D.int
        <|> Multiple <$> D.field "files" (D.list oneFileP))
    <*> D.field "name" D.text
    <*> D.field "piece length" D.int
    <*> D.field "pieces" D.string

oneFileP :: D.Parser OneFileInfo
oneFileP =
  OneFileInfo
    <$> D.field "length" D.int
    <*> D.field "path" (D.list D.text)

encodeBenc :: MetaInfo -> BL.ByteString
encodeBenc = BB.toLazyByteString . E.toBuilder . metaInfoE

metaInfoE :: MetaInfo -> E.Encoding
metaInfoE m = E.dict' $
     E.field "announce" E.text (_announce m)
  <> foldMap (E.field "announce-list" (E.list (E.list E.text)))
             (_announceList m)
  <> foldMap (E.field "comment" E.text) (_comment m)
  <> foldMap (E.field "created by" E.text) (_createdBy m)
  <> foldMap (E.field "creation date" E.int) (_creationDate m)
  <> E.field "info" infoE (_info m)

infoE :: Info -> E.Encoding
infoE i = E.dict' $
     (case _singleOrMultiple i of
        Single l    -> E.field "length" E.int l
        Multiple fs -> E.field "files" (E.list oneFileE) fs)
  <> E.field "name" E.text (_name i)
  <> E.field "piece length" E.int (_pieceLength i)
  <> E.field "pieces" E.string (_pieces i)

oneFileE :: OneFileInfo -> E.Encoding
oneFileE o = E.dict' $
     E.field "length" E.int (__length o)
  <> E.field "path" (E.list E.text) (_path o)

------------------------------
-- bencode
------------------------------

decodeBencode :: B.ByteString -> MetaInfo
decodeBencode =
  either error id .
  Ben.runBReader metaInfoR .
  maybe (error "fail") id .
  Ben.bRead .
  BL.fromStrict

metaInfoR :: Ben.BReader MetaInfo
metaInfoR =
  MetaInfo
    <$> Ben.dict "announce" btext
    <*> optional (
          Ben.dict "announce-list" (
            V.fromList <$> Ben.list (V.fromList <$> Ben.list btext)))
    <*> optional (Ben.dict "comment" btext)
    <*> optional (Ben.dict "created by" btext)
    <*> optional (Ben.dict "creation date" bint)
    <*> Ben.dict "info" infoR

infoR :: Ben.BReader Info
infoR =
  Info
    <$> (   Single <$> Ben.dict "length" bint
        <|> Multiple <$> Ben.dict "files" (V.fromList <$> Ben.list oneFileR))
    <*> Ben.dict "name" btext
    <*> Ben.dict "piece length" bint
    <*> Ben.dict "pieces" (BL.toStrict <$> Ben.bbytestring)

oneFileR :: Ben.BReader OneFileInfo
oneFileR =
  OneFileInfo
    <$> Ben.dict "length" bint
    <*> Ben.dict "path" (V.fromList <$> Ben.list btext)

bint :: Ben.BReader Int
bint = fromIntegral <$> Ben.bint

btext :: Ben.BReader T.Text
btext = T.decodeUtf8 . BL.toStrict <$> Ben.bbytestring

encodeBencode :: MetaInfo -> BL.ByteString
encodeBencode = Ben.bPack . metaInfoW

metaInfoW :: MetaInfo -> Ben.BEncode
metaInfoW m = Ben.BDict $ M.fromList $
     [ ("announce", Ben.BString $ unbtext (_announce m)) ]
  ++ optW "announce-list"
       (Ben.BList . map (Ben.BList . map (Ben.BString . unbtext) . V.toList) . V.toList
          <$> _announceList m)
  ++ optW "comment"       (Ben.BString . unbtext <$> _comment m)
  ++ optW "created by"    (Ben.BString . unbtext <$> _createdBy m)
  ++ optW "creation date" (Ben.BInt . fromIntegral <$> _creationDate m)
  ++ [ ("info", infoW (_info m)) ]

optW :: String -> Maybe Ben.BEncode -> [(String, Ben.BEncode)]
optW k = maybe [] (\x -> [(k,x)])

infoW :: Info -> Ben.BEncode
infoW i = Ben.BDict $ M.fromList $
     (case _singleOrMultiple i of
        Single l    -> [("length", Ben.BInt $ fromIntegral l)]
        Multiple fs -> [("files", Ben.BList $ map oneFileW (V.toList fs))])
  ++ [ ("name"        , Ben.BString $ unbtext (_name i))
     , ("piece length", Ben.BInt $ fromIntegral (_pieceLength i))
     , ("pieces"      , Ben.BString $ BL.fromStrict (_pieces i))
     ]

oneFileW :: OneFileInfo -> Ben.BEncode
oneFileW o = Ben.BDict $ M.fromList
  [ ("length", Ben.BInt $ fromIntegral (__length o))
  , ("path"  , Ben.BList $ map (Ben.BString . unbtext) $ V.toList (_path o))
  ]

unbtext :: T.Text -> BL.ByteString
unbtext = BL.fromStrict . T.encodeUtf8

------------------------------
-- AttoBencode
------------------------------

decodeAttoBencode :: B.ByteString -> MetaInfo
decodeAttoBencode = fromJust . Atto.decode

instance Atto.FromBencode MetaInfo where
  fromBencode (Atto.BDict d) =
    MetaInfo <$> (T.decodeUtf8 <$> d Atto..: "announce")
             <*> Just (
                   V.fromList . map (V.fromList . map T.decodeUtf8)
                     <$> d Atto..: "announce-list")
             <*> Just (T.decodeUtf8 <$> d Atto..: "comment")
             <*> Just (T.decodeUtf8 <$> d Atto..: "created by")
             <*> Just (d Atto..: "creation date")
             <*> d Atto..: "info"
  fromBencode _ = Nothing

instance Atto.FromBencode Info where
  fromBencode (Atto.BDict d) =
    Info <$> (   Single <$> d Atto..: "length"
             <|> Multiple . V.fromList <$> d Atto..: "files")
         <*> (T.decodeUtf8 <$> d Atto..: "name")
         <*> d Atto..: "piece length"
         <*> d Atto..: "pieces"
  fromBencode _ = Nothing

instance Atto.FromBencode OneFileInfo where
  fromBencode (Atto.BDict d) =
    OneFileInfo
      <$> d Atto..: "length"
      <*> (V.fromList . map T.decodeUtf8 <$> d Atto..: "path")
  fromBencode _ = Nothing

encodeAttoBencode :: MetaInfo -> B.ByteString
encodeAttoBencode = Atto.encode

instance Atto.ToBencode MetaInfo where
  toBencode m = Atto.dict $
       [ "announce" Atto..= T.encodeUtf8 (_announce m) ]
    ++ opt "announce-list"
           (map (map T.encodeUtf8 . V.toList) . V.toList <$> _announceList m)
    ++ opt "comment"       (T.encodeUtf8 <$> _comment m)
    ++ opt "created by"    (T.encodeUtf8 <$> _createdBy m)
    ++ opt "creation date" (_creationDate m)
    ++ [ "info" Atto..= _info m ]

-- Why is this not already in the library
opt :: Atto.ToBencode a => B.ByteString -> Maybe a -> [(B.ByteString, Atto.BValue)]
opt k = maybe [] (\x -> [k Atto..= x])

instance Atto.ToBencode Info where
  toBencode i = Atto.dict $
       (case _singleOrMultiple i of
          Single l    -> ["length" Atto..= l]
          Multiple fs -> ["files"  Atto..= V.toList fs])
    ++ [ "name"         Atto..= T.encodeUtf8 (_name i)
       , "piece length" Atto..= _pieceLength i
       , "pieces"       Atto..= _pieces i
       ]

instance Atto.ToBencode OneFileInfo where
  toBencode o = Atto.dict
    [ "length" Atto..= __length o
    , "path"   Atto..= map T.encodeUtf8 (V.toList (_path o))
    ]

------------------------------
-- bencoding
------------------------------

decodeBencoding :: B.ByteString -> MetaInfo
decodeBencoding = either error id . Ing.decode

encodeBencoding :: MetaInfo -> BL.ByteString
encodeBencoding = Ing.encode

-- You may have noticed that the keys in the records are sorted.
-- This is not because I find it neat, but because bencoding forces you to
-- hand-sort the order of keys when encoding and decoding! Yikes!

instance Ing.BEncode MetaInfo where
  toBEncode m = Ing.toDict $
           "announce"      Ing..=! _announce m
    Ing..: "announce-list" Ing..=? (map V.toList . V.toList <$> _announceList m)
    Ing..: "comment"       Ing..=? _comment m
    Ing..: "created by"    Ing..=? _createdBy m
    Ing..: "creation date" Ing..=? _creationDate m
    Ing..: "info"          Ing..=! _info m
    Ing..: Ing.endDict
  fromBEncode = Ing.fromDict $
    MetaInfo Ing.<$>! "announce"
             <*> optional
                   (V.fromList . map V.fromList
                      <$> Ing.field (Ing.req "announce-list"))
             Ing.<*>? "comment"
             Ing.<*>? "created by"
             Ing.<*>? "creation date"
             Ing.<*>! "info"

instance Ing.BEncode Info where
  toBEncode i = Ing.toDict $
          (case _singleOrMultiple i of
            Single l    -> "length" Ing..=! l
            Multiple fs -> "files" Ing..=! V.toList fs)
    Ing..: "name"         Ing..=! _name i
    Ing..: "piece length" Ing..=! _pieceLength i
    Ing..: "pieces"       Ing..=! _pieces i
    Ing..: Ing.endDict
  fromBEncode = Ing.fromDict $
    Info <$> (   Single Ing.<$>! "length"
             <|> Multiple . V.fromList Ing.<$>! "files")
         Ing.<*>! "name"
         Ing.<*>! "piece length"
         Ing.<*>! "pieces"

instance Ing.BEncode OneFileInfo where
  toBEncode o = Ing.toDict $
           "length" Ing..=! __length o
    Ing..: "path"   Ing..=! V.toList (_path o)
    Ing..: Ing.endDict
  fromBEncode = Ing.fromDict $
    OneFileInfo Ing.<$>! "length"
                <*> (V.fromList <$> Ing.field (Ing.req "path"))

------------------------------
-- Results
------------------------------
{-
$ cabal run benc-compare -- +RTS -T
All
  crossref
    Decode
      benc:              OK
        21.3 ms ± 902 μs,  24 MB allocated,  27 MB copied,  30 MB peak memory
      bencode:           OK
        218  ms ± 7.7 ms, 737 MB allocated,  66 MB copied,  47 MB peak memory
      AttoBencode:       OK
        44.6 ms ± 4.0 ms, 129 MB allocated,  41 MB copied,  47 MB peak memory
      bencoding:         OK
        39.1 ms ± 2.3 ms, 104 MB allocated,  40 MB copied,  47 MB peak memory
      bencode match:     OK
      AttoBencode match: OK
      bencoding match:   OK
    Encode
      benc:              OK
        9.17 ms ± 487 μs,  42 MB allocated, 590 KB copied,  58 MB peak memory
      bencode:           OK
        37.8 ms ± 860 μs, 113 MB allocated,  40 MB copied,  91 MB peak memory
      AttoBencode:       OK
        19.7 ms ± 1.8 ms, 109 MB allocated, 1.6 MB copied,  91 MB peak memory
      bencoding:         OK
        11.9 ms ± 916 μs,  67 MB allocated, 484 KB copied,  91 MB peak memory
      bencode match:     OK
      AttoBencode match: OK
      bencoding match:   OK
  ubuntu
    Decode
      benc:              OK
        1.30 μs ±  90 ns, 3.6 KB allocated,   1 B  copied,  91 MB peak memory
      bencode:           OK
        29.0 μs ± 2.6 μs, 121 KB allocated,  79 B  copied,  91 MB peak memory
      AttoBencode:       OK
        3.01 μs ± 102 ns,  17 KB allocated,   4 B  copied,  91 MB peak memory
      bencoding:         OK
        2.44 μs ± 175 ns,  15 KB allocated,   4 B  copied,  91 MB peak memory
      bencode match:     OK
      AttoBencode match: OK
      bencoding match:   OK
    Encode
      benc:              OK
        1.58 μs ± 101 ns,  11 KB allocated,   1 B  copied,  91 MB peak memory
      bencode:           OK
        3.17 μs ± 174 ns,  19 KB allocated,  13 B  copied,  91 MB peak memory
      AttoBencode:       OK
        10.1 μs ± 993 ns, 295 KB allocated,  27 B  copied,  91 MB peak memory
      bencoding:         OK
        1.81 μs ± 129 ns,  15 KB allocated,   2 B  copied,  91 MB peak memory
      bencode match:     OK
      AttoBencode match: OK
      bencoding match:   OK
-}
