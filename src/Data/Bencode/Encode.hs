{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Conversions from Haskell values to Bencoded @ByteString@s.
--
module Data.Bencode.Encode
  (
    -- * Quick start
    -- $quick

    -- * Encoding
    Encoding
  , toBuilder

    -- * Primary encoders
  , string
  , integer
  , list
  , dict

    -- * More encoders
  , text
  , int
  , word
  , field
  , dict'
  , FieldEncodings
  , value
  , int64
  , int32
  , int16
  , int8
  , word64
  , word32
  , word16
  , word8

    -- * Recipes #recipes#
    -- $recipes
  ) where

import Data.Int
import Data.Monoid (Endo(..))
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import Data.Bencode.Type (Value(..))

-- | An encoded Bencode value.
newtype Encoding = Encoding { unEncoding :: BB.Builder }

-- | Get a ByteString 'BB.Builder' representation for an encoded Bencode value.
toBuilder :: Encoding -> BB.Builder
toBuilder = unEncoding

-- | Encode a bytestring as a Bencode string.
string :: B.ByteString -> Encoding
string s = Encoding $ BB.intDec (B.length s) <> BB.char7 ':' <> BB.byteString s

-- | Encode an integer as a Bencode integer.
integer :: Integer -> Encoding
integer = integer_ BB.integerDec

-- | Encode a @Vector@ as a Bencode list, using the given encoder for elements.
list :: (a -> Encoding) -> V.Vector a -> Encoding
list enc vs =
  Encoding $ BB.char7 'l' <> foldMap (unEncoding . enc) vs <> BB.char7 'e'

-- | Encode a @Map@ as a Bencode dictionary, using the given encoder for values.
dict :: (a -> Encoding) -> M.Map B.ByteString a -> Encoding
dict enc kvs = Encoding $ BB.char7 'd' <> f kvs <> BB.char7 'e'
  where
    f = M.foldMapWithKey (\k v -> unEncoding (string k) <> unEncoding (enc v))

-- | Encode @Text@ as a Bencode string. As per the Bencode specification, all
-- text must be encoded as UTF-8 strings.
text :: T.Text -> Encoding
text = string . T.encodeUtf8
-- TODO: Check if Text's encodeUtf8Builder is more efficient. But we would
-- also need to know the UTF-8 len, which is only viable for text >= 2.0.

-- | Encode an @Int@ as a Bencode integer.
int :: Int -> Encoding
int = integer_ BB.intDec

-- | Encode a @Word@ as a Bencode integer.
word :: Word -> Encoding
word = integer_ BB.wordDec

-- | A key-value encoding for a Bencode dictionary.
field :: B.ByteString -> (a -> Encoding) -> a -> FieldEncodings
field k enc v = FE (Endo ((k, enc v):))
{-# INLINE field #-}

-- | Encode Bencode key-value pairs as a Bencode dictionary.
--
-- __WARNING__: If there are duplicate keys in the @FieldEncodings@, an
-- arbitrary key-value pair among them will be encoded and the rest discarded.
dict' :: FieldEncodings -> Encoding
dict' = dict id . M.fromList . ($ []) . appEndo . unFE

-- | Key-value encodings for a Bencode dictionary.
newtype FieldEncodings = FE { unFE :: Endo [(B.ByteString, Encoding)] }
  deriving (Semigroup, Monoid)
-- FieldEncodings is not just a type alias because there are multiple ways to
-- do this, and in case the implementation changes it will not be a breaking
-- change.

-- | Encode a @Value@.
value :: Value -> Encoding
value v = case v of
  String s  -> string s
  Integer i -> integer i
  List vs   -> list value vs
  Dict vs   -> dict value vs

-- | Encode an @Int64@ as a Bencode integer.
int64 :: Int64 -> Encoding
int64 = integer_ BB.int64Dec

-- | Encode an @Int32@ as a Bencode integer.
int32 :: Int32 -> Encoding
int32 = integer_ BB.int32Dec

-- | Encode an @Int16@ as a Bencode integer.
int16 :: Int16 -> Encoding
int16 = integer_ BB.int16Dec

-- | Encode an @Int8@ as a Bencode integer.
int8 :: Int8 -> Encoding
int8 = integer_ BB.int8Dec

-- | Encode a @Word64@ as a Bencode integer.
word64 :: Word64 -> Encoding
word64 = integer_ BB.word64Dec

-- | Encode a @Word32@ as a Bencode integer.
word32 :: Word32 -> Encoding
word32 = integer_ BB.word32Dec

-- | Encode a @Word16@ as a Bencode integer.
word16 :: Word16 -> Encoding
word16 = integer_ BB.word16Dec

-- | Encode a @Word8@ as a Bencode integer.
word8 :: Word8 -> Encoding
word8 = integer_ BB.word8Dec

integer_ :: (a -> BB.Builder) -> a -> Encoding
integer_ f = \x -> Encoding $ BB.char7 'i' <> f x <> BB.char7 'e'
{-# INLINE integer_ #-}

------------------------------
-- Documentation
------------------------------

-- $quick
-- Encoding is done using encoders. An encoder is simply a function from a
-- Haskell type to 'Encoding'. There are encoders for the four Bencode types:
--
-- * 'string' encodes 'B.ByteString's as Bencode strings
-- * 'integer' encodes 'Prelude.Integer's as Bencode integers
-- * 'list' encodes 'V.Vector's as Bencode lists
-- * 'dict' encodes 'M.Map's with 'B.ByteString' keys as Bencode dictionaries
--
-- These can used to build more complex encoders for arbitrary types.
--
-- @
-- data File = File
--   { hash :: ByteString
--   , size :: Integer
--   , tags :: Vector Text
--   } deriving Show
-- @
--
-- It is reasonable to encode a @File@ as a Bencode dictionary with the field
-- names as keys, and appropriate types for the values.
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
-- import qualified Data.Bencode.Encode as E
--
-- encodeFile :: File -> E.'Encoding'
-- encodeFile (File hash size tags) = E.'dict'' $
--      E.'field' "hash" E.'string' hash
--   <> E.'field' "size" E.'integer' size
--   <> E.'field' "tags" (E.'list' E.'text') tags
-- @
--
-- Applying 'toBuilder' to an 'Encoding' gives a @ByteString@
-- 'Data.ByteString.Builder', which can then be converted to a lazy
-- @ByteString@, written to a file, or used otherwise.
--
-- @
-- import qualified Data.ByteString.Builder (toLazyByteString)
-- import qualified Data.Vector as V
-- @
--
-- >>> toLazyByteString $ encodeFile $ File "xxxx" 1024 (V.fromList ["work", "backup"])
-- "d4:hash4:xxxx4:sizei1024e4:tagsl4:work6:backupee"
--
-- In this module, encodings are total conversions from Haskell values to
-- @ByteString@s. If some data should fail to encode, it should be handled
-- separately.
--
-- For more examples, see the [Recipes](#g:recipes) section at the end of this
-- page.


-- $recipes
-- Recipes for some common and uncommon usages.
--
-- The following preface is assumed.
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
-- import Data.ByteString.Builder (toLazyByteString)
-- import Data.Text (Text)
-- import qualified Data.Bencode.Encode as E
--
-- toLBS = toLazyByteString . E.toBuilder
-- @
--
-- === Encode an optional field
--
-- @
-- data File = File { name :: Text, size :: Maybe Int }
--
-- encodeFile :: File -> E.'Encoding'
-- encodeFile (File name size) = E.'dict'' $
--      E.'field' "name" E.'text' name
--   <> 'foldMap' (E.'field' "size" E.'int') size
-- @
--
-- >>> toLBS $ encodeFile $ File "hello.txt" (Just 16)
-- "d4:name9:hello.txt4:sizei16ee"
-- >>> toLBS $ encodeFile $ File "hello.txt" Nothing
-- "d4:name9:hello.txte"
--
-- === Encode an enum
--
-- @
-- data Color = Red | Green | Blue
--
-- encodeColor :: Color -> E.'Encoding'
-- encodeColor = E.'text' . toText
--   where
--     toText Red   = "red"
--     toText Green = "green"
--     toText Blue  = "blue"
-- @
--
-- >>> toLBS $ encodeColor Green
-- "5:green"
--
-- === Encode fields differently based on the value
--
-- @
-- data Response = Response { id_ :: Int, result :: Either Text ByteString }
--
-- encodeResponse :: Response -> E.'Encoding'
-- encodeResponse (Response id_ result) = E.'dict'' $
--      E.'field' "id" E.'int' id_
--   <> either err ok result
--   where
--     err reason =
--          E.'field' "status" E.'text' "failure"
--       <> E.'field' "reason" E.'text' reason
--     ok data_ =
--          E.'field' "status" E.'text' "success"
--       <> E.'field' "data" E.'string' data_
-- @
--
-- >>> toLBS $ encodeResponse $ Response 42 (Left "unauthorized")
-- "d2:idi42e6:reason12:unauthorized6:status7:failuree"
-- >>> toLBS $ encodeResponse $ Response 42 (Right "0000")
-- "d4:data4:00002:idi42e6:status7:successe"
--
-- === Encode as nested dicts
--
-- @
-- data File = File { name :: Text, size :: Int }
--
-- encodeFile :: File -> E.'Encoding'
-- encodeFile (File name size) = E.'dict'' $
--      E.'field' "name" E.'text' name
--   <> E.'field' "metadata" id (E.'dict'' $
--        E.'field' "info" id (E.'dict'' $
--          E.'field' "size" E.'int' size))
-- @
--
-- >>> toLBS $ encodeFile $ File "hello.txt" 32
-- "d8:metadatad4:infod4:sizei32eee4:name9:hello.txte"
--
