{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Conversions from Bencoded @ByteString@s to Haskell values.
--
-- == Introduction
--
-- Decoding is done using parsers. There are parsers for the four Bencode types:
--
-- * 'string' decodes Bencode strings as 'B.ByteString's
-- * 'integer' decodes Bencode integers as 'Prelude.Integer's
-- * 'list' decodes Bencode lists as 'V.Vector's
-- * 'dict' decodes Bencode dictionaries as 'M.Map's with 'B.ByteString' keys.
--
-- These can be used to build more complex parsers for arbitrary types.
--
-- @
-- data File = File
--   { hash :: ByteString
--   , size :: Integer
--   , tags :: Vector Text
--   } deriving Show
-- @
--
-- Assuming a @File@ is encoded as a Bencode dictionary with the field names as
-- keys and appropriate value types, a parser for @File@ can be defined as
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
-- import qualified Data.Bencode.Decode as D
--
-- fileParser :: D.'Parser' File
-- fileParser =
--   File \<\$> D.'field' "hash" D.'string'
--        \<*> D.'field' "size" D.'integer'
--        \<*> D.'field' "tags" (D.'list' D.'text')
-- @
--
-- The parser can then be run on a @ByteString@ with 'decode'.
--
-- >>> D.decode fileParser "d4:hash4:xxxx4:sizei1024e4:tagsl4:work6:backupee"
-- Right (File {hash = "xxxx", size = 1024, tags = ["work","backup"]})
--
-- Of course, invalid Bencode or Bencode that does not satisfy our @File@ parser
-- will fail to decode.
--
-- >>> D.decode fileParser "d4:hash4:xxxx4:tagsl4:work6:backupee"
-- Left "KeyNotFound \"size\""
--
-- For more examples, see the \"Recipes\" section at the end of this page.
--
module Data.Bencode.Decode
  ( -- * Parser
    Parser
  , decode
  , decodeMaybe

    -- * Primary parsers
  , string
  , integer
  , list
  , dict

    -- * More parsers
  , stringEq
  , text
  , textEq
  , int
  , intEq
  , word
  , field
  , value
  , fail

    -- * Recipes
    --
    -- $recipes
  ) where

import Prelude hiding (fail)
import Control.Monad hiding (fail)
import Control.Monad.ST
import Control.Monad.Trans.Reader
import Control.Applicative
import qualified Data.Foldable as F
import qualified Data.Primitive.Array as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import qualified Data.Bencode.AST as AST
import Data.Bencode.Type
import Data.Bencode.Util (readKnownNaturalAsInt, readKnownNaturalAsWord)

newtype ParseResult a = ParseResult { unParseResult :: Either String a }
  deriving (Functor, Applicative, Monad)

failResult :: String -> ParseResult a
failResult = ParseResult . Left
{-# INLINE failResult #-}

instance Alternative ParseResult where
  empty = failResult "Alternative.empty"
  l <|> r = ParseResult $ unParseResult l <> unParseResult r
  -- Discards left error, not ideal

-- | A parser from a Bencode value to a Haskell value.
newtype Parser a = Parser { runParser :: AST.Value -> ParseResult a }
  deriving (Functor, Applicative, Alternative, Monad)
    via ReaderT AST.Value ParseResult

lift :: ParseResult a -> Parser a
lift = Parser . const
{-# INLINE lift #-}

failParser :: String -> Parser a
failParser = lift . failResult
{-# INLINE failParser #-}

-- | Decode a value from the given @ByteString@. If decoding fails, returns
-- @Left@ with a failure message.
decode :: Parser a -> B.ByteString -> Either String a
decode p s = AST.parseOnly s >>= unParseResult . runParser p

-- | Decode a value from the given @ByteString@. If decoding fails, returns
-- @Nothing@.
decodeMaybe :: Parser a -> B.ByteString -> Maybe a
decodeMaybe p = either (const Nothing) Just . decode p

errTypeMismatch :: String -> AST.Value -> ParseResult a
errTypeMismatch a b = failResult $ "TypeMismatch " ++ a ++ " " ++ b'
  where
    b' = case b of
      AST.String _  -> "String"
      AST.Integer _ -> "Integer"
      AST.List _    -> "List"
      AST.Dict _    -> "Dict"

-- Parsers below are all marked INLINE because they match on the AST
-- constructor and return Eithers. When inlined, GHC is able to optimize the
-- nested case matches using "case merging" and get rid of the intemeditate
-- Eithers using "case-of-case".

stringDirect :: Parser B.ByteString
stringDirect = Parser $ \v -> case v of
  AST.String s -> pure s
  _ -> errTypeMismatch "String" v
{-# INLINE stringDirect #-}

integerDirect :: Parser B.ByteString
integerDirect = Parser $ \v -> case v of
  AST.Integer s -> pure s
  _ -> errTypeMismatch "Integer" v
{-# INLINE integerDirect #-}

listDirect :: Parser (A.Array AST.Value)
listDirect = Parser $ \v -> case v of
  AST.List a -> pure a
  _ -> errTypeMismatch "List" v
{-# INLINE listDirect #-}

dictDirect :: Parser (A.Array AST.KeyValue)
dictDirect = Parser $ \v -> case v of
  AST.Dict a -> pure a
  _ -> errTypeMismatch "Dict" v
{-# INLINE dictDirect #-}

-- | Decode a Bencode string as a ByteString. Fails on a non-string.
string :: Parser B.ByteString
string = stringDirect
{-# INLINE string #-}

-- | Decode a Bencode integer as an Integer. Fails on a non-integer.
integer :: Parser Integer
integer = toI <$!> integerDirect
  where
    -- BC.readInteger will be making redundant digit checks since we already
    -- know it to be a valid integer.
    -- But it has an efficient divide-and-conquer algorithm compared to the
    -- simple but O(n^2) foldl' (\acc x -> acc * 10 + x) 0.
    -- We can reimplement the algorithm without the redundant checks if we
    -- really want.
    toI s = case BC.readInteger s of
      Nothing    -> error "Data.Bencode.Decode.integer: should not happen"
      Just (i,_) -> i
{-# INLINE integer #-}

-- | Decode a Bencode list with the given parser for elements. Fails on a
-- non-list or if any element in the list fails to parse.
list :: Parser a -> Parser (V.Vector a)
list p = listDirect >>= lift . traverseAToV (runParser p)
{-# INLINE list #-}

traverseAToV :: (a -> ParseResult b) -> A.Array a -> ParseResult (V.Vector b)
traverseAToV f a = runST $ do
  let n = A.sizeofArray a
  v <- VM.new n
  let loop i | i == n = pure Nothing
      loop i = case f (A.indexArray a i) of
        ParseResult (Left e)  -> pure (Just e)
        ParseResult (Right x) -> VM.write v i x *> loop (i+1)
  res <- loop 0
  maybe (pure <$> V.unsafeFreeze v) (pure . failResult) res
{-# INLINABLE traverseAToV #-}

-- | Decode a Bencode dict with the given parser for values. Fails on a
-- non-dict or if any value in the dict fails to parse.
dict :: Parser a -> Parser (M.Map B.ByteString a)
dict p =
  dictDirect >>= lift . fmap M.fromDistinctAscList . traverse f .  F.toList
  where
    f (AST.KeyValue k v) = (,) k <$> runParser p v
{-# INLINE dict #-}

-- | Succeeds only on a Bencode string that equals the given string.
stringEq :: B.ByteString -> Parser ()
stringEq s = stringDirect >>= \s' ->
  if s == s'
  then pure ()
  else failParser $ "StringNotEq " ++ show s ++ " " ++ show s'
{-# INLINE stringEq #-}

-- | Decode a bencode string as UTF-8 text. Fails on a non-string or if the
-- string is not valid UTF-8.
text :: Parser T.Text
text =
  stringDirect >>=
  either (const (failParser "UTF8DecodeFailure")) pure . T.decodeUtf8'
{-# INLINE text #-}

-- | Succeeds only on a Bencode string that equals the given text.
textEq :: T.Text -> Parser ()
textEq t = text >>= \t' ->
  if t == t'
  then pure ()
  else failParser $ "TextNotEq " ++ show t ++ " " ++ show t'
{-# INLINE textEq #-}

-- | Decode a Bencode integer as an @Int@. Fails on a non-integer or if the
-- integer is out of bounds for an @Int@.
int :: Parser Int
int = integerDirect >>= maybe (failParser "IntOutOfBounds") pure . go
  where
    go s = case BC.uncons s of
      Just ('-', s') -> readKnownNaturalAsInt True s'
      _              -> readKnownNaturalAsInt False s
{-# INLINE int #-}

-- | Succeeds only on a Bencode integer that equals the given value.
intEq :: Int -> Parser ()
intEq i = int >>= \i' ->
  if i == i'
  then pure ()
  else failParser $ "IntNotEq " ++ show i ++ " " ++ show i'
{-# INLINE intEq #-}

-- | Decode a Bencode integer as a @Word@. Fails on a non-integer or if the
-- integer is out of bounds for a @Word@.
word :: Parser Word
word = integerDirect >>= maybe (failParser "WordOutOfBounds") pure . go
  where
    go s = case BC.uncons s of
      Just ('-',_) -> Nothing
      _            -> readKnownNaturalAsWord s
{-# INLINE word #-}

-- | Decode a @Value@. Always succeeds for valid Bencode.
value :: Parser Value
value = String  <$> string
    <|> Integer <$> integer
    <|> List    <$> list value
    <|> Dict    <$> dict value

-- | Always fails with the given message.
fail :: String -> Parser a
fail = failParser . ("Fail: " ++ )
{-# INLINE fail #-}

-- | Decode a value with the given parser for the given key. Fails on a
-- non-dict, if the key is absent, or if the value parser fails.
field :: B.ByteString -> Parser a -> Parser a
field k p =
  dictDirect >>=
  maybe (failParser $ "KeyNotFound " ++ show k) pure . binarySearch k >>=
  lift . runParser p
{-# INLINE field #-}

----------
-- Utils

-- | Binary search. The array must be sorted by key.
binarySearch :: B.ByteString -> A.Array AST.KeyValue -> Maybe AST.Value
binarySearch k a = go 0 (A.sizeofArray a)
  where
    go l r | l == r = Nothing
    go l r = case compare k k' of
      LT -> go l m
      EQ -> Just v
      GT -> go (m+1) r
      where
        -- Overflow, careful!
        m = fromIntegral ((fromIntegral (l+r) :: Word) `div` 2) :: Int
        AST.KeyValue k' v = A.indexArray a m
{-# INLINABLE binarySearch #-}

-- $recipes
-- Recipes for some common and uncommon usages.
--
-- The following preface is assumed.
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
-- import Data.ByteString (ByteString)
-- import Data.Text (Text)
-- import qualified Data.Bencode.Decode as D
-- @
--
-- === Decode an optional field
--
-- @
-- import Control.Applicative ('optional')
--
-- data File = File { name :: Text, size :: Maybe Int } deriving Show
--
-- fileParser :: D.'Parser' File
-- fileParser =
--   File
--     \<$> D.'field' "name" D.'text'
--     \<*> optional (D.'field' "size" D.'int')
-- @
--
-- >>> D.decode fileParser "d4:name9:hello.txt4:sizei16ee"
-- Right (File {name = "hello.txt", size = Just 16})
-- >>> D.decode fileParser "d4:name9:hello.txte"
-- Right (File {name = "hello.txt", size = Nothing})
--
-- === Decode an enum
--
-- @
-- import Control.Applicative ('(<|>)')
--
-- data Color = Red | Green | Blue deriving Show
--
-- colorParser :: D.'Parser' Color
-- colorParser =
--       Red   \<$ D.'stringEq' "red"
--   \<|> Green \<$ D.'stringEq' "green"
--   \<|> Blue  \<$ D.'stringEq' "blue"
--   \<|> D.'fail' "unknown color"
-- @
--
-- >>> D.decode colorParser "5:green"
-- Right Green
-- >>> D.decode colorParser "5:black"
-- Left "Fail: unknown color"
--
-- === Decode differently based on dict contents
--
-- @
-- import Control.Applicative ('(<|>)')
--
-- data Response = Response
--   { id_    :: Int
--   , result :: Either Text ByteString
--   } deriving Show
--
-- responseParser :: D.'Parser' Response
-- responseParser = do
--   id_ <- D.'field' "id" D.'int'
--   success <- D.'field' "status" $
--         False \<$ D.'stringEq' "failure"
--     \<|> True  \<$ D.'stringEq' "success"
--     \<|> D.'fail' "unknown status"
--   Response id_
--     \<$> if success
--         then Right \<$> D.'field' "data" D.'string'
--         else Left  \<$> D.'field' "reason" D.'text'
-- @
--
-- >>> D.decode responseParser "d2:idi42e6:reason12:unauthorized6:status7:failuree"
-- Right (Response {id_ = 42, result = Left "unauthorized"})
-- >>> D.decode responseParser "d4:data4:00002:idi42e6:status7:successe"
-- Right (Response {id_ = 42, result = Right "0000"})
--
-- === Decode nested dicts
--
-- @
-- data File = File { name :: Text, size :: Int } deriving Show
--
-- fileParser :: D.'Parser' File
-- fileParser =
--   File
--     \<$> D.'field' "name" D.'text'
--     \<*> D.'field' "metadata" (D.'field' "info" (D.'field' "size" D.'int'))
-- @
--
-- >>> D.decode fileParser "d8:metadatad4:infod4:sizei32eee4:name9:hello.txte"
-- Right (File {name = "hello.txt", size = 32})
--
