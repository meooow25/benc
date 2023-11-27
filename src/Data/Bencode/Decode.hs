{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Conversions from Bencoded @ByteString@s to Haskell values.
--
module Data.Bencode.Decode
  (
    -- * Quick start
    -- $quick

    -- * Parser
    Parser
  , decode
  , decodeMaybe

    -- * String parsers
  , string
  , stringEq
  , text
  , textEq

    -- * Integer parsers
  , integer
  , int
  , intEq
  , int64
  , int32
  , int16
  , int8
  , word
  , word64
  , word32
  , word16
  , word8

    -- * List parsers
  , list
  , index
  , elem
  , list'
  , Elems

    -- * Dictionary parsers
  , dict
  , field
  , field'
  , dict'
  , Fields'

    -- * Miscellaneous
  , value
  , fail

    -- * Recipes #recipes#
    -- $recipes
  ) where

import Prelude hiding (elem, fail)
import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Data.Int
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Foldable as F
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Primitive.Array as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified GHC.Exts as X

import Data.Bencode.Type (Value(..))
import qualified Data.Bencode.Util as Util
import qualified Data.Bencode.AST as AST

newtype ParseResult a = ParseResult { unParseResult :: Either String a }
  deriving (Functor, Applicative, Monad)

failResult :: String -> ParseResult a
failResult = ParseResult . Left
{-# INLINE failResult #-}

instance Alternative ParseResult where
  empty = failResult "Alternative.empty"
  l <|> r = ParseResult $ unParseResult l <> unParseResult r
  -- Discards left error, not ideal

instance MonadPlus ParseResult
-- Does not satisfy MonadPlus laws because of the failure String
-- But required for Alternative (StateT _ ParseResult)

-- | A parser from a Bencode value to a Haskell value.
newtype Parser a = Parser { runParser_ :: ReaderT AST.Value ParseResult a }
  deriving (Functor, Applicative, Alternative, Monad)

runParser :: Parser a -> AST.Value -> ParseResult a
runParser = runReaderT . runParser_
{-# INLINE runParser #-}

liftP :: ParseResult a -> Parser a
liftP = Parser . lift
{-# INLINE liftP #-}

failParser :: String -> Parser a
failParser = liftP . failResult
{-# INLINE failParser #-}

-- | Decode a value from the given @ByteString@. If decoding fails, returns
-- @Left@ with a failure message. The message is a short human-readable error
-- description and should not be relied on programmatically.
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
stringDirect = Parser $ ReaderT $ \v -> case v of
  AST.String s -> pure s
  _            -> errTypeMismatch "String" v
{-# INLINE stringDirect #-}

integerDirect :: Parser B.ByteString
integerDirect = Parser $ ReaderT $ \v -> case v of
  AST.Integer s -> pure s
  _             -> errTypeMismatch "Integer" v
{-# INLINE integerDirect #-}

listDirect :: Parser (A.Array AST.Value)
listDirect = Parser $ ReaderT $ \v -> case v of
  AST.List a -> pure a
  _          -> errTypeMismatch "List" v
{-# INLINE listDirect #-}

dictDirect :: Parser (A.Array AST.KeyValue)
dictDirect = Parser $ ReaderT $ \v -> case v of
  AST.Dict a -> pure a
  _          -> errTypeMismatch "Dict" v
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
list p = listDirect >>= liftP . traverseAToV (runParser p)
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
  dictDirect >>= liftP . fmap M.fromDistinctAscList . traverse f .  F.toList
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
      Just ('-', s') -> Util.readKnownNaturalAsInt True s'
      _              -> Util.readKnownNaturalAsInt False s
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
      _            -> Util.readKnownNaturalAsWord s
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
--
-- If keys should not be left over in the dict, use 'field'' and 'dict''
-- instead.
field :: B.ByteString -> Parser a -> Parser a
field k p = do
  a <- dictDirect
  case binarySearch k a of
    (# _ |            #) -> failParser $ "KeyNotFound " ++ show k
    (#   | (# _, x #) #) -> liftP $ runParser p x
{-# INLINE field #-}

-- | Decode a value with the given parser for the given key. Convert to a
-- @Parser@ with 'dict''.
field' :: B.ByteString -> Parser a -> Fields' a
field' k p = Fields' $ ReaderT $ \a -> case binarySearch k a of
  (# _ |             #) -> lift . failResult $ "KeyNotFound " ++ show k
  (#   | (# i#, v #) #) -> lift (runParser p v) <* modify' (IS.insert (X.I# i#))
{-# INLINE field' #-}

-- | Create a @Parser@ from a 'Fields''. Fails on a non-dict, if a key is
-- absent, or if any value fails to parse. Also fails if there are leftover
-- unparsed keys in the dict.
--
-- If leftover keys should be ignored, use 'field' instead.
dict' :: Fields' a -> Parser a
dict' fs = do
  a <- dictDirect
  liftP $ do
    (v, is) <- runStateT (runReaderT (runFields' fs) a) IS.empty
    if IS.size is == A.sizeofArray a
    then pure v
    else let i = head $ filter (`IS.notMember` is) [0..]
             AST.KeyValue k _ = A.indexArray a i
         in failResult $ "UnrecognizedKey " ++ show k
{-# INLINE dict' #-}

-- | Key-value parsers. See 'dict'' and 'field''.
newtype Fields' a = Fields'
  { runFields' ::
      ReaderT (A.Array AST.KeyValue) (StateT IS.IntSet ParseResult) a
  } deriving (Functor, Applicative, Alternative, Monad)
-- We could use WriterT (CPS) but StateT is a teeny bit more efficient because
-- we can do IS.insert x instead of IS.union (IS.singleton x).

-- | Decode a list element with the given parser at the given (0-based) index.
-- Fails on a non-list, if the index is out of bounds, or if the element parser
-- fails.
--
-- Also see 'elem' and 'list''.
index :: Int -> Parser a -> Parser a
index i _ | i < 0 = failParser "IndexOutOfBounds"
index i p = do
  a <- listDirect
  if i < A.sizeofArray a
  then liftP $ runParser p (A.indexArray a i)
  else failParser "IndexOutOfBounds"
{-# INLINE index #-}

-- | Decode the next list element with the given parser.
elem :: Parser a -> Elems a
elem p = Elems $ ReaderT $ \a -> do
  i <- get
  if i < A.sizeofArray a
  then lift (runParser p (A.indexArray a i)) <* (put $! i+1)
  else lift $ failResult "ListElemsExhausted"
{-# INLINE elem #-}

-- | Create a @Parser@ from an @Elems@. Fails on a non-list, if the number of
-- elements does not match the @Elems@ exactly, or if any element parser fails.
list' :: Elems a -> Parser a
list' es = do
  a <- listDirect
  liftP $ do
    (x, i) <- runStateT (runReaderT (runElems es) a) 0
    if i == A.sizeofArray a
    then pure x
    else failResult $ "ListElemsLeft"
{-# INLINE list' #-}

-- | List elements parser. See 'elem' and 'list''.
newtype Elems a = Elems
  { runElems :: ReaderT (A.Array AST.Value) (StateT Int ParseResult) a
  } deriving (Functor, Applicative, Alternative, Monad)

-- | Decode a Bencode integer as an @Int64@. Fails on a non-integer or if the
-- integer is out of bounds for an @Int64@.
int64 :: Parser Int64
int64 = integerDirect >>= maybe (failParser "IntOutOfBounds") pure . go
  where
    go s = case BC.uncons s of
      Just ('-', s') -> Util.readKnownNaturalAsInt64 True s'
      _              -> Util.readKnownNaturalAsInt64 False s
{-# INLINE int64 #-}

-- | Decode a Bencode integer as an @Int32@. Fails on a non-integer or if the
-- integer is out of bounds for an @Int32@.
int32 :: Parser Int32
int32 = intL32
{-# INLINE int32 #-}

-- | Decode a Bencode integer as an @Int16@. Fails on a non-integer or if the
-- integer is out of bounds for an @Int16@.
int16 :: Parser Int16
int16 = intL32
{-# INLINE int16 #-}

-- | Decode a Bencode integer as an @Int8@. Fails on a non-integer or if the
-- integer is out of bounds for an @Int8@.
int8 :: Parser Int8
int8 = intL32
{-# INLINE int8 #-}

-- Parse an Int(<=32) via Int.
intL32 :: forall a. (Bounded a, Integral a) => Parser a
intL32 = int >>= \i ->
  if fromIntegral (minBound :: a) <= i && i <= fromIntegral (maxBound :: a)
  then pure $! fromIntegral i
  else failParser "IntOutOfBounds"
{-# INLINE intL32 #-}

-- | Decode a Bencode integer as a @Word64@. Fails on a non-integer or if the
-- integer is out of bounds for a @Word64@.
word64 :: Parser Word64
word64 = integerDirect >>= maybe (failParser "WordOutOfBounds") pure . go
  where
    go s = case BC.uncons s of
      Just ('-', _) -> Nothing
      _             -> Util.readKnownNaturalAsWord64 s
{-# INLINE word64 #-}

-- | Decode a Bencode integer as a @Word32@. Fails on a non-integer or if the
-- integer is out of bounds for a @Word32@.
word32 :: Parser Word32
word32 = wordL32
{-# INLINE word32 #-}

-- | Decode a Bencode integer as a @Word16@. Fails on a non-integer or if the
-- integer is out of bounds for a @Word16@.
word16 :: Parser Word16
word16 = wordL32
{-# INLINE word16 #-}

-- | Decode a Bencode integer as a @Word8@. Fails on a non-integer or if the
-- integer is out of bounds for a @Word8@.
word8 :: Parser Word8
word8 = wordL32
{-# INLINE word8 #-}

-- Parse a Word(<=32) via Word.
wordL32 :: forall a. (Bounded a, Integral a) => Parser a
wordL32 = word >>= \i ->
  if fromIntegral (minBound :: a) <= i && i <= fromIntegral (maxBound :: a)
  then pure $! fromIntegral i
  else failParser "WordOutOfBounds"
{-# INLINE wordL32 #-}


-- | Binary search. The array must be sorted by key.
binarySearch
  :: B.ByteString
  -> A.Array AST.KeyValue
  -> (# (# #) | (# X.Int#,  AST.Value #) #)
binarySearch k a = go 0 (A.sizeofArray a)
  where
    go l r | l == r = (# (# #) | #)
    go l r = case compare k k' of
      LT -> go l m
      EQ -> (# | (# case m of X.I# m# -> m#, v #) #)
      GT -> go (m+1) r
      where
        -- Overflow, careful!
        m = fromIntegral ((fromIntegral (l+r) :: Word) `div` 2) :: Int
        AST.KeyValue k' v = A.indexArray a m
{-# INLINABLE binarySearch #-}
-- binarySearch returns an unboxed type, which serves as an equivalent of
-- Maybe (Int, Value). This is to avoid allocating the (,) and the I#. This
-- won't be necessary if GHC gets
-- https://gitlab.haskell.org/ghc/ghc/-/issues/14259.
-- A (not-so-good) alternative is to inline binarySearch.

------------------------------
-- Documentation
------------------------------

-- $quick
-- Decoding is done using parsers. This module defines parsers that can be
-- composed to build parsers for arbitrary types.
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
-- Of course, invalid Bencode or Bencode that does not satisfy the @File@ parser
-- will fail to decode.
--
-- >>> D.decode fileParser "d4:hash4:xxxx4:tagsl4:work6:backupee"
-- Left "KeyNotFound \"size\""
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
-- === Decode a dict, failing on leftover keys
--
-- @
-- data File = File { name :: Text, size :: Int } deriving Show
--
-- fileParser :: D.'Parser' File
-- fileParser = D.'dict'' $
--   File
--     \<$> D.'field'' "name" D.'text'
--     \<*> D.'field'' "size" D.'int'
-- @
--
-- >>> D.decode fileParser "d4:name9:hello.txt4:sizei32ee"
-- Right (File {name = "hello.txt", size = 32})
-- >>> D.decode fileParser "d6:hiddeni1e4:name9:hello.txt4:sizei32ee"
-- Left "UnrecognizedKey \"hidden\""
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
-- === Decode a heterogeneous list
--
-- @
-- data File = File { name :: Text, size :: Int } deriving Show
--
-- fileParser :: D.'Parser' File
-- fileParser = D.'list'' $
--   File
--     \<$> D.'elem' D.'text'
--     \<*> D.'elem' D.'int'
-- @
--
-- >>> D.decode fileParser "l9:hello.txti32ee"
-- Right (File {name = "hello.txt", size = 32})
--
