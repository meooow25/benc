{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict #-}
module Data.Bencode.AST
  ( Value(..)
  , KeyValue(..)
  , parseOnly
  ) where

-- ATTENTION: This module is Strict!
-- Prefer to add definitions where laziness is desirable in another module
-- instead of here with a ~.

import Data.Char (isDigit)
import Data.List (intercalate)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Primitive.Array as A

import qualified Data.Bencode.Util as Util

-- | The Bencode AST.
data Value
  = String  {-# UNPACK #-} B.ByteString
  -- ^ Slice of the input @ByteString@.
  | Integer {-# UNPACK #-} B.ByteString
  -- ^ Slice of the input @ByteString@, containing a valid integer. Parsing
  -- into an integral type is done later if required.
  | List    {-# UNPACK #-} (A.Array Value)
  | Dict    {-# UNPACK #-} (A.Array KeyValue)
  deriving (Eq, Show)

-- | A Bencode dict's key-value pair.
data KeyValue = KeyValue
  {-# UNPACK #-} B.ByteString-- ^ Slice of the input @ByteString@.
  Value
  deriving (Eq, Show)

newtype Pos = Pos { unPos :: Int } deriving (Show, Num)

-- | Either an error or the parsed value together with the unparsed
-- section of the input and number of bytes parsed.
type ParseOneResult = Either String (Value, B.ByteString, Int)

data Stack
  = SNil
  | SList {-# UNPACK #-} Int [Value] Stack
  | SDict {-# UNPACK #-} B.ByteString {-# UNPACK #-} Int [KeyValue] Stack

-- | Parse one Bencode value from the given bytestring. Fails if the string is
-- not fully consumed.
parseOnly :: B.ByteString -> Either String Value
parseOnly s = case parseOne s of
  Left e -> Left e
  Right (v, s', n) ->
    if B.null s'
    then Right v
    else errorAtPos "ExpectedEOF" (Pos n)

-- | Parse one Bencode value from the given bytestring.
parseOne :: B.ByteString -> ParseOneResult
parseOne s = case BC.uncons s of
  Nothing -> errItem Nothing pos
  Just (c,s1) -> case c of
    _ | isDigit c ->
      parseString s pos $ \str s2 pos2 -> Right (String str, s2, unPos pos2)
    'i' -> do
      parseInteger s1 (pos+1) $ \i s2 pos2 -> Right (Integer i, s2, unPos pos2)
    'l' -> parseList SNil 0 [] s1 (pos+1)
    'd' -> parseDict SNil s1 (pos+1)
    _   -> errItem (Just c) pos
  where
    pos = Pos 0

-- | Parse a Bencode list. After the \'l\' marker.
parseList :: Stack -> Int -> [Value] -> B.ByteString -> Pos -> ParseOneResult
parseList stk n acc s pos = case BC.uncons s of
  Nothing -> errItemOrEnd Nothing pos
  Just (c,s1) -> case c of
    _ | isDigit c -> do
      parseString s pos $ \str -> parseList stk (n+1) (String str : acc)
    'i' -> do
      parseInteger s1 (pos+1) $ \i -> parseList stk (n+1) (Integer i : acc)
    'l' -> parseList (SList n acc stk) 0 [] s1 (pos+1)
    'd' -> parseDict (SList n acc stk) s1 (pos+1)
    'e' -> resumeParse stk (List (Util.arrayFromRevListN n acc)) s1 (pos+1)
    _   -> errItemOrEnd (Just c) pos

-- | Parse a Bencode dict. After the \'d\' marker.
parseDict :: Stack -> B.ByteString -> Pos -> ParseOneResult
parseDict stk s pos = case BC.uncons s of
  Nothing -> errStringOrEnd Nothing pos
  Just (c1,s1) -> case c1 of
    _ | isDigit c1 -> do
      parseString s pos $ \key s2 pos2 ->
        case BC.uncons s2 of
          Nothing -> errItem Nothing pos2
          Just (c3,s3) -> case c3 of
            _ | isDigit c3 -> do
              parseString s2 pos2 $ \str ->
                parseDict1 key stk 1 [KeyValue key (String str)]
            'i' -> do
              parseInteger s3 (pos2+1) $ \i ->
                parseDict1 key stk 1 [KeyValue key (Integer i)]
            'l' -> parseList (SDict key 0 [] stk) 0 [] s3 (pos2+1)
            'd' -> parseDict (SDict key 0 [] stk) s3 (pos2+1)
            _   -> errItem (Just c3) pos2
    'e' -> resumeParse stk (Dict (Util.arrayFromRevListN 0 [])) s1 (pos+1)
    _   -> errStringOrEnd (Just c1) pos

-- | Parse a Bencode dict. After the first key-value pair.
parseDict1 :: B.ByteString -> Stack -> Int -> [KeyValue] -> B.ByteString -> Pos
           -> ParseOneResult
parseDict1 pkey stk n acc s pos = case BC.uncons s of
  Nothing -> errStringOrEnd Nothing pos
  Just (c1,s1) -> case c1 of
    _ | isDigit c1 -> do
      parseString s pos $ \key s2 pos2 ->
        if pkey >= key
        then errUnsortedKeys pkey key pos
        else case BC.uncons s2 of
          Nothing -> errItem Nothing pos2
          Just (c3,s3) -> case c3 of
            _ | isDigit c3 -> do
              parseString s2 pos2 $ \str ->
                parseDict1 key stk (n+1) (KeyValue key (String str) : acc)
            'i' -> do
              parseInteger s3 (pos2+1) $ \i ->
                parseDict1 key stk (n+1) (KeyValue key (Integer i) : acc)
            'l' -> parseList (SDict key n acc stk) 0 [] s3 (pos2+1)
            'd' -> parseDict (SDict key n acc stk) s3 (pos2+1)
            _   -> errItem (Just c3) pos2
    'e' -> resumeParse stk (Dict (Util.arrayFromRevListN n acc)) s1 (pos+1)
    _   -> errStringOrEnd (Just c1) pos

-- | Add the value to the previously incomplete value on the stack, and resume
-- parsing it.
resumeParse :: Stack -> Value -> B.ByteString -> Pos -> ParseOneResult
resumeParse stk x s pos = case stk of
  SNil               -> Right (x, s, unPos pos)
  SList n xs stk1    -> parseList stk1 (n+1) (x:xs) s pos
  SDict k n acc stk1 -> parseDict1 k stk1 (n+1) (KeyValue k x : acc) s pos
{-# INLINE resumeParse #-}

-- | Parse a Bencode integer. After the \'i\' to the \'e\'.
parseInteger :: B.ByteString -> Pos
             -> (B.ByteString -> B.ByteString -> Pos -> ParseOneResult)
             -> ParseOneResult
parseInteger s pos k = case BC.uncons s of
  Nothing -> errDigit Nothing pos
  Just (c1,s1) -> case c1 of
    '0' -> end (B.take 1 s) s1 (pos+1)
    '-' -> case BC.span isDigit s1 of
      (x,s2) -> case BC.uncons x of
        Just (c3,_) | c3 /= '0' ->
          let n = B.length x + 1 in end (B.take n s) s2 (pos + Pos n)
        _ -> errNZDigit (fmap fst (BC.uncons s2)) (pos+1)
    _ -> case BC.span isDigit s of
      (x,s2) -> if B.null x
        then errDigitOrNeg (Just c1) pos
        else let n = B.length x in end x s2 (pos + Pos n)
  where
    end x s' pos' = case BC.uncons s' of
      Nothing -> errEnd Nothing pos'
      Just (c,s'') -> case c of
        'e' -> k x s'' (pos'+1)
        _   -> errEnd (Just c) pos'
    {-# INLINE end #-}
{-# INLINE parseInteger #-}

-- | Parse a Bencode string. From the length count to the end of the string.
parseString :: B.ByteString -> Pos
            -> (B.ByteString -> B.ByteString -> Pos -> ParseOneResult)
            -> ParseOneResult
parseString s pos k = case BC.span isDigit s of
  (digs,s1) ->
    case Util.readKnownNaturalAsInt False (BC.dropWhile (=='0') digs) of
      Nothing -> errTooLargeStringLength pos
      Just n ->
        let pos2 = pos + Pos (B.length digs)
        in case BC.uncons s1 of
            Nothing -> errColon Nothing pos2
            Just (c3,s3) -> case c3 of
              ':' -> case B.splitAt n s3 of
                (str,s4) | B.length str == n -> k str s4 (pos2 + 1 + Pos n)
                _ -> errTooLargeStringLength pos
              _   -> errColon (Just c3) pos2
{-# INLINE parseString #-}

------------------------------
-- Error stuff

errorAtPos :: String -> Pos -> Either String a
errorAtPos e (Pos n) = Left $ "ParseErrorAt " ++ show n ++ ": " ++ e

mismatch :: [String] -> Maybe Char -> Pos -> Either String a
mismatch cs c = errorAtPos $
  "ExpectedOneOfButGot [" ++ intercalate "," cs ++ "] " ++ maybe "EOF" show c

errItem, errItemOrEnd, errStringOrEnd, errEnd, errDigit, errNZDigit,
  errColon, errDigitOrNeg :: Maybe Char -> Pos -> Either String a
errItem        = mismatch ["Digit", show 'i', show 'l', show 'd']
errItemOrEnd   = mismatch ["Digit", show 'i', show 'l', show 'd', show 'e']
errStringOrEnd = mismatch ["Digit", show 'e']
errEnd         = mismatch [show 'e']
errDigit       = mismatch ["Digit"]
errNZDigit     = mismatch ["NonZeroDigit"]
errDigitOrNeg  = mismatch ["Digit", show '-']
errColon       = mismatch [show ':']

errUnsortedKeys :: B.ByteString -> B.ByteString -> Pos -> Either String a
errUnsortedKeys pkey key = errorAtPos $
  "UnsortedKeys " ++ show pkey ++ " " ++ show key

errTooLargeStringLength :: Pos -> Either String a
errTooLargeStringLength = errorAtPos "TooLargeStringLength"
