{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}  -- Arbitrary instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Vector as V

import qualified Data.Bencode.Decode as D
import qualified Data.Bencode.Encode as E
import qualified Data.Bencode.Type as Ben

main :: IO ()
main = defaultMain $ localOption (QuickCheckTests 2000) $ testGroup "Tests"
  [ astTests
  , decodeTests
  , encodeTests
  , encodeDecodeTests
  ]

-- Would like to test Data.Bencode.AST.parseOnly but since it's not exposed
-- test via D.decode D.value.
astTests :: TestTree
astTests = testGroup "AST"
  [ testGroup "valid Bencode"
    [ testGroup "string"
      [ testCase "0:" $ D.decode D.value "0:" @?= Right (Ben.String "")
      , testCase "3:foo" $ D.decode D.value "3:foo" @?= Right (Ben.String "foo")
      , testCase "<binary>" $ D.decode D.value "2:\x00\xff" @?= Right (Ben.String "\x00\xff")
      ]
    , testGroup "integer"
      [ testCase "i0e" $ D.decode D.value "i0e" @?= Right (Ben.Integer 0)
      , testCase "i1e" $ D.decode D.value "i1e" @?= Right (Ben.Integer 1)
      , testCase "i-1e" $ D.decode D.value "i-1e" @?= Right (Ben.Integer (-1))
      , testCase "i98765432109876543210e" $ D.decode D.value "i98765432109876543210e" @?= Right (Ben.Integer 98765432109876543210)
      ]
    , testGroup "list"
      [ testCase "le" $ D.decode D.value "le" @?= Right (Ben.List [])
      , testCase "li1e3:fooe" $ D.decode D.value "li1e3:fooe" @?= Right (Ben.List [Ben.Integer 1, Ben.String "foo"])
      , testCase "lllll3:fooeeeee" $ D.decode D.value "lllll3:fooeeeee" @?= Right (Ben.List [Ben.List [Ben.List [Ben.List [Ben.List [Ben.String "foo"]]]]])
      ]
    , testGroup "dict"
      [ testCase "de" $ D.decode D.value "de" @?= Right (Ben.Dict [])
      , testCase "d3:fooi0ee" $ D.decode D.value "d3:fooi1ee" @?= Right (Ben.Dict [("foo", Ben.Integer 1)])
      , testCase "d3:bari0e3:fooi1ee" $ D.decode D.value "d3:bari0e3:fooi1ee" @?= Right (Ben.Dict [("bar", Ben.Integer 0), ("foo", Ben.Integer 1)])
      , testCase "d3:food3:foodeee" $ D.decode D.value "d3:food3:foodeee" @?= Right (Ben.Dict [("foo", Ben.Dict [("foo", Ben.Dict [])])])
      ]
    ]
  , testGroup "invalid Bencode fails"
    [ testGroup "no items"
      [ testCase "<empty>" $ D.decode D.value "" @?= Left "ParseErrorAt 0: ExpectedOneOfButGot [Digit,'i','l','d'] EOF"
      , testCase "a" $ D.decode D.value "a" @?= Left "ParseErrorAt 0: ExpectedOneOfButGot [Digit,'i','l','d'] 'a'"
      , testCase ":" $ D.decode D.value ":" @?= Left "ParseErrorAt 0: ExpectedOneOfButGot [Digit,'i','l','d'] ':'"
      ]
    , testGroup "string"
      [ let x = show (2^(64 :: Int) :: Integer) {- overflows to 0 -} in
        testCase (x <> ":") $ D.decode D.value (BC.pack x <> ":") @?= Left "ParseErrorAt 0: TooLargeStringLength"
      , let x = show (fromIntegral (maxBound :: Int) + 1 :: Integer) {- overflows to -1 -} in
        testCase (x <> ":") $ D.decode D.value (BC.pack x <> ":") @?= Left "ParseErrorAt 0: TooLargeStringLength"
      , testCase "3" $ D.decode D.value "3" @?= Left "ParseErrorAt 1: ExpectedOneOfButGot [':'] EOF"
      , testCase "3foo" $ D.decode D.value "3foo" @?= Left "ParseErrorAt 1: ExpectedOneOfButGot [':'] 'f'"
      , testCase "4:foo" $ D.decode D.value "4:foo" @?= Left "ParseErrorAt 0: TooLargeStringLength"
      ]
    ]
    , testGroup "integer"
      [ testCase "i" $ D.decode D.value "i" @?= Left "ParseErrorAt 1: ExpectedOneOfButGot [Digit] EOF"
      , testCase "i01e" $ D.decode D.value "i01" @?= Left "ParseErrorAt 2: ExpectedOneOfButGot ['e'] '1'"
      , testCase "i-" $ D.decode D.value "i-" @?= Left "ParseErrorAt 2: ExpectedOneOfButGot [NonZeroDigit] EOF"
      , testCase "i-0e" $ D.decode D.value "i-0e" @?= Left "ParseErrorAt 2: ExpectedOneOfButGot [NonZeroDigit] 'e'"
      , testCase "ifooe" $ D.decode D.value "ifooe" @?= Left "ParseErrorAt 1: ExpectedOneOfButGot [Digit,'-'] 'f'"
      , testCase "i12" $ D.decode D.value "i12" @?= Left "ParseErrorAt 3: ExpectedOneOfButGot ['e'] EOF"
      , testCase "i12d" $ D.decode D.value "i12d" @?= Left "ParseErrorAt 3: ExpectedOneOfButGot ['e'] 'd'"
      ]
    , testGroup "list"
      [ testCase "l" $ D.decode D.value "l" @?= Left "ParseErrorAt 1: ExpectedOneOfButGot [Digit,'i','l','d','e'] EOF"
      , testCase "lfoo" $ D.decode D.value "lfoo" @?= Left "ParseErrorAt 1: ExpectedOneOfButGot [Digit,'i','l','d','e'] 'f'"
      , testCase "l3:foo" $ D.decode D.value "l3:foo" @?= Left "ParseErrorAt 6: ExpectedOneOfButGot [Digit,'i','l','d','e'] EOF"
      , testCase "l3:foobar" $ D.decode D.value "l3:foobar" @?= Left "ParseErrorAt 6: ExpectedOneOfButGot [Digit,'i','l','d','e'] 'b'"
      ]
    , testGroup "dict"
      [ testCase "d" $ D.decode D.value "d" @?= Left "ParseErrorAt 1: ExpectedOneOfButGot [Digit,'e'] EOF"
      , testCase "dfoo" $ D.decode D.value "dfoo" @?= Left "ParseErrorAt 1: ExpectedOneOfButGot [Digit,'e'] 'f'"
      , testCase "d3:foo" $ D.decode D.value "d3:foo" @?= Left "ParseErrorAt 6: ExpectedOneOfButGot [Digit,'i','l','d'] EOF"
      , testCase "d3:foobar" $ D.decode D.value "d3:foobar" @?= Left "ParseErrorAt 6: ExpectedOneOfButGot [Digit,'i','l','d'] 'b'"
      , testCase "di1e3:foo" $ D.decode D.value "di1e3:foo" @?= Left "ParseErrorAt 1: ExpectedOneOfButGot [Digit,'e'] 'i'"
      , testCase "d3:fooi0e3:bari1ee" $ D.decode D.value "d3:fooi0e3:bari1ee" @?= Left "ParseErrorAt 9: UnsortedKeys \"foo\" \"bar\""
      ]
    , testGroup "leftover"
      [ testCase "3:foo3:bar" $ D.decode D.value "3:foo3:bar" @?= Left "ParseErrorAt 5: ExpectedEOF"
      ]
  ]

decodeTests :: TestTree
decodeTests = testGroup "Decode"
  [ testGroup "string"
    [ testCase "0:" $ D.decode D.string "0:" @?= Right ""
    , testCase "3:foo" $ D.decode D.string "3:foo" @?= Right "foo"
    , testCase "type mismatch integer" $ D.decode D.string "i0e" @?= Left "TypeMismatch String Integer"
    , testCase "type mismatch list" $ D.decode D.string "le" @?= Left "TypeMismatch String List"
    , testCase "type mismatch dict" $ D.decode D.string "de" @?= Left "TypeMismatch String Dict"
    ]
  , testGroup "integer"
    [ testCase "i0e" $ D.decode D.integer "i0e" @?= Right 0
    , testCase "i-32e" $ D.decode D.integer "i-32e" @?= Right (-32)
    , testCase "i98765432109876543210e" $ D.decode D.integer "i98765432109876543210e" @?= Right 98765432109876543210
    , testCase "type mismatch string" $ D.decode D.integer "0:" @?= Left "TypeMismatch Integer String"
    , testCase "type mismatch list" $ D.decode D.integer "le" @?= Left "TypeMismatch Integer List"
    , testCase "type mismatch dict" $ D.decode D.integer "de" @?= Left "TypeMismatch Integer Dict"
    ]
  , testGroup "list"
    [ testCase "le" $ D.decode (D.list D.value) "le" @?= Right []
    , testCase "l3:foo3:bare" $ D.decode (D.list D.string) "l3:foo3:bare" @?= Right ["foo", "bar"]
    , testCase "elem type mismatch" $ D.decode (D.list D.string) "l3:fooi1ee" @?= Left "TypeMismatch String Integer"
    , testCase "type mismatch string" $ D.decode (D.list D.value) "0:" @?= Left "TypeMismatch List String"
    , testCase "type mismatch integer" $ D.decode (D.list D.value) "i0e" @?= Left "TypeMismatch List Integer"
    , testCase "type mismatch dict" $ D.decode (D.list D.value) "de" @?= Left "TypeMismatch List Dict"
    ]
  , testGroup "dict"
    [ testCase "de" $ D.decode (D.dict D.value) "de" @?= Right []
    , testCase "d3:foo3:bare" $ D.decode (D.dict D.string) "d3:foo3:bare" @?= Right [("foo", "bar")]
    , testCase "value type mismatch" $ D.decode (D.dict D.string) "d3:foo3:bar3:quxi1ee" @?= Left "TypeMismatch String Integer"
    , testCase "type mismatch string" $ D.decode (D.dict D.value) "0:" @?= Left "TypeMismatch Dict String"
    , testCase "type mismatch integer" $ D.decode (D.dict D.value) "i0e" @?= Left "TypeMismatch Dict Integer"
    , testCase "type mismatch list" $ D.decode (D.dict D.value) "le" @?= Left "TypeMismatch Dict List"
    ]
  , testGroup "text"
    [ testCase "0:" $ D.decode D.text "0:" @?= Right ""
    , testCase "3:foo" $ D.decode D.text "3:foo" @?= Right "foo"
    , testCase "こんにちは" $ D.decode D.text "15:\227\129\147\227\130\147\227\129\171\227\129\161\227\129\175" @?= Right "こんにちは"
    , testCase "invalid UTF-8" $ D.decode D.text "2:\xd8\x00" @?= Left "UTF8DecodeFailure"
    , testCase "type mismatch integer" $ D.decode D.text "i0e" @?= Left "TypeMismatch String Integer"
    , testCase "type mismatch list" $ D.decode D.text"le" @?= Left "TypeMismatch String List"
    , testCase "type mismatch dict" $ D.decode D.text"de" @?= Left "TypeMismatch String Dict"
    ]
  , testGroupIntegral "int" False D.int
  , testGroupIntegral "int64" False D.int64
  , testGroupIntegral "int32" False D.int32
  , testGroupIntegral "int16" False D.int16
  , testGroupIntegral "int8" False D.int8
  , testGroupIntegral "word" True D.word
  , testGroupIntegral "word64" True D.word64
  , testGroupIntegral "word32" True D.word32
  , testGroupIntegral "word16" True D.word16
  , testGroupIntegral "word8" True D.word8
  , testGroup "index"
    [ testCase "l3:fooe" $ D.decode (D.index 0 D.string) "l3:fooe" @?= Right "foo"
    , testCase "out of bounds -1" $ D.decode (D.index (-1) D.string) "l3:fooe" @?= Left "IndexOutOfBounds"
    , testCase "out of bounds 1" $ D.decode (D.index 1 D.string) "l3:fooe" @?= Left "IndexOutOfBounds"
    , testCase "li2ee" $ D.decode (D.index 0 D.string) "li2ee" @?= Left "TypeMismatch String Integer"
    , let p =     D.index 5 (Left <$> D.string)
              <|> D.index 0 (Right <$> D.int)
              <|> D.index 1 (Left <$> D.string) in
      testCase "alt" $ D.decode p "l3:foo3:bare" @?= Right (Left "bar")
    , let p = (,,) <$> D.index 1 D.string
                   <*> D.index 2 (D.list D.integer)
                   <*> D.index 0 D.integer in
      testCase "multiple types" $ D.decode p "li1e3:twoli0ei0ei0eee" @?= Right ("two", [0,0,0], 1)
    , testCase "type mismatch string" $ D.decode (D.index 0 D.string) "0:" @?= Left "TypeMismatch List String"
    , testCase "type mismatch integer" $ D.decode (D.index 0 D.string) "i0e" @?= Left "TypeMismatch List Integer"
    , testCase "type mismatch dict" $ D.decode (D.index 0 D.string) "de" @?= Left "TypeMismatch List Dict"
    ]
  , testGroup "list'"
    [ testCase "l3:fooe" $ D.decode (D.list' $ D.elem D.string) "l3:fooe" @?= Right "foo"
    , testCase "too short" $ D.decode (D.list' $ D.elem D.string) "le" @?= Left "ListElemsExhausted"
    , testCase "too long" $ D.decode (D.list' $ D.elem D.string) "l3:foo3:bare" @?= Left "ListElemsLeft"
    , testCase "elem type mismatch" $ D.decode (D.list' $ D.elem D.string) "li2ee" @?= Left "TypeMismatch String Integer"
    , let p = D.list' $
                    D.elem D.string
                <|> D.elem D.integer *> D.elem D.string in
      testCase "Elem alt" $ D.decode p "li1e3:fooe" @?= Right "foo"
    , let p = D.list' $
                (,,) <$> D.elem D.integer
                     <*> D.elem D.string
                     <*> D.elem (D.list D.integer) in
      testCase "multiple types" $ D.decode p "li1e3:twoli0ei0ei0eee" @?= Right (1, "two", [0,0,0])
    , testCase "type mismatch string" $ D.decode (D.list' $ pure ()) "0:" @?= Left "TypeMismatch List String"
    , testCase "type mismatch integer" $ D.decode (D.list' $ pure ()) "i0e" @?= Left "TypeMismatch List Integer"
    , testCase "type mismatch dict" $ D.decode (D.list' $ pure ()) "de" @?= Left "TypeMismatch List Dict"
    ]
  , testGroup "field"
    [ testCase "key not found" $ D.decode (D.field "foo" D.string) "de" @?= Left "KeyNotFound \"foo\""
    , testCase "d3:foo3:bare" $ D.decode (D.field "foo" D.string) "d3:foo3:bare" @?= Right "bar"
    , testCase "extra keys ignored" $ D.decode (D.field "baz" D.string) "d3:baz3:qux3:foo3:bare" @?= Right "qux"
    , let p = D.field "bar" D.string <|> D.field "foo" D.string in
      testCase "alt" $ D.decode p "d3:foo3:bare" @?= Right "bar"
    , testCase "value type mismatch" $ D.decode (D.field "foo" D.string) "d3:fooi2ee" @?= Left "TypeMismatch String Integer"
    , let p = (,,) <$> D.field "one" D.integer
                   <*> D.field "two" D.string
                   <*> D.field "three" (D.list D.integer) in
      testCase "multiple types" $ D.decode p "d3:onei1e5:threeli0ei0ei0ee3:two3:twoe" @?= Right (1, "two", [0,0,0])
    , testCase "type mismatch string" $ D.decode (D.field "foo" D.string) "0:" @?= Left "TypeMismatch Dict String"
    , testCase "type mismatch integer" $ D.decode (D.field "foo" D.string) "i0e" @?= Left "TypeMismatch Dict Integer"
    , testCase "type mismatch list" $ D.decode (D.field "foo" D.string) "le" @?= Left "TypeMismatch Dict List"
    ]
  , testGroup "dict'"
    [ testCase "key not found" $ D.decode (D.dict' $ D.field' "foo" D.string) "de" @?= Left "KeyNotFound \"foo\""
    , testCase "d3:foo3:bare" $ D.decode (D.dict' $ D.field' "foo" D.string) "d3:foo3:bare" @?= Right "bar"
    , testCase "extra keys fails" $ D.decode (D.dict' $ D.field' "baz" D.string) "d3:baz3:qux3:foo3:bare" @?= Left "UnrecognizedKey \"foo\""
    , let p = D.dict' $ D.field' "bar" D.string <|> D.field' "foo" D.string in
      testCase "Fields alt" $ D.decode p "d3:foo3:bare" @?= Right "bar"
    , testCase "value type mismatch" $ D.decode (D.dict' $ D.field' "foo" D.string) "d3:fooi2ee" @?= Left "TypeMismatch String Integer"
    , let p = D.dict' $
                (,,) <$> D.field' "one" D.integer
                     <*> D.field' "two" D.string
                     <*> D.field' "three" (D.list D.integer) in
      testCase "multiple types" $ D.decode p "d3:onei1e5:threeli0ei0ei0ee3:two3:twoe" @?= Right (1, "two", [0,0,0])
    , testCase "type mismatch string" $ D.decode (D.dict' $ pure ()) "0:" @?= Left "TypeMismatch Dict String"
    , testCase "type mismatch integer" $ D.decode (D.dict' $ pure ()) "i0e" @?= Left "TypeMismatch Dict Integer"
    , testCase "type mismatch list" $ D.decode (D.dict' $ pure ()) "le" @?= Left "TypeMismatch Dict List"
    ]
  , testGroup "fail"
    [ testCase "3:foo" $ D.decode (D.fail "error!" :: D.Parser B.ByteString) "3:foo" @?= Left "Fail: error!"
    ]
  ]
  where
    testGroupIntegral :: forall a. (Bounded a, Integral a, Show a)
                      => String -> Bool -> D.Parser a -> TestTree
    testGroupIntegral name isW p = testGroup name
      [ testCase "i0e" $ D.decode p "i0e" @?= Right 0
      , testCase "i32e" $ D.decode p "i32e" @?= Right 32
      , testCase "i-32e" $ D.decode p "i-32e" @?= if isW then Left oob else Right (-32)
      , testCase "minBound-1" $ D.decode p ("i" <> BC.pack (show (minBoundI - 1)) <> "e") @?= Left oob
      , testCase "minBound" $ D.decode p ("i" <> BC.pack (show minBoundI) <> "e") @?= Right minBound
      , testCase "maxBound" $ D.decode p ("i" <> BC.pack (show maxBoundI) <> "e") @?= Right maxBound
      , testCase "maxBound+1" $ D.decode p ("i" <> BC.pack (show (maxBoundI + 1)) <> "e") @?= Left oob
      , testCase "i98765432109876543210e" $ D.decode p "i98765432109876543210e" @?= Left oob
      , testCase "type mismatch string" $ D.decode p "0:" @?= Left "TypeMismatch Integer String"
      , testCase "type mismatch list" $ D.decode p "le" @?= Left "TypeMismatch Integer List"
      , testCase "type mismatch dict" $ D.decode p "de" @?= Left "TypeMismatch Integer Dict"
      ]
      where
        minBoundI, maxBoundI :: Integer
        minBoundI = fromIntegral (minBound :: a)
        maxBoundI = fromIntegral (maxBound :: a)
        oob = if isW then "WordOutOfBounds" else "IntOutOfBounds"

encodeTests :: TestTree
encodeTests = testGroup "Encode"
  [ testGroup "string"
    [ testCase "<empty>" $ enc E.string "" @?= "0:"
    , testCase "Hello, World!" $ enc E.string "Hello, World!" @?= "13:Hello, World!"
    ]
  , testGroup "integer"
    [ testCase "0" $ enc E.integer 0 @?= "i0e"
    , testCase "1" $ enc E.integer 1 @?= "i1e"
    , testCase "-1" $ enc E.integer (-1) @?= "i-1e"
    , testCase "98765432109876543210" $ enc E.integer 98765432109876543210 @?= "i98765432109876543210e"
    ]
  , testGroup "list"
    [ testCase "[]" $ enc (E.list E.integer) [] @?= "le"
    , testCase "[2,3,1]" $ enc (E.list E.integer) [2,3,1] @?= "li2ei3ei1ee"
    ]
  , testGroup "dict"
    [ testCase "{}" $ enc (E.dict E.integer) [] @?= "de"
    , testCase "{one:1,two:2,three:3}" $ enc (E.dict E.integer) [("one",1),("two",2),("three",3)] @?= "d3:onei1e5:threei3e3:twoi2ee"
    ]
  , testGroup "text"
    [ testCase "Hello, World!" $ enc E.text "Hello, World!" @?= "13:Hello, World!"
    , testCase "こんにちは" $ enc E.text "こんにちは" @?= "15:\227\129\147\227\130\147\227\129\171\227\129\161\227\129\175"
    ]
  , testGroupIntegral "int" False E.int
  , testGroupIntegral "int64" False E.int64
  , testGroupIntegral "int32" False E.int32
  , testGroupIntegral "int16" False E.int16
  , testGroupIntegral "int8" False E.int8
  , testGroupIntegral "word" True E.word
  , testGroupIntegral "word64" True E.word64
  , testGroupIntegral "word32" True E.word32
  , testGroupIntegral "word16" True E.word16
  , testGroupIntegral "word8" True E.word8
  , testGroup "field"
    [ testCase "{}" $ enc id (E.dict' mempty) @?= "de"
    , let e =  E.dict' $
               E.field "one" E.integer 1
            <> E.field "two" E.string "two"
            <> E.field "three" (E.list E.integer) [0,0,0] in
      testCase "{one:1,two:two,three:[0,0,0]}" $ enc id e @?= "d3:onei1e5:threeli0ei0ei0ee3:two3:twoe"
    ]
  ]
  where
    testGroupIntegral :: forall a. (Bounded a, Integral a, Show a)
                      => String -> Bool -> (a -> E.Encoding) -> TestTree
    testGroupIntegral name isW e = testGroup name $
      [ testCase "0" $ enc e 0 @?= "i0e"
      , testCase "32" $ enc e 32 @?= "i32e"
      ] ++
      [ testCase "-32" $ enc e (-32) @?= "i-32e" | not isW ] ++
      [ testCase "minBound" $ enc e minBound @?= "i" <> BLC.pack (show (minBound :: a)) <> "e"
      , testCase "maxBound" $ enc e maxBound @?= "i" <> BLC.pack (show (maxBound :: a)) <> "e"
      ]

enc :: (a -> E.Encoding) -> a -> BL.ByteString 
enc f = BB.toLazyByteString . E.toBuilder . f

encodeDecodeTests :: TestTree
encodeDecodeTests = testGroup "EncodeDecode, decode . encode == Right"
  [ testProperty "Value" $
      \v -> (D.decode D.value . toBS . E.value) v === Right v
  , testProperty "Val" $
      \(TV t v) ->
        (fmap Clean . D.decode (mkParser t) . toBS . encodeVal) v
        === Right (Clean v)
  ]
  where
    toBS = BL.toStrict . BB.toLazyByteString . E.toBuilder

instance Arbitrary Ben.Value where
  arbitrary = sized $ \n -> do
    n' <- choose (0,n)
    go (n'+1)
    where
      go 1 = do
        sOrI <- arbitrary
        if sOrI
        then Ben.String <$> arbitrary
        else Ben.Integer <$> arbitrary
      go n = do
        ns <- partition (n-1)
        lOrD <- arbitrary
        if lOrD
        then Ben.List . V.fromList <$> traverse go ns
        else Ben.Dict . M.fromList
               <$> traverse (\n' -> (,) <$> arbitrary <*> go n') ns

  shrink (Ben.String s)  = Ben.String <$> shrink s
  shrink (Ben.Integer i) = Ben.Integer <$> shrink i
  shrink (Ben.List xs)   = Ben.List . V.fromList <$> shrink (V.toList xs)
  shrink (Ben.Dict kxs)  = Ben.Dict . M.fromList <$> shrink (M.toList kxs)

instance Arbitrary B.ByteString where
  arbitrary = B.pack <$> arbitrary
  shrink = map B.pack . shrink . B.unpack

-- The code below generates some random "type" (Typ) together with a "value"
-- (Val) of that type.
-- The Typ describes how to generate a value and how to parse a value.
-- A Val can have some junk which will be encoded but not decoded, to test that
-- parsers which are supposed to ignore data do so (field and index).

data Val
  = String !B.ByteString
  | Integer !Integer
  | List !(V.Vector Val)
  | Dict !(M.Map B.ByteString Val)
  | Int !Int
  | Word !Word
  | Fields
      !(M.Map B.ByteString Val) -- ^ contents
      !(M.Map B.ByteString Val) -- ^ junk
  | Fields' !(M.Map B.ByteString Val)
  | Index
      !(V.Vector Val) -- ^ contents and junk together
      !(V.Vector Int) -- ^ indices of the contents
  | Elems !(V.Vector Val)
  deriving Show

data Sized a = Sized !Int !a deriving Show

data Typ
  = TString
  | TInteger
  | TList !(Sized Typ)
  | TDict !(Sized Typ)
  | TInt
  | TWord
  | TFields !(M.Map B.ByteString (Sized Typ)) !(M.Map B.ByteString (Sized Typ))
  | TFields' !(M.Map B.ByteString (Sized Typ))
  | TIndex !(V.Vector (Sized Typ)) !(V.Vector Int)
  | TElems !(V.Vector (Sized Typ))
  deriving Show

-- Newtype to compare the non-junk parts of a Val via Eq.
newtype Clean = Clean Val deriving Show

instance Eq Clean where
  Clean x1 == Clean x2 = case (x1,x2) of
    (String s1   , String s2   ) -> s1 == s2
    (Integer i1  , Integer i2  ) -> i1 == i2
    (List l1     , List l2     ) -> fmap Clean l1 == fmap Clean l2
    (Dict d1     , Dict d2     ) -> fmap Clean d1 == fmap Clean d2
    (Int i1      , Int i2      ) -> i1 == i2
    (Word w1     , Word w2     ) -> w1 == w2
    (Fields m1 _ , Fields m2 _ ) -> fmap Clean m1 == fmap Clean m2
    (Fields' m1  , Fields' m2  ) -> fmap Clean m1 == fmap Clean m2
    (Index l1 is1, Index l2 is2) -> fmap Clean (V.backpermute l1 is1) ==
                                    fmap Clean (V.backpermute l2 is2)
    (Elems l1    , Elems l2    ) -> fmap Clean l1 == fmap Clean l2
    _                            -> False

encodeVal :: Val -> E.Encoding
encodeVal x = case x of
  String s     -> E.string s
  Integer i    -> E.integer i
  List l       -> E.list encodeVal l
  Dict d       -> E.dict encodeVal d
  Int i        -> E.int i
  Word w       -> E.word w
  Fields m1 m2 -> E.dict' $
                    M.foldMapWithKey (\k -> E.field k encodeVal) (m1 <> m2)
  Fields' m    -> E.dict' $ M.foldMapWithKey (\k -> E.field k encodeVal) m
  Index l _    -> E.list encodeVal l
  Elems l      -> E.list encodeVal l

mkParser :: Sized Typ -> D.Parser Val
mkParser (Sized _ t) = case t of
  TString  -> String <$> D.string
  TInteger -> Integer <$> D.integer
  TList t' -> List <$> D.list (mkParser t')
  TDict t' -> Dict <$> D.dict (mkParser t')
  TInt     -> Int <$> D.int
  TWord    -> Word <$> D.word
  TFields m1 _ ->
    Fields <$> M.traverseWithKey (\k -> D.field k . mkParser) m1
           <*> pure M.empty
  TFields' m ->
    Fields' <$> D.dict' (M.traverseWithKey (\k -> D.field' k . mkParser) m)
  TIndex l is ->
    Index <$> traverse (\i -> D.index i (mkParser (l V.! i))) is
          <*> pure (V.generate (V.length is) id)
  TElems l -> Elems <$> D.list' (traverse (D.elem . mkParser) l)

data TV = TV !(Sized Typ) !Val deriving Show

instance Arbitrary TV where
  arbitrary = sized $ \n -> do
    n' <- choose (0,n)
    go (n'+1)
    where
      go n = do
        t <- genTyp n
        v <- genVal n t
        pure $ TV t v

-- | Generate a Typ and its minimum number of nodes, such that it is <= n.
genTyp :: Int -> Gen (Sized Typ)
genTyp n | n <= 0 = error "genTyp n | n <= 0"
genTyp 1 = Sized 1 <$> elements [TString, TInteger, TInt, TWord]
genTyp n = oneof
  [ Sized 1 . TList <$> genTypMany (n-1)
  , Sized 1 . TDict <$> genTypMany (n-1)
  , do
      n' <- choose (0, n-1)
      Sized n1 m1 <- goMap n'
      Sized n2 m2 <- goMap (n-1-n')
      pure $ Sized (n1+n2+1) (TFields m1 m2)
  , do
      Sized n1 m1 <- goMap (n-1)
      pure $ Sized (n1+1) (TFields' m1)
  , do
      Sized n1 v1 <- goVec (n-1)
      is <- V.fromList <$> sublistOf [0 .. V.length v1 - 1]
      pure $ Sized (n1+1) (TIndex v1 is)
  , do
      Sized n1 v1 <- goVec (n-1)
      pure $ Sized (n1+1) (TElems v1)
  ]
  where
    genTypMany m = partition m >>= genTyp . minimum
    goMap m = do
      ms <- partition m
      kvs <- traverse (\m' -> (,) <$> arbitrary <*> genTyp m') ms
      pure $ sizedF (M.fromList kvs)
    goVec m = do
      ms <- partition m
      sizedF . V.fromList <$> traverse genTyp ms
    sizedF xs = Sized (sizeF xs) xs

-- | Generate a value with at most n nodes
genVal :: Int -> Sized Typ -> Gen Val
genVal n (Sized n' _) | n < n' = error "genVal: n < size of type"
genVal n (Sized _ t) = case t of
  TString -> String <$> arbitrary
  TInteger -> Integer <$> arbitrary
  TList t' -> List . V.fromList <$> goMany (n-1) t'
  TDict t' -> do
    vs <- goMany (n-1) t'
    ks <- replicateM (length vs) arbitrary
    pure $ Dict $ M.fromList $ zip ks vs
  TInt -> Int <$> arbitrary
  TWord -> Word <$> arbitrary
  TFields mp1 mp2 -> do
    ~[m1,m2] <- partitionWithMin (n-1) [sizeF mp1, sizeF mp2]
    Fields <$> goMap m1 mp1 <*> goMap m2 mp2
  TFields' mp -> Fields' <$> goMap (n-1) mp
  TIndex l is -> flip Index is . V.fromList <$> goVec (n-1) (F.toList l)
  TElems l -> Elems . V.fromList <$> goVec (n-1) (F.toList l)
  where
    goMany m t'@(Sized n' _) = do
      ns <- partitionWithMin m (replicate (div m n') n')
      traverse (flip genVal t') ns
    goMap m mp = do
      ns <- partitionWithMin m (sizes (M.elems mp))
      M.fromList . zip (M.keys mp) <$> zipWithM genVal ns (M.elems mp)
    goVec m l = do
      ns <- partitionWithMin m (sizes l)
      zipWithM genVal ns l

sizeF :: Foldable f => f (Sized a) -> Int
sizeF = F.foldl' (\acc (Sized n _) -> acc + n) 0

sizes :: Functor f => f (Sized a) -> f Int
sizes = fmap (\(Sized n _) -> n)

-- | Partition m into pieces with a given minimum value of each piece.
partitionWithMin :: Int -> [Int] -> Gen [Int]
partitionWithMin m xs | m < sum xs = error "partitionWithMin: n not big enough"
partitionWithMin m xs = zipWith (+) xs <$> partitionN (length xs) (m - sum xs)

-- | Partition m into n pieces. Pieces can be 0.
partitionN :: Int -> Int -> Gen [Int]
partitionN n0 _ | n0 <= 0 = error "partitionN: n <= 0"
partitionN n0 m0 = go n0 m0 >>= shuffle
  where
    go 1 m = pure [m]
    go n 0 = (0:) <$> go (n-1) 0
    go n m = do
      x <- choose (1,m)
      (x:) <$> go (n-1) (m-x)

-- | Partition into pieces >= 0
partition :: Int -> Gen [Int]
partition m0 | m0 < 0 = error "partition: m < 0"
partition m0 = go m0 >>= shuffle
  where
    go 0 = pure []
    go m = do
      x <- choose (1,m)
      (x:) <$> go (m-x)
