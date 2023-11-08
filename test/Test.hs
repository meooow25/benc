{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}  -- Arbitrary instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map as M
import qualified Data.Vector as V

import qualified Data.Bencode.Decode as D
import qualified Data.Bencode.Encode as E
import qualified Data.Bencode.Type as Ben

main :: IO ()
main = defaultMain $ testGroup "Tests"
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
    , testCase "i0e" $ D.decode D.string "i0e" @?= Left "TypeMismatch String Integer"
    , testCase "le" $ D.decode D.string "le" @?= Left "TypeMismatch String List"
    , testCase "de" $ D.decode D.string "de" @?= Left "TypeMismatch String Dict"
    ]
  , testGroup "integer"
    [ testCase "i0e" $ D.decode D.integer "i0e" @?= Right 0
    , testCase "i-32e" $ D.decode D.integer "i-32e" @?= Right (-32)
    , testCase "i98765432109876543210e" $ D.decode D.integer "i98765432109876543210e" @?= Right 98765432109876543210
    , testCase "0:" $ D.decode D.integer "0:" @?= Left "TypeMismatch Integer String"
    , testCase "le" $ D.decode D.integer "le" @?= Left "TypeMismatch Integer List"
    , testCase "de" $ D.decode D.integer "de" @?= Left "TypeMismatch Integer Dict"
    ]
  , testGroup "list"
    [ testCase "le" $ D.decode (D.list D.value) "le" @?= Right []
    , testCase "l3:foo3:bare" $ D.decode (D.list D.string) "l3:foo3:bare" @?= Right ["foo", "bar"]
    , testCase "l3:fooi1ee" $ D.decode (D.list D.string) "l3:fooi1ee" @?= Left "TypeMismatch String Integer"
    , testCase "0:" $ D.decode (D.list D.value) "0:" @?= Left "TypeMismatch List String"
    , testCase "i0e" $ D.decode (D.list D.value) "i0e" @?= Left "TypeMismatch List Integer"
    , testCase "de" $ D.decode (D.list D.value) "de" @?= Left "TypeMismatch List Dict"
    ]
  , testGroup "dict"
    [ testCase "de" $ D.decode (D.dict D.value) "de" @?= Right []
    , testCase "d3:foo3:bare" $ D.decode (D.dict D.string) "d3:foo3:bare" @?= Right [("foo", "bar")]
    , testCase "d3:foo3:bar3:quxi1ee" $ D.decode (D.dict D.string) "d3:foo3:bar3:quxi1ee" @?= Left "TypeMismatch String Integer"
    , testCase "0:" $ D.decode (D.dict D.value) "0:" @?= Left "TypeMismatch Dict String"
    , testCase "i0e" $ D.decode (D.dict D.value) "i0e" @?= Left "TypeMismatch Dict Integer"
    , testCase "le" $ D.decode (D.dict D.value)"le" @?= Left "TypeMismatch Dict List"
    ]
  , testGroup "text"
    [ testCase "0:" $ D.decode D.text "0:" @?= Right ""
    , testCase "3:foo" $ D.decode D.text "3:foo" @?= Right "foo"
    , testCase "こんにちは" $ D.decode D.text "15:\227\129\147\227\130\147\227\129\171\227\129\161\227\129\175" @?= Right "こんにちは"
    , testCase "<invalid UTF-8>" $ D.decode D.text "2:\xd8\x00" @?= Left "UTF8DecodeFailure"
    , testCase "i0e" $ D.decode D.text"i0e" @?= Left "TypeMismatch String Integer"
    , testCase "le" $ D.decode D.text"le" @?= Left "TypeMismatch String List"
    , testCase "de" $ D.decode D.text"de" @?= Left "TypeMismatch String Dict"
    ]
  , testGroup "int"
    [ testCase "i0e" $ D.decode D.int "i0e" @?= Right 0
    , testCase "i-32e" $ D.decode D.int "i-32e" @?= Right (-32)
    , let x = show (fromIntegral (minBound :: Int) - 1 :: Integer) in
      testCase "minBound-1" $ D.decode D.int ("i" <> BC.pack x <> "e") @?= Left "IntOutOfBounds"
    , testCase "minBound" $ D.decode D.int ("i" <> BC.pack (show (minBound :: Int)) <> "e") @?= Right minBound
    , testCase "maxBound" $ D.decode D.int ("i" <> BC.pack (show (maxBound :: Int)) <> "e") @?= Right maxBound
    , let x = show (fromIntegral (maxBound :: Int) + 1 :: Integer) in
      testCase "maxBound+1" $ D.decode D.int ("i" <> BC.pack x <> "e") @?= Left "IntOutOfBounds"
    , testCase "i98765432109876543210e" $ D.decode D.int "i98765432109876543210e" @?= Left "IntOutOfBounds"
    , testCase "0:" $ D.decode D.int "0:" @?= Left "TypeMismatch Integer String"
    , testCase "le" $ D.decode D.int "le" @?= Left "TypeMismatch Integer List"
    , testCase "de" $ D.decode D.int "de" @?= Left "TypeMismatch Integer Dict"
    ]
  , testGroup "word"
    [ testCase "i0e" $ D.decode D.word "i0e" @?= Right 0
    , testCase "i-1e" $ D.decode D.word "i-1e" @?= Left "WordOutOfBounds"
    , testCase "maxBound" $ D.decode D.word ("i" <> BC.pack (show (maxBound :: Word)) <> "e") @?= Right maxBound
    , let x = show (fromIntegral (maxBound :: Word) + 1 :: Integer) in
      testCase "maxBound+1" $ D.decode D.word ("i" <> BC.pack x <> "e") @?= Left "WordOutOfBounds"
    , testCase "i98765432109876543210e" $ D.decode D.word "i98765432109876543210e" @?= Left "WordOutOfBounds"
    , testCase "0:" $ D.decode D.word "0:" @?= Left "TypeMismatch Integer String"
    , testCase "le" $ D.decode D.word "le" @?= Left "TypeMismatch Integer List"
    , testCase "de" $ D.decode D.word "de" @?= Left "TypeMismatch Integer Dict"
    ]
  , testGroup "field"
    [ testCase "d3:foo3:bare" $ D.decode (D.field "foo" D.string) "d3:foo3:bare" @?= Right "bar"
    , testCase "d3:fooi2ee" $ D.decode (D.field "foo" D.string) "d3:fooi2ee" @?= Left "TypeMismatch String Integer"
    , let p = (,,) <$> D.field "one" D.integer
                   <*> D.field "two" D.string
                   <*> D.field "three" (D.list D.integer) in
      testCase "d3:onei1e5:threeli0ei0ei0ee3:two3:twoe" $
        D.decode p "d3:onei1e5:threeli0ei0ei0ee3:two3:twoe" @?= Right (1, "two", [0,0,0])
    ]
  , testGroup "fail"
    [ testCase "3:foo" $ D.decode (D.fail "error!" :: D.Parser B.ByteString) "3:foo" @?= Left "Fail: error!"
    ]
  ]

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
  , testGroup "int"
    [ testCase "0" $ enc E.int 0 @?= "i0e"
    , testCase "1" $ enc E.int 1 @?= "i1e"
    , testCase "-1" $ enc E.int (-1) @?= "i-1e"
    , testCase "minBound" $ enc E.int minBound @?= "i" <> BLC.pack (show (minBound :: Int)) <> "e"
    , testCase "maxBound" $ enc E.int maxBound @?= "i" <> BLC.pack (show (maxBound :: Int)) <> "e"
    ]
  , testGroup "word"
    [ testCase "0" $ enc E.word 0 @?= "i0e"
    , testCase "1" $ enc E.word 1 @?= "i1e"
    , testCase "minBound" $ enc E.word minBound @?= "i0e"
    , testCase "maxBound" $ enc E.word maxBound @?= "i" <> BLC.pack (show (maxBound :: Word)) <> "e"
    ]
  , testGroup "field"
    [ testCase "{}" $ enc id (E.dict' mempty) @?= "de"
    , let e =  E.dict' $
               E.field "one" E.integer 1
            <> E.field "two" E.string "two"
            <> E.field "three" (E.list E.integer) [0,0,0] in
      testCase "{one:1,two:two,three:[0,0,0]}" $ enc id e @?= "d3:onei1e5:threeli0ei0ei0ee3:two3:twoe"
    ]
  ]

enc :: (a -> E.Encoding) -> a -> BL.ByteString 
enc f = BB.toLazyByteString . E.toBuilder . f

encodeDecodeTests :: TestTree
encodeDecodeTests = testGroup "EncodeDecode"
  [ testProperty "decode . encode == Right" $ withMaxSuccess 1000 $
      \v -> ( D.decode D.value .
              BL.toStrict .
              BB.toLazyByteString .
              E.toBuilder .
              E.value ) v
            === Right v
  ]

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
        ns <- partition n >>= shuffle
        lOrD <- arbitrary
        if lOrD
        then Ben.List . V.fromList <$> traverse go ns
        else Ben.Dict . M.fromList
               <$> traverse (\n' -> (,) <$> arbitrary <*> go n') ns
      partition 0 = pure []
      partition n = do
        x <- choose (1,n)
        (x:) <$> partition (n-x)

instance Arbitrary B.ByteString where
  arbitrary = B.pack <$> arbitrary
