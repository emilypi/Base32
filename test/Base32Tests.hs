{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
module Main
( main
, tests
) where


import Data.Bifunctor
import Data.ByteString (ByteString)
import "base32" Data.ByteString.Base32 as B32
import "base32" Data.ByteString.Base32.Hex as B32H
import "memory" Data.ByteArray.Encoding as Mem
import Data.ByteString.Random (random)
import Data.Functor (void)
import Data.Text (pack)

import Test.Tasty
import Test.Tasty.HUnit


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Base32 Tests"
    [ testVectors
    , sanityTests
    , alphabetTests
    ]

testVectors :: TestTree
testVectors = testGroup "RFC 4648 Test Vectors"
    [ testGroup "encode/decode"
      [ testCaseB32 "" ""
      , testCaseB32 "f" "MY======"
      , testCaseB32 "fo" "MZXQ===="
      , testCaseB32 "foo" "MZXW6==="
      , testCaseB32 "foob" "MZXW6YQ="
      , testCaseB32 "fooba" "MZXW6YTB"
      , testCaseB32 "foobar" "MZXW6YTBOI======"
      ]
    , testGroup "encode/decode hex"
      [ testCaseB32' "" ""
      , testCaseB32' "f" "CO======"
      , testCaseB32' "fo" "CPNG===="
      , testCaseB32' "foo" "CPNMU==="
      , testCaseB32' "foob" "CPNMUOG="
      , testCaseB32' "fooba" "CPNMUOJ1"
      , testCaseB32' "foobar" "CPNMUOJ1E8======"
      ]
    ]
  where
    testCaseB32 s t =
      testCaseSteps (show $ if s == "" then "empty" else s) $ \step -> do
        let t' = B32.encodeBase32' s
            s' = B32.decodeBase32 t'

        step "compare encoding + decoding w/ padding"
        t @=? t'

        step "compare decoding w/ padding"
        Right s @=? s'

    testCaseB32' s t =
      testCaseSteps (show $ if s == "" then "empty" else s) $ \step -> do
        let t' = B32H.encodeBase32' s
            s' = B32H.decodeBase32 t'
            u = B32H.encodeBase32' s
            v = B32H.decodeBase32 u

        step "compare hex encoding w/ padding"
        t @=? t'

        step "compare hex decoding w/ padding"
        Right s @=? s'

        step "compare hex encoding w/o padding"
        t @=? t'

        step "compare hex decoding w/o padding"
        Right s @=? v

sanityTests :: TestTree
sanityTests = testGroup "Sanity tests"
    [ testGroup "very large bytestrings don't segfault"
        [ chonk
        ]
    , testGroup "`memory` sanity checks"
        [ compare32 3
        , compare32 4
        , compare32 5
        , compare32 6
        , compare32 1000
        , compare32 100000
        ]
    , testGroup "roundtrip encode/decode"
        [ roundtrip 3
        , roundtrip 4
        , roundtrip 5
        , roundtrip 1000
        , roundtrip 100000
        ]
    ]
  where
    chonk = testCase ("Encoding huge bytestrings doesn't result in OOM or segfault") $ do
      bs <- random 1000000
      void $ return $ B32.encodeBase32' bs
      void $ return $ B32H.encodeBase32' bs

    compare32 n = testCase ("Testing " ++ show n ++ "-sized bytestrings") $ do
      bs <- random n
      B32.encodeBase32' bs @=? Mem.convertToBase Mem.Base32 bs
      B32.decodeBase32 (B32.encodeBase32' bs) @=?
        first pack (Mem.convertFromBase @ByteString Mem.Base32 (Mem.convertToBase Mem.Base32 bs))

    roundtrip n = testCase ("Roundtrip encode/decode for " ++ show n ++ "-sized bytestrings") $ do
      bs <- random n
      B32.decodeBase32 (B32.encodeBase32' bs) @=? Right bs
      B32H.decodeBase32 (B32H.encodeBase32' bs) @=? Right bs

alphabetTests :: TestTree
alphabetTests = testGroup "Alphabet tests"
    [ base32Tests 0
    , base32Tests 4
    , base32Tests 5
    , base32Tests 6
    , base32Tests 100
    , base32HexTests 0
    , base32HexTests 4
    , base32HexTests 5
    , base32HexTests 6
    , base32HexTests 100
    ]
  where
    base32Tests n = testCase ("Conforms to Base32 alphabet: " ++ show n) $ do
      bs <- random n
      let b = B32.encodeBase32' bs
      assertBool ("failed validity: " ++ show b) $ B32.isValidBase32 b
      assertBool ("failed correctness: " ++ show b) $ B32.isBase32 b

    base32HexTests n = testCase ("Conforms to Base32hex alphabet: " ++ show n) $ do
      bs <- random n
      let b = B32H.encodeBase32' bs
      assertBool ("failed validity: " ++ show b) $ B32H.isValidBase32Hex b
      assertBool ("failed correctness: " ++ show b) $ B32H.isBase32Hex b
