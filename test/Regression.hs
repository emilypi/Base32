{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Regression (regressions) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.DeepSeq (force)
import Control.Exception (ErrorCall, bracket, evaluate, try)
import Control.Monad (forM_, forever)
import Data.Base32.Types
import Data.Bits (xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base32 as B
import qualified Data.ByteString.Base32.Hex as H
import Data.ByteString.Base32.Internal (decodeBase32_, encodeBase32_)
import Data.ByteString.Base32.Internal.Short (decodeShort, encodeShort)
import Data.ByteString.Base32.Internal.Tables (stdDecodeTable)
import Data.ByteString.Base32.Internal.Utils (encodedLength)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Base32 as L
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Base32 as S
import qualified Data.ByteString.Short.Base32.Hex as SH
import Data.Either (isLeft, isRight)
import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Encoding.Base32 as T
import qualified Data.Text.Lazy.Encoding.Base32 as TL
import qualified Data.Text.Short as TS
import qualified Data.Text.Short.Encoding.Base32 as TS
import Foreign.Marshal.Array (peekArray, withArray)
import Internal
import System.Mem (performGC)
import System.Timeout (timeout)
import Test.QuickCheck hiding (label)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

regressions :: TestTree
regressions =
  testGroup
    "Release regressions"
    [ testCase "custom alphabets and tables retain scalar semantics" $ do
        let input = BS.pack (take 1000 (cycle [0 .. 255]))
            alphabet = "ACBDEFGHIJKLMNOPQRSTUVWXYZ234567"#
            swap w
              | w == 66 = 67
              | w == 67 = 66
              | otherwise = w
            expected = BS.map swap (extractBase32 (B.encodeBase32' input))
        encodeBase32_ alphabet input @?= expected
        encodeShort alphabet True (SBS.toShort input) @?= SBS.toShort expected
        table <- peekArray 256 stdDecodeTable
        let replace i w
              | i == 66 = 2
              | i == 67 = 1
              | otherwise = w
        withArray (zipWith replace [0 :: Int ..] table) $ \custom -> do
          decodeBase32_ custom expected >>= (@?= Right input)
          decodeShort custom OptionalPadding (SBS.toShort expected)
            @?= Right (SBS.toShort input)
    , testCase "allocation lengths reject overflow before allocation" $ do
        forM_ [-1, maxBound] $ \n -> do
          result <- try (evaluate (encodedLength n)) :: IO (Either ErrorCall Int)
          assertBool "expected checked capacity failure" (isLeft result)
        map encodedLength [0 .. 5] @?= [0, 8, 8, 8, 8, 8]
    , testCase "strict codecs handle sliced inputs" $
        forM_ [0 .. 31] $ \offset -> do
          let payload = BS.pack (take 1001 (cycle [0 .. 255]))
              input = BS.drop offset (BS.replicate offset 99 <> payload)
              encoded = extractBase32 (B.encodeBase32' input)
              sliced = BS.drop offset (BS.replicate offset 99 <> encoded)
          B.decodeBase32Untyped sliced @?= Right payload
          B.decodeBase32 (assertBase32 @'StdPadded sliced) @?= payload
    , parserTests b32
    , parserTests lb32
    , parserTests sb32
    , parserTests t32
    , parserTests tl32
    , parserTests ts32
    , testCase "strict and short SIMD boundaries, both alphabets" $
        forM_ ([0 .. 170] ++ [255, 256, 257, 1023, 1024, 1025, 100000]) $ \n -> do
          let input = BS.pack (take n (cycle [0 .. 255]))
              short = SBS.toShort input
              encoded = B.encodeBase32' input
              encodedHex = H.encodeBase32' input
          extractBase32 (S.encodeBase32' short) @?= SBS.toShort (extractBase32 encoded)
          extractBase32 (SH.encodeBase32' short)
            @?= SBS.toShort (extractBase32 encodedHex)
          B.decodeBase32 encoded @?= input
          H.decodeBase32 encodedHex @?= input
          S.decodeBase32 (S.encodeBase32' short) @?= short
          SH.decodeBase32 (SH.encodeBase32' short) @?= short
          B.decodeBase32 (B.encodeBase32Unpadded' input) @?= input
          S.decodeBase32 (S.encodeBase32Unpadded' short) @?= short
    , testCase "typed lazy decoding streams without whole-input validation" $ do
        output <- timeout 1000000 $ evaluate $ force $
          BL.take 16 (L.decodeBase32 (L.encodeBase32' (BL.repeat 255)))
        output @?= Just (BL.replicate 16 255)
    , testCase "parsed padding provenance selects the matching typed decoder" $ do
        fmap B.decodeBase32Padded (B.parseBase32Padded "MY======") @?= Right "f"
        fmap H.decodeBase32Unpadded (H.parseBase32Unpadded "CO") @?= Right "f"
        fmap S.decodeBase32Padded (S.parseBase32Padded "MY======") @?= Right "f"
        fmap SH.decodeBase32Unpadded (SH.parseBase32Unpadded "CO") @?= Right "f"
    , testCase "lazy padding at every chunk boundary" $ do
        let encoded = extractBase32 (B.encodeBase32' (BS.replicate 81 102))
        forM_ [0 .. BS.length encoded] $ \i -> do
          let chunks = BL.fromChunks [BS.take i encoded, BS.drop i encoded]
          fmap extractBase32 (L.parseBase32Padded chunks) @?= Right chunks
          L.decodeBase32Untyped chunks @?= Right (BL.replicate 81 102)
          L.decodeBase32 (assertBase32 @'StdPadded chunks) @?= BL.replicate 81 102
    , testCase "invalid bytes after SIMD prefixes keep global offsets" $
        forM_ [0 .. 159] $ \i -> do
          let input = BS.replicate i 65 <> "!" <> BS.replicate (159 - i) 65
              expected = Left (T.pack ("invalid character at offset: " ++ show i))
          B.decodeBase32Untyped input @?= expected
          S.decodeBase32Untyped (SBS.toShort input) @?= expected
    , testProperty "strict and short malformed inputs agree" $ \(bytes :: [Word]) ->
        let input = BS.pack (map fromIntegral bytes)
            short = SBS.toShort input
         in conjoin
              [ fmap SBS.toShort (B.decodeBase32Untyped input)
                  === S.decodeBase32Untyped short
              , fmap SBS.toShort (H.decodeBase32Untyped input)
                  === SH.decodeBase32Untyped short
              , fmap SBS.toShort (B.decodeBase32PaddedUntyped input)
                  === S.decodeBase32PaddedUntyped short
              , fmap SBS.toShort (B.decodeBase32UnpaddedUntyped input)
                  === S.decodeBase32UnpaddedUntyped short
              ]
    , testCase "text parsers accept binary; decoders check UTF-8" $ do
        assertBool "binary parser" (isRight (T.parseBase32Padded "74======"))
        assertBool "invalid UTF-8" (isLeft (T.decodeBase32Untyped "74======"))
        assertBool "lazy invalid UTF-8" (isLeft (TL.decodeBase32Untyped "74======"))
        assertBool "short invalid UTF-8" (isLeft (TS.decodeBase32Untyped "74======"))
    , testCase "short arrays remain valid during concurrent collection" $
        bracket (forkIO (forever (performGC >> threadDelay 1000))) killThread $ \_ ->
          forM_ [0 .. 99 :: Int] $ \salt -> do
            let input =
                  SBS.toShort
                    ( BS.pack
                        [fromIntegral (i `xor` salt) | i <- [0 .. 8191 :: Int]]
                    )
            output <- evaluate (force (S.decodeBase32 (S.encodeBase32' input)))
            output @?= input
    , testCase "typed text roundtrip" $ do
        T.decodeBase32 (T.encodeBase32 "λ☃") @?= "λ☃"
        TS.decodeBase32 (TS.encodeBase32 (TS.fromText "λ☃")) @?= TS.fromText "λ☃"
    ]

parserTests ::
  (Arbitrary a, IsString a, Eq a, Show a) =>
  Harness a ->
  TestTree
parserTests Harness{..} =
  testGroup
    (label ++ " parsers")
    [ testProperty "typed standard decoding preserves padding provenance" $ \input ->
        let padded = assertBase32 @'StdPadded (encode input)
            unpadded = assertBase32 @'StdUnpadded (encodeNopad input)
         in conjoin
              [ decodeTyped padded === input
              , decodeTyped unpadded === input
              , decodePaddedTyped padded === input
              , decodeUnpaddedTyped unpadded === input
              ]
    , testProperty "typed hex decoding preserves padding provenance" $ \input ->
        let padded = assertBase32 @'HexPadded (encodeHex input)
            unpadded = assertBase32 @'HexUnpadded (encodeHexNopad input)
         in conjoin
              [ decodeHexTyped padded === input
              , decodeHexTyped unpadded === input
              , decodeHexPaddedTyped padded === input
              , decodeHexUnpaddedTyped unpadded === input
              ]
    , testProperty "preserve padded standard input" $ \input ->
        parsePaddedValue (encode input) === Right (encode input)
    , testProperty "preserve unpadded hex input" $ \input ->
        parseHexUnpaddedValue (encodeHexNopad input) === Right (encodeHexNopad input)
    , testCase "preserve original letter case" $ do
        parseValue "my======" @?= Right "my======"
        parseHexValue "co======" @?= Right "co======"
        decodeTyped (assertBase32 @'StdPadded "my======") @?= "f"
        decodeHexTyped (assertBase32 @'HexPadded "co======") @?= "f"
    , testCase "enforce padding policies" $ do
        assertBool "padded requires padding" (isLeft (parsePaddedValue "MY"))
        assertBool "unpadded rejects padding" (isLeft (parseUnpaddedValue "MY======"))
        assertBool "hex padded requires padding" (isLeft (parseHexPaddedValue "CO"))
        assertBool
          "hex unpadded rejects padding"
          (isLeft (parseHexUnpaddedValue "CO======"))
        parseValue "" @?= Right ""
        parseHexValue "" @?= Right ""
    , testCase "reject nonzero unused bits in all tail lengths" $ do
        forM_
          [ "MZ"
          , "MZXR"
          , "MZXW7"
          , "MZXW6YR"
          , "MZ======"
          , "MZXR===="
          , "MZXW7==="
          , "MZXW6YR="
          ]
          $ \value ->
            assertBool (show value)
              (isLeft (parsePaddedValue value) && isLeft (parseUnpaddedValue value))
        forM_
          [ "CP"
          , "CPNH"
          , "CPNMV"
          , "CPNMUOH"
          , "CP======"
          , "CPNH===="
          , "CPNMV==="
          , "CPNMUOH="
          ]
          $ \value ->
            assertBool (show value)
              (isLeft (parseHexPaddedValue value) && isLeft (parseHexUnpaddedValue value))
    , testCase "parser and checked decoder report the same errors" $
        forM_ ["M", "!Y======", "MZ======", "MY====", "MY======MY======"] $ \value ->
          case decodePad value of
            Left err -> parseValue value @?= Left err
            Right _ -> assertFailure "expected malformed fixture"
    ]
