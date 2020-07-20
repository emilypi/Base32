{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module       : Main
-- Copyright    : (c) 2019-2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- This module contains the test implementation for the `base32` package
--
module Main
( main
) where


import Prelude hiding (length)

import Data.Bifunctor (second)
import qualified "memory" Data.ByteArray.Encoding as Mem
import qualified Data.ByteString as BS
import Data.ByteString.Internal (c2w)
import "base32" Data.ByteString.Base32 as B32
import "base32" Data.ByteString.Base32.Hex as B32H
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Encoding.Base32.Error (Base32Error(..))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Short as TS
import Data.Word

import Internal

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Base32 Tests"
  [ mkTree b32
    [ mkPropTree
    , mkUnitTree
    ]
  , mkTree bl32
    [ mkPropTree
    , mkUnitTree
    ]
  , mkTree bs32
    [ mkPropTree
    , mkUnitTree
    ]
  , mkTree t32
    [ mkPropTree
    , mkUnitTree
    , mkDecodeTree T.decodeUtf8' b32
    ]
  , mkTree tl32
    [ mkPropTree
    , mkUnitTree
    , mkDecodeTree TL.decodeUtf8' bl32
    ]
  , mkTree ts32
    [ mkPropTree
    , mkUnitTree
    , mkDecodeTree
      (second TS.fromText . T.decodeUtf8' . SBS.fromShort) bs32
    ]
  ]

-- ---------------------------------------------------------------- --
-- Test tree generation

-- | Make a test tree for a given label
--
mkTree
  :: forall a b proxy
  . Harness a b
  => proxy a
  -> [proxy a -> TestTree]
  -> TestTree
mkTree a = testGroup (label @a) . fmap ($ a)

-- | Make a test group with some name, lifting a test tree up to the correct
-- type information via some Harness
--
mkTests
  :: forall a b proxy
  . Harness a b
  => String
  -> [proxy a -> TestTree]
  -> proxy a
  -> TestTree
mkTests context ts = testGroup context . (<*>) ts . pure

-- | Make property tests for a given harness instance
--
mkPropTree :: forall a b proxy. Harness a b => proxy a -> TestTree
mkPropTree = mkTests "Property Tests"
  [ prop_roundtrip
  , prop_correctness
  , prop_padding_invariants
  , const prop_mem_coherence
  ]

-- | Make unit tests for a given harness instance
--
mkUnitTree
  :: forall a b proxy
  . Harness a b
  => proxy a
  -> TestTree
mkUnitTree = mkTests "Unit tests"
  [ rfcVectors
  , offsetVectors
  , validityTests
  ]

-- | Make unit tests for textual 'decode*With' functions
--
mkDecodeTree
  :: forall t a b c e proxy
  . ( TextHarness a b c
    , Harness t c
    , Show e
    )
  => (c -> Either e b)
  -> proxy t
  -> proxy a
  -> TestTree
mkDecodeTree utf8 t = mkTests "Decoding tests"
  [ decodeWithVectors utf8 t
  ]

-- ---------------------------------------------------------------- --
-- Property tests

prop_roundtrip :: forall a b proxy. Harness a b => proxy a -> TestTree
prop_roundtrip _ = testGroup "prop_roundtrip"
  [ testProperty "prop_std_roundtrip" $ \(bs :: b) ->
      Right (encode bs) == decode (encode (encode bs))
  , testProperty "prop_hex_roundtrip" $ \(bs :: b) ->
      Right (encodeHex bs) == decodeHex (encodeHex (encodeHex bs))
  , testProperty "prop_hex_roundtrip_nopad" $ \(bs :: b) ->
      Right (encodeHexNopad bs)
        == decodeHexNopad (encodeHexNopad (encodeHexNopad bs))
  ]

prop_correctness :: forall a b proxy. Harness a b => proxy a -> TestTree
prop_correctness _ = testGroup "prop_validity"
  [ testProperty "prop_std_valid" $ \(bs :: b) ->
    validate (encode bs)
  , testProperty "prop_hex_valid" $ \(bs :: b) ->
    validateHex (encodeHex bs)
  , testProperty "prop_std_correct" $ \(bs :: b) ->
    correct (encode bs)
  , testProperty "prop_hex_correct" $ \(bs :: b) ->
    correctHex (encodeHex bs)
  ]

prop_padding_invariants :: forall a b proxy. Harness a b => proxy a -> TestTree
prop_padding_invariants _ = testGroup "prop_padding_invariants"
  [ testProperty "prop_hex_nopad_roundtrip" $ \(bs :: b) ->
      Right (encodeHexNopad bs)
        == decodeHexNopad (encodeHexNopad (encodeHexNopad bs))

  , testProperty "prop_hex_pad_roundtrip" $ \(bs :: b) ->
      Right (encodeHex bs) == decodeHexPad (encodeHex (encodeHex bs))

  , testProperty "prop_hex_decode_invariant" $ \(bs :: b) ->
      ( decodeHexNopad (encodeHexNopad (encodeHex bs))
      == decodeHex (encodeHex (encodeHex bs))
      ) ||
      ( decodeHexPad (encodeHex (encodeHex bs))
      == decodeHex (encodeHex (encodeHex bs))
      )

  , testProperty "prop_std_decode_invariant" $ \(bs :: b) ->
      ( decodeNopad (encodeNopad (encode bs))
      == decode (encode (encode bs))
      ) ||
      ( decodePad (encode (encode bs))
      == decode (encode (encode bs))
      )

  , testProperty "prop_hex_padding_coherence" $ \(bs :: b) ->
      Right (encodeHex bs) == decodeHex (encodeHex (encodeHex bs))
      && Right (encodeHex bs) == decodeHexPad (encodeHex (encodeHex bs))

  , testProperty "prop_hex_nopadding_coherence" $ \(bs :: b) ->
      Right (encodeHexNopad bs) == decodeHexNopad (encodeHexNopad (encodeHexNopad bs))
      && Right (encodeHexNopad bs) == decodeHex (encodeHexNopad (encodeHexNopad bs))
  ]

-- | just a sanity check against `memory`
--
prop_mem_coherence :: TestTree
prop_mem_coherence = testGroup "prop_mem_coherence"
  [ testProperty "prop_std_mem_coherence" $ \bs ->
      Right bs == B32.decodeBase32 (B32.encodeBase32' bs)
      && Right bs == Mem.convertFromBase Mem.Base32 (Mem.convertToBase @BS.ByteString @BS.ByteString Mem.Base32 bs)
  ]

-- ---------------------------------------------------------------- --
-- Unit tests

-- | RFC 4328 test vectors
--
rfcVectors :: forall a b proxy. Harness a b => proxy a -> TestTree
rfcVectors _ = testGroup "RFC 4648 Test Vectors"
    [ testGroup "std alphabet"
      [ testCaseStd "" ""
      , testCaseStd "f" "MY======"
      , testCaseStd "fo" "MZXQ===="
      , testCaseStd "foo" "MZXW6==="
      , testCaseStd "foob" "MZXW6YQ="
      , testCaseStd "fooba" "MZXW6YTB"
      , testCaseStd "foobar" "MZXW6YTBOI======"
      ]
    , testGroup "hex alphabet"
      [ testCaseHex "" ""
      , testCaseHex "f" "CO======"
      , testCaseHex "fo" "CPNG===="
      , testCaseHex "foo" "CPNMU==="
      , testCaseHex "foob" "CPNMUOG="
      , testCaseHex "fooba" "CPNMUOJ1"
      , testCaseHex "foobar" "CPNMUOJ1E8======"
      ]
    ]
  where
    testCaseStd s t =
      testCaseSteps (show $ if s == "" then "empty" else s) $ \step -> do
        step "encode is sound"
        t @=? encode @a s

        step "decode is sound"
        Right s @=? decode (encode s)

    testCaseHex s t =
      testCaseSteps (show $ if s == "" then "empty" else s) $ \step -> do
        step "encode is sound"
        t @=? encodeHex @a s

        step "decode is sound"
        Right s @=? decodeHexPad t

-- | Unit test trees for the `decode*With` family of text-valued functions
--
decodeWithVectors
  :: forall t a b c e proxy
  . ( TextHarness a c b
    , Harness t b
    , Show e
    )
  => (b -> Either e c)
    -- ^ utf8
  -> proxy t
    -- ^ witness to the bytestring-ey dictionaries
  -> proxy a
    -- ^ witness to the text dictionaries
  -> TestTree
decodeWithVectors utf8 _ _ = testGroup "DecodeWith* unit tests"
  [ testGroup "decodeWith negative tests"
    [ testCase "decodeWith non-utf8 inputs on decodeUtf8" $ do
      case decodeWith_ @a utf8 "\1079743" of
        Left (DecodeError _) -> return ()
        _ -> assertFailure "decoding phase"
    , testCase "decodeWith valid utf8 inputs on decodeUtf8" $ do
      case decodeWith_ @a utf8 (encode @t "\1079743") of
        Left (ConversionError _) -> return ()
        _ -> assertFailure "conversion phase"
    , testCase "decodeHexWith non-utf8 inputs on decodeUtf8" $ do
      case decodeHexWith_ @a utf8 "\1079743" of
        Left (DecodeError _) -> return ()
        _ -> assertFailure "decoding phase"
    , testCase "decodeHexWith valid utf8 inputs on decodeUtf8" $ do
      case decodeHexWith_ @a utf8 (encodeHex @t "\1079743") of
        Left (ConversionError _) -> return ()
        _ -> assertFailure "conversion phase"
    , testCase "decodeHexPaddedWith non-utf8 inputs on decodeUtf8" $ do
      case decodeHexPaddedWith_ @a utf8 "\1079743" of
        Left (DecodeError _) -> return ()
        _ -> assertFailure "decoding phase"
    , testCase "decodePaddedWith valid utf8 inputs on decodeUtf8" $ do
      case decodeHexPaddedWith_ @a utf8 (encodeHex @t "\1079743") of
        Left (ConversionError _) -> return ()
        _ -> assertFailure "conversion phase"
    , testCase "decodeUnpaddedWith non-utf8 inputs on decodeUtf8" $ do
      case decodeHexUnpaddedWith_ @a utf8 "\1079743" of
        Left (DecodeError _) -> return ()
        _ -> assertFailure "decoding phase"
    , testCase "decodeUnpaddedWith valid utf8 inputs on decodeUtf8" $ do
      case decodeHexUnpaddedWith_ @a utf8 (encodeHexNopad @t "\1079743") of
        Left (ConversionError _) -> return ()
        _ -> assertFailure "conversion phase"
    ]
  , testGroup "decodeWith positive tests"
    [ testCase "decodeWith utf8 inputs on decodeUtf8" $ do
      a <- either (assertFailure . show) pure $ decode @a "MZXW6YTBOI======"
      b <- either (assertFailure . show) pure $ decodeWith_ @a utf8 "MZXW6YTBOI======"
      a @=? b
    , testCase "decodeHexWith utf8 inputs on decodeUtf8" $ do
      a <- either (assertFailure . show) pure $ decodeHex @a "CPNMUOJ1E8======"
      b <- either (assertFailure . show) pure $ decodeHexWith_ @a utf8 "CPNMUOJ1E8======"
      a @=? b
    , testCase "decodeHexPaddedWith utf8 inputs on decodeUtf8" $ do
      a <- either (assertFailure . show) pure $ decodeHexPad @a "CPNMUOJ1E8======"
      b <- either (assertFailure . show) pure $ decodeHexPaddedWith_ @a utf8 "CPNMUOJ1E8======"
      a @=? b
    , testCase "decodeHexUnpaddedWith utf8 inputs on decodeUtf8" $ do
      a <- either (assertFailure . show) pure $ decodeHexNopad @a "CPNMUOJ1"
      b <- either (assertFailure . show) pure $ decodeHexUnpaddedWith_ @a utf8 "CPNMUOJ1"
      a @=? b
    ]
  ]

-- | Validity unit tests for the URL workflow
--
validityTests :: forall a b proxy. Harness a b => proxy a -> TestTree
validityTests _ = testGroup "Validity and correctness unit tests"
  [ testGroup "Validity unit tests"
    [ testCase "Hex padding tests" $ do
      not (validateHex @a "C") @? "C"
      validateHex @a "CO" @? "CO"
      validateHex @a "CPNG" @? "CPNG"
      validateHex @a "CPNMU" @? "CPNMU"
      validateHex @a "CPNMUOG" @? "CPNMUOG"
      validateHex @a "CPNMUOJ1" @? "CPNMUOJ1"
      validateHex @a "CPNMUOJ1E8" @? "CPNMUOJ1E8"
      validateHex @a "CO======" @? "CO======"
      validateHex @a "CPNG====" @? "CPNG===="
      validateHex @a "CPNMU===" @? "CPNMU==="
      validateHex @a "CPNMUOG=" @? "CPNMUOG="
      validateHex @a "CPNMUOJ1" @? "CPNMUOJ1"
      validateHex @a "CPNMUOJ1E8======" @? "CPNMUOJ1E8======"
    , testCase "Std padding tests" $ do
      not (validate @a "M") @? "M"
      validate @a "MY" @? "MY"
      validate @a "MZXQ" @? "MZXQ"
      validate @a "MZXW6" @? "MZXW6"
      validate @a "MZXW6YQ" @? "MZXW6YQ"
      validate @a "MZXW6YTB" @? "MZXW6YTB"
      validate @a "MZXW6YTBOI" @? "MZXW6YTBOI"
      validate @a "MY======" @? "MY======"
      validate @a "MZXQ====" @? "MZXQ===="
      validate @a "MZXW6===" @? "MZXW6==="
      validate @a "MZXW6YQ=" @? "MZXW6YQ="
      validate @a "MZXW6YTB" @? "MZXW6YTB"
      validate @a "MZXW6YTBOI======" @? "MZXW6YTBOI======"
    ]
  , testGroup "Correctness unit tests"
    [ testCase "Hex tests" $ do
      not (correctHex @a "C") @? "C"
      correctHex @a "CO" @? "CO"
      correctHex @a "CPNG" @? "CPNG"
      correctHex @a "CPNMU" @? "CPNMU"
      correctHex @a "CPNMUOG" @? "CPNMUOG"
      correctHex @a "CPNMUOJ1" @? "CPNMUOJ1"
      correctHex @a "CPNMUOJ1E8" @? "CPNMUOJ1E8"
      correctHex @a "CO======" @? "CO======"
      correctHex @a "CPNG====" @? "CPNG===="
      correctHex @a "CPNMU===" @? "CPNMU==="
      correctHex @a "CPNMUOG=" @? "CPNMUOG="
      correctHex @a "CPNMUOJ1" @? "CPNMUOJ1"
      correctHex @a "CPNMUOJ1E8======" @? "CPNMUOJ1E8======"
    , testCase "Std tests" $ do
      not (correct @a "M") @? "M"
      correct @a "MY" @? "MY"
      correct @a "MZXQ" @? "MZXQ"
      correct @a "MZXW6" @? "MZXW6"
      correct @a "MZXW6YQ" @? "MZXW6YQ"
      correct @a "MZXW6YTB" @? "MZXW6YTB"
      correct @a "MZXW6YTBOI" @? "MZXW6YTBOI"
      correct @a "MY======" @? "MY======"
      correct @a "MZXQ====" @? "MZXQ===="
      correct @a "MZXW6===" @? "MZXW6==="
      correct @a "MZXW6YQ=" @? "MZXW6YQ="
      correct @a "MZXW6YTB" @? "MZXW6YTB"
      correct @a "MZXW6YTBOI======" @? "MZXW6YTBOI======"
    ]
  ]

-- | Offset test vectors. This stresses the invalid char + incorrect padding
-- offset error messages
--
offsetVectors :: forall a b proxy. Harness a b => proxy a -> TestTree
offsetVectors _ = testGroup "Offset tests"
  [ testGroup "Hex - Invalid padding"
    [ testCase "Invalid staggered padding" $ do
      decodeHex @a "=PNMUOJ1E8======" @?= Left "invalid padding at offset: 0"
      decodeHex @a "C=NMUOJ1E8======" @?= Left "invalid padding at offset: 1"
      decodeHex @a "CP=MUOJ1E8======" @?= Left "invalid padding at offset: 2"
      decodeHex @a "CPN=UOJ1E8======" @?= Left "invalid padding at offset: 3"
      decodeHex @a "CPNM=OJ1E8======" @?= Left "invalid padding at offset: 4"
      decodeHex @a "CPNMU=J1E8======" @?= Left "invalid padding at offset: 5"
      decodeHex @a "CPNMUO=1E8======" @?= Left "invalid padding at offset: 6"
      decodeHex @a "CPNMUOJ=E8======" @?= Left "invalid padding at offset: 7"
    , testCase "Invalid character coverage" $ do
      decodeHex @a "%PNMUOJ1E8======" @?= Left "invalid character at offset: 0"
      decodeHex @a "C%NMUOJ1E8======" @?= Left "invalid character at offset: 1"
      decodeHex @a "CP%MUOJ1E8======" @?= Left "invalid character at offset: 2"
      decodeHex @a "CPN%UOJ1E8======" @?= Left "invalid character at offset: 3"
      decodeHex @a "CPNM%OJ1E8======" @?= Left "invalid character at offset: 4"
      decodeHex @a "CPNMU%J1E8======" @?= Left "invalid character at offset: 5"
      decodeHex @a "CPNMUO%1E8======" @?= Left "invalid character at offset: 6"
      decodeHex @a "CPNMUOJ%E8======" @?= Left "invalid character at offset: 7"
    ]
  , testGroup "Std - Invalid padding"
    [ testCase "Invalid staggered padding" $ do
      decode @a "=ZXW6YTBOI======" @?= Left "invalid padding at offset: 0"
      decode @a "M=XW6YTBOI======" @?= Left "invalid padding at offset: 1"
      decode @a "MZ=W6YTBOI======" @?= Left "invalid padding at offset: 2"
      decode @a "MZX=6YTBOI======" @?= Left "invalid padding at offset: 3"
      decode @a "MZXW=YTBOI======" @?= Left "invalid padding at offset: 4"
      decode @a "MZXW6=TBOI======" @?= Left "invalid padding at offset: 5"
      decode @a "MZXW6Y=BOI======" @?= Left "invalid padding at offset: 6"
      decode @a "MZXW6YT=OI======" @?= Left "invalid padding at offset: 7"
    , testCase "Invalid character coverage" $ do
      decode @a "%ZXW6YTBOI======" @?= Left "invalid character at offset: 0"
      decode @a "M%XW6YTBOI======" @?= Left "invalid character at offset: 1"
      decode @a "MZ%W6YTBOI======" @?= Left "invalid character at offset: 2"
      decode @a "MZX%6YTBOI======" @?= Left "invalid character at offset: 3"
      decode @a "MZXW%YTBOI======" @?= Left "invalid character at offset: 4"
      decode @a "MZXW6%TBOI======" @?= Left "invalid character at offset: 5"
      decode @a "MZXW6Y%BOI======" @?= Left "invalid character at offset: 6"
      decode @a "MZXW6YT%OI======" @?= Left "invalid character at offset: 7"
    ]
  ]
