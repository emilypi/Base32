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

import qualified Data.ByteString as BS
import Data.ByteString.Internal (c2w)
import "base32" Data.ByteString.Base32 as B32
import "base32" Data.ByteString.Base32.Hex as B32H
import qualified "memory" Data.ByteArray.Encoding as Mem
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
  , mkTree t32
    [ mkPropTree
    , mkUnitTree
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
rfcVectors _ = testGroup "RFC 4328 Test Vectors"
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