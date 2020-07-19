{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module       : Main
-- Copyright    : (c) 2019-2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- This module contains internal test harnesses for `base32`
--
module Internal where


import qualified Data.ByteString as BS
import "base32" Data.ByteString.Base32 as B32
import "base32" Data.ByteString.Base32.Hex as B32H
import Data.Proxy
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import "base32" Data.Text.Encoding.Base32 as T32
import "base32" Data.Text.Encoding.Base32.Hex as T32U

import Test.QuickCheck hiding (label)

-- ------------------------------------------------------------------ --
-- Test Harnesses

data Impl
  = B32
  | B32H
  | T32
  | T32H

b32 :: Proxy 'B32
b32 = Proxy

t32 :: Proxy 'T32
t32 = Proxy

b32H :: Proxy 'B32H
b32H = Proxy

t32H :: Proxy 'T32H
t32H = Proxy

-- | This class provides the generic API definition for
-- the base32 std alphabet
--
class
  ( Eq bs
  , Show bs
  , Arbitrary bs
  , CoArbitrary bs
  , IsString bs
  ) => Harness (a :: Impl) bs | a -> bs, bs -> a
  where

  label :: String

  encode :: bs -> bs
  encodeNopad :: bs -> bs
  encodeHex :: bs -> bs
  encodeHexNopad :: bs -> bs

  decode :: bs -> Either Text bs
  decodeHex :: bs -> Either Text bs

  decodePad :: bs -> Either Text bs
  decodeHexPad :: bs -> Either Text bs

  decodeNopad :: bs -> Either Text bs
  decodeHexNopad :: bs -> Either Text bs

  correct :: bs -> Bool
  correctHex :: bs -> Bool

  validate :: bs -> Bool
  validateHex :: bs -> Bool


instance Harness 'B32 BS.ByteString where
  label = "ByteString"

  encode = B32.encodeBase32'
  encodeNopad = B32.encodeBase32Unpadded'

  decode = B32.decodeBase32
  decodePad = B32.decodeBase32Padded
  decodeNopad = B32.decodeBase32Unpadded
  correct = B32.isBase32
  validate = B32.isValidBase32

  encodeHex = B32H.encodeBase32'
  encodeHexNopad = B32H.encodeBase32Unpadded'
  decodeHex = B32H.decodeBase32
  decodeHexPad = B32H.decodeBase32Padded
  decodeHexNopad = B32H.decodeBase32Unpadded
  correctHex = B32H.isBase32Hex
  validateHex = B32H.isValidBase32Hex

instance Harness 'T32 Text where
  label = "Text"

  encode = T32.encodeBase32
  encodeNopad = T32.encodeBase32Unpadded
  decode = T32.decodeBase32
  decodeNopad = T32.decodeBase32Unpadded
  decodePad = T32.decodeBase32Padded
  correct = T32.isBase32

  encodeHex = T32U.encodeBase32
  encodeHexNopad = T32U.encodeBase32Unpadded
  decodeHex = T32U.decodeBase32
  decodeHexPad = T32U.decodeBase32Padded
  decodeHexNopad = T32U.decodeBase32Unpadded

  correctHex = T32U.isBase32Hex
  validateHex = T32U.isValidBase32Hex
  validate = T32.isValidBase32

-- ------------------------------------------------------------------ --
-- Quickcheck instances

instance Arbitrary BS.ByteString where
    arbitrary = BS.pack <$> arbitrary
    shrink xs = BS.pack <$> shrink (BS.unpack xs)

instance CoArbitrary BS.ByteString where
    coarbitrary = coarbitrary . BS.unpack

instance Arbitrary T.Text where
    arbitrary = T.pack <$> arbitrary
    shrink xs = T.pack <$> shrink (T.unpack xs)

instance CoArbitrary T.Text where
    coarbitrary = coarbitrary . T.unpack
