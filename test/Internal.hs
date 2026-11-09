{-# LANGUAGE RankNTypes #-}
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
-- Copyright    : (c) 2019-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- This module contains internal test harnesses for `base32`
--
module Internal
( Harness(..)
, TextHarness(..)
, b32
, lb32
, sb32
, t32
, tl32
, ts32
, tt32
, ttl32
, tts32
) where


import Data.Base32.Types (Alphabet(..), Base32, extractBase32)
import Data.Either (isRight)
import Test.QuickCheck hiding (label)
import Data.Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base32 as B32
import qualified Data.ByteString.Base32.Hex as B32H
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Base32 as BL32
import qualified Data.ByteString.Lazy.Base32.Hex as BL32H
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Base32 as BS32
import qualified Data.ByteString.Short.Base32.Hex as BS32H
import qualified Data.Text as T
import qualified Data.Text.Encoding.Base32 as T32
import qualified Data.Text.Encoding.Base32.Hex as T32H
import Data.Text.Encoding.Base32.Error
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding.Base32 as TL32
import qualified Data.Text.Lazy.Encoding.Base32.Hex as TL32H
import qualified Data.Text.Short as TS
import qualified Data.Text.Short.Encoding.Base32 as TS32
import qualified Data.Text.Short.Encoding.Base32.Hex as TS32H

-- ------------------------------------------------------------------ --
-- Test Harnesses



-- | This class provides the generic API definition for
-- the base32 std alphabet
--
data Harness bs = Harness
  { label :: String
  , encode :: bs -> bs
  , encodeNopad :: bs -> bs
  , encodeHex :: bs -> bs
  , encodeHexNopad :: bs -> bs
  , decodeTyped :: Base32 'Std bs -> bs
  , decodePaddedTyped :: Base32 'Std bs -> bs
  , decodeUnpaddedTyped :: Base32 'Std bs -> bs
  , decodeHexTyped :: Base32 'Hex bs -> bs
  , decodeHexPaddedTyped :: Base32 'Hex bs -> bs
  , decodeHexUnpaddedTyped :: Base32 'Hex bs -> bs
  , decode :: bs -> Either Text bs
  , decodeHex :: bs -> Either Text bs
  , decodePad :: bs -> Either Text bs
  , decodeHexPad :: bs -> Either Text bs
  , decodeNopad :: bs -> Either Text bs
  , decodeHexNopad :: bs -> Either Text bs
  , parseValue :: bs -> Either Text bs
  , parseHexValue :: bs -> Either Text bs
  , parsePaddedValue :: bs -> Either Text bs
  , parseUnpaddedValue :: bs -> Either Text bs
  , parseHexPaddedValue :: bs -> Either Text bs
  , parseHexUnpaddedValue :: bs -> Either Text bs
  , correct :: bs -> Bool
  , correctHex :: bs -> Bool
  , validate :: bs -> Bool
  , validateHex :: bs -> Bool
  }

b32 :: Harness BS.ByteString
b32 = Harness
  { label = "ByteString"
  , encode = extractBase32 . B32.encodeBase32'
  , encodeNopad = extractBase32 . B32.encodeBase32Unpadded'
  , decodeTyped = B32.decodeBase32
  , decodePaddedTyped = B32.decodeBase32Padded
  , decodeUnpaddedTyped = B32.decodeBase32Unpadded
  , decodeHexTyped = B32H.decodeBase32
  , decodeHexPaddedTyped = B32H.decodeBase32Padded
  , decodeHexUnpaddedTyped = B32H.decodeBase32Unpadded
  , decode = B32.decodeBase32Untyped
  , decodePad = B32.decodeBase32PaddedUntyped
  , decodeNopad = B32.decodeBase32UnpaddedUntyped
  , parseValue = fmap extractBase32 . B32.parseBase32
  , parseHexValue = fmap extractBase32 . B32H.parseBase32
  , parsePaddedValue = fmap extractBase32 . B32.parseBase32Padded
  , parseUnpaddedValue = fmap extractBase32 . B32.parseBase32Unpadded
  , parseHexPaddedValue = fmap extractBase32 . B32H.parseBase32Padded
  , parseHexUnpaddedValue = fmap extractBase32 . B32H.parseBase32Unpadded
  , correct = isRight . B32.parseBase32
  , validate = isRight . B32.parseBase32
  , encodeHex = extractBase32 . B32H.encodeBase32'
  , encodeHexNopad = extractBase32 . B32H.encodeBase32Unpadded'
  , decodeHex = B32H.decodeBase32Untyped
  , decodeHexPad = B32H.decodeBase32PaddedUntyped
  , decodeHexNopad = B32H.decodeBase32UnpaddedUntyped
  , correctHex = isRight . B32H.parseBase32
  , validateHex = isRight . B32H.parseBase32
  }

lb32 :: Harness LBS.ByteString
lb32 = Harness
  { label = "Lazy ByteString"
  , encode = extractBase32 . BL32.encodeBase32'
  , encodeNopad = extractBase32 . BL32.encodeBase32Unpadded'
  , decodeTyped = BL32.decodeBase32
  , decodePaddedTyped = BL32.decodeBase32Padded
  , decodeUnpaddedTyped = BL32.decodeBase32Unpadded
  , decodeHexTyped = BL32H.decodeBase32
  , decodeHexPaddedTyped = BL32H.decodeBase32Padded
  , decodeHexUnpaddedTyped = BL32H.decodeBase32Unpadded
  , decode = BL32.decodeBase32Untyped
  , decodePad = BL32.decodeBase32PaddedUntyped
  , decodeNopad = BL32.decodeBase32UnpaddedUntyped
  , parseValue = fmap extractBase32 . BL32.parseBase32
  , parseHexValue = fmap extractBase32 . BL32H.parseBase32
  , parsePaddedValue = fmap extractBase32 . BL32.parseBase32Padded
  , parseUnpaddedValue = fmap extractBase32 . BL32.parseBase32Unpadded
  , parseHexPaddedValue = fmap extractBase32 . BL32H.parseBase32Padded
  , parseHexUnpaddedValue = fmap extractBase32 . BL32H.parseBase32Unpadded
  , correct = isRight . BL32.parseBase32
  , validate = isRight . BL32.parseBase32
  , encodeHex = extractBase32 . BL32H.encodeBase32'
  , encodeHexNopad = extractBase32 . BL32H.encodeBase32Unpadded'
  , decodeHex = BL32H.decodeBase32Untyped
  , decodeHexPad = BL32H.decodeBase32PaddedUntyped
  , decodeHexNopad = BL32H.decodeBase32UnpaddedUntyped
  , correctHex = isRight . BL32H.parseBase32
  , validateHex = isRight . BL32H.parseBase32
  }

sb32 :: Harness SBS.ShortByteString
sb32 = Harness
  { label = "Short ByteString"
  , encode = extractBase32 . BS32.encodeBase32'
  , encodeNopad = extractBase32 . BS32.encodeBase32Unpadded'
  , decodeTyped = BS32.decodeBase32
  , decodePaddedTyped = BS32.decodeBase32Padded
  , decodeUnpaddedTyped = BS32.decodeBase32Unpadded
  , decodeHexTyped = BS32H.decodeBase32
  , decodeHexPaddedTyped = BS32H.decodeBase32Padded
  , decodeHexUnpaddedTyped = BS32H.decodeBase32Unpadded
  , decode = BS32.decodeBase32Untyped
  , decodePad = BS32.decodeBase32PaddedUntyped
  , decodeNopad = BS32.decodeBase32UnpaddedUntyped
  , parseValue = fmap extractBase32 . BS32.parseBase32
  , parseHexValue = fmap extractBase32 . BS32H.parseBase32
  , parsePaddedValue = fmap extractBase32 . BS32.parseBase32Padded
  , parseUnpaddedValue = fmap extractBase32 . BS32.parseBase32Unpadded
  , parseHexPaddedValue = fmap extractBase32 . BS32H.parseBase32Padded
  , parseHexUnpaddedValue = fmap extractBase32 . BS32H.parseBase32Unpadded
  , correct = isRight . BS32.parseBase32
  , validate = isRight . BS32.parseBase32
  , encodeHex = extractBase32 . BS32H.encodeBase32'
  , encodeHexNopad = extractBase32 . BS32H.encodeBase32Unpadded'
  , decodeHex = BS32H.decodeBase32Untyped
  , decodeHexPad = BS32H.decodeBase32PaddedUntyped
  , decodeHexNopad = BS32H.decodeBase32UnpaddedUntyped
  , correctHex = isRight . BS32H.parseBase32
  , validateHex = isRight . BS32H.parseBase32
  }

t32 :: Harness T.Text
t32 = Harness
  { label = "Text"
  , encode = extractBase32 . T32.encodeBase32
  , encodeNopad = extractBase32 . T32.encodeBase32Unpadded
  , decodeTyped = T32.decodeBase32
  , decodePaddedTyped = T32.decodeBase32Padded
  , decodeUnpaddedTyped = T32.decodeBase32Unpadded
  , decodeHexTyped = T32H.decodeBase32
  , decodeHexPaddedTyped = T32H.decodeBase32Padded
  , decodeHexUnpaddedTyped = T32H.decodeBase32Unpadded
  , decode = T32.decodeBase32Untyped
  , decodePad = T32.decodeBase32PaddedUntyped
  , decodeNopad = T32.decodeBase32UnpaddedUntyped
  , parseValue = fmap extractBase32 . T32.parseBase32
  , parseHexValue = fmap extractBase32 . T32H.parseBase32
  , parsePaddedValue = fmap extractBase32 . T32.parseBase32Padded
  , parseUnpaddedValue = fmap extractBase32 . T32.parseBase32Unpadded
  , parseHexPaddedValue = fmap extractBase32 . T32H.parseBase32Padded
  , parseHexUnpaddedValue = fmap extractBase32 . T32H.parseBase32Unpadded
  , correct = isRight . T32.parseBase32
  , validate = isRight . T32.parseBase32
  , encodeHex = extractBase32 . T32H.encodeBase32
  , encodeHexNopad = extractBase32 . T32H.encodeBase32Unpadded
  , decodeHex = T32H.decodeBase32Untyped
  , decodeHexPad = T32H.decodeBase32PaddedUntyped
  , decodeHexNopad = T32H.decodeBase32UnpaddedUntyped
  , correctHex = isRight . T32H.parseBase32
  , validateHex = isRight . T32H.parseBase32
  }

tl32 :: Harness TL.Text
tl32 = Harness
  { label = "Lazy Text"
  , encode = extractBase32 . TL32.encodeBase32
  , encodeNopad = extractBase32 . TL32.encodeBase32Unpadded
  , decodeTyped = TL32.decodeBase32
  , decodePaddedTyped = TL32.decodeBase32Padded
  , decodeUnpaddedTyped = TL32.decodeBase32Unpadded
  , decodeHexTyped = TL32H.decodeBase32
  , decodeHexPaddedTyped = TL32H.decodeBase32Padded
  , decodeHexUnpaddedTyped = TL32H.decodeBase32Unpadded
  , decode = TL32.decodeBase32Untyped
  , decodePad = TL32.decodeBase32PaddedUntyped
  , decodeNopad = TL32.decodeBase32UnpaddedUntyped
  , parseValue = fmap extractBase32 . TL32.parseBase32
  , parseHexValue = fmap extractBase32 . TL32H.parseBase32
  , parsePaddedValue = fmap extractBase32 . TL32.parseBase32Padded
  , parseUnpaddedValue = fmap extractBase32 . TL32.parseBase32Unpadded
  , parseHexPaddedValue = fmap extractBase32 . TL32H.parseBase32Padded
  , parseHexUnpaddedValue = fmap extractBase32 . TL32H.parseBase32Unpadded
  , correct = isRight . TL32.parseBase32
  , validate = isRight . TL32.parseBase32
  , encodeHex = extractBase32 . TL32H.encodeBase32
  , encodeHexNopad = extractBase32 . TL32H.encodeBase32Unpadded
  , decodeHex = TL32H.decodeBase32Untyped
  , decodeHexPad = TL32H.decodeBase32PaddedUntyped
  , decodeHexNopad = TL32H.decodeBase32UnpaddedUntyped
  , correctHex = isRight . TL32H.parseBase32
  , validateHex = isRight . TL32H.parseBase32
  }

ts32 :: Harness TS.ShortText
ts32 = Harness
  { label = "Short Lazy Text"
  , encode = extractBase32 . TS32.encodeBase32
  , encodeNopad = extractBase32 . TS32.encodeBase32Unpadded
  , decodeTyped = TS32.decodeBase32
  , decodePaddedTyped = TS32.decodeBase32Padded
  , decodeUnpaddedTyped = TS32.decodeBase32Unpadded
  , decodeHexTyped = TS32H.decodeBase32
  , decodeHexPaddedTyped = TS32H.decodeBase32Padded
  , decodeHexUnpaddedTyped = TS32H.decodeBase32Unpadded
  , decode = TS32.decodeBase32Untyped
  , decodePad = TS32.decodeBase32PaddedUntyped
  , decodeNopad = TS32.decodeBase32UnpaddedUntyped
  , parseValue = fmap extractBase32 . TS32.parseBase32
  , parseHexValue = fmap extractBase32 . TS32H.parseBase32
  , parsePaddedValue = fmap extractBase32 . TS32.parseBase32Padded
  , parseUnpaddedValue = fmap extractBase32 . TS32.parseBase32Unpadded
  , parseHexPaddedValue = fmap extractBase32 . TS32H.parseBase32Padded
  , parseHexUnpaddedValue = fmap extractBase32 . TS32H.parseBase32Unpadded
  , correct = isRight . TS32.parseBase32
  , validate = isRight . TS32.parseBase32
  , encodeHex = extractBase32 . TS32H.encodeBase32
  , encodeHexNopad = extractBase32 . TS32H.encodeBase32Unpadded
  , decodeHex = TS32H.decodeBase32Untyped
  , decodeHexPad = TS32H.decodeBase32PaddedUntyped
  , decodeHexNopad = TS32H.decodeBase32UnpaddedUntyped
  , correctHex = isRight . TS32H.parseBase32
  , validateHex = isRight . TS32H.parseBase32
  }

-- -------------------------------------------------------------------- --
-- Text-specific harness

data TextHarness bs cs = TextHarness
  { decodeWith_ :: forall err. (bs -> Either err cs) -> bs -> Either (Base32Error err) cs
  , decodePaddedWith_
      :: forall err. (bs -> Either err cs) -> bs -> Either (Base32Error err) cs
  , decodeUnpaddedWith_
      :: forall err. (bs -> Either err cs) -> bs -> Either (Base32Error err) cs
  , decodeHexWith_
      :: forall err. (bs -> Either err cs) -> bs -> Either (Base32Error err) cs
  , decodeHexPaddedWith_
      :: forall err. (bs -> Either err cs) -> bs -> Either (Base32Error err) cs
  , decodeHexUnpaddedWith_
      :: forall err. (bs -> Either err cs) -> bs -> Either (Base32Error err) cs
  }

tt32 :: TextHarness BS.ByteString Text
tt32 = TextHarness
  { decodeWith_ = T32.decodeBase32With
  , decodePaddedWith_ = T32.decodeBase32PaddedWith
  , decodeUnpaddedWith_ = T32.decodeBase32UnpaddedWith
  , decodeHexWith_ = T32H.decodeBase32With
  , decodeHexPaddedWith_ = T32H.decodeBase32PaddedWith
  , decodeHexUnpaddedWith_ = T32H.decodeBase32UnpaddedWith
  }

ttl32 :: TextHarness LBS.ByteString TL.Text
ttl32 = TextHarness
  { decodeWith_ = TL32.decodeBase32With
  , decodePaddedWith_ = TL32.decodeBase32PaddedWith
  , decodeUnpaddedWith_ = TL32.decodeBase32UnpaddedWith
  , decodeHexWith_ = TL32H.decodeBase32With
  , decodeHexPaddedWith_ = TL32H.decodeBase32PaddedWith
  , decodeHexUnpaddedWith_ = TL32H.decodeBase32UnpaddedWith
  }

tts32 :: TextHarness SBS.ShortByteString TS.ShortText
tts32 = TextHarness
  { decodeWith_ = TS32.decodeBase32With
  , decodePaddedWith_ = TS32.decodeBase32PaddedWith
  , decodeUnpaddedWith_ = TS32.decodeBase32UnpaddedWith
  , decodeHexWith_ = TS32H.decodeBase32With
  , decodeHexPaddedWith_ = TS32H.decodeBase32PaddedWith
  , decodeHexUnpaddedWith_ = TS32H.decodeBase32UnpaddedWith
  }

-- ------------------------------------------------------------------ --
-- Quickcheck instances

instance Arbitrary BS.ByteString where
    arbitrary = BS.pack <$> arbitrary
    shrink xs = BS.pack <$> shrink (BS.unpack xs)

instance CoArbitrary BS.ByteString where
    coarbitrary = coarbitrary . BS.unpack

instance Arbitrary LBS.ByteString where
    arbitrary = LBS.pack <$> arbitrary
    shrink xs = LBS.pack <$> shrink (LBS.unpack xs)

instance CoArbitrary LBS.ByteString where
    coarbitrary = coarbitrary . LBS.unpack

instance Arbitrary SBS.ShortByteString where
    arbitrary = SBS.pack <$> arbitrary
    shrink xs = SBS.pack <$> shrink (SBS.unpack xs)

instance CoArbitrary SBS.ShortByteString where
    coarbitrary = coarbitrary . SBS.unpack

instance Arbitrary T.Text where
    arbitrary = T.pack . getUnicodeString <$> arbitrary
    shrink xs = T.pack . getUnicodeString <$> shrink (UnicodeString $ T.unpack xs)

instance Arbitrary TL.Text where
    arbitrary = TL.pack . getUnicodeString <$> arbitrary
    shrink xs = TL.pack . getUnicodeString <$> shrink (UnicodeString $ TL.unpack xs)

instance CoArbitrary T.Text where
    coarbitrary = coarbitrary . T.unpack

instance CoArbitrary TL.Text where
    coarbitrary = coarbitrary . TL.unpack

instance Arbitrary TS.ShortText where
  arbitrary = TS.fromText <$> arbitrary
  shrink xs = TS.fromText <$> shrink (TS.toText xs)

instance CoArbitrary TS.ShortText where
  coarbitrary = coarbitrary . TS.toText
