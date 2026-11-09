{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Module       : Data.ByteString.Base32
-- Copyright    : (c) 2019-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.ByteString.ByteString'-valued combinators for
-- implementing the RFC 4648 specification of the Base32
-- encoding format. This includes padded and unpadded decoding variants, as well as
-- internal and external validation for canonicity.
--
module Data.ByteString.Base32
( -- * Encoding
  encodeBase32
, encodeBase32'
, encodeBase32Unpadded
, encodeBase32Unpadded'
  -- * Decoding
, decodeBase32Untyped
, decodeBase32UnpaddedUntyped
, decodeBase32PaddedUntyped
  -- * Validation
, decodeBase32
, decodeBase32Padded
, decodeBase32Unpadded
, parseBase32Padded
, parseBase32Unpadded
) where


import Data.Base32.Types

import qualified Data.ByteString as BS
import Data.ByteString.Internal (ByteString(..))
import Data.ByteString.Base32.Internal.Head
import Data.ByteString.Base32.Internal.Tables
import Data.ByteString.Base32.Internal.Utils (validateLastNPads)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as T

import System.IO.Unsafe (unsafeDupablePerformIO)


-- | Encode a 'ByteString' value as a Base32 'Text' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> encodeBase32 (BS8.pack "Sun")
-- "KN2W4==="
--
encodeBase32 :: ByteString -> Base32 'StdPadded Text
encodeBase32 = fmap T.decodeUtf8 . encodeBase32'
{-# INLINE encodeBase32 #-}

-- | Encode a 'ByteString' value as a Base32 'ByteString' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> encodeBase32' (BS8.pack "Sun")
-- "KN2W4==="
--
encodeBase32' :: ByteString -> Base32 'StdPadded ByteString
encodeBase32' = assertBase32 . encodeBase32_ "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"#
{-# INLINE encodeBase32' #-}

-- | Decode an arbitrarily padded Base32-encoded 'ByteString' value. If its length
-- is not a multiple of 8, then padding characters will be added to fill out the
-- input to a multiple of 8 for safe decoding, as Base32-encoded values are
-- optionally padded.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> decodeBase32Untyped (BS8.pack "KN2W4===")
-- Right "Sun"
--
-- >>> decodeBase32Untyped (BS8.pack "KN2W4")
-- Right "Sun"
--
-- >>> decodeBase32Untyped (BS8.pack "KN2W===")
-- Left "Base32-encoded bytestring has invalid padding"
--
decodeBase32Untyped :: ByteString -> Either Text ByteString
decodeBase32Untyped bs@(BS _ !l)
    | l == 0 = Right bs
    | r == 0 = unsafeDupablePerformIO $ decodeBase32_ stdDecodeTable bs
    | r == 2 = unsafeDupablePerformIO $
        decodeBase32_ stdDecodeTable (BS.append bs (BS.replicate 6 0x3d))
    | r == 4 = validateLastNPads 2 bs $
        decodeBase32_ stdDecodeTable (BS.append bs (BS.replicate 4 0x3d))
    | r == 5 = validateLastNPads 3 bs $
        decodeBase32_ stdDecodeTable (BS.append bs (BS.replicate 3 0x3d))
    | r == 7 = validateLastNPads 5 bs $
        decodeBase32_ stdDecodeTable (BS.append bs (BS.replicate 1 0x3d))
    | otherwise = Left (Text.pack "Base32-encoded bytestring has invalid size")
  where
    !r = l `rem` 8
{-# INLINE decodeBase32Untyped #-}

-- | Encode a 'ByteString' value as a Base32 'Text' value without padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> encodeBase32Unpadded (BS8.pack "Sun")
-- "KN2W4"
--
encodeBase32Unpadded :: ByteString -> Base32 'StdUnpadded Text
encodeBase32Unpadded = fmap T.decodeUtf8 . encodeBase32Unpadded'
{-# INLINE encodeBase32Unpadded #-}

-- | Encode a 'ByteString' value as a Base32 'ByteString' value without padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> encodeBase32Unpadded' (BS8.pack "Sun")
-- "KN2W4"
--
encodeBase32Unpadded' :: ByteString -> Base32 'StdUnpadded ByteString
encodeBase32Unpadded' = assertBase32
  . encodeBase32NoPad_ "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"#
{-# INLINE encodeBase32Unpadded' #-}

-- | Decode an unpadded Base32-encoded 'ByteString' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> decodeBase32UnpaddedUntyped (BS8.pack "KN2W4")
-- Right "Sun"
--
-- >>> decodeBase32UnpaddedUntyped (BS8.pack "KN2W4===")
-- Left "Base32-encoded bytestring has invalid padding"
--
decodeBase32UnpaddedUntyped :: ByteString -> Either Text ByteString
decodeBase32UnpaddedUntyped bs@(BS _ !l)
    | l == 0 = Right bs
    | r == 0 = validateLastNPads 1 bs $ decodeBase32_ stdDecodeTable bs
    | r == 2 = unsafeDupablePerformIO $
        decodeBase32_ stdDecodeTable (BS.append bs (BS.replicate 6 0x3d))
    | r == 4 = validateLastNPads 1 bs $
        decodeBase32_ stdDecodeTable (BS.append bs (BS.replicate 4 0x3d))
    | r == 5 = validateLastNPads 1 bs $
        decodeBase32_ stdDecodeTable (BS.append bs (BS.replicate 3 0x3d))
    | r == 7 = validateLastNPads 1 bs $
        decodeBase32_ stdDecodeTable (BS.append bs (BS.replicate 1 0x3d))
    | otherwise = Left (Text.pack "Base32-encoded bytestring has invalid size")
  where
    !r = l `rem` 8
{-# INLINE decodeBase32UnpaddedUntyped #-}

-- | Decode a padded Base32-encoded 'ByteString' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> decodeBase32PaddedUntyped (BS8.pack "KN2W4===")
-- Right "Sun"
--
-- >>> decodeBase32PaddedUntyped (BS8.pack "KN2W4")
-- Left "Base32-encoded bytestring requires padding"
--
decodeBase32PaddedUntyped :: ByteString -> Either Text ByteString
decodeBase32PaddedUntyped bs@(BS _ !l)
    | l == 0 = Right bs
    | r == 1 = Left (Text.pack "Base32-encoded bytestring has invalid size")
    | r == 3 = Left (Text.pack "Base32-encoded bytestring has invalid size")
    | r == 6 = Left (Text.pack "Base32-encoded bytestring has invalid size")
    | r /= 0 = Left (Text.pack "Base32-encoded bytestring requires padding")
    | otherwise = unsafeDupablePerformIO $ decodeBase32_ stdDecodeTable bs
  where
    !r = l `rem` 8
{-# INLINE decodeBase32PaddedUntyped #-}

-- | Decode established Base32 provenance without repeating validation.
-- Use 'parseBase32Padded' or 'parseBase32Unpadded' for external input.
--
-- >>> decodeBase32 (encodeBase32' (BS8.pack "Sun"))
-- "Sun"
-- >>> decodeBase32 (encodeBase32Unpadded' (BS8.pack "Sun"))
-- "Sun"
--
decodeBase32 :: StdAlphabet k => Base32 k ByteString -> ByteString
decodeBase32 = decodeBase32Typed_ stdDecodeTable
{-# INLINE decodeBase32 #-}

-- | Decode established Base32 provenance without repeating validation.
-- Use 'parseBase32Padded' to check the padding policy on external input.
-- The type establishes the alphabet and padding policy.
--
-- >>> decodeBase32Padded (encodeBase32' (BS8.pack "Sun"))
-- "Sun"
--
decodeBase32Padded :: Base32 'StdPadded ByteString -> ByteString
decodeBase32Padded = decodeBase32
{-# INLINE decodeBase32Padded #-}

-- | Validate encoded input, retaining its original representation and case.
-- Checks alphabet, padding, length, and zero unused bits. Empty input is valid.
-- This validates Base32 bytes, not the character encoding of the payload.
--
-- >>> parseBase32Padded (BS8.pack "kn2w4===")
-- Right "kn2w4==="
-- >>> fmap decodeBase32Padded (parseBase32Padded (BS8.pack "KN2W4==="))
-- Right "Sun"
-- >>> parseBase32Padded (BS8.pack "")
-- Right ""
-- >>> parseBase32Padded (BS8.pack "KN2W4")
-- Left "Base32-encoded bytestring requires padding"
--
parseBase32Padded :: ByteString -> Either Text (Base32 'StdPadded ByteString)
parseBase32Padded value = assertBase32 value
  <$ (decodeBase32PaddedUntyped) value
{-# INLINE parseBase32Padded #-}

-- | Decode established Base32 provenance without repeating validation.
-- Use 'parseBase32Unpadded' to check the padding policy on external input.
-- The type establishes the alphabet and padding policy.
--
-- >>> decodeBase32Unpadded (encodeBase32Unpadded' (BS8.pack "Sun"))
-- "Sun"
--
decodeBase32Unpadded :: Base32 'StdUnpadded ByteString -> ByteString
decodeBase32Unpadded = decodeBase32
{-# INLINE decodeBase32Unpadded #-}

-- | Validate encoded input, retaining its original representation and case.
-- Checks alphabet, padding, length, and zero unused bits. Empty input is valid.
-- This validates Base32 bytes, not the character encoding of the payload.
--
-- >>> parseBase32Unpadded (BS8.pack "kn2w4")
-- Right "kn2w4"
-- >>> fmap decodeBase32Unpadded (parseBase32Unpadded (BS8.pack "KN2W4"))
-- Right "Sun"
-- >>> parseBase32Unpadded (BS8.pack "")
-- Right ""
-- >>> parseBase32Unpadded (BS8.pack "KN2W4===")
-- Left "Base32-encoded bytestring has invalid padding"
-- >>> isLeft (parseBase32Unpadded (BS8.pack "MZ"))
-- True
--
parseBase32Unpadded :: ByteString -> Either Text (Base32 'StdUnpadded ByteString)
parseBase32Unpadded value = assertBase32 value
  <$ (decodeBase32UnpaddedUntyped) value
{-# INLINE parseBase32Unpadded #-}

-- $setup
-- >>> import qualified Data.ByteString.Char8 as BS8
-- >>> import Data.Either (isLeft)
