{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module       : Data.ByteString.Short.Base32
-- Copyright    : (c) 2019-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.ByteString.Short.ShortByteString'-valued combinators for
-- implementing the RFC 4648 specification of the Base32
-- encoding format. This includes strictly padded/unpadded decoding
-- variants, as well as internal and external validation for canonicity.
module Data.ByteString.Short.Base32
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
  , parseBase32
  , parseBase32Padded
  , parseBase32Unpadded
  ) where

import Data.Base32.Types
import Data.ByteString.Base32.Internal.Short
import Data.ByteString.Base32.Internal.Tables
import qualified Data.Text as ErrorText

import Data.ByteString.Short (ShortByteString)
import Data.Text (Text)
import Data.Text.Short (ShortText)
import Data.Text.Short.Unsafe (fromShortByteStringUnsafe)

-- | Encode a 'ShortByteString' value as a Base32 'Text' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> encodeBase32 "Sun"
-- "KN2W4==="
encodeBase32 :: ShortByteString -> Base32 'Std ShortText
encodeBase32 = fmap fromShortByteStringUnsafe . encodeBase32'
{-# INLINE encodeBase32 #-}

-- | Encode a 'ShortByteString' as a Base32 'ShortByteString' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> encodeBase32' "Sun"
-- "KN2W4==="
encodeBase32' :: ShortByteString -> Base32 'Std ShortByteString
encodeBase32' =
  assertBase32
    . encodeShort
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"#
      True

-- | Decode an arbitrarily padded Base32 encoded 'ShortByteString' value. If its length is
-- not a multiple
-- of 8, then padding chars will be added to fill out the input to a multiple of
-- 8 for safe decoding as Base32-encoded values are optionally padded.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> decodeBase32Untyped "KN2W4==="
-- Right "Sun"
--
-- >>> decodeBase32Untyped "KN2W4"
-- Right "Sun"
--
-- >>> decodeBase32Untyped "KN2W==="
-- Left "Base32-encoded bytestring has invalid padding"
decodeBase32Untyped :: ShortByteString -> Either Text ShortByteString
decodeBase32Untyped = decodeShort stdDecodeTable OptionalPadding
{-# INLINE decodeBase32Untyped #-}

-- | Encode a 'ShortByteString' value as Base32 'Text' without padding. Note that for
-- Base32,
-- padding is optional. If you call this function, you will simply be encoding
-- as Base32 and stripping padding chars from the output.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> encodeBase32Unpadded "Sun"
-- "KN2W4"
encodeBase32Unpadded :: ShortByteString -> Base32 'Std ShortText
encodeBase32Unpadded = fmap fromShortByteStringUnsafe . encodeBase32Unpadded'
{-# INLINE encodeBase32Unpadded #-}

-- | Encode a 'ShortByteString' value as Base32 without padding. Note that for Base32,
-- padding is optional. If you call this function, you will simply be encoding
-- as Base32 and stripping padding chars from the output.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> encodeBase32Unpadded' "Sun"
-- "KN2W4"
encodeBase32Unpadded' :: ShortByteString -> Base32 'Std ShortByteString
encodeBase32Unpadded' =
  assertBase32
    . encodeShort
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"#
      False

-- | Decode an unpadded Base32-encoded 'ShortByteString' value. Input strings are
-- required to be unpadded, and will undergo validation prior to decoding to
-- confirm.
--
-- In general, unless unpadded Base32 is explicitly required, it is
-- safer to call 'decodeBase32Untyped'.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> decodeBase32UnpaddedUntyped "KN2W4"
-- Right "Sun"
--
-- >>> decodeBase32UnpaddedUntyped "KN2W4==="
-- Left "Base32-encoded bytestring has invalid padding"
decodeBase32UnpaddedUntyped :: ShortByteString -> Either Text ShortByteString
decodeBase32UnpaddedUntyped = decodeShort stdDecodeTable ForbidPadding
{-# INLINE decodeBase32UnpaddedUntyped #-}

-- | Decode a padded Base32-encoded 'ShortByteString' value. Input strings are
-- required to be correctly padded, and will be validated prior to decoding
-- to confirm.
--
-- In general, unless padded Base32 is explicitly required, it is
-- safer to call 'decodeBase32Untyped'.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> decodeBase32PaddedUntyped "KN2W4==="
-- Right "Sun"
--
-- >>> decodeBase32PaddedUntyped "KN2W4"
-- Left "Base32-encoded bytestring requires padding"
decodeBase32PaddedUntyped :: ShortByteString -> Either Text ShortByteString
decodeBase32PaddedUntyped = decodeShort stdDecodeTable RequirePadding
{-# INLINE decodeBase32PaddedUntyped #-}

-- | Decode a value with established alphabet provenance.
-- Use 'parseBase32' to validate external input first.
-- A false assertion or incompatible padding policy causes an error.
decodeBase32 :: Base32 'Std ShortByteString -> ShortByteString
decodeBase32 =
  either (error . ErrorText.unpack) id
    . decodeBase32Untyped
    . extractBase32
{-# INLINE decodeBase32 #-}

-- | Validate encoded input, retaining its original representation and case.
-- Checks alphabet, padding, length, and zero unused bits. Empty input is valid.
-- This validates Base32 bytes, not the character encoding of the payload.
parseBase32 :: ShortByteString -> Either Text (Base32 'Std ShortByteString)
parseBase32 value =
  assertBase32 value
    <$ (decodeBase32Untyped) value
{-# INLINE parseBase32 #-}

-- | Decode a value with established alphabet provenance.
-- Use 'parseBase32Padded' to validate external input first.
-- A false assertion or incompatible padding policy causes an error.
decodeBase32Padded :: Base32 'Std ShortByteString -> ShortByteString
decodeBase32Padded =
  either (error . ErrorText.unpack) id
    . decodeBase32PaddedUntyped
    . extractBase32
{-# INLINE decodeBase32Padded #-}

-- | Validate encoded input, retaining its original representation and case.
-- Checks alphabet, padding, length, and zero unused bits. Empty input is valid.
-- This validates Base32 bytes, not the character encoding of the payload.
parseBase32Padded :: ShortByteString -> Either Text (Base32 'Std ShortByteString)
parseBase32Padded value =
  assertBase32 value
    <$ (decodeBase32PaddedUntyped) value
{-# INLINE parseBase32Padded #-}

-- | Decode a value with established alphabet provenance.
-- Use 'parseBase32Unpadded' to validate external input first.
-- A false assertion or incompatible padding policy causes an error.
decodeBase32Unpadded :: Base32 'Std ShortByteString -> ShortByteString
decodeBase32Unpadded =
  either (error . ErrorText.unpack) id
    . decodeBase32UnpaddedUntyped
    . extractBase32
{-# INLINE decodeBase32Unpadded #-}

-- | Validate encoded input, retaining its original representation and case.
-- Checks alphabet, padding, length, and zero unused bits. Empty input is valid.
-- This validates Base32 bytes, not the character encoding of the payload.
parseBase32Unpadded :: ShortByteString -> Either Text (Base32 'Std ShortByteString)
parseBase32Unpadded value =
  assertBase32 value
    <$ (decodeBase32UnpaddedUntyped) value
{-# INLINE parseBase32Unpadded #-}
