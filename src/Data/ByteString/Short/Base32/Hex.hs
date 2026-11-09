{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Module       : Data.ByteString.Short.Base32.Hex
-- Copyright    : (c) 2019-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.ByteString.Short.ShortByteString'-valued combinators for
-- implementing the RFC 4648 specification of the Base32hex
-- encoding format. This includes strictly padded/unpadded and decoding
-- variants, as well as internal and external validation for canonicity.
--
module Data.ByteString.Short.Base32.Hex
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


import Data.ByteString.Base32.Internal.Short
import Data.ByteString.Base32.Internal.Tables
import Data.ByteString.Short (ShortByteString)
import Data.Base32.Types

import Data.Text (Text)
import Data.Text.Short (ShortText)
import Data.Text.Short.Unsafe (fromShortByteStringUnsafe)

-- | Encode a 'ShortByteString' value as a Base32hex 'Text' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> encodeBase32 "Sun"
-- "ADQMS==="
--
encodeBase32 :: ShortByteString -> Base32 'HexPadded ShortText
encodeBase32 = fmap fromShortByteStringUnsafe . encodeBase32'
{-# INLINE encodeBase32 #-}

-- | Encode a 'ShortByteString' as a Base32hex 'ShortByteString' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> encodeBase32' "Sun"
-- "ADQMS==="
--
encodeBase32' :: ShortByteString -> Base32 'HexPadded ShortByteString
encodeBase32' =
  assertBase32
    . encodeShort
      "0123456789ABCDEFGHIJKLMNOPQRSTUV"#
      True

-- | Decode an arbitrarily padded Base32hex encoded 'ShortByteString' value. If its length
-- is not a multiple
-- of 8, then padding chars will be added to fill out the input to a multiple of
-- 8 for safe decoding as Base32hex-encoded values are optionally padded.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> decodeBase32Untyped "ADQMS==="
-- Right "Sun"
--
-- >>> decodeBase32Untyped "ADQMS"
-- Right "Sun"
--
-- >>> decodeBase32Untyped "ADQM==="
-- Left "Base32-encoded bytestring has invalid padding"
--
decodeBase32Untyped :: ShortByteString -> Either Text ShortByteString
decodeBase32Untyped = decodeShort hexDecodeTable OptionalPadding
{-# INLINE decodeBase32Untyped #-}

-- | Encode a 'ShortByteString' value as Base32hex 'Text' without padding. Note that for
-- Base32hex,
-- padding is optional. If you call this function, you will simply be encoding
-- as Base32hex and stripping padding chars from the output.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> encodeBase32Unpadded "Sun"
-- "ADQMS"
--
encodeBase32Unpadded :: ShortByteString -> Base32 'HexUnpadded ShortText
encodeBase32Unpadded = fmap fromShortByteStringUnsafe . encodeBase32Unpadded'
{-# INLINE encodeBase32Unpadded #-}

-- | Encode a 'ShortByteString' value as Base32hex without padding. Note that for
-- Base32hex,
-- padding is optional. If you call this function, you will simply be encoding
-- as Base32hex and stripping padding chars from the output.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> encodeBase32Unpadded' "Sun"
-- "ADQMS"
--
encodeBase32Unpadded' :: ShortByteString -> Base32 'HexUnpadded ShortByteString
encodeBase32Unpadded' =
  assertBase32
    . encodeShort
      "0123456789ABCDEFGHIJKLMNOPQRSTUV"#
      False

-- | Decode an unpadded Base32hex-encoded 'ShortByteString' value. Input strings are
-- required to be unpadded, and will undergo validation prior to decoding to
-- confirm.
--
-- In general, unless unpadded Base32hex is explicitly required, it is
-- safer to call 'decodeBase32Untyped'.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> decodeBase32UnpaddedUntyped "ADQMS"
-- Right "Sun"
--
-- >>> decodeBase32UnpaddedUntyped "ADQMS==="
-- Left "Base32-encoded bytestring has invalid padding"
--
decodeBase32UnpaddedUntyped :: ShortByteString -> Either Text ShortByteString
decodeBase32UnpaddedUntyped = decodeShort hexDecodeTable ForbidPadding
{-# INLINE decodeBase32UnpaddedUntyped #-}

-- | Decode a padded Base32hex-encoded 'ShortByteString' value. Input strings are
-- required to be correctly padded, and will be validated prior to decoding
-- to confirm.
--
-- In general, unless padded Base32hex is explicitly required, it is
-- safer to call 'decodeBase32Untyped'.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> decodeBase32PaddedUntyped "ADQMS==="
-- Right "Sun"
--
-- >>> decodeBase32PaddedUntyped "ADQMS"
-- Left "Base32-encoded bytestring requires padding"
--
decodeBase32PaddedUntyped :: ShortByteString -> Either Text ShortByteString
decodeBase32PaddedUntyped = decodeShort hexDecodeTable RequirePadding
{-# INLINE decodeBase32PaddedUntyped #-}

-- | Decode a typed Base32hex-encoded 'ShortByteString' value.
--
-- This function is padding-agnostic.
--
-- >>> decodeBase32 (encodeBase32' "Sun")
-- "Sun"
--
-- >>> decodeBase32 (encodeBase32Unpadded' "Sun")
-- "Sun"
--
decodeBase32 :: HexAlphabet k => Base32 k ShortByteString -> ShortByteString
decodeBase32 = decodeShortTyped hexDecodeTable
{-# INLINE decodeBase32 #-}

-- | Decode a typed, padded Base32hex-encoded 'ShortByteString' value.
--
-- >>> decodeBase32Padded (encodeBase32' "Sun")
-- "Sun"
--
decodeBase32Padded :: Base32 'HexPadded ShortByteString -> ShortByteString
decodeBase32Padded = decodeBase32
{-# INLINE decodeBase32Padded #-}

-- | Parse and validate a base32hex-encoded value, assuming that it has padding.
--
-- >>> parseBase32Padded "adqms==="
-- Right "adqms==="
--
-- >>> fmap decodeBase32Padded (parseBase32Padded "ADQMS===")
-- Right "Sun"
--
-- >>> parseBase32Padded ""
-- Right ""
--
-- >>> parseBase32Padded "ADQMS"
-- Left "Base32-encoded bytestring requires padding"
--
parseBase32Padded :: ShortByteString -> Either Text (Base32 'HexPadded ShortByteString)
parseBase32Padded value = assertBase32 value
  <$ (decodeBase32PaddedUntyped) value
{-# INLINE parseBase32Padded #-}

-- | Decode a typed, unpadded Base32hex-encoded value.
--
-- >>> decodeBase32Unpadded (encodeBase32Unpadded' "Sun")
-- "Sun"
--
decodeBase32Unpadded :: Base32 'HexUnpadded ShortByteString -> ShortByteString
decodeBase32Unpadded = decodeBase32
{-# INLINE decodeBase32Unpadded #-}

-- | Parse and validate a base32hex-encoded value, assuming that it is unpadded.
--
-- >>> parseBase32Unpadded "adqms"
-- Right "adqms"
--
-- >>> fmap decodeBase32Unpadded (parseBase32Unpadded "ADQMS")
-- Right "Sun"
--
-- >>> parseBase32Unpadded ""
-- Right ""
--
-- >>> parseBase32Unpadded "ADQMS==="
-- Left "Base32-encoded bytestring has invalid padding"
--
-- >>> isLeft (parseBase32Unpadded "CP")
-- True
--
parseBase32Unpadded
  :: ShortByteString -> Either Text (Base32 'HexUnpadded ShortByteString)
parseBase32Unpadded value = assertBase32 value
  <$ (decodeBase32UnpaddedUntyped) value
{-# INLINE parseBase32Unpadded #-}

-- $setup
-- >>> :seti -XOverloadedStrings
-- >>> import Data.Either (isLeft)
