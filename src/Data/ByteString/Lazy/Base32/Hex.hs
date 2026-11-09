{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Module       : Data.ByteString.Lazy.Base32.Hex
-- Copyright    : (c) 2019-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.ByteString.Lazy.ByteString'-valued combinators for
-- implementing the RFC 4648 specification of the Base32hex
-- encoding format. This includes strictly padded/unpadded
-- decoding variants, as well as internal and external validation for canonicity.
--
module Data.ByteString.Lazy.Base32.Hex
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

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base32.Hex as B32H
import Data.ByteString.Base32.Internal.Utils (reChunkN)
import Data.ByteString.Lazy (fromChunks, toChunks)
import Data.ByteString.Lazy.Internal (ByteString(..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL


-- | Encode a 'ByteString' value as a Base32hex 'TL.Text' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> encodeBase32 "Sun"
-- "ADQMS==="
encodeBase32 :: ByteString -> Base32 'Hex TL.Text
encodeBase32 = fmap TL.decodeUtf8 . encodeBase32'
{-# INLINE encodeBase32 #-}

-- | Encode a 'ByteString' as a Base32hex 'ByteString' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> encodeBase32' "Sun"
-- "ADQMS==="
--
encodeBase32' :: ByteString -> Base32 'Hex ByteString
encodeBase32' = assertBase32 . fromChunks
    . fmap (extractBase32 . B32H.encodeBase32')
    . reChunkN 5
    . toChunks

-- | Decode an arbitrarily padded Base32hex encoded 'ByteString' value. If its length is
-- not a multiple
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
-- >>> decodeBase32Untyped "ADQMS==="
-- Left "Base32-encoded bytestring has invalid padding"
--
decodeBase32Untyped :: ByteString -> Either T.Text ByteString
decodeBase32Untyped = fmap (fromChunks . (:[]))
    . B32H.decodeBase32Untyped
    . BS.concat
    . toChunks
{-# INLINE decodeBase32Untyped #-}

-- | Encode a 'ByteString' value as Base32hex 'TL.Text' without padding. Note that for
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
encodeBase32Unpadded :: ByteString -> Base32 'Hex TL.Text
encodeBase32Unpadded = fmap TL.decodeUtf8 . encodeBase32Unpadded'
{-# INLINE encodeBase32Unpadded #-}

-- | Encode a 'ByteString' value as Base32hex without padding. Note that for Base32hex,
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
encodeBase32Unpadded' :: ByteString -> Base32 'Hex ByteString
encodeBase32Unpadded' = assertBase32 . fromChunks
    . fmap (extractBase32 . B32H.encodeBase32Unpadded')
    . reChunkN 5
    . toChunks

-- | Decode an unpadded Base32hex-encoded 'ByteString' value. Input strings are
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
decodeBase32UnpaddedUntyped :: ByteString -> Either T.Text ByteString
decodeBase32UnpaddedUntyped = fmap (fromChunks . (:[]))
    . B32H.decodeBase32UnpaddedUntyped
    . BS.concat
    . toChunks
{-# INLINE decodeBase32UnpaddedUntyped #-}

-- | Decode a padded Base32hex-encoded 'ByteString' value. Input strings are
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
decodeBase32PaddedUntyped :: ByteString -> Either T.Text ByteString
decodeBase32PaddedUntyped = fmap (fromChunks . (:[]))
    . B32H.decodeBase32PaddedUntyped
    . BS.concat
    . toChunks
{-# INLINE decodeBase32PaddedUntyped #-}

-- | Decode established Base32 provenance without repeating validation.
-- Use 'parseBase32' to validate external input first.
decodeBase32 :: Base32 'Hex ByteString -> ByteString
decodeBase32 = fromChunks
  . fmap (B32H.decodeBase32 . assertBase32)
  . reChunkN 8 . toChunks . extractBase32
{-# INLINE decodeBase32 #-}

-- | Validate encoded input, retaining its original representation and case.
-- Checks alphabet, padding, length, and zero unused bits. Empty input is valid.
-- This validates Base32 bytes, not the character encoding of the payload.
parseBase32 :: ByteString -> Either T.Text (Base32 'Hex ByteString)
parseBase32 value = assertBase32 value
  <$ (decodeBase32Untyped) value
{-# INLINE parseBase32 #-}

-- | Decode established Base32 provenance without repeating validation.
-- Use 'parseBase32Padded' to check the padding policy on external input.
-- Typed decoding accepts either padding form; the alphabet is already known.
decodeBase32Padded :: Base32 'Hex ByteString -> ByteString
decodeBase32Padded = decodeBase32
{-# INLINE decodeBase32Padded #-}

-- | Validate encoded input, retaining its original representation and case.
-- Checks alphabet, padding, length, and zero unused bits. Empty input is valid.
-- This validates Base32 bytes, not the character encoding of the payload.
parseBase32Padded :: ByteString -> Either T.Text (Base32 'Hex ByteString)
parseBase32Padded value = assertBase32 value
  <$ (decodeBase32PaddedUntyped) value
{-# INLINE parseBase32Padded #-}

-- | Decode established Base32 provenance without repeating validation.
-- Use 'parseBase32Unpadded' to check the padding policy on external input.
-- Typed decoding accepts either padding form; the alphabet is already known.
decodeBase32Unpadded :: Base32 'Hex ByteString -> ByteString
decodeBase32Unpadded = decodeBase32
{-# INLINE decodeBase32Unpadded #-}

-- | Validate encoded input, retaining its original representation and case.
-- Checks alphabet, padding, length, and zero unused bits. Empty input is valid.
-- This validates Base32 bytes, not the character encoding of the payload.
parseBase32Unpadded :: ByteString -> Either T.Text (Base32 'Hex ByteString)
parseBase32Unpadded value = assertBase32 value
  <$ (decodeBase32UnpaddedUntyped) value
{-# INLINE parseBase32Unpadded #-}
