{-# LANGUAGE Safe #-}
{-# LANGUAGE DataKinds #-}

-- |
-- Module       : Data.Text.Short.Encoding.Base32.Hex
-- Copyright    : (c) 2019-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.Text.Short.ShortText'-valued combinators
-- implementing the RFC 4648 specification for the Base32hex
-- encoding format. This includes strictly padded/unpadded
-- decoding variants, and external + internal validations for canonicity.
--
module Data.Text.Short.Encoding.Base32.Hex
( -- * Encoding
  encodeBase32
, encodeBase32Unpadded
  -- * Decoding
, decodeBase32Untyped
, decodeBase32With
, decodeBase32UnpaddedUntyped
, decodeBase32UnpaddedWith
, decodeBase32PaddedUntyped
, decodeBase32PaddedWith
  -- * Validation
, decodeBase32
, decodeBase32Padded
, decodeBase32Unpadded
, parseBase32
, parseBase32Padded
, parseBase32Unpadded
) where


import Data.Base32.Types
import qualified Data.Text as ErrorText

import Data.Bifunctor (first)
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Base32.Hex as BS32H
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Base32.Error
import Data.Text.Short


-- | Encode a 'ShortText' value in Base32hex with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> encodeBase32 "Sun"
-- "ADQMS==="
--
encodeBase32 :: ShortText -> Base32 'Hex ShortText
encodeBase32 =
  BS32H.encodeBase32
    . toShortByteString
{-# INLINE encodeBase32 #-}

-- | Decode an arbitrarily padded Base32hex-encoded 'ShortText' value. If its length is
-- not a multiple
-- of 8, then padding chars will be added to fill out the input to a multiple of
-- 8 for safe decoding as base32hex encodings are optionally padded.
--
-- Decoded bytes are interpreted as UTF-8. Invalid Base32 or UTF-8
-- returns 'Left'. Use the corresponding @decode*With@ function
-- to choose a different character encoding.
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
decodeBase32Untyped :: ShortText -> Either Text ShortText
decodeBase32Untyped value =
  BS32H.decodeBase32Untyped (toShortByteString value)
    >>= maybe (Left (ErrorText.pack "invalid UTF-8 payload")) Right
      . fromShortByteString
{-# INLINE decodeBase32Untyped #-}

-- | Attempt to decode a 'ShortByteString' value as Base32hex, converting from
-- 'ShortByteString' to 'ShortText' according to some encoding function. In practice,
-- The conversion can report a character-encoding error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- @
-- 'decodeBase32With' '(fmap fromText . Data.Text.Encoding.decodeUtf8' . toText)'
--   :: 'ShortByteString' -> 'Either' ('Base32Error' 'String') 'ShortText'
-- @
decodeBase32With
    :: (ShortByteString -> Either err ShortText)
      -- ^ convert a bytestring to text (e.g. '(fmap fromText .
      -- Data.Text.Encoding.decodeUtf8' . toText)')
    -> ShortByteString
      -- ^ Input text to decode
    -> Either (Base32Error err) ShortText
decodeBase32With f t = case BS32H.decodeBase32Untyped t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase32With #-}

-- | Encode a 'ShortText' value in Base32hex without padding. Note that for Base32hex,
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
encodeBase32Unpadded :: ShortText -> Base32 'Hex ShortText
encodeBase32Unpadded =
  BS32H.encodeBase32Unpadded
    . toShortByteString
{-# INLINE encodeBase32Unpadded #-}

-- | Decode an unpadded Base32hex encoded 'ShortText' value.
--
-- Decoded bytes are interpreted as UTF-8. Invalid Base32 or UTF-8
-- returns 'Left'. Use the corresponding @decode*With@ function
-- to choose a different character encoding.
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
decodeBase32UnpaddedUntyped :: ShortText -> Either Text ShortText
decodeBase32UnpaddedUntyped value =
  BS32H.decodeBase32UnpaddedUntyped (toShortByteString value)
    >>= maybe (Left (ErrorText.pack "invalid UTF-8 payload")) Right
      . fromShortByteString
{-# INLINE decodeBase32UnpaddedUntyped #-}

-- | Attempt to decode an unpadded 'ShortByteString' value as Base32hex, converting from
-- 'ShortByteString' to 'ShortText' according to some encoding function. In practice,
-- The conversion can report a character-encoding error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- @
-- 'decodeBase32UnpaddedWith' '(fmap fromText . Data.Text.Encoding.decodeUtf8' . toText)'
--   :: 'ShortByteString' -> 'Either' ('Base32Error' 'String') 'ShortText'
-- @
decodeBase32UnpaddedWith
    :: (ShortByteString -> Either err ShortText)
      -- ^ convert a bytestring to text (e.g. '(fmap fromText .
      -- Data.Text.Encoding.decodeUtf8' . toText)')
    -> ShortByteString
      -- ^ Input text to decode
    -> Either (Base32Error err) ShortText
decodeBase32UnpaddedWith f t = case BS32H.decodeBase32UnpaddedUntyped t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase32UnpaddedWith #-}

-- | Decode an padded Base32hex encoded 'ShortText' value
--
-- Decoded bytes are interpreted as UTF-8. Invalid Base32 or UTF-8
-- returns 'Left'. Use the corresponding @decode*With@ function
-- to choose a different character encoding.
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
decodeBase32PaddedUntyped :: ShortText -> Either Text ShortText
decodeBase32PaddedUntyped value =
  BS32H.decodeBase32PaddedUntyped (toShortByteString value)
    >>= maybe (Left (ErrorText.pack "invalid UTF-8 payload")) Right
      . fromShortByteString
{-# INLINE decodeBase32PaddedUntyped #-}

-- | Attempt to decode a padded 'ShortByteString' value as Base32hex, converting from
-- 'ShortByteString' to 'ShortText' according to some encoding function. In practice,
-- The conversion can report a character-encoding error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- @
-- 'decodeBase32With' '(fmap fromText . Data.Text.Encoding.decodeUtf8' . toText)'
--   :: 'ShortByteString' -> 'Either' ('Base32Error' 'String') 'ShortText'
-- @
decodeBase32PaddedWith
    :: (ShortByteString -> Either err ShortText)
      -- ^ convert a bytestring to text (e.g. 'Data.Text.Encoding.decodeUtf8'')
    -> ShortByteString
      -- ^ Input text to decode
    -> Either (Base32Error err) ShortText
decodeBase32PaddedWith f t = case BS32H.decodeBase32PaddedUntyped t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase32PaddedWith #-}

-- | Decode established Base32 provenance without repeating validation.
-- Use 'parseBase32' to validate external input first.
-- Decoded Text payloads must be valid UTF-8.
-- Character conversion is separate from the Base32 provenance.
decodeBase32 :: Base32 'Hex ShortText -> ShortText
decodeBase32 = fromText . TE.decodeUtf8 . SBS.fromShort
  . BS32H.decodeBase32 . fmap toShortByteString
{-# INLINE decodeBase32 #-}

-- | Validate encoded input, retaining its original representation and case.
-- Checks alphabet, padding, length, and zero unused bits. Empty input is valid.
-- This validates Base32 bytes, not the character encoding of the payload.
parseBase32 :: ShortText -> Either Text (Base32 'Hex ShortText)
parseBase32 value = assertBase32 value
  <$ (BS32H.decodeBase32Untyped . toShortByteString) value
{-# INLINE parseBase32 #-}

-- | Decode established Base32 provenance without repeating validation.
-- Use 'parseBase32Padded' to check the padding policy on external input.
-- Typed decoding accepts either padding form; the alphabet is already known.
-- Decoded Text payloads must be valid UTF-8.
-- Character conversion is separate from the Base32 provenance.
decodeBase32Padded :: Base32 'Hex ShortText -> ShortText
decodeBase32Padded = decodeBase32
{-# INLINE decodeBase32Padded #-}

-- | Validate encoded input, retaining its original representation and case.
-- Checks alphabet, padding, length, and zero unused bits. Empty input is valid.
-- This validates Base32 bytes, not the character encoding of the payload.
parseBase32Padded :: ShortText -> Either Text (Base32 'Hex ShortText)
parseBase32Padded value = assertBase32 value
  <$ (BS32H.decodeBase32PaddedUntyped . toShortByteString) value
{-# INLINE parseBase32Padded #-}

-- | Decode established Base32 provenance without repeating validation.
-- Use 'parseBase32Unpadded' to check the padding policy on external input.
-- Typed decoding accepts either padding form; the alphabet is already known.
-- Decoded Text payloads must be valid UTF-8.
-- Character conversion is separate from the Base32 provenance.
decodeBase32Unpadded :: Base32 'Hex ShortText -> ShortText
decodeBase32Unpadded = decodeBase32
{-# INLINE decodeBase32Unpadded #-}

-- | Validate encoded input, retaining its original representation and case.
-- Checks alphabet, padding, length, and zero unused bits. Empty input is valid.
-- This validates Base32 bytes, not the character encoding of the payload.
parseBase32Unpadded :: ShortText -> Either Text (Base32 'Hex ShortText)
parseBase32Unpadded value = assertBase32 value
  <$ (BS32H.decodeBase32UnpaddedUntyped . toShortByteString) value
{-# INLINE parseBase32Unpadded #-}
