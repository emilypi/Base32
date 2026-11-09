{-# LANGUAGE Safe #-}
{-# LANGUAGE DataKinds #-}

-- |
-- Module       : Data.Text.Short.Encoding.Base32
-- Copyright    : (c) 2019-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.Text.Short.ShortText'-valued combinators
-- implementing the RFC 4648 specification for the Base32
-- encoding format. This includes strictly padded/unpadded decoding
-- variants, and external + internal validations for canonicity.
--
module Data.Text.Short.Encoding.Base32
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
import qualified Data.ByteString.Short.Base32 as BS32
import Data.Text (Text)
import Data.Text.Encoding.Base32.Error
import Data.Text.Short


-- | Encode a 'ShortText' value in Base32 with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> encodeBase32 "Sun"
-- "KN2W4==="
--
encodeBase32 :: ShortText -> Base32 'Std ShortText
encodeBase32 =
  BS32.encodeBase32
    . toShortByteString
{-# INLINE encodeBase32 #-}

-- | Decode an arbitrarily padded Base32-encoded 'ShortText' value. If its length is not a
-- multiple
-- of 8, then padding chars will be added to fill out the input to a multiple of
-- 8 for safe decoding as base32 encodings are optionally padded.
--
-- Decoded bytes are interpreted as UTF-8. Invalid Base32 or UTF-8
-- returns 'Left'. Use the corresponding @decode*With@ function
-- to choose a different character encoding.
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
decodeBase32Untyped :: ShortText -> Either Text ShortText
decodeBase32Untyped value =
  BS32.decodeBase32Untyped (toShortByteString value)
    >>= maybe (Left (ErrorText.pack "invalid UTF-8 payload")) Right
      . fromShortByteString
{-# INLINE decodeBase32Untyped #-}

-- | Attempt to decode a 'ShortByteString' value as Base32, converting from
-- 'ShortByteString' to 'ShortText' according to some encoding function. In practice,
-- The conversion can report a character-encoding error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- @
-- 'decodeBase32With'
--   (maybe (Left "invalid UTF-8") Right . fromShortByteString)
--   :: 'ShortByteString' -> 'Either' ('Base32Error' 'String') 'ShortText'
-- @
decodeBase32With
    :: (ShortByteString -> Either err ShortText)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ShortByteString
      -- ^ Input text to decode
    -> Either (Base32Error err) ShortText
decodeBase32With f t = case BS32.decodeBase32Untyped t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase32With #-}

-- | Encode a 'ShortText' value in Base32 without padding. Note that for Base32,
-- padding is optional. If you call this function, you will simply be encoding
-- as Base32 and stripping padding chars from the output.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> encodeBase32Unpadded "Sun"
-- "KN2W4"
--
encodeBase32Unpadded :: ShortText -> Base32 'Std ShortText
encodeBase32Unpadded =
  BS32.encodeBase32Unpadded
    . toShortByteString
{-# INLINE encodeBase32Unpadded #-}

-- | Decode an unpadded Base32 encoded 'ShortText' value.
--
-- Decoded bytes are interpreted as UTF-8. Invalid Base32 or UTF-8
-- returns 'Left'. Use the corresponding @decode*With@ function
-- to choose a different character encoding.
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
decodeBase32UnpaddedUntyped :: ShortText -> Either Text ShortText
decodeBase32UnpaddedUntyped value =
  BS32.decodeBase32UnpaddedUntyped (toShortByteString value)
    >>= maybe (Left (ErrorText.pack "invalid UTF-8 payload")) Right
      . fromShortByteString
{-# INLINE decodeBase32UnpaddedUntyped #-}

-- | Attempt to decode an unpadded 'ShortByteString' value as Base32, converting from
-- 'ShortByteString' to 'ShortText' according to some encoding function. In practice,
-- The conversion can report a character-encoding error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- @
-- 'decodeBase32UnpaddedWith'
--   (maybe (Left "invalid UTF-8") Right . fromShortByteString)
--   :: 'ShortByteString' -> 'Either' ('Base32Error' 'String') 'ShortText'
-- @
decodeBase32UnpaddedWith
    :: (ShortByteString -> Either err ShortText)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ShortByteString
      -- ^ Input text to decode
    -> Either (Base32Error err) ShortText
decodeBase32UnpaddedWith f t = case BS32.decodeBase32UnpaddedUntyped t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase32UnpaddedWith #-}

-- | Decode an padded Base32 encoded 'ShortText' value
--
-- Decoded bytes are interpreted as UTF-8. Invalid Base32 or UTF-8
-- returns 'Left'. Use the corresponding @decode*With@ function
-- to choose a different character encoding.
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
decodeBase32PaddedUntyped :: ShortText -> Either Text ShortText
decodeBase32PaddedUntyped value =
  BS32.decodeBase32PaddedUntyped (toShortByteString value)
    >>= maybe (Left (ErrorText.pack "invalid UTF-8 payload")) Right
      . fromShortByteString
{-# INLINE decodeBase32PaddedUntyped #-}

-- | Attempt to decode a padded 'ShortByteString' value as Base32, converting from
-- 'ShortByteString' to 'ShortText' according to some encoding function. In practice,
-- The conversion can report a character-encoding error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- @
-- 'decodeBase32With'
--   (maybe (Left "invalid UTF-8") Right . fromShortByteString)
--   :: 'ShortByteString' -> 'Either' ('Base32Error' 'String') 'ShortText'
-- @
decodeBase32PaddedWith
    :: (ShortByteString -> Either err ShortText)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ShortByteString
      -- ^ Input text to decode
    -> Either (Base32Error err) ShortText
decodeBase32PaddedWith f t = case BS32.decodeBase32PaddedUntyped t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase32PaddedWith #-}

-- | Decode a value with established alphabet provenance.
-- Use 'parseBase32' to validate external input first.
-- A false assertion or incompatible padding policy causes an error.
-- Decoded bytes must be valid UTF-8; use the checked decoder otherwise.
decodeBase32 :: Base32 'Std ShortText -> ShortText
decodeBase32 = either (error . ErrorText.unpack) id
  . decodeBase32Untyped . extractBase32
{-# INLINE decodeBase32 #-}

-- | Validate encoded input, retaining its original representation and case.
-- Checks alphabet, padding, length, and zero unused bits. Empty input is valid.
-- This validates Base32 bytes, not the character encoding of the payload.
parseBase32 :: ShortText -> Either Text (Base32 'Std ShortText)
parseBase32 value = assertBase32 value
  <$ (BS32.decodeBase32Untyped . toShortByteString) value
{-# INLINE parseBase32 #-}

-- | Decode a value with established alphabet provenance.
-- Use 'parseBase32Padded' to validate external input first.
-- A false assertion or incompatible padding policy causes an error.
-- Decoded bytes must be valid UTF-8; use the checked decoder otherwise.
decodeBase32Padded :: Base32 'Std ShortText -> ShortText
decodeBase32Padded = either (error . ErrorText.unpack) id
  . decodeBase32PaddedUntyped . extractBase32
{-# INLINE decodeBase32Padded #-}

-- | Validate encoded input, retaining its original representation and case.
-- Checks alphabet, padding, length, and zero unused bits. Empty input is valid.
-- This validates Base32 bytes, not the character encoding of the payload.
parseBase32Padded :: ShortText -> Either Text (Base32 'Std ShortText)
parseBase32Padded value = assertBase32 value
  <$ (BS32.decodeBase32PaddedUntyped . toShortByteString) value
{-# INLINE parseBase32Padded #-}

-- | Decode a value with established alphabet provenance.
-- Use 'parseBase32Unpadded' to validate external input first.
-- A false assertion or incompatible padding policy causes an error.
-- Decoded bytes must be valid UTF-8; use the checked decoder otherwise.
decodeBase32Unpadded :: Base32 'Std ShortText -> ShortText
decodeBase32Unpadded = either (error . ErrorText.unpack) id
  . decodeBase32UnpaddedUntyped . extractBase32
{-# INLINE decodeBase32Unpadded #-}

-- | Validate encoded input, retaining its original representation and case.
-- Checks alphabet, padding, length, and zero unused bits. Empty input is valid.
-- This validates Base32 bytes, not the character encoding of the payload.
parseBase32Unpadded :: ShortText -> Either Text (Base32 'Std ShortText)
parseBase32Unpadded value = assertBase32 value
  <$ (BS32.decodeBase32UnpaddedUntyped . toShortByteString) value
{-# INLINE parseBase32Unpadded #-}
