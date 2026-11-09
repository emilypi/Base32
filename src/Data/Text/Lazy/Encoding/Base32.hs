{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}
-- |
-- Module       : Data.Text.Lazy.Encoding.Base32
-- Copyright    : (c) 2019-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.Text.Lazy.Text'-valued combinators for
-- implementing the RFC 4648 specification of the Base32
-- encoding format. This includes strictly padded/unpadded decoding
-- variants, as well as internal and external validation for canonicity.
--
module Data.Text.Lazy.Encoding.Base32
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
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Base32 as BL32

import qualified Data.Text as T
import Data.Text.Encoding.Base32.Error
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

-- | Encode a 'TL.Text' value in Base32 with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> encodeBase32 "Sun"
-- "KN2W4==="
--
encodeBase32 :: TL.Text -> Base32 'Std TL.Text
encodeBase32 = BL32.encodeBase32 . TL.encodeUtf8
{-# INLINE encodeBase32 #-}

-- | Decode an arbitrarily padded Base32-encoded 'TL.Text' value. If its length is not a
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
decodeBase32Untyped :: TL.Text -> Either T.Text TL.Text
decodeBase32Untyped value = BL32.decodeBase32Untyped (TL.encodeUtf8 value)
  >>= first (ErrorText.pack . show) . TL.decodeUtf8'
{-# INLINE decodeBase32Untyped #-}

-- | Attempt to decode a lazy 'ByteString' value as Base32, converting from
-- 'ByteString' to 'TL.Text' according to some encoding function. In practice,
-- This is something like 'TL.decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- @
-- 'decodeBase32With' 'TL.decodeUtf8''
--   :: 'ByteString' -> 'Either' ('Base32Error' UnicodeException) 'TL.Text'
-- @
decodeBase32With
    :: (ByteString -> Either err TL.Text)
      -- ^ convert a bytestring to text (e.g. 'TL.decodeUtf8'')
    -> ByteString
      -- ^ Input text to decode
    -> Either (Base32Error err) TL.Text
decodeBase32With f t = case BL32.decodeBase32Untyped t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase32With #-}

-- | Encode a 'TL.Text' value in Base32 without padding. Note that for Base32,
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
encodeBase32Unpadded :: TL.Text -> Base32 'Std TL.Text
encodeBase32Unpadded = BL32.encodeBase32Unpadded . TL.encodeUtf8
{-# INLINE encodeBase32Unpadded #-}

-- | Decode an unpadded Base32 encoded 'TL.Text' value.
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
decodeBase32UnpaddedUntyped :: TL.Text -> Either T.Text TL.Text
decodeBase32UnpaddedUntyped value = BL32.decodeBase32UnpaddedUntyped (TL.encodeUtf8 value)
  >>= first (ErrorText.pack . show) . TL.decodeUtf8'
{-# INLINE decodeBase32UnpaddedUntyped #-}

-- | Attempt to decode an unpadded lazy 'ByteString' value as Base32, converting from
-- 'ByteString' to 'TL.Text' according to some encoding function. In practice,
-- This is something like 'TL.decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- @
-- 'decodeBase32UnpaddedWith' 'TL.decodeUtf8''
--   :: 'ByteString' -> 'Either' ('Base32Error' UnicodeException) 'TL.Text'
-- @
decodeBase32UnpaddedWith
    :: (ByteString -> Either err TL.Text)
      -- ^ convert a bytestring to text (e.g. 'TL.decodeUtf8'')
    -> ByteString
      -- ^ Input text to decode
    -> Either (Base32Error err) TL.Text
decodeBase32UnpaddedWith f t = case BL32.decodeBase32UnpaddedUntyped t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase32UnpaddedWith #-}

-- | Decode an padded Base32 encoded 'TL.Text' value
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
decodeBase32PaddedUntyped :: TL.Text -> Either T.Text TL.Text
decodeBase32PaddedUntyped value = BL32.decodeBase32PaddedUntyped (TL.encodeUtf8 value)
  >>= first (ErrorText.pack . show) . TL.decodeUtf8'
{-# INLINE decodeBase32PaddedUntyped #-}

-- | Attempt to decode a padded lazy 'ByteString' value as Base32, converting from
-- 'ByteString' to 'TL.Text' according to some encoding function. In practice,
-- This is something like 'TL.decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Example__:
--
-- @
-- 'decodeBase32PaddedWith' 'TL.decodeUtf8''
--   :: 'ByteString' -> 'Either' ('Base32Error' UnicodeException) 'TL.Text'
-- @
decodeBase32PaddedWith
    :: (ByteString -> Either err TL.Text)
      -- ^ convert a bytestring to text (e.g. 'TL.decodeUtf8'')
    -> ByteString
      -- ^ Input text to decode
    -> Either (Base32Error err) TL.Text
decodeBase32PaddedWith f t = case BL32.decodeBase32PaddedUntyped t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase32PaddedWith #-}

-- | Decode established Base32 provenance without repeating validation.
-- Use 'parseBase32' to validate external input first.
-- Decoded Text payloads must be valid UTF-8.
-- Character conversion is separate from the Base32 provenance.
decodeBase32 :: Base32 'Std TL.Text -> TL.Text
decodeBase32 = TL.decodeUtf8 . BL32.decodeBase32 . fmap TL.encodeUtf8
{-# INLINE decodeBase32 #-}

-- | Validate encoded input, retaining its original representation and case.
-- Checks alphabet, padding, length, and zero unused bits. Empty input is valid.
-- This validates Base32 bytes, not the character encoding of the payload.
parseBase32 :: TL.Text -> Either T.Text (Base32 'Std TL.Text)
parseBase32 value = assertBase32 value
  <$ (BL32.decodeBase32Untyped . TL.encodeUtf8) value
{-# INLINE parseBase32 #-}

-- | Decode established Base32 provenance without repeating validation.
-- Use 'parseBase32Padded' to check the padding policy on external input.
-- Typed decoding accepts either padding form; the alphabet is already known.
-- Decoded Text payloads must be valid UTF-8.
-- Character conversion is separate from the Base32 provenance.
decodeBase32Padded :: Base32 'Std TL.Text -> TL.Text
decodeBase32Padded = decodeBase32
{-# INLINE decodeBase32Padded #-}

-- | Validate encoded input, retaining its original representation and case.
-- Checks alphabet, padding, length, and zero unused bits. Empty input is valid.
-- This validates Base32 bytes, not the character encoding of the payload.
parseBase32Padded :: TL.Text -> Either T.Text (Base32 'Std TL.Text)
parseBase32Padded value = assertBase32 value
  <$ (BL32.decodeBase32PaddedUntyped . TL.encodeUtf8) value
{-# INLINE parseBase32Padded #-}

-- | Decode established Base32 provenance without repeating validation.
-- Use 'parseBase32Unpadded' to check the padding policy on external input.
-- Typed decoding accepts either padding form; the alphabet is already known.
-- Decoded Text payloads must be valid UTF-8.
-- Character conversion is separate from the Base32 provenance.
decodeBase32Unpadded :: Base32 'Std TL.Text -> TL.Text
decodeBase32Unpadded = decodeBase32
{-# INLINE decodeBase32Unpadded #-}

-- | Validate encoded input, retaining its original representation and case.
-- Checks alphabet, padding, length, and zero unused bits. Empty input is valid.
-- This validates Base32 bytes, not the character encoding of the payload.
parseBase32Unpadded :: TL.Text -> Either T.Text (Base32 'Std TL.Text)
parseBase32Unpadded value = assertBase32 value
  <$ (BL32.decodeBase32UnpaddedUntyped . TL.encodeUtf8) value
{-# INLINE parseBase32Unpadded #-}
