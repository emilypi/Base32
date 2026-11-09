{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}

-- |
-- Module       : Data.Text.Encoding.Base32
-- Copyright    : (c) 2019-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.Text.Text'-valued combinators for
-- implementing the RFC 4648 specification of the Base32
-- encoding format. This includes strictly padded/unpadded decoding variants, as well as
-- internal and external validation for canonicity.
module Data.Text.Encoding.Base32
  ( encodeBase32
  , decodeBase32Untyped
  , decodeBase32With
  , encodeBase32Unpadded
  , decodeBase32UnpaddedUntyped
  , decodeBase32UnpaddedWith
  , decodeBase32PaddedUntyped
  , decodeBase32PaddedWith
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
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base32 as B32

import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Text.Encoding.Base32.Error (Base32Error (..))

-- | Encode a 'Text' value in Base32 with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> encodeBase32 "Sun"
-- "KN2W4==="
encodeBase32 :: Text -> Base32 'Std Text
encodeBase32 = B32.encodeBase32 . T.encodeUtf8
{-# INLINE encodeBase32 #-}

-- | Decode an arbitrarily padded Base32-encoded 'Text' value. If its length is not a
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
decodeBase32Untyped :: Text -> Either Text Text
decodeBase32Untyped value =
  B32.decodeBase32Untyped (T.encodeUtf8 value)
    >>= first (ErrorText.pack . show) . T.decodeUtf8'
{-# INLINE decodeBase32Untyped #-}

-- | Attempt to decode a 'Text' value as Base32, converting from
-- 'ByteString' to 'Text' according to some encoding function. In practice,
-- This is something like 'T.decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Example__:
--
-- @
-- 'decodeBase32With' 'T.decodeUtf8''
--   :: 'ByteString' -> 'Either' ('Base32Error' UnicodeException) 'Text'
-- @
decodeBase32With ::
  -- | convert a bytestring to text (e.g. 'T.decodeUtf8'')
  (ByteString -> Either err Text) ->
  -- | Input text to decode
  ByteString ->
  Either (Base32Error err) Text
decodeBase32With f t = case B32.decodeBase32Untyped t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase32With #-}

-- | Encode a 'Text' value in Base32 without padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> encodeBase32Unpadded "Sun"
-- "KN2W4"
encodeBase32Unpadded :: Text -> Base32 'Std Text
encodeBase32Unpadded = B32.encodeBase32Unpadded . T.encodeUtf8
{-# INLINE encodeBase32Unpadded #-}

-- | Decode an unpadded Base32 encoded 'Text' value.
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
decodeBase32UnpaddedUntyped :: Text -> Either Text Text
decodeBase32UnpaddedUntyped value =
  B32.decodeBase32UnpaddedUntyped (T.encodeUtf8 value)
    >>= first (ErrorText.pack . show) . T.decodeUtf8'
{-# INLINE decodeBase32UnpaddedUntyped #-}

-- | Attempt to decode an unpadded 'ByteString' value as Base32, converting from
-- 'ByteString' to 'Text' according to some encoding function. In practice,
-- This is something like 'T.decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Example__:
--
-- @
-- 'decodeBase32UnpaddedWith' 'T.decodeUtf8''
--   :: 'ByteString' -> 'Either' ('Base32Error' UnicodeException) 'Text'
-- @
decodeBase32UnpaddedWith ::
  -- | convert a bytestring to text (e.g. 'T.decodeUtf8'')
  (ByteString -> Either err Text) ->
  -- | Input text to decode
  ByteString ->
  Either (Base32Error err) Text
decodeBase32UnpaddedWith f t = case B32.decodeBase32UnpaddedUntyped t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase32UnpaddedWith #-}

-- | Decode an padded Base32 encoded 'Text' value
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
decodeBase32PaddedUntyped :: Text -> Either Text Text
decodeBase32PaddedUntyped value =
  B32.decodeBase32PaddedUntyped (T.encodeUtf8 value)
    >>= first (ErrorText.pack . show) . T.decodeUtf8'
{-# INLINE decodeBase32PaddedUntyped #-}

-- | Attempt to decode a padded 'ByteString' value as Base32, converting from
-- 'ByteString' to 'Text' according to some encoding function. In practice,
-- This is something like 'T.decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Example__:
--
-- @
-- 'decodeBase32PaddedWith' 'T.decodeUtf8''
--   :: 'ByteString' -> 'Either' ('Base32Error' UnicodeException) 'Text'
-- @
decodeBase32PaddedWith ::
  -- | convert a bytestring to text (e.g. 'T.decodeUtf8'')
  (ByteString -> Either err Text) ->
  -- | Input text to decode
  ByteString ->
  Either (Base32Error err) Text
decodeBase32PaddedWith f t = case B32.decodeBase32PaddedUntyped t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase32PaddedWith #-}

-- | Decode a value with established alphabet provenance.
-- Use 'parseBase32' to validate external input first.
-- A false assertion or incompatible padding policy causes an error.
-- Decoded bytes must be valid UTF-8; use the checked decoder otherwise.
decodeBase32 :: Base32 'Std Text -> Text
decodeBase32 =
  either (error . ErrorText.unpack) id
    . decodeBase32Untyped
    . extractBase32
{-# INLINE decodeBase32 #-}

-- | Validate encoded input, retaining its original representation and case.
-- Checks alphabet, padding, length, and zero unused bits. Empty input is valid.
-- This validates Base32 bytes, not the character encoding of the payload.
parseBase32 :: Text -> Either Text (Base32 'Std Text)
parseBase32 value =
  assertBase32 value
    <$ (B32.decodeBase32Untyped . T.encodeUtf8) value
{-# INLINE parseBase32 #-}

-- | Decode a value with established alphabet provenance.
-- Use 'parseBase32Padded' to validate external input first.
-- A false assertion or incompatible padding policy causes an error.
-- Decoded bytes must be valid UTF-8; use the checked decoder otherwise.
decodeBase32Padded :: Base32 'Std Text -> Text
decodeBase32Padded =
  either (error . ErrorText.unpack) id
    . decodeBase32PaddedUntyped
    . extractBase32
{-# INLINE decodeBase32Padded #-}

-- | Validate encoded input, retaining its original representation and case.
-- Checks alphabet, padding, length, and zero unused bits. Empty input is valid.
-- This validates Base32 bytes, not the character encoding of the payload.
parseBase32Padded :: Text -> Either Text (Base32 'Std Text)
parseBase32Padded value =
  assertBase32 value
    <$ (B32.decodeBase32PaddedUntyped . T.encodeUtf8) value
{-# INLINE parseBase32Padded #-}

-- | Decode a value with established alphabet provenance.
-- Use 'parseBase32Unpadded' to validate external input first.
-- A false assertion or incompatible padding policy causes an error.
-- Decoded bytes must be valid UTF-8; use the checked decoder otherwise.
decodeBase32Unpadded :: Base32 'Std Text -> Text
decodeBase32Unpadded =
  either (error . ErrorText.unpack) id
    . decodeBase32UnpaddedUntyped
    . extractBase32
{-# INLINE decodeBase32Unpadded #-}

-- | Validate encoded input, retaining its original representation and case.
-- Checks alphabet, padding, length, and zero unused bits. Empty input is valid.
-- This validates Base32 bytes, not the character encoding of the payload.
parseBase32Unpadded :: Text -> Either Text (Base32 'Std Text)
parseBase32Unpadded value =
  assertBase32 value
    <$ (B32.decodeBase32UnpaddedUntyped . T.encodeUtf8) value
{-# INLINE parseBase32Unpadded #-}
