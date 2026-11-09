{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
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
--
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
import Data.Text.Encoding.Base32.Error (Base32Error(..))

-- | Encode a 'Text' value in Base32 with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> encodeBase32 (Text.pack "Sun")
-- "KN2W4==="
--
encodeBase32 :: Text -> Base32 'StdPadded Text
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
-- >>> decodeBase32Untyped (Text.pack "KN2W4===")
-- Right "Sun"
--
-- >>> decodeBase32Untyped (Text.pack "KN2W4")
-- Right "Sun"
--
-- >>> decodeBase32Untyped (Text.pack "KN2W===")
-- Left "Base32-encoded bytestring has invalid padding"
decodeBase32Untyped :: Text -> Either Text Text
decodeBase32Untyped value = B32.decodeBase32Untyped (T.encodeUtf8 value)
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
-- >>> decodeBase32With UTF8.decodeUtf8' (BS8.pack "KN2W4===")
-- Right "Sun"
-- >>> decodeBase32With UTF8.decodeUtf8' (BS8.pack "!")
-- Left (DecodeError "Base32-encoded bytestring has invalid size")
-- >>> decodeBase32With (const (Left "unsupported encoding")) (BS8.pack "KN2W4===")
-- Left (ConversionError "unsupported encoding")
--
decodeBase32With
    :: (ByteString -> Either err Text)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ByteString
      -- ^ Input text to decode
    -> Either (Base32Error err) Text
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
-- >>> encodeBase32Unpadded (Text.pack "Sun")
-- "KN2W4"
--
encodeBase32Unpadded :: Text -> Base32 'StdUnpadded Text
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
-- >>> decodeBase32UnpaddedUntyped (Text.pack "KN2W4")
-- Right "Sun"
--
-- >>> decodeBase32UnpaddedUntyped (Text.pack "KN2W4===")
-- Left "Base32-encoded bytestring has invalid padding"
decodeBase32UnpaddedUntyped :: Text -> Either Text Text
decodeBase32UnpaddedUntyped value = B32.decodeBase32UnpaddedUntyped (T.encodeUtf8 value)
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
-- >>> decodeBase32UnpaddedWith UTF8.decodeUtf8' (BS8.pack "KN2W4")
-- Right "Sun"
-- >>> decodeBase32UnpaddedWith UTF8.decodeUtf8' (BS8.pack "!")
-- Left (DecodeError "Base32-encoded bytestring has invalid size")
-- >>> decodeBase32UnpaddedWith (const (Left "unsupported encoding")) (BS8.pack "KN2W4")
-- Left (ConversionError "unsupported encoding")
--
decodeBase32UnpaddedWith
    :: (ByteString -> Either err Text)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ByteString
      -- ^ Input text to decode
    -> Either (Base32Error err) Text
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
-- >>> decodeBase32PaddedUntyped (Text.pack "KN2W4===")
-- Right "Sun"
--
-- >>> decodeBase32PaddedUntyped (Text.pack "KN2W4")
-- Left "Base32-encoded bytestring requires padding"
decodeBase32PaddedUntyped :: Text -> Either Text Text
decodeBase32PaddedUntyped value = B32.decodeBase32PaddedUntyped (T.encodeUtf8 value)
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
-- >>> decodeBase32PaddedWith UTF8.decodeUtf8' (BS8.pack "KN2W4===")
-- Right "Sun"
-- >>> decodeBase32PaddedWith UTF8.decodeUtf8' (BS8.pack "!")
-- Left (DecodeError "Base32-encoded bytestring has invalid size")
-- >>> decodeBase32PaddedWith (const (Left "unsupported encoding")) (BS8.pack "KN2W4===")
-- Left (ConversionError "unsupported encoding")
--
decodeBase32PaddedWith
    :: (ByteString -> Either err Text)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ByteString
      -- ^ Input text to decode
    -> Either (Base32Error err) Text
decodeBase32PaddedWith f t = case B32.decodeBase32PaddedUntyped t of
    Left de -> Left $ DecodeError de
    Right a -> first ConversionError (f a)
{-# INLINE decodeBase32PaddedWith #-}

-- | Decode established Base32 provenance without repeating validation.
-- Use 'parseBase32Padded' or 'parseBase32Unpadded' for external input.
-- Decoded Text payloads must be valid UTF-8.
-- Character conversion is separate from the Base32 provenance.
--
-- >>> decodeBase32 (encodeBase32 (Text.pack "Sun"))
-- "Sun"
-- >>> decodeBase32 (encodeBase32Unpadded (Text.pack "Sun"))
-- "Sun"
--
decodeBase32 :: StdAlphabet k => Base32 k Text -> Text
decodeBase32 = T.decodeUtf8 . B32.decodeBase32 . fmap T.encodeUtf8
{-# INLINE decodeBase32 #-}

-- | Decode established Base32 provenance without repeating validation.
-- Use 'parseBase32Padded' to check the padding policy on external input.
-- The type establishes the alphabet and padding policy.
-- Decoded Text payloads must be valid UTF-8.
-- Character conversion is separate from the Base32 provenance.
--
-- >>> decodeBase32Padded (encodeBase32 (Text.pack "Sun"))
-- "Sun"
--
decodeBase32Padded :: Base32 'StdPadded Text -> Text
decodeBase32Padded = decodeBase32
{-# INLINE decodeBase32Padded #-}

-- | Validate encoded input, retaining its original representation and case.
-- Checks alphabet, padding, length, and zero unused bits. Empty input is valid.
-- This validates Base32 bytes, not the character encoding of the payload.
--
-- >>> parseBase32Padded (Text.pack "kn2w4===")
-- Right "kn2w4==="
-- >>> fmap decodeBase32Padded (parseBase32Padded (Text.pack "KN2W4==="))
-- Right "Sun"
-- >>> parseBase32Padded (Text.pack "")
-- Right ""
-- >>> parseBase32Padded (Text.pack "KN2W4")
-- Left "Base32-encoded bytestring requires padding"
--
parseBase32Padded :: Text -> Either Text (Base32 'StdPadded Text)
parseBase32Padded value = assertBase32 value
  <$ (B32.decodeBase32PaddedUntyped . T.encodeUtf8) value
{-# INLINE parseBase32Padded #-}

-- | Decode established Base32 provenance without repeating validation.
-- Use 'parseBase32Unpadded' to check the padding policy on external input.
-- The type establishes the alphabet and padding policy.
-- Decoded Text payloads must be valid UTF-8.
-- Character conversion is separate from the Base32 provenance.
--
-- >>> decodeBase32Unpadded (encodeBase32Unpadded (Text.pack "Sun"))
-- "Sun"
--
decodeBase32Unpadded :: Base32 'StdUnpadded Text -> Text
decodeBase32Unpadded = decodeBase32
{-# INLINE decodeBase32Unpadded #-}

-- | Validate encoded input, retaining its original representation and case.
-- Checks alphabet, padding, length, and zero unused bits. Empty input is valid.
-- This validates Base32 bytes, not the character encoding of the payload.
--
-- >>> parseBase32Unpadded (Text.pack "kn2w4")
-- Right "kn2w4"
-- >>> fmap decodeBase32Unpadded (parseBase32Unpadded (Text.pack "KN2W4"))
-- Right "Sun"
-- >>> parseBase32Unpadded (Text.pack "")
-- Right ""
-- >>> parseBase32Unpadded (Text.pack "KN2W4===")
-- Left "Base32-encoded bytestring has invalid padding"
-- >>> isLeft (parseBase32Unpadded (Text.pack "MZ"))
-- True
--
parseBase32Unpadded :: Text -> Either Text (Base32 'StdUnpadded Text)
parseBase32Unpadded value = assertBase32 value
  <$ (B32.decodeBase32UnpaddedUntyped . T.encodeUtf8) value
{-# INLINE parseBase32Unpadded #-}

-- $setup
-- >>> import qualified Data.Text as Text
-- >>> import Data.Either (isLeft)
-- >>> import qualified Data.ByteString.Char8 as BS8
-- >>> import qualified Data.Text.Encoding as UTF8
