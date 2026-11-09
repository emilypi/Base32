{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Safe #-}
-- |
-- Module       : Data.Text.Encoding.Base32.Hex
-- Copyright    : (c) 2019-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.Text.Text'-valued combinators for
-- implementing the RFC 4648 specification of the Base32hex
-- encoding format. This includes strictly padded and unpadded
-- decoding variants, as well as internal and external validation
-- for canonicity.
--
module Data.Text.Encoding.Base32.Hex
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
import qualified Data.ByteString.Base32.Hex as B32H

import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Text.Encoding.Base32.Error (Base32Error(..))

-- | Encode a 'Text' value in Base32hex with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> encodeBase32 "Sun"
-- "ADQMS==="
--
-- @since 1.0
--
encodeBase32 :: Text -> Base32 'HexPadded Text
encodeBase32 = B32H.encodeBase32 . T.encodeUtf8
{-# INLINE encodeBase32 #-}

-- | Decode a padded Base32hex-encoded 'Text' value. If its length is not a multiple
-- of 8, then padding chars will be added to fill out the input to a multiple of
-- 8 for safe decoding as base32hex encodings are optionally padded.
--
-- Decoded bytes are interpreted as UTF-8. Invalid Base32 or UTF-8
-- returns 'Left'. Use the corresponding @decode*With@ function
-- to choose a different character encoding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
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
-- @since 1.0
--
decodeBase32Untyped :: Text -> Either Text Text
decodeBase32Untyped value = B32H.decodeBase32Untyped (T.encodeUtf8 value)
  >>= first (ErrorText.pack . show) . T.decodeUtf8'
{-# INLINE decodeBase32Untyped #-}

-- | Attempt to decode a 'ByteString' value as Base32hex, converting from
-- 'ByteString' to 'Text' according to some encoding function. In practice,
-- This is something like 'T.decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> decodeBase32With UTF8.decodeUtf8' "ADQMS==="
-- Right "Sun"
--
-- >>> decodeBase32With UTF8.decodeUtf8' "!"
-- Left (DecodeError "Base32-encoded bytestring has invalid size")
--
-- >>> decodeBase32With (const (Left "unsupported encoding")) "ADQMS==="
-- Left (ConversionError "unsupported encoding")
--
decodeBase32With
    :: (ByteString -> Either err Text)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ByteString
      -- ^ Input text to decode
    -> Either (Base32Error err) Text
decodeBase32With f t = case B32H.decodeBase32Untyped t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase32With #-}

-- | Encode a 'Text' value in Base32hex without padding. Note that for Base32hex,
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
-- @since 1.0
--
encodeBase32Unpadded :: Text -> Base32 'HexUnpadded Text
encodeBase32Unpadded = B32H.encodeBase32Unpadded . T.encodeUtf8
{-# INLINE encodeBase32Unpadded #-}


-- | Decode an unpadded Base32hex encoded 'Text' value.
--
-- Decoded bytes are interpreted as UTF-8. Invalid Base32 or UTF-8
-- returns 'Left'. Use the corresponding @decode*With@ function
-- to choose a different character encoding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> decodeBase32UnpaddedUntyped "ADQMS"
-- Right "Sun"
--
-- >>> decodeBase32UnpaddedUntyped "ADQMS==="
-- Left "Base32-encoded bytestring has invalid padding"
--
-- @since 1.0
--
decodeBase32UnpaddedUntyped :: Text -> Either Text Text
decodeBase32UnpaddedUntyped value = B32H.decodeBase32UnpaddedUntyped (T.encodeUtf8 value)
  >>= first (ErrorText.pack . show) . T.decodeUtf8'
{-# INLINE decodeBase32UnpaddedUntyped #-}


-- | Attempt to decode an unpadded 'ByteString' value as Base32hex, converting from
-- 'ByteString' to 'Text' according to some encoding function. In practice,
-- This is something like 'T.decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Example__:
--
-- >>> decodeBase32UnpaddedWith UTF8.decodeUtf8' "ADQMS"
-- Right "Sun"
--
-- >>> decodeBase32UnpaddedWith UTF8.decodeUtf8' "!"
-- Left (DecodeError "Base32-encoded bytestring has invalid size")
--
-- >>> decodeBase32UnpaddedWith (const (Left "unsupported encoding")) "ADQMS"
-- Left (ConversionError "unsupported encoding")
--
decodeBase32UnpaddedWith
    :: (ByteString -> Either err Text)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ByteString
      -- ^ Input text to decode
    -> Either (Base32Error err) Text
decodeBase32UnpaddedWith f t = case B32H.decodeBase32UnpaddedUntyped t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase32UnpaddedWith #-}


-- | Decode an padded Base32hex encoded 'Text' value
--
-- Decoded bytes are interpreted as UTF-8. Invalid Base32 or UTF-8
-- returns 'Left'. Use the corresponding @decode*With@ function
-- to choose a different character encoding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> decodeBase32PaddedUntyped "ADQMS==="
-- Right "Sun"
--
-- >>> decodeBase32PaddedUntyped "ADQMS"
-- Left "Base32-encoded bytestring requires padding"
--
-- @since 1.0
--
decodeBase32PaddedUntyped :: Text -> Either Text Text
decodeBase32PaddedUntyped value = B32H.decodeBase32PaddedUntyped (T.encodeUtf8 value)
  >>= first (ErrorText.pack . show) . T.decodeUtf8'
{-# INLINE decodeBase32PaddedUntyped #-}

-- | Attempt to decode a padded 'ByteString' value as Base32hex, converting from
-- 'ByteString' to 'Text' according to some encoding function. In practice,
-- This is something like 'T.decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Example__:
--
-- >>> decodeBase32PaddedWith UTF8.decodeUtf8' "ADQMS==="
-- Right "Sun"
--
-- >>> decodeBase32PaddedWith UTF8.decodeUtf8' "!"
-- Left (DecodeError "Base32-encoded bytestring has invalid size")
--
-- >>> decodeBase32PaddedWith (const (Left "unsupported encoding")) "ADQMS==="
-- Left (ConversionError "unsupported encoding")
--
decodeBase32PaddedWith
    :: (ByteString -> Either err Text)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ByteString
      -- ^ Input text to decode
    -> Either (Base32Error err) Text
decodeBase32PaddedWith f t = case B32H.decodeBase32PaddedUntyped t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase32PaddedWith #-}

-- | Decode a typed Base32hex-encoded 'Text' value.
--
-- This function is padding-agnostic.
-- The decoded payload must be valid UTF-8.
--
-- >>> decodeBase32 (encodeBase32 "Sun")
-- "Sun"
--
-- >>> decodeBase32 (encodeBase32Unpadded "Sun")
-- "Sun"
--
-- @since 1.0
--
decodeBase32 :: HexAlphabet k => Base32 k Text -> Text
decodeBase32 = T.decodeUtf8 . B32H.decodeBase32 . fmap T.encodeUtf8
{-# INLINE decodeBase32 #-}

-- | Decode a typed, padded Base32hex-encoded 'Text' value.
--
-- The decoded payload must be valid UTF-8.
--
-- >>> decodeBase32Padded (encodeBase32 "Sun")
-- "Sun"
--
-- @since 1.0
--
decodeBase32Padded :: Base32 'HexPadded Text -> Text
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
-- @since 1.0
--
parseBase32Padded :: Text -> Either Text (Base32 'HexPadded Text)
parseBase32Padded value = assertBase32 value
  <$ (B32H.decodeBase32PaddedUntyped . T.encodeUtf8) value
{-# INLINE parseBase32Padded #-}

-- | Decode a typed, unpadded Base32hex-encoded value.
--
-- The decoded payload must be valid UTF-8.
--
-- >>> decodeBase32Unpadded (encodeBase32Unpadded "Sun")
-- "Sun"
--
-- @since 1.0
--
decodeBase32Unpadded :: Base32 'HexUnpadded Text -> Text
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
-- @since 1.0
--
parseBase32Unpadded :: Text -> Either Text (Base32 'HexUnpadded Text)
parseBase32Unpadded value = assertBase32 value
  <$ (B32H.decodeBase32UnpaddedUntyped . T.encodeUtf8) value
{-# INLINE parseBase32Unpadded #-}

-- $setup
-- >>> :seti -XOverloadedStrings
-- >>> import Data.Either (isLeft)
-- >>> import qualified Data.Text.Encoding as UTF8
