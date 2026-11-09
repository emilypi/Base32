{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
--
-- @since 1.0
--
encodeBase32 :: ByteString -> Base32 'HexPadded TL.Text
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
-- @since 1.0
--
encodeBase32' :: ByteString -> Base32 'HexPadded ByteString
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
-- >>> decodeBase32Untyped "ADQM==="
-- Left "Base32-encoded bytestring has invalid padding"
--
-- @since 1.0
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
--
-- @since 1.0
--
encodeBase32Unpadded :: ByteString -> Base32 'HexUnpadded TL.Text
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
-- @since 1.0
--
encodeBase32Unpadded' :: ByteString -> Base32 'HexUnpadded ByteString
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
-- @since 1.0
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
-- @since 1.0
--
decodeBase32PaddedUntyped :: ByteString -> Either T.Text ByteString
decodeBase32PaddedUntyped = fmap (fromChunks . (:[]))
    . B32H.decodeBase32PaddedUntyped
    . BS.concat
    . toChunks
{-# INLINE decodeBase32PaddedUntyped #-}

-- | Decode a typed Base32hex-encoded 'ByteString' value.
--
-- This function is padding-agnostic.
--
-- >>> decodeBase32 (encodeBase32' "Sun")
-- "Sun"
--
-- >>> decodeBase32 (encodeBase32Unpadded' "Sun")
-- "Sun"
--
-- @since 1.0
--
decodeBase32 :: forall k. HexAlphabet k => Base32 k ByteString -> ByteString
decodeBase32 = fromChunks
  . fmap (B32H.decodeBase32
      . (assertBase32 :: BS.ByteString -> Base32 k BS.ByteString))
  . reChunkN 8 . toChunks . extractBase32
{-# INLINE decodeBase32 #-}

-- | Decode a typed, padded Base32hex-encoded 'ByteString' value.
--
-- >>> decodeBase32Padded (encodeBase32' "Sun")
-- "Sun"
--
-- @since 1.0
--
decodeBase32Padded :: Base32 'HexPadded ByteString -> ByteString
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
parseBase32Padded :: ByteString -> Either T.Text (Base32 'HexPadded ByteString)
parseBase32Padded value = assertBase32 value
  <$ (decodeBase32PaddedUntyped) value
{-# INLINE parseBase32Padded #-}

-- | Decode a typed, unpadded Base32hex-encoded value.
--
-- >>> decodeBase32Unpadded (encodeBase32Unpadded' "Sun")
-- "Sun"
--
-- @since 1.0
--
decodeBase32Unpadded :: Base32 'HexUnpadded ByteString -> ByteString
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
parseBase32Unpadded :: ByteString -> Either T.Text (Base32 'HexUnpadded ByteString)
parseBase32Unpadded value = assertBase32 value
  <$ (decodeBase32UnpaddedUntyped) value
{-# INLINE parseBase32Unpadded #-}

-- $setup
-- >>> :seti -XOverloadedStrings
-- >>> import Data.Either (isLeft)
