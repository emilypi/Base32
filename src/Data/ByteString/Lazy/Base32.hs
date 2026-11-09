{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Module       : Data.ByteString.Lazy.Base32
-- Copyright    : (c) 2019-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.ByteString.Lazy.ByteString'-valued combinators for
-- implementing the RFC 4648 specification of the Base32
-- encoding format. This includes strictly padded/unpadded
-- decoding variants, as well as internal and external validation for canonicity.
--
module Data.ByteString.Lazy.Base32
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
import qualified Data.ByteString.Base32 as B32
import Data.ByteString.Base32.Internal.Utils (reChunkN)
import Data.ByteString.Lazy (fromChunks, toChunks)
import Data.ByteString.Lazy.Internal (ByteString(..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL


-- | Encode a 'ByteString' value as a Base32 'TL.Text' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> encodeBase32 "Sun"
-- "KN2W4==="
--
-- @since 1.0
--
encodeBase32 :: ByteString -> Base32 'StdPadded TL.Text
encodeBase32 = fmap TL.decodeUtf8 . encodeBase32'
{-# INLINE encodeBase32 #-}

-- | Encode a 'ByteString' as a Base32 'ByteString' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> encodeBase32' "Sun"
-- "KN2W4==="
--
-- @since 1.0
--
encodeBase32' :: ByteString -> Base32 'StdPadded ByteString
encodeBase32' = assertBase32 . fromChunks
  . fmap (extractBase32 . B32.encodeBase32')
  . reChunkN 5
  . toChunks

-- | Decode an arbitrarily padded Base32 encoded 'ByteString' value. If its length is not
-- a multiple
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
--
-- @since 1.0
--
decodeBase32Untyped :: ByteString -> Either T.Text ByteString
decodeBase32Untyped = fmap (fromChunks . (:[]))
  . B32.decodeBase32Untyped
  . BS.concat
  . toChunks
{-# INLINE decodeBase32Untyped #-}

-- | Encode a 'ByteString' value as Base32 'TL.Text' without padding.
-- Note that for Base32,
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
-- @since 1.0
--
encodeBase32Unpadded :: ByteString -> Base32 'StdUnpadded TL.Text
encodeBase32Unpadded = fmap TL.decodeUtf8 . encodeBase32Unpadded'
{-# INLINE encodeBase32Unpadded #-}

-- | Encode a 'ByteString' value as Base32 without padding. Note that for Base32,
-- padding is optional. If you call this function, you will simply be encoding
-- as Base32 and stripping padding chars from the output.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
-- === __Examples__:
--
-- >>> encodeBase32Unpadded' "Sun"
-- "KN2W4"
--
-- @since 1.0
--
encodeBase32Unpadded' :: ByteString -> Base32 'StdUnpadded ByteString
encodeBase32Unpadded' = assertBase32 . fromChunks
  . fmap (extractBase32 . B32.encodeBase32Unpadded')
  . reChunkN 5
  . toChunks

-- | Decode an unpadded Base32-encoded 'ByteString' value. Input strings are
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
--
-- @since 1.0
--
decodeBase32UnpaddedUntyped :: ByteString -> Either T.Text ByteString
decodeBase32UnpaddedUntyped = fmap (fromChunks . (:[]))
  . B32.decodeBase32UnpaddedUntyped
  . BS.concat
  . toChunks
{-# INLINE decodeBase32UnpaddedUntyped #-}

-- | Decode a padded Base32-encoded 'ByteString' value. Input strings are
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
--
-- @since 1.0
--
decodeBase32PaddedUntyped :: ByteString -> Either T.Text ByteString
decodeBase32PaddedUntyped = fmap (fromChunks . (:[]))
  . B32.decodeBase32PaddedUntyped
  . BS.concat
  . toChunks
{-# INLINE decodeBase32PaddedUntyped #-}

-- | Decode a typed Base32-encoded 'ByteString' value.
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
decodeBase32 :: forall k. StdAlphabet k => Base32 k ByteString -> ByteString
decodeBase32 = fromChunks
  . fmap (B32.decodeBase32
      . (assertBase32 :: BS.ByteString -> Base32 k BS.ByteString))
  . reChunkN 8 . toChunks . extractBase32
{-# INLINE decodeBase32 #-}

-- | Decode a typed, padded Base32-encoded 'ByteString' value.
--
-- >>> decodeBase32Padded (encodeBase32' "Sun")
-- "Sun"
--
-- @since 1.0
--
decodeBase32Padded :: Base32 'StdPadded ByteString -> ByteString
decodeBase32Padded = decodeBase32
{-# INLINE decodeBase32Padded #-}

-- | Parse and validate a base32-encoded value, assuming that it has padding.
--
-- >>> parseBase32Padded "kn2w4==="
-- Right "kn2w4==="
--
-- >>> fmap decodeBase32Padded (parseBase32Padded "KN2W4===")
-- Right "Sun"
--
-- >>> parseBase32Padded ""
-- Right ""
--
-- >>> parseBase32Padded "KN2W4"
-- Left "Base32-encoded bytestring requires padding"
--
-- @since 1.0
--
parseBase32Padded :: ByteString -> Either T.Text (Base32 'StdPadded ByteString)
parseBase32Padded value = assertBase32 value
  <$ (decodeBase32PaddedUntyped) value
{-# INLINE parseBase32Padded #-}

-- | Decode a typed, unpadded Base32-encoded value.
--
-- >>> decodeBase32Unpadded (encodeBase32Unpadded' "Sun")
-- "Sun"
--
-- @since 1.0
--
decodeBase32Unpadded :: Base32 'StdUnpadded ByteString -> ByteString
decodeBase32Unpadded = decodeBase32
{-# INLINE decodeBase32Unpadded #-}

-- | Parse and validate a base32-encoded value, assuming that it is unpadded.
--
-- >>> parseBase32Unpadded "kn2w4"
-- Right "kn2w4"
--
-- >>> fmap decodeBase32Unpadded (parseBase32Unpadded "KN2W4")
-- Right "Sun"
--
-- >>> parseBase32Unpadded ""
-- Right ""
--
-- >>> parseBase32Unpadded "KN2W4==="
-- Left "Base32-encoded bytestring has invalid padding"
--
-- >>> isLeft (parseBase32Unpadded "MZ")
-- True
--
-- @since 1.0
--
parseBase32Unpadded :: ByteString -> Either T.Text (Base32 'StdUnpadded ByteString)
parseBase32Unpadded value = assertBase32 value
  <$ (decodeBase32UnpaddedUntyped) value
{-# INLINE parseBase32Unpadded #-}

-- $setup
-- >>> :seti -XOverloadedStrings
-- >>> import Data.Either (isLeft)
