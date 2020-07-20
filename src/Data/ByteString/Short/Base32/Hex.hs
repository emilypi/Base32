{-# LANGUAGE Trustworthy #-}
-- |
-- Module       : Data.ByteString.Short.Base32.Hex
-- Copyright    : (c) 2019-2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.ByteString.Short.ShortByteString'-valued combinators for
-- implementing the RFC 4648 specification of the Base32hex
-- encoding format. This includes strictly padded/unpadded and lenient decoding
-- variants, as well as internal and external validation for canonicity.
--
module Data.ByteString.Short.Base32.Hex
( -- * Encoding
  encodeBase32
, encodeBase32'
, encodeBase32Unpadded
, encodeBase32Unpadded'
  -- * Decoding
, decodeBase32
, decodeBase32Unpadded
, decodeBase32Padded
--, decodeBase32Lenient
  -- * Validation
, isBase32Hex
, isValidBase32Hex
) where


import qualified Data.ByteString.Base32.Hex as B32H
import Data.ByteString.Short (ShortByteString, fromShort, toShort)
import Data.Text (Text)
import Data.Text.Short (ShortText)
import Data.Text.Short.Unsafe (fromShortByteStringUnsafe)

-- | Encode a 'ShortByteString' value as a Base32hex 'Text' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> encodeBase32 "<<?>>"
-- "PDw_Pj4="
--
encodeBase32 :: ShortByteString -> ShortText
encodeBase32 = fromShortByteStringUnsafe . encodeBase32'
{-# INLINE encodeBase32 #-}

-- | Encode a 'ShortByteString' as a Base32hex 'ShortByteString' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> encodeBase32' "<<?>>"
-- "PDw_Pj4="
--
encodeBase32' :: ShortByteString -> ShortByteString
encodeBase32' = toShort . B32H.encodeBase32' . fromShort

-- | Decode a padded Base32hex encoded 'ShortByteString' value. If its length is not a multiple
-- of 4, then padding chars will be added to fill out the input to a multiple of
-- 4 for safe decoding as Base32hex-encoded values are optionally padded.
--
-- For a decoder that fails on unpadded input of incorrect size, use 'decodeBase32Unpadded'.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> decodeBase32 "PDw_Pj4="
-- Right "<<?>>"
--
-- >>> decodeBase32 "PDw_Pj4"
-- Right "<<?>>"
--
-- >>> decodeBase32 "PDw-Pg="
-- Left "Base32-encoded bytestring has invalid padding"
--
-- >>> decodeBase32 "PDw-Pg"
-- Right "<<>>"
--
decodeBase32 :: ShortByteString -> Either Text ShortByteString
decodeBase32 = fmap toShort . B32H.decodeBase32 . fromShort

{-# INLINE decodeBase32 #-}

-- | Encode a 'ShortByteString' value as Base32hex 'Text' without padding. Note that for Base32hex,
-- padding is optional. If you call this function, you will simply be encoding
-- as Base32hex and stripping padding chars from the output.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>,
--      <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
-- === __Examples__:
--
-- >>> encodeBase32Unpadded "<<?>>"
-- "PDw_Pj4"
--
encodeBase32Unpadded :: ShortByteString -> ShortText
encodeBase32Unpadded = fromShortByteStringUnsafe . encodeBase32Unpadded'
{-# INLINE encodeBase32Unpadded #-}

-- | Encode a 'ShortByteString' value as Base32hex without padding. Note that for Base32hex,
-- padding is optional. If you call this function, you will simply be encoding
-- as Base32hex and stripping padding chars from the output.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>,
--      <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
-- === __Examples__:
--
-- >>> encodeBase32Unpadded' "<<?>>"
-- "PDw_Pj4"
--
encodeBase32Unpadded' :: ShortByteString -> ShortByteString
encodeBase32Unpadded' = toShort . B32H.encodeBase32Unpadded' . fromShort

-- | Decode an unpadded Base32hex-encoded 'ShortByteString' value. Input strings are
-- required to be unpadded, and will undergo validation prior to decoding to
-- confirm.
--
-- In general, unless unpadded Base32hex is explicitly required, it is
-- safer to call 'decodeBase32'.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>,
--      <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
-- === __Examples__:
--
-- >>> decodeBase32Unpadded "PDw_Pj4"
-- Right "<<?>>"
--
-- >>> decodeBase32Unpadded "PDw_Pj4="
-- Left "Base32-encoded bytestring has invalid padding"
--
decodeBase32Unpadded :: ShortByteString -> Either Text ShortByteString
decodeBase32Unpadded = fmap toShort . B32H.decodeBase32Unpadded . fromShort
{-# INLINE decodeBase32Unpadded #-}

-- | Decode a padded Base32hex-encoded 'ShortByteString' value. Input strings are
-- required to be correctly padded, and will be validated prior to decoding
-- to confirm.
--
-- In general, unless padded Base32hex is explicitly required, it is
-- safer to call 'decodeBase32'.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>,
--      <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
-- === __Examples__:
--
-- >>> decodeBase32Padded "PDw_Pj4="
-- Right "<<?>>"
--
-- >>> decodeBase32Padded "PDw_Pj4"
-- Left "Base32-encoded bytestring requires padding"
--
decodeBase32Padded :: ShortByteString -> Either Text ShortByteString
decodeBase32Padded = fmap toShort . B32H.decodeBase32Padded . fromShort
{-# INLINE decodeBase32Padded #-}

-- -- | Leniently decode an unpadded Base32hex-encoded 'ShortByteString'. This function
-- -- will not generate parse errors. If input data contains padding chars,
-- -- then the input will be parsed up until the first pad character.
-- --
-- -- __Note:__ This is not RFC 4648-compliant.
-- --
-- -- === __Examples__:
-- --
-- -- >>> decodeBase32Lenient "PDw_Pj4="
-- -- "<<?>>"
-- --
-- -- >>> decodeBase32Lenient "PDw_%%%$}Pj4"
-- -- "<<?>>"
-- --
-- decodeBase32Lenient :: ShortByteString -> ShortByteString
-- decodeBase32Lenient = toShort . B32H.decodeBase32Lenient . fromShort
-- {-# INLINE decodeBase32Lenient #-}

-- | Tell whether a 'ShortByteString' is Base32hex-encoded.
--
-- === __Examples__:
--
-- >>> isBase32Hex "PDw_Pj4="
-- True
--
-- >>> isBase32Hex "PDw_Pj4"
-- True
--
-- >>> isBase32Hex "PDw_Pj"
-- False
--
isBase32Hex :: ShortByteString -> Bool
isBase32Hex = B32H.isBase32Hex . fromShort
{-# INLINE isBase32Hex #-}

-- | Tell whether a 'ShortByteString' is a valid Base32hex format.
--
-- This will not tell you whether or not this is a correct Base32hex representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base32 encoded 'ShortByteString' value, use 'isBase32Hex'.
--
-- === __Examples__:
--
-- >>> isValidBase32Hex "PDw_Pj4="
-- True
--
-- >>> isValidBase32Hex "PDw_Pj"
-- True
--
-- >>> isValidBase32Hex "%"
-- False
--
isValidBase32Hex :: ShortByteString -> Bool
isValidBase32Hex = B32H.isValidBase32Hex . fromShort
{-# INLINE isValidBase32Hex #-}
