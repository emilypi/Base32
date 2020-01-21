{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module       : Data.ByteString.Base32
-- Copyright 	: (c) 2020 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- This module contains the combinators implementing the
-- RFC 4648 specification for the Base32 encoding including
-- unpadded and lenient variants
--
module Data.ByteString.Base32
( encodeBase32
, encodeBase32'
, decodeBase32
, encodeBase32Unpadded
, encodeBase32Unpadded'
, decodeBase32Unpadded
-- , decodeBase32Lenient
, isBase32
, isValidBase32
) where


import Data.ByteString (ByteString)
import Data.ByteString.Base32.Internal
import Data.Either (isRight)
import Data.Text (Text)
import qualified Data.Text.Encoding as T


-- | Encode a 'ByteString' value as Base32 'Text' with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
encodeBase32 :: ByteString -> Text
encodeBase32 = T.decodeUtf8 . encodeBase32'
{-# INLINE encodeBase32 #-}

-- | Encode a 'ByteString' value as a Base32 'ByteString'  value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
encodeBase32' :: ByteString -> ByteString
encodeBase32' = encodeBase32_ stdAlphabet

-- | Decode a padded Base32-encoded 'ByteString' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
decodeBase32 :: ByteString -> Either Text ByteString
decodeBase32 = decodeBase32_ False stdDecodeTable
{-# INLINE decodeBase32 #-}

-- | Encode a 'ByteString' value as Base32 'Text' without padding.
--
-- __Note:__ in some circumstances, the use of padding ("=") in base-encoded data
-- is not required or used. This is not one of them. If you are absolutely sure
-- the length of your bytestring is divisible by 3, this function will be the same
-- as 'encodeBase32' with padding, however, if not, you may see garbage appended to
-- your bytestring.
--
-- Only call unpadded variants when you can make assumptions about the length of
-- your input data.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
encodeBase32Unpadded :: ByteString -> Text
encodeBase32Unpadded = T.decodeUtf8 . encodeBase32Unpadded'
{-# INLINE encodeBase32Unpadded #-}

-- | Encode a 'ByteString' value as Base32 without padding.
--
-- __Note:__ in some circumstances, the use of padding ("=") in base-encoded data
-- is not required or used. This is not one of them. If you are absolutely sure
-- the length of your bytestring is divisible by 3, this function will be the same
-- as 'encodeBase32' with padding, however, if not, you may see garbage appended to
-- your bytestring.
--
-- Only call unpadded variants when you can make assumptions about the length of
-- your input data.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
encodeBase32Unpadded' :: ByteString -> ByteString
encodeBase32Unpadded' = encodeBase32Nopad_ stdAlphabet

-- | Decode an unpadded Base32-encoded 'ByteString'.
--
-- __Note:__ Only call unpadded variants when you can make assumptions
-- about the length of your input data.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
decodeBase32Unpadded :: ByteString -> Either Text ByteString
decodeBase32Unpadded = decodeBase32_ False stdDecodeTable
{-# INLINE decodeBase32Unpadded #-}

-- -- | Leniently decode an unpadded Base32-encoded 'ByteString' value. This function
-- -- will not generate parse errors. If input data contains padding chars,
-- -- then the input will be parsed up until the first pad character.
-- --
-- -- __Note:__ This is not RFC 4648-compliant.
-- --
-- decodeBase32Lenient :: ByteString -> ByteString
-- decodeBase32Lenient = decodeBase32Lenient_ decodeB32Table
-- {-# INLINE decodeBase32Lenient #-}

-- | Tell whether a 'ByteString' value is base32 encoded.
--
isBase32 :: ByteString -> Bool
isBase32 bs = isValidBase32 bs && isRight (decodeBase32 bs)
{-# INLINE isBase32 #-}

-- | Tell whether a 'ByteString' value is a valid Base32 format.
--
-- This will not tell you whether or not this is a correct Base32url representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base32 encoded 'ByteString' value, use 'isBase32'.
--
isValidBase32 :: ByteString -> Bool
isValidBase32 = validateBase32 "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"
{-# INLINE isValidBase32 #-}
