{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module       : Data.ByteString.Base32.Hex
-- Copyright 	: (c) 2020 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- This module contains the combinators implementing the
-- RFC 4648 specification for the Base32-Hex encoding including
-- unpadded and lenient variants
--
module Data.ByteString.Base32.Hex
( encodeBase32
, encodeBase32'
, decodeBase32
, encodeBase32Unpadded
, encodeBase32Unpadded'
, decodeBase32Unpadded
-- , decodeBase32Lenient
, isBase32Hex
, isValidBase32Hex
) where

import Data.ByteString (ByteString)
import Data.ByteString.Base32.Internal
import Data.ByteString.Base32.Internal.Head
import Data.ByteString.Base32.Internal.Tables
import Data.Either (isRight)
import Data.Text (Text)
import qualified Data.Text.Encoding as T


-- | Encode a 'ByteString' value as a Base32url 'Text' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-5 RFC-4648 section 5>
--
encodeBase32 :: ByteString -> Text
encodeBase32 = T.decodeUtf8 . encodeBase32'
{-# INLINE encodeBase32 #-}

-- | Encode a 'ByteString' as a Base32url 'ByteString' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-5 RFC-4648 section 5>
--
encodeBase32' :: ByteString -> ByteString
encodeBase32' = encodeBase32_ "0123456789ABCDEFGHIJKLMNOPQRSTUV"#
{-# INLINE encodeBase32' #-}

-- | Decode a padded Base32url encoded 'ByteString' value. If its length is not a multiple
-- of 4, then padding chars will be added to fill out the input to a multiple of
-- 4 for safe decoding as Base32url-encoded values are optionally padded.
--
-- For a decoder that fails on unpadded input of incorrect size, use 'decodeBase32Unpadded'.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
decodeBase32 :: ByteString -> Either Text ByteString
decodeBase32 = decodeBase32_ False hexDecodeTable
{-# INLINE decodeBase32 #-}

-- | Encode a 'ByteString' as a Base32url 'Text' value without padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-5 RFC-4648 section 5>
--
encodeBase32Unpadded :: ByteString -> Text
encodeBase32Unpadded = T.decodeUtf8 . encodeBase32Unpadded'
{-# INLINE encodeBase32Unpadded #-}

-- | Encode a 'ByteString' as a Base32url 'ByteString' value without padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-5 RFC-4648 section 5>
--
encodeBase32Unpadded' :: ByteString -> ByteString
encodeBase32Unpadded' = encodeBase32NoPad_ "0123456789ABCDEFGHIJKLMNOPQRSTUV"#
{-# INLINE encodeBase32Unpadded' #-}

-- | Decode an optionally padded Base32url encoded 'ByteString' value.
-- If its length is not a multiple of 4, then padding chars will be
-- added to fill out the input to a multiple of 4 for safe decoding.
--
-- For a decoder that fails on unpadded input of incorrect size, use 'decodeBase32Unpadded'.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
decodeBase32Unpadded :: ByteString -> Either Text ByteString
decodeBase32Unpadded = decodeBase32_ True hexDecodeTable
{-# INLINE decodeBase32Unpadded #-}

-- -- | Leniently decode an unpadded Base32url-encoded 'ByteString'. This function
-- -- will not generate parse errors. If input data contains padding chars,
-- -- then the input will be parsed up until the first pad character.
-- --
-- -- __Note:__ This is not RFC 4648-compliant.
-- --
-- decodeBase32Lenient :: ByteString -> ByteString
-- decodeBase32Lenient = decodeBase32Lenient_ decodeB32HexTable
-- {-# INLINE decodeBase32Lenient #-}

-- | Tell whether a 'ByteString' is Base32url-encoded.
--
isBase32Hex :: ByteString -> Bool
isBase32Hex bs = isValidBase32Hex bs && isRight (decodeBase32 bs)
{-# INLINE isBase32Hex #-}

-- | Tell whether a 'ByteString' is a valid Base32url format.
--
-- This will not tell you whether or not this is a correct Base32url representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base32 encoded 'ByteString' value, use 'isBase32Hex'.
--
isValidBase32Hex :: ByteString -> Bool
isValidBase32Hex = validateBase32 "0123456789ABCDEFGHIJKLMNOPQRSTUV"
{-# INLINE isValidBase32Hex #-}
