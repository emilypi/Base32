{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
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
, decodeBase32Padded
-- , decodeBase32Lenient
, isBase32
, isValidBase32
) where


import qualified Data.ByteString as BS
import Data.ByteString.Internal (ByteString(..))
import Data.ByteString.Base32.Internal
import Data.ByteString.Base32.Internal.Head
import Data.ByteString.Base32.Internal.Tables
import Data.Either (isRight)
import Data.Text (Text)
import qualified Data.Text.Encoding as T

import System.IO.Unsafe (unsafeDupablePerformIO)


-- | Encode a 'ByteString' value as Base32 'Text' with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
encodeBase32 :: ByteString -> Text
encodeBase32 = T.decodeUtf8 . encodeBase32'
{-# INLINE encodeBase32 #-}

-- | Encode a 'ByteString' value as a Base32 'ByteString'  value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
encodeBase32' :: ByteString -> ByteString
encodeBase32' = encodeBase32_ "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"#
{-# INLINE encodeBase32' #-}

-- | Decode a padded Base32-encoded 'ByteString' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
decodeBase32 :: ByteString -> Either Text ByteString
decodeBase32 bs@(PS _ _ !l)
    | l == 0 = Right bs
    | r == 0 = unsafeDupablePerformIO $ decodeBase32_ dlen stdDecodeTable bs
    | r == 2 = unsafeDupablePerformIO $ decodeBase32_ dlen stdDecodeTable (BS.append bs "======")
    | r == 4 = validateLastNPads 2 bs $ decodeBase32_ dlen stdDecodeTable (BS.append bs "====")
    | r == 5 = validateLastNPads 3 bs $ decodeBase32_ dlen stdDecodeTable (BS.append bs "===")
    | r == 7 = validateLastNPads 5 bs $ decodeBase32_ dlen stdDecodeTable (BS.append bs "=")
    | otherwise = Left "Base32-encoded bytestring has invalid size"
  where
    !r = l `rem` 8
    !q = l `quot` 8
    !dlen = q * 8
{-# INLINE decodeBase32 #-}

-- | Encode a 'ByteString' value as a Base32 'Text' value without padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>,
--      <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
--
encodeBase32Unpadded :: ByteString -> Text
encodeBase32Unpadded = T.decodeUtf8 . encodeBase32Unpadded'
{-# INLINE encodeBase32Unpadded #-}

-- | Encode a 'ByteString' value as a Base32 'ByteString'  value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>,
--      <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
encodeBase32Unpadded' :: ByteString -> ByteString
encodeBase32Unpadded' = encodeBase32NoPad_ "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"#
{-# INLINE encodeBase32Unpadded' #-}

-- | Decode an arbitarily padded Base32-encoded 'ByteString' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>,
--      <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
decodeBase32Unpadded :: ByteString -> Either Text ByteString
decodeBase32Unpadded bs@(PS _ _ !l)
    | l == 0 = Right bs
    | r == 0 = validateLastNPads 1 bs $ decodeBase32_ dlen stdDecodeTable bs
    | r == 2 = unsafeDupablePerformIO $ decodeBase32_ dlen stdDecodeTable (BS.append bs "======")
    | r == 4 = validateLastNPads 1 bs $ decodeBase32_ dlen stdDecodeTable (BS.append bs "====")
    | r == 5 = validateLastNPads 1 bs $ decodeBase32_ dlen stdDecodeTable (BS.append bs "===")
    | r == 7 = validateLastNPads 1 bs $ decodeBase32_ dlen stdDecodeTable (BS.append bs "=")
    | otherwise = Left "Base32-encoded bytestring has invalid size"
  where
    !q = l `quot` 8
    !r = l `rem` 8
    !dlen = q * 3
{-# INLINE decodeBase32Unpadded #-}

-- | Decode a padded Base32-encoded 'ByteString' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>,
--      <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
decodeBase32Padded :: ByteString -> Either Text ByteString
decodeBase32Padded bs@(PS _ _ !l)
    | l == 0 = Right bs
    | r == 1 = Left "Base32-encoded bytestring has invalid size"
    | r == 3 = Left "Base32-encoded bytestring has invalid size"
    | r == 6 = Left "Base32-encoded bytestring has invalid size"
    | r /= 0 = Left "Base32-encoded bytestring requires padding"
    | otherwise = unsafeDupablePerformIO $ decodeBase32_ dlen stdDecodeTable bs
  where
    !q = l `quot` 8
    !r = l `rem` 8
    !dlen = q * 3
{-# INLINE decodeBase32Padded #-}

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
-- This will not tell you whether or not this is a correct Base32 representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base32 encoded 'ByteString' value, use 'isBase32'.
--
isValidBase32 :: ByteString -> Bool
isValidBase32 = validateBase32 "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"
{-# INLINE isValidBase32 #-}
