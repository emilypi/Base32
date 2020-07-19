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
, decodeBase32Padded
-- , decodeBase32Lenient
, isBase32Hex
, isValidBase32Hex
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


-- | Encode a 'ByteString' value as a Base32hex 'Text' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
encodeBase32 :: ByteString -> Text
encodeBase32 = T.decodeUtf8 . encodeBase32'
{-# INLINE encodeBase32 #-}

-- | Encode a 'ByteString' as a Base32hex 'ByteString' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
encodeBase32' :: ByteString -> ByteString
encodeBase32' = encodeBase32_ "0123456789ABCDEFGHIJKLMNOPQRSTUV"#
{-# INLINE encodeBase32' #-}

-- | Decode a padded Base32hex encoded 'ByteString' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
decodeBase32 :: ByteString -> Either Text ByteString
decodeBase32 bs@(PS _ _ !l)
    | l == 0 = Right bs
    | r == 0 = unsafeDupablePerformIO $ decodeBase32_ dlen hexDecodeTable bs
    | r == 2 = unsafeDupablePerformIO $ decodeBase32_ dlen hexDecodeTable (BS.append bs "======")
    | r == 4 = validateLastNPads 2 bs $ decodeBase32_ dlen hexDecodeTable (BS.append bs "====")
    | r == 5 = validateLastNPads 3 bs $ decodeBase32_ dlen hexDecodeTable (BS.append bs "===")
    | r == 7 = validateLastNPads 5 bs $ decodeBase32_ dlen hexDecodeTable (BS.append bs "=")
    | otherwise = Left "Base32-encoded bytestring has invalid size"
  where
    !r = l `rem` 8
    !q = l `quot` 8
    !dlen = q * 8
{-# INLINE decodeBase32 #-}

-- | Encode a 'ByteString' as a Base32hex 'Text' value without padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>,
--      <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
encodeBase32Unpadded :: ByteString -> Text
encodeBase32Unpadded = T.decodeUtf8 . encodeBase32Unpadded'
{-# INLINE encodeBase32Unpadded #-}

-- | Encode a 'ByteString' as a Base32hex 'ByteString' value without padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>,
--      <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
encodeBase32Unpadded' :: ByteString -> ByteString
encodeBase32Unpadded' = encodeBase32NoPad_ "0123456789ABCDEFGHIJKLMNOPQRSTUV"#
{-# INLINE encodeBase32Unpadded' #-}

-- | Decode an arbitrarily padded Base32hex encoded 'ByteString' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>,
--      <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
decodeBase32Unpadded :: ByteString -> Either Text ByteString
decodeBase32Unpadded bs@(PS _ _ !l)
    | l == 0 = Right bs
    | r == 0 = validateLastNPads 1 bs $ decodeBase32_ dlen hexDecodeTable bs
    | r == 2 = unsafeDupablePerformIO $ decodeBase32_ dlen hexDecodeTable (BS.append bs "======")
    | r == 4 = validateLastNPads 1 bs $ decodeBase32_ dlen hexDecodeTable (BS.append bs "====")
    | r == 5 = validateLastNPads 1 bs $ decodeBase32_ dlen hexDecodeTable (BS.append bs "===")
    | r == 7 = validateLastNPads 1 bs $ decodeBase32_ dlen hexDecodeTable (BS.append bs "=")
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
    | otherwise = unsafeDupablePerformIO $ decodeBase32_ dlen hexDecodeTable bs
  where
    !q = l `quot` 8
    !r = l `rem` 8
    !dlen = q * 3
{-# INLINE decodeBase32Padded #-}

-- -- | Leniently decode an unpadded Base32hex-encoded 'ByteString'. This function
-- -- will not generate parse errors. If input data contains padding chars,
-- -- then the input will be parsed up until the first pad character.
-- --
-- -- __Note:__ This is not RFC 4648-compliant.
-- --
-- decodeBase32Lenient :: ByteString -> ByteString
-- decodeBase32Lenient = decodeBase32Lenient_ decodeB32HexTable
-- {-# INLINE decodeBase32Lenient #-}

-- | Tell whether a 'ByteString' is Base32hex-encoded.
--
isBase32Hex :: ByteString -> Bool
isBase32Hex bs = isValidBase32Hex bs && isRight (decodeBase32 bs)
{-# INLINE isBase32Hex #-}

-- | Tell whether a 'ByteString' is a valid Base32hex format.
--
-- This will not tell you whether or not this is a correct Base32hex representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base32 encoded 'ByteString' value, use 'isBase32Hex'.
--
isValidBase32Hex :: ByteString -> Bool
isValidBase32Hex = validateBase32 "0123456789ABCDEFGHIJKLMNOPQRSTUV"
{-# INLINE isValidBase32Hex #-}
