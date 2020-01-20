{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module       : Data.Text.Encoding.Base32.Hex
-- Copyright 	: (c) 2019 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- This module contains the combinators implementing the
-- RFC 4648 specification for the Base32-Hex encoding including
-- unpadded and lenient variants
module Data.Text.Encoding.Base32.Hex
( encodeBase32
-- , decodeBase32
, encodeBase32Unpadded
-- , decodeBase32Unpadded
-- , decodeBase32Lenient
-- , isBase32Hex
, isValidBase32Hex
) where


import qualified Data.ByteString.Base32.Hex as B32U

import Data.Text (Text)
import qualified Data.Text.Encoding as T

-- | Encode a 'Text' value in Base32url with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-5 RFC-4648 section 5>
--
encodeBase32 :: Text -> Text
encodeBase32 = B32U.encodeBase32 . T.encodeUtf8
{-# INLINE encodeBase32 #-}

-- -- | Decode a padded Base32url-encoded 'Text' value. If its length is not a multiple
-- -- of 4, then padding chars will be added to fill out the input to a multiple of
-- -- 4 for safe decoding as base32url encodings are optionally padded.
-- --
-- -- For a decoder that fails on unpadded input of incorrect size, use 'decodeBase32Unpadded'.
-- --
-- -- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
-- --
-- decodeBase32 :: Text -> Either Text Text
-- decodeBase32 = fmap T.decodeUtf8 . B32U.decodeBase32 . T.encodeUtf8
-- {-# INLINE decodeBase32 #-}

-- | Encode a 'Text' value in Base32url without padding. Note that for Base32url,
-- padding is optional. If you call this function, you will simply be encoding
-- as Base32url and stripping padding chars from the output.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
encodeBase32Unpadded :: Text -> Text
encodeBase32Unpadded = B32U.encodeBase32Unpadded . T.encodeUtf8
{-# INLINE encodeBase32Unpadded #-}

-- -- | Decode an unpadded Base32url encoded 'Text' value
-- --
-- -- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
-- --
-- decodeBase32Unpadded :: Text -> Either Text Text
-- decodeBase32Unpadded = fmap T.decodeUtf8
--     . B32U.decodeBase32Unpadded
--     . T.encodeUtf8
-- {-# INLINE decodeBase32Unpadded #-}

-- -- | Leniently decode an unpadded Base32url-encoded 'Text'. This function
-- -- will not generate parse errors. If input data contains padding chars,
-- -- then the input will be parsed up until the first pad character.
-- --
-- -- __Note:__ This is not RFC 4648-compliant.
-- --
-- decodeBase32Lenient :: Text -> Text
-- decodeBase32Lenient = T.decodeUtf8
--     . B32U.decodeBase32Lenient
--     . T.encodeUtf8
-- {-# INLINE decodeBase32Lenient #-}

-- -- | Tell whether a 'Text' value is Base32url-encoded.
-- --
-- isBase32Hex :: Text -> Bool
-- isBase32Hex = B32U.isBase32Hex . T.encodeUtf8
-- {-# INLINE isBase32Hex #-}

-- | Tell whether a 'Text' value is a valid Base32url format.
--
-- This will not tell you whether or not this is a correct Base32url representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base32 encoded 'Text' value, use 'isBase32Hex'.
--
isValidBase32Hex :: Text -> Bool
isValidBase32Hex = B32U.isValidBase32Hex . T.encodeUtf8
{-# INLINE isValidBase32Hex #-}
