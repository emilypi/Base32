{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module       : Data.Text.Encoding.Base32
-- Copyright 	: (c) 2019 Emily Pillmore
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
module Data.Text.Encoding.Base32
( encodeBase32
, decodeBase32
, encodeBase32Unpadded
, decodeBase32Unpadded
-- , decodeBase32Lenient
, isBase32
, isValidBase32
) where


import qualified Data.ByteString.Base32 as B32

import Data.Text (Text)
import qualified Data.Text.Encoding as T

-- | Encode a 'Text' value in Base32 with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
encodeBase32 :: Text -> Text
encodeBase32 = B32.encodeBase32 . T.encodeUtf8
{-# INLINE encodeBase32 #-}

-- | Decode a padded Base32-encoded 'Text' value
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>
--
decodeBase32 :: Text -> Either Text Text
decodeBase32 = fmap T.decodeUtf8 . B32.decodeBase32 . T.encodeUtf8
{-# INLINE decodeBase32 #-}

-- | Encode a 'Text' value in Base32 without padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>,
--      <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
encodeBase32Unpadded :: Text -> Text
encodeBase32Unpadded = B32.encodeBase32Unpadded . T.encodeUtf8
{-# INLINE encodeBase32Unpadded #-}

-- | Decode an arbitrarily padded Base32-encoded 'Text'
--
-- See: <https://tools.ietf.org/html/rfc4648#section-6 RFC-4648 section 6>,
--      <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
decodeBase32Unpadded :: Text -> Either Text Text
decodeBase32Unpadded = fmap T.decodeUtf8
    . B32.decodeBase32Unpadded
    . T.encodeUtf8
{-# INLINE decodeBase32Unpadded #-}

-- -- | Leniently decode a Base32-encoded 'Text' value. This function
-- -- will not generate parse errors. If input data contains padding chars,
-- -- then the input will be parsed up until the first pad character.
-- --
-- -- __Note:__ This is not RFC 4648-compliant.
-- --
-- decodeBase32Lenient :: Text -> Text
-- decodeBase32Lenient = T.decodeUtf8
--     . B32.decodeBase32Lenient
--     . T.encodeUtf8
-- {-# INLINE decodeBase32Lenient #-}

-- | Tell whether a 'Text' value is Base32-encoded.
--
isBase32 :: Text -> Bool
isBase32 = B32.isBase32 . T.encodeUtf8
{-# INLINE isBase32 #-}

-- | Tell whether a 'Text' value is a valid Base32 format.
--
-- This will not tell you whether or not this is a correct Base32 representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base32 encoded 'Text' value, use 'isBase32'.
--
isValidBase32 :: Text -> Bool
isValidBase32 = B32.isValidBase32 . T.encodeUtf8
{-# INLINE isValidBase32 #-}
