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
, decodeBase32
, decodeBase32With
, encodeBase32Unpadded
, decodeBase32Unpadded
, decodeBase32UnpaddedWith
, decodeBase32Padded
, decodeBase32PaddedWith
-- , decodeBase32Lenient
, isBase32Hex
, isValidBase32Hex
) where


import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base32.Hex as B32H

import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Text.Encoding.Base32.Error (Base32Error(..))

-- | Encode a 'Text' value in Base32hex with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
encodeBase32 :: Text -> Text
encodeBase32 = B32H.encodeBase32 . T.encodeUtf8
{-# INLINE encodeBase32 #-}

-- | Decode a padded Base32hex-encoded 'Text' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
decodeBase32 :: Text -> Either Text Text
decodeBase32 = fmap T.decodeLatin1 . B32H.decodeBase32 . T.encodeUtf8
{-# INLINE decodeBase32 #-}

-- | Attempt to decode a 'ByteString' value as Base32url, converting from
-- 'ByteString' to 'Text' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- @
-- 'decodeBase32With' 'T.decodeUtf8''
--   :: 'Text' -> 'Either' ('Base32Error' 'UnicodeException') 'Text'
-- @
--
decodeBase32With
    :: (ByteString -> Either err Text)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ByteString
      -- ^ Input text to decode
    -> Either (Base32Error err) Text
decodeBase32With f t = case B32H.decodeBase32 t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase32With #-}

-- | Encode a 'Text' value in Base32hex without padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>,
--      <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
encodeBase32Unpadded :: Text -> Text
encodeBase32Unpadded = B32H.encodeBase32Unpadded . T.encodeUtf8
{-# INLINE encodeBase32Unpadded #-}

-- | Decode an arbitrarily padded Base32hex encoded 'Text' value
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>,
--      <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
decodeBase32Unpadded :: Text -> Either Text Text
decodeBase32Unpadded = fmap T.decodeLatin1
    . B32H.decodeBase32Unpadded
    . T.encodeUtf8
{-# INLINE decodeBase32Unpadded #-}

-- | Attempt to decode an unpadded 'ByteString' value as Base32url, converting from
-- 'ByteString' to 'Text' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Example__:
--
-- @
-- 'decodeBase32UnpaddedWith' 'T.decodeUtf8''
--   :: 'ByteString' -> 'Either' ('Base32Error' 'UnicodeException') 'Text'
-- @
--
decodeBase32UnpaddedWith
    :: (ByteString -> Either err Text)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ByteString
      -- ^ Input text to decode
    -> Either (Base32Error err) Text
decodeBase32UnpaddedWith f t = case B32H.decodeBase32Unpadded t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase32UnpaddedWith #-}

-- | Decode an arbitrarily padded Base32hex encoded 'Text' value
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>,
--      <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
decodeBase32Padded :: Text -> Either Text Text
decodeBase32Padded = fmap T.decodeLatin1
    . B32H.decodeBase32Padded
    . T.encodeUtf8
{-# INLINE decodeBase32Padded #-}

-- | Attempt to decode a padded 'ByteString' value as Base32url, converting from
-- 'ByteString' to 'Text' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Example__:
--
-- @
-- 'decodeBase32PaddedWith' 'T.decodeUtf8''
--   :: 'ByteString' -> 'Either' ('Base32Error' 'UnicodeException') 'Text'
-- @
--
decodeBase32PaddedWith
    :: (ByteString -> Either err Text)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ByteString
      -- ^ Input text to decode
    -> Either (Base32Error err) Text
decodeBase32PaddedWith f t = case B32H.decodeBase32Padded t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase32PaddedWith #-}

-- -- -- | Leniently decode an unpadded Base32hex-encoded 'Text'. This function
-- -- -- will not generate parse errors. If input data contains padding chars,
-- -- -- then the input will be parsed up until the first pad character.
-- -- --
-- -- -- __Note:__ This is not RFC 4648-compliant.
-- -- --
-- -- decodeBase32Lenient :: Text -> Text
-- -- decodeBase32Lenient = T.decodeUtf8
-- --     . B32H.decodeBase32Lenient
-- --     . T.encodeUtf8
-- -- {-# INLINE decodeBase32Lenient #-}

-- | Tell whether a 'Text' value is Base32hex-encoded.
--
isBase32Hex :: Text -> Bool
isBase32Hex = B32H.isBase32Hex . T.encodeUtf8
{-# INLINE isBase32Hex #-}

-- | Tell whether a 'Text' value is a valid Base32hex format.
--
-- This will not tell you whether or not this is a correct Base32hex representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base32 encoded 'Text' value, use 'isBase32Hex'.
--
isValidBase32Hex :: Text -> Bool
isValidBase32Hex = B32H.isValidBase32Hex . T.encodeUtf8
{-# INLINE isValidBase32Hex #-}
