{-# LANGUAGE Safe #-}
-- |
-- Module       : Data.Text.Lazy.Encoding.Base32
-- Copyright    : (c) 2019-2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.Text.Lazy.Text'-valued combinators for
-- implementing the RFC 4648 specification of the Base32
-- encoding format. This includes strictly padded/unpadded and lenient decoding
-- variants, as well as internal and external validation for canonicity.
--
module Data.Text.Lazy.Encoding.Base32
( -- * Encoding
  encodeBase32
, encodeBase32Unpadded
  -- * Decoding
, decodeBase32
, decodeBase32With
, decodeBase32Unpadded
, decodeBase32UnpaddedWith
, decodeBase32Padded
, decodeBase32PaddedWith
--, decodeBase32Lenient
  -- * Validation
, isBase32
, isValidBase32
) where


import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Base32 as BL32

import qualified Data.Text as T
import Data.Text.Encoding.Base32.Error
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

-- | Encode a 'TL.Text' value in Base32 with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-5 RFC-4648 section 5>
--
-- === __Examples__:
--
-- >>> encodeBase32 "<<?>>"
-- "PDw_Pj4="
--
encodeBase32 :: TL.Text -> TL.Text
encodeBase32 = BL32.encodeBase32 . TL.encodeUtf8
{-# INLINE encodeBase32 #-}

-- | Decode a padded Base32-encoded 'TL.Text' value. If its length is not a multiple
-- of 4, then padding chars will be added to fill out the input to a multiple of
-- 4 for safe decoding as base32 encodings are optionally padded.
--
-- For a decoder that fails on unpadded input, use 'decodeBase32Unpadded'.
--
-- /Note:/ This function makes sure that decoding is total by deferring to
-- 'T.decodeLatin1'. This will always round trip for any valid Base32-encoded
-- text value, but it may not round trip for bad inputs. The onus is on the
-- caller to make sure inputs are valid. If unsure, defer to `decodeBase32With`
-- and pass in a custom decode function.
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
decodeBase32 :: TL.Text -> Either T.Text TL.Text
decodeBase32 = fmap TL.decodeLatin1 . BL32.decodeBase32 . TL.encodeUtf8
{-# INLINE decodeBase32 #-}

-- | Attempt to decode a lazy 'ByteString' value as Base32, converting from
-- 'ByteString' to 'TL.Text' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- @
-- 'decodeBase32With' 'TL.decodeUtf8''
--   :: 'ByteString' -> 'Either' ('Base32Error' 'UnicodeException') 'TL.Text'
-- @
--
decodeBase32With
    :: (ByteString -> Either err TL.Text)
      -- ^ convert a bytestring to text (e.g. 'TL.decodeUtf8'')
    -> ByteString
      -- ^ Input text to decode
    -> Either (Base32Error err) TL.Text
decodeBase32With f t = case BL32.decodeBase32 t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase32With #-}

-- | Encode a 'TL.Text' value in Base32 without padding. Note that for Base32,
-- padding is optional. If you call this function, you will simply be encoding
-- as Base32 and stripping padding chars from the output.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
-- === __Examples__:
--
-- >>> encodeBase32Unpadded "<<?>>"
-- "PDw_Pj4"
--
encodeBase32Unpadded :: TL.Text -> TL.Text
encodeBase32Unpadded = BL32.encodeBase32Unpadded . TL.encodeUtf8
{-# INLINE encodeBase32Unpadded #-}

-- | Decode an unpadded Base32 encoded 'TL.Text' value.
--
-- /Note:/ This function makes sure that decoding is total by deferring to
-- 'T.decodeLatin1'. This will always round trip for any valid Base32-encoded
-- text value, but it may not round trip for bad inputs. The onus is on the
-- caller to make sure inputs are valid. If unsure, defer to `decodeBase32WUnpaddedWith`
-- and pass in a custom decode function.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> decodeBase32Unpadded "PDw_Pj4"
-- Right "<<?>>"
--
-- >>> decodeBase32Unpadded "PDw_Pj4="
-- Left "Base32-encoded bytestring has invalid padding"
--
decodeBase32Unpadded :: TL.Text -> Either T.Text TL.Text
decodeBase32Unpadded = fmap TL.decodeLatin1
    . BL32.decodeBase32Unpadded
    . TL.encodeUtf8
{-# INLINE decodeBase32Unpadded #-}

-- | Attempt to decode an unpadded lazy 'ByteString' value as Base32, converting from
-- 'ByteString' to 'TL.Text' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- @
-- 'decodeBase32UnpaddedWith' 'TL.decodeUtf8''
--   :: 'ByteString' -> 'Either' ('Base32Error' 'UnicodeException') 'TL.Text'
-- @
--
decodeBase32UnpaddedWith
    :: (ByteString -> Either err TL.Text)
      -- ^ convert a bytestring to text (e.g. 'TL.decodeUtf8'')
    -> ByteString
      -- ^ Input text to decode
    -> Either (Base32Error err) TL.Text
decodeBase32UnpaddedWith f t = case BL32.decodeBase32Unpadded t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase32UnpaddedWith #-}

-- | Decode an padded Base32 encoded 'TL.Text' value
--
-- /Note:/ This function makes sure that decoding is total by deferring to
-- 'T.decodeLatin1'. This will always round trip for any valid Base32-encoded
-- text value, but it may not round trip for bad inputs. The onus is on the
-- caller to make sure inputs are valid. If unsure, defer to `decodeBase32PaddedWith`
-- and pass in a custom decode function.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> decodeBase32Padded "PDw_Pj4="
-- Right "<<?>>"
--
-- >>> decodeBase32Padded "PDw_Pj4"
-- Left "Base32-encoded bytestring requires padding"
--
decodeBase32Padded :: TL.Text -> Either T.Text TL.Text
decodeBase32Padded = fmap TL.decodeLatin1
    . BL32.decodeBase32Padded
    . TL.encodeUtf8
{-# INLINE decodeBase32Padded #-}

-- | Attempt to decode a padded lazy 'ByteString' value as Base32, converting from
-- 'ByteString' to 'TL.Text' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Example__:
--
-- @
-- 'decodeBase32PaddedWith' 'T.decodeUtf8''
--   :: 'ByteString' -> 'Either' ('Base32Error' 'UnicodeException') 'Text'
-- @
--
decodeBase32PaddedWith
    :: (ByteString -> Either err TL.Text)
      -- ^ convert a bytestring to text (e.g. 'TL.decodeUtf8'')
    -> ByteString
      -- ^ Input text to decode
    -> Either (Base32Error err) TL.Text
decodeBase32PaddedWith f t = case BL32.decodeBase32Padded t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase32PaddedWith #-}

-- -- | Leniently decode an unpadded Base32-encoded 'TL.Text'. This function
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
-- decodeBase32Lenient :: TL.Text -> TL.Text
-- decodeBase32Lenient = TL.decodeLatin1
--     . BL32.decodeBase32Lenient
--     . TL.encodeUtf8
-- {-# INLINE decodeBase32Lenient #-}

-- | Tell whether a 'TL.Text' value is Base32-encoded
--
-- === __Examples__:
--
-- >>> isBase32 "PDw_Pj4="
-- True
--
-- >>> isBase32 "PDw_Pj4"
-- True
--
-- >>> isBase32 "PDw_Pj"
-- False
--
isBase32 :: TL.Text -> Bool
isBase32 = BL32.isBase32 . TL.encodeUtf8
{-# INLINE isBase32 #-}

-- | Tell whether a 'TL.Text' value is a valid Base32 format.
--
-- This will not tell you whether or not this is a correct Base32 representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base32 encoded 'TL.Text' value, use 'isBase32'.
--
-- === __Examples__:
--
-- >>> isValidBase32 "PDw_Pj4="
-- True
--
-- >>> isValidBase32 "PDw_Pj"
-- True
--
-- >>> isValidBase32 "%"
-- False
--
isValidBase32 :: TL.Text -> Bool
isValidBase32 = BL32.isValidBase32 . TL.encodeUtf8
{-# INLINE isValidBase32 #-}
