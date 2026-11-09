{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Module       : Data.ByteString.Base32.Hex
-- Copyright    : (c) 2019-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.ByteString.ByteString'-valued combinators for
-- implementing the RFC 4648 specification of the Base32hex
-- encoding format. This includes padded and unpadded decoding variants, as well as
-- internal and external validation for canonicity.
--
module Data.ByteString.Base32.Hex
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
import Data.ByteString.Internal (ByteString(..))
import Data.ByteString.Base32.Internal.Head
import Data.ByteString.Base32.Internal.Tables
import Data.ByteString.Base32.Internal.Utils (validateLastNPads)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as T

import System.IO.Unsafe (unsafeDupablePerformIO)


-- | Encode a 'ByteString' value as a Base32hex 'Text' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> encodeBase32 "Sun"
-- "ADQMS==="
--
encodeBase32 :: ByteString -> Base32 'HexPadded Text
encodeBase32 = fmap T.decodeUtf8 . encodeBase32'
{-# INLINE encodeBase32 #-}

-- | Encode a 'ByteString' value as a Base32hex 'ByteString' value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> encodeBase32' "Sun"
-- "ADQMS==="
--
encodeBase32' :: ByteString -> Base32 'HexPadded ByteString
encodeBase32' = assertBase32 . encodeBase32_ "0123456789ABCDEFGHIJKLMNOPQRSTUV"#
{-# INLINE encodeBase32' #-}

-- | Decode an arbitrarily padded Base32hex-encoded 'ByteString' value. If its length
-- is not a multiple of 8, then padding characters will be added to fill out the
-- input to a multiple of 8 for safe decoding, as Base32hex-encoded values are
-- optionally padded.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> decodeBase32Untyped "ADQMS==="
-- Right "Sun"
--
-- >>> decodeBase32Untyped "ADQMS"
-- Right "Sun"
--
-- >>> decodeBase32Untyped "ADQM==="
-- Left "Base32-encoded bytestring has invalid padding"
--
decodeBase32Untyped :: ByteString -> Either Text ByteString
decodeBase32Untyped bs@(BS _ !l)
    | l == 0 = Right bs
    | r == 0 = unsafeDupablePerformIO $ decodeBase32_ hexDecodeTable bs
    | r == 2 = unsafeDupablePerformIO $
        decodeBase32_ hexDecodeTable (BS.append bs (BS.replicate 6 0x3d))
    | r == 4 = validateLastNPads 2 bs $
        decodeBase32_ hexDecodeTable (BS.append bs (BS.replicate 4 0x3d))
    | r == 5 = validateLastNPads 3 bs $
        decodeBase32_ hexDecodeTable (BS.append bs (BS.replicate 3 0x3d))
    | r == 7 = validateLastNPads 5 bs $
        decodeBase32_ hexDecodeTable (BS.append bs (BS.replicate 1 0x3d))
    | otherwise = Left (Text.pack "Base32-encoded bytestring has invalid size")
  where
    !r = l `rem` 8
{-# INLINE decodeBase32Untyped #-}

-- | Encode a 'ByteString' value as a Base32hex 'Text' value without padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> encodeBase32Unpadded "Sun"
-- "ADQMS"
--
encodeBase32Unpadded :: ByteString -> Base32 'HexUnpadded Text
encodeBase32Unpadded = fmap T.decodeUtf8 . encodeBase32Unpadded'
{-# INLINE encodeBase32Unpadded #-}

-- | Encode a 'ByteString' value as a Base32hex 'ByteString' value without padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> encodeBase32Unpadded' "Sun"
-- "ADQMS"
--
encodeBase32Unpadded' :: ByteString -> Base32 'HexUnpadded ByteString
encodeBase32Unpadded' = assertBase32
  . encodeBase32NoPad_ "0123456789ABCDEFGHIJKLMNOPQRSTUV"#
{-# INLINE encodeBase32Unpadded' #-}

-- | Decode an unpadded Base32hex-encoded 'ByteString' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> decodeBase32UnpaddedUntyped "ADQMS"
-- Right "Sun"
--
-- >>> decodeBase32UnpaddedUntyped "ADQMS==="
-- Left "Base32-encoded bytestring has invalid padding"
--
decodeBase32UnpaddedUntyped :: ByteString -> Either Text ByteString
decodeBase32UnpaddedUntyped bs@(BS _ !l)
    | l == 0 = Right bs
    | r == 0 = validateLastNPads 1 bs $ decodeBase32_ hexDecodeTable bs
    | r == 2 = unsafeDupablePerformIO $
        decodeBase32_ hexDecodeTable (BS.append bs (BS.replicate 6 0x3d))
    | r == 4 = validateLastNPads 1 bs $
        decodeBase32_ hexDecodeTable (BS.append bs (BS.replicate 4 0x3d))
    | r == 5 = validateLastNPads 1 bs $
        decodeBase32_ hexDecodeTable (BS.append bs (BS.replicate 3 0x3d))
    | r == 7 = validateLastNPads 1 bs $
        decodeBase32_ hexDecodeTable (BS.append bs (BS.replicate 1 0x3d))
    | otherwise = Left (Text.pack "Base32-encoded bytestring has invalid size")
  where
    !r = l `rem` 8
{-# INLINE decodeBase32UnpaddedUntyped #-}

-- | Decode a padded Base32hex-encoded 'ByteString' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-7 RFC-4648 section 7>
--
-- === __Examples__:
--
-- >>> decodeBase32PaddedUntyped "ADQMS==="
-- Right "Sun"
--
-- >>> decodeBase32PaddedUntyped "ADQMS"
-- Left "Base32-encoded bytestring requires padding"
--
decodeBase32PaddedUntyped :: ByteString -> Either Text ByteString
decodeBase32PaddedUntyped bs@(BS _ !l)
    | l == 0 = Right bs
    | r == 1 = Left (Text.pack "Base32-encoded bytestring has invalid size")
    | r == 3 = Left (Text.pack "Base32-encoded bytestring has invalid size")
    | r == 6 = Left (Text.pack "Base32-encoded bytestring has invalid size")
    | r /= 0 = Left (Text.pack "Base32-encoded bytestring requires padding")
    | otherwise = unsafeDupablePerformIO $ decodeBase32_ hexDecodeTable bs
  where
    !r = l `rem` 8
{-# INLINE decodeBase32PaddedUntyped #-}

-- | Decode a typed Base32hex-encoded 'ByteString' value. 
-- 
-- This function is padding-agnostic.
--
-- >>> decodeBase32 (encodeBase32' "Sun")
-- "Sun"
-- >>> decodeBase32 (encodeBase32Unpadded' "Sun")
-- "Sun"
--
decodeBase32 :: HexAlphabet k => Base32 k ByteString -> ByteString
decodeBase32 = decodeBase32Typed_ hexDecodeTable
{-# INLINE decodeBase32 #-}

-- | Decode a typed, padded Base32hex-encoded 'ByteString' value 
--
-- >>> decodeBase32Padded (encodeBase32' "Sun")
-- "Sun"
--
decodeBase32Padded :: Base32 'HexPadded ByteString -> ByteString
decodeBase32Padded = decodeBase32
{-# INLINE decodeBase32Padded #-}

-- | Parse and validate a base32hex-encoded value, assuming that it has padding.
--
-- >>> parseBase32Padded "adqms==="
-- Right "adqms==="
--
-- >>> fmap decodeBase32Padded (parseBase32Padded "ADQMS===")
-- Right "Sun"
-- 
-- >>> parseBase32Padded ""
-- Right ""
--
-- >>> parseBase32Padded "ADQMS"
-- Left "Base32-encoded bytestring requires padding"
--
parseBase32Padded :: ByteString -> Either Text (Base32 'HexPadded ByteString)
parseBase32Padded value = assertBase32 value
  <$ (decodeBase32PaddedUntyped) value
{-# INLINE parseBase32Padded #-}

-- | Decode a typed, unpadded Base32hex-encoded value.
--
-- >>> decodeBase32Unpadded (encodeBase32Unpadded' "Sun")
-- "Sun"
--
decodeBase32Unpadded :: Base32 'HexUnpadded ByteString -> ByteString
decodeBase32Unpadded = decodeBase32
{-# INLINE decodeBase32Unpadded #-}

-- | Parse and validate a base32hex-encoded value, assuming that is unpadded.
--
-- >>> parseBase32Unpadded "adqms"
-- Right "adqms"
-- >>> fmap decodeBase32Unpadded (parseBase32Unpadded "ADQMS")
-- Right "Sun"
-- >>> parseBase32Unpadded ""
-- Right ""
-- >>> parseBase32Unpadded "ADQMS==="
-- Left "Base32-encoded bytestring has invalid padding"
-- >>> isLeft (parseBase32Unpadded "CP")
-- True
--
parseBase32Unpadded :: ByteString -> Either Text (Base32 'HexUnpadded ByteString)
parseBase32Unpadded value = assertBase32 value
  <$ (decodeBase32UnpaddedUntyped) value
{-# INLINE parseBase32Unpadded #-}

-- $setup
-- >>> :seti -XOverloadedStrings
-- >>> import Data.Either (isLeft)
