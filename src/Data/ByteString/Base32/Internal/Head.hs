{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module       : Data.ByteString.Base32.Internal.Head
-- Copyright    : (c) 2020-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- Allocation and entry points for the strict encoding and decoding loops.
--
module Data.ByteString.Base32.Internal.Head
( encodeBase32_
, encodeBase32NoPad_
, decodeBase32_
, decodeBase32Typed_
) where


import Prelude hiding (length, null)
import Data.Base32.Types (Base32, extractBase32)
import qualified Data.ByteString as BS
import Data.ByteString (length, null)
import Data.ByteString.Internal
import Data.ByteString.Base32.Internal.Utils (encodedLength)
import Data.ByteString.Base32.Internal.Loop
import Data.ByteString.Base32.Internal.Tail
import Data.Text (Text)
import qualified Data.Text as Text

import Foreign.Ptr
import Foreign.ForeignPtr

import GHC.Exts
import GHC.ForeignPtr
import GHC.Word

import System.IO.Unsafe


-- | Head of the padded base32 encoding loop.
--
-- This function takes an alphabet in the form of an unboxed 'Addr#',
-- allocates the correct number of bytes that will be written, and
-- executes the inner encoding loop against that data.
--
encodeBase32_ :: Addr# -> ByteString -> ByteString
encodeBase32_ !lut (BS !sfp !l) = unsafeDupablePerformIO $ do
    dfp <- mallocPlainForeignPtrBytes dlen
    withForeignPtr dfp $ \dptr ->
      withForeignPtr sfp $ \sptr -> do
        let !end = plusPtr sptr l
        innerLoop lut
          (castPtr dptr) sptr
          end (loopTail lut dfp dptr end)
  where
    !dlen = encodedLength l

-- | Head of the unpadded base32 encoding loop.
--
-- This function takes an alphabet in the form of an unboxed 'Addr#',
-- allocates the correct number of bytes that will be written, and
-- executes the inner encoding loop against that data.
--
encodeBase32NoPad_ :: Addr# -> ByteString -> ByteString
encodeBase32NoPad_ !lut (BS !sfp !l) = unsafeDupablePerformIO $ do
    !dfp <- mallocPlainForeignPtrBytes dlen
    withForeignPtr dfp $ \dptr ->
      withForeignPtr sfp $ \sptr -> do
        let !end = plusPtr sptr l
        innerLoop lut
          (castPtr dptr) sptr
          end (loopTailNoPad lut dfp dptr end)
  where
    !dlen = encodedLength l

-- | Head of the base32 decoding loop.
--
-- This function takes a base32-decoding lookup table and base32-encoded
-- bytestring, allocates the correct number of bytes that will be written,
-- and executes the inner decoding loop against that data.
--
decodeBase32_ :: Ptr Word8 -> ByteString -> IO (Either Text ByteString)
decodeBase32_ table bs
  | null bs = pure (Right bs)
  | length bs `rem` 8 /= 0 =
      pure (Left (Text.pack "Base32 input requires complete quanta"))
  | otherwise = decodeComplete table bs

decodeComplete :: Ptr Word8 -> ByteString -> IO (Either Text ByteString)
decodeComplete (Ptr !dtable) (BS !sfp !slen) =
    withForeignPtr sfp $ \sptr -> do
      dfp <- mallocPlainForeignPtrBytes dlen
      withForeignPtr dfp $ \dptr -> do
        let !end = plusPtr sptr slen
        decodeLoop dtable dfp dptr sptr end
  where
    !dlen = (slen `quot` 8) * 5

-- | Decode established Base32 provenance without validation.
-- Padding contributes no payload; partial final quanta need no copying.
decodeBase32Typed_ :: Ptr Word8 -> Base32 alphabet ByteString -> ByteString
decodeBase32Typed_ (Ptr table) encoded = unsafeDupablePerformIO $ do
    dfp <- mallocPlainForeignPtrBytes dlen
    withForeignPtr dfp $ \dst ->
      withForeignPtr sfp $ \src -> decodeLoopTyped table dst src slen
    pure (BS dfp dlen)
  where
    BS sfp slen = BS.dropWhileEnd (== 61) (extractBase32 encoded)
    dlen = (slen `quot` 8) * 5 + (slen `rem` 8) * 5 `quot` 8
{-# INLINE decodeBase32Typed_ #-}
