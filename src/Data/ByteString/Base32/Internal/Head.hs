{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Data.ByteString.Base32.Internal.Head
  ( encodeBase32_
  , encodeBase32NoPad_
  , decodeBase32_
  ) where

import Data.ByteString (length, null)
import Data.ByteString.Base32.Internal.Loop
import Data.ByteString.Base32.Internal.Tail
import Data.ByteString.Base32.Internal.Utils (encodedLength)
import Data.ByteString.Internal
import Data.Text (Text)
import Prelude hiding (length, null)

import Foreign.ForeignPtr
import Foreign.Ptr

import GHC.Exts
import GHC.ForeignPtr
import GHC.Word

import System.IO.Unsafe

-- | Head of the padded base32 encoding loop.
--
-- This function takes an alphabet in the form of an unboxed 'Addr#',
-- allocates the correct number of bytes that will be written, and
-- executes the inner encoding loop against that data.
encodeBase32_ :: Addr# -> ByteString -> ByteString
encodeBase32_ !lut (BS !sfp !l) = unsafeDupablePerformIO $ do
  dfp <- mallocPlainForeignPtrBytes dlen
  withForeignPtr dfp $ \dptr ->
    withForeignPtr sfp $ \sptr -> do
      let !end = plusPtr sptr l
      innerLoop
        lut
        (castPtr dptr)
        sptr
        end
        (loopTail lut dfp dptr end)
 where
  !dlen = encodedLength l

-- | Head of the unpadded base32 encoding loop.
--
-- This function takes an alphabet in the form of an unboxed 'Addr#',
-- allocates the correct number of bytes that will be written, and
-- executes the inner encoding loop against that data.
encodeBase32NoPad_ :: Addr# -> ByteString -> ByteString
encodeBase32NoPad_ !lut (BS !sfp !l) = unsafeDupablePerformIO $ do
  !dfp <- mallocPlainForeignPtrBytes dlen
  withForeignPtr dfp $ \dptr ->
    withForeignPtr sfp $ \sptr -> do
      let !end = plusPtr sptr l
      innerLoop
        lut
        (castPtr dptr)
        sptr
        end
        (loopTailNoPad lut dfp dptr end)
 where
  !dlen = encodedLength l

-- | Head of the base32 decoding loop.
--
-- This function takes a base32-decoding lookup table and base32-encoded
-- bytestring, allocates the correct number of bytes that will be written,
-- and executes the inner decoding loop against that data.
decodeBase32_ :: Ptr Word8 -> ByteString -> IO (Either Text ByteString)
decodeBase32_ table bs
  | null bs = pure (Right bs)
  | length bs `rem` 8 /= 0 = pure (Left "Base32 input requires complete quanta")
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
