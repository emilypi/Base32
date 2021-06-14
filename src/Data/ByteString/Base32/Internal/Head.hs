{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
module Data.ByteString.Base32.Internal.Head
( encodeBase32_
, encodeBase32NoPad_
, decodeBase32_
) where


import Data.ByteString.Internal
import Data.ByteString.Base32.Internal.Loop
import Data.ByteString.Base32.Internal.Tail
import Data.Text (Text)

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
encodeBase32_ !lut (PS !sfp !o !l) = unsafeDupablePerformIO $ do
    dfp <- mallocPlainForeignPtrBytes dlen
    withForeignPtr dfp $ \dptr ->
      withForeignPtr sfp $ \sptr -> do
        let !end = plusPtr sptr (l + o)
        innerLoop lut
          (castPtr dptr) (plusPtr sptr o)
          end (loopTail lut dfp dptr end)
  where
    !dlen = ceiling (fromIntegral @_ @Double l / 5) * 8

-- | Head of the unpadded base32 encoding loop.
--
-- This function takes an alphabet in the form of an unboxed 'Addr#',
-- allocates the correct number of bytes that will be written, and
-- executes the inner encoding loop against that data.
--
encodeBase32NoPad_ :: Addr# -> ByteString -> ByteString
encodeBase32NoPad_ !lut (PS !sfp !o !l) = unsafeDupablePerformIO $ do
    !dfp <- mallocPlainForeignPtrBytes dlen
    withForeignPtr dfp $ \dptr ->
      withForeignPtr sfp $ \sptr -> do
        let !end = plusPtr sptr (l + o)
        innerLoop lut
          (castPtr dptr) (plusPtr sptr o)
          end (loopTailNoPad lut dfp dptr end)
  where
    !dlen = ceiling (fromIntegral @_ @Double l / 5) * 8

-- | Head of the base32 decoding loop.
--
-- This function takes a base32-decoding lookup table and base32-encoded
-- bytestring, allocates the correct number of bytes that will be written,
-- and executes the inner decoding loop against that data.
--
decodeBase32_ :: Ptr Word8 -> ByteString -> IO (Either Text ByteString)
decodeBase32_ (Ptr !dtable) (PS !sfp !soff !slen) =
    withForeignPtr sfp $ \sptr -> do
      dfp <- mallocPlainForeignPtrBytes dlen
      withForeignPtr dfp $ \dptr -> do
        let !end = plusPtr sptr (soff + slen)
        decodeLoop dtable dfp dptr (plusPtr sptr soff) end
  where
    !dlen = ceiling (fromIntegral @_ @Double slen / 1.6)
