{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.ByteString.Base32.Internal.Head
( encodeBase32_
, encodeBase32NoPad_
, decodeBase32_
) where


import Data.ByteString.Internal
import Data.ByteString.Base32.Internal.Loop
import Data.ByteString.Base32.Internal.Tail
import Data.ByteString.Base32.Internal.Utils
import Data.Text (Text)

import Foreign.Ptr
import Foreign.ForeignPtr

import GHC.Exts
import GHC.ForeignPtr
import GHC.Word

import System.IO.Unsafe


-- | Head of the base32 encoding loop - marshal data, assemble loops
--
encodeBase32_ :: Addr# -> ByteString -> ByteString
encodeBase32_ !lut (PS !sfp !o !l) =
    unsafeDupablePerformIO $ do
      dfp <- mallocPlainForeignPtrBytes dlen
      withForeignPtr dfp $ \dptr ->
        withForeignPtr sfp $ \sptr -> do
          let !end = plusPtr sptr (l + o)
          innerLoop
            lut
            (castPtr dptr)
            (plusPtr sptr o)
            end
            (loopTail lut dfp dptr end)
  where
    !q = (l * 8) `quot` 5
    !r = (l * 8) `rem` 5
    !dlen = padCeilN 8 (q + if r == 0 then 0 else 1)

-- | Head of the unpadded base32 encoding loop - marshal data, assemble loops
--
encodeBase32NoPad_ :: Addr# -> ByteString -> ByteString
encodeBase32NoPad_ !lut (PS !sfp !o !l) =
    unsafeDupablePerformIO $ do
      !dfp <- mallocPlainForeignPtrBytes dlen
      withForeignPtr dfp $ \dptr ->
        withForeignPtr sfp $ \sptr -> do
          let !end = plusPtr sptr (l + o)
          innerLoop lut
            (castPtr dptr)
            (plusPtr sptr o)
            end
            (loopTailNoPad lut dfp dptr end)
  where
    !q = (l * 8) `quot` 5
    !r = (l * 8) `rem` 5
    !dlen = padCeilN 8 (q + if r == 0 then 0 else 1)

-- | Head of the base32 decoding loop - marshal data, assemble loops
--
decodeBase32_ :: Int -> ForeignPtr Word8 -> ByteString -> IO (Either Text ByteString)
decodeBase32_ !dlen !dtfp (PS !sfp !soff !slen) =
    withForeignPtr dtfp $ \(Ptr dtable) ->
    withForeignPtr sfp $ \sptr -> do
      dfp <- mallocPlainForeignPtrBytes dlen
      withForeignPtr dfp $ \dptr -> do
        let !end = plusPtr sptr (soff + slen)
        decodeLoop dtable dptr (plusPtr sptr soff) end (decodeTail dtable dfp end)
