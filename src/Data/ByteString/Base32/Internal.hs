{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module       : Data.ByteString.Base32.Internal
-- Copyright 	: (c) 2020 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- Internal module defining the encoding and decoding
-- processes and tables.
--
module Data.ByteString.Base32.Internal
( -- * Alphabets
  stdAlphabet
, hexAlphabet
) where


import Data.Bits
import qualified Data.ByteString as BS
import Data.ByteString.Internal

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import GHC.Exts
import GHC.Word

import System.IO.Unsafe


-- | Read 'Word8' index off alphabet addr
--
aix :: Word8 -> Addr# -> Word8
aix (W8# i) alpha = W8# (indexWord8OffAddr# alpha (word2Int# i))
{-# INLINE aix #-}

stdAlphabet :: Ptr Word8
stdAlphabet = Ptr "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"#

hexAlphabet :: Ptr Word8
hexAlphabet = Ptr "0123456789ABCDEFGHIJKLMNOPQRSTUV"#

encodeBase32_ :: Ptr Word8 -> ByteString -> ByteString
encodeBase32_ !alpha (PS !sfp !soff !slen) =
    unsafeCreate dlen $ \dptr ->
    withForeignPtr sfp $ \sptr ->
      encodeBase32_'
        alpha
        dptr
        (plusPtr sptr soff)
        (plusPtr sptr (soff + slen))

  where
    !dlen = 8 * ((slen + 4) `div` 5)

encodeBase32_'
    :: Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> IO ()
encodeBase32_' !(Ptr addr#) !dptr !sptr !end = go dptr sptr
  where
    go dst src
      | plusPtr src 4 >= end = finalize dst src
      | otherwise = do
        a <- peek (castPtr src)
        b <- peek (plusPtr src 4)

#ifdef WORDS_BIGENDIAN
        encodeOctet dst a b
#else
        encodeOctet dst (byteSwap32 a) b
#endif
        go (plusPtr dst 8) (plusPtr src 5)

    padN !d 0 = return ()
    padN !d !n = poke @Word8 d 0x3d >> padN (plusPtr d 1) (n - 1)

    encodeOctet !dst !lo !hi =
      let k !w !i = poke @Word8 (plusPtr dst i) (aix (fromIntegral w .&. 0x1f) addr#)
          !a = fromIntegral (shiftR lo 27)
          !b = shiftR lo 22
          !c = shiftR lo 17
          !d = shiftR lo 12
          !e = shiftR lo 7
          !f = shiftR lo 2
          !g = (shiftL lo 3) .|. fromIntegral (shiftR hi 5)
          !h = fromIntegral hi
      in k a 0 >> k b 1 >> k c 2 >> k d 3 >> k e 4 >> k f 5 >> k g 6 >> k h 7

    finalize :: Ptr Word8 -> Ptr Word8 -> IO ()
    finalize !dst !src
      | src == end = return () -- src + 0 = end
      | otherwise = do
        a <- peekByteOff src 0

        if
          | plusPtr src 1 == end -> do -- 2 6
            let x = shiftR (a .&. 0xf8) 3
                y = shiftL (a .&. 0x07) 2

            poke dst (aix x addr#)
            poke (plusPtr dst 1) (aix y addr#)
            padN (plusPtr dst 2) 6

          | plusPtr src 4 == end -> undefined -- 6 1

          | plusPtr src 3 == end -> undefined -- 4 3
          | plusPtr src 2 == end -> undefined -- 3 4
