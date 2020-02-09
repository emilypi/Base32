{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
module Data.ByteString.Base32.Internal.Loop
( innerLoop
, innerLoopNoPad
) where

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString.Base32.Internal.Utils

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable

import GHC.Exts
import GHC.Word


innerLoop
    :: Addr#
    -> Ptr Word64
    -> Ptr Word8
    -> Ptr Word8
    -> (Ptr Word8 -> Ptr Word8 -> IO ())
    -> IO ()
innerLoop !lut !dptr !sptr !end finish = go dptr sptr
  where
    lix64 a = return $ w64 (aix (fromIntegral a .&. 0x1f) lut)

    go !dst !src
      | plusPtr src 4 >= end = finish (castPtr dst) src
      | otherwise = do
#ifdef WORDS_BIGENDIAN
        !t <- peek @Word32 (castPtr src)
#else
        !t <- byteSwap32 <$> peek @Word32 (castPtr src)
#endif
        !u <- w32 <$> peek (plusPtr src 4)

        !aa <- lix64 (unsafeShiftR t 27)
        !bb <- lix64 (unsafeShiftR t 22)
        !cc <- lix64 (unsafeShiftR t 17)
        !dd <- lix64 (unsafeShiftR t 12)
        !ee <- lix64 (unsafeShiftR t 7)
        !ff <- lix64 (unsafeShiftR t 2)
        !gg <- lix64 ((unsafeShiftL t 3) .|. (unsafeShiftR u 5))
        !hh <- lix64 u

        w <- return $ aa
          .|. (unsafeShiftL bb 8)
          .|. (unsafeShiftL cc 16)
          .|. (unsafeShiftL dd 24)
          .|. (unsafeShiftL ee 32)
          .|. (unsafeShiftL ff 40)
          .|. (unsafeShiftL gg 48)
          .|. (unsafeShiftL hh 56)

        poke dst w

        go (plusPtr dst 8) (plusPtr src 5)

innerLoopNoPad
    :: Addr#
    -> Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> (Ptr Word8 -> Ptr Word8 -> Int -> IO ByteString)
    -> IO ByteString
innerLoopNoPad !lut !dptr !sptr !end finish = go dptr sptr 0
  where
    lix !a !p = poke @Word8 p (aix (fromIntegral (a .&. 0x1f)) lut)

    go !dst !src !n
      | plusPtr src 4 >= end = finish dst src n
      | otherwise = do
#ifdef WORDS_BIGENDIAN
        !t <- peek @Word32 (castPtr src)
#else
        !t <- byteSwap32 <$> peek @Word32 (castPtr src)
#endif
        !u <- w32 <$> peek (plusPtr src 4)

        !aa <- lix (unsafeShiftR t 27) dst
        !bb <- lix (unsafeShiftR t 22) (plusPtr dst 1)
        !cc <- lix (unsafeShiftR t 17) (plusPtr dst 2)
        !dd <- lix (unsafeShiftR t 12) (plusPtr dst 3)
        !ee <- lix (unsafeShiftR t 7) (plusPtr dst 4)
        !ff <- lix (unsafeShiftR t 2) (plusPtr dst 5)
        !gg <- lix ((unsafeShiftL t 3) .|. (unsafeShiftR u 5)) (plusPtr dst 6)
        !hh <- lix u (plusPtr dst 7)

        go (plusPtr dst 8) (plusPtr src 5) (n + 8)
