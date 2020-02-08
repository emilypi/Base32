{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
module Data.ByteString.Base32.Internal.Loop
( innerLoop
, innerLoopNopad
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
    :: Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> (Ptr Word8 -> Ptr Word8 -> IO ())
    -> IO ()
innerLoop !lut !dptr !sptr !end finish = go dptr sptr
  where
    lut64 a dst = poke dst =<< peekElemOff lut (fromIntegral a .&. 0x1f)

    go !dst !src
      | plusPtr src 4 >= end = finish dst src
      | otherwise = do
#ifdef WORDS_BIGENDIAN
        !t <- peek @Word32 (castPtr src)
#else
        !t <- byteSwap32 <$> peek @Word32 (castPtr src)
#endif

        !u <- w32 <$> peek (plusPtr src 4)

        let !a = (unsafeShiftR t 27)
            !b = (unsafeShiftR t 22)
            !c = (unsafeShiftR t 17)
            !d = (unsafeShiftR t 12)
            !e = (unsafeShiftR t 7)
            !f = (unsafeShiftR t 2)
            !g = ((unsafeShiftL t 3) .|. (unsafeShiftR u 5))

        !aa <- lut64 a dst
        !bb <- lut64 b (plusPtr dst 1)
        !cc <- lut64 c (plusPtr dst 2)
        !dd <- lut64 d (plusPtr dst 3)
        !ee <- lut64 e (plusPtr dst 4)
        !ff <- lut64 f (plusPtr dst 5)
        !gg <- lut64 g (plusPtr dst 6)
        !hh <- lut64 u (plusPtr dst 7)

        go (plusPtr dst 8) (plusPtr src 5)

innerLoopNopad
    :: Ptr Word8
    -> Ptr Word64
    -> Ptr Word8
    -> Ptr Word8
    -> (Ptr Word8 -> Ptr Word8 -> Int -> IO ByteString)
    -> IO ByteString
innerLoopNopad (Ptr !lut) !dptr !sptr !end finish = go dptr sptr 0
  where
    lix dst n a = poke (plusPtr dst n) (aix (fromIntegral (a .&. 0x1f)) lut)

    go !dst !src !n
      | plusPtr src 5 >= end = finish (castPtr dst) src n
      | otherwise = do
#ifdef WORDS_BIGENDIAN
        !t <- peek @Word32 (castPtr src)
#else
        !t <- byteSwap32 <$> peek @Word32 (castPtr src)
#endif
        !u <- w32 <$> peek (plusPtr src 4)

        !aa <- lix dst 0 (unsafeShiftR t 27)
        !bb <- lix dst 1 (unsafeShiftR t 22)
        !cc <- lix dst 2 (unsafeShiftR t 17)
        !dd <- lix dst 3 (unsafeShiftR t 12)
        !ee <- lix dst 4 (unsafeShiftR t 7)
        !ff <- lix dst 5 (unsafeShiftR t 2)
        !gg <- lix dst 6 ((unsafeShiftL t 3) .|. (unsafeShiftR u 5))
        !hh <- lix dst 7 u

        go (plusPtr dst 8) (plusPtr src 5) (n + 8)
