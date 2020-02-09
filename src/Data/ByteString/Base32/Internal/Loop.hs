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


-- ------------------------------------------------------------------------ --
-- Encoding loops

innerLoop
    :: Addr#
    -> Ptr Word64
    -> Ptr Word8
    -> Ptr Word8
    -> (Ptr Word8 -> Ptr Word8 -> IO ())
    -> IO ()
innerLoop !lut !dptr !sptr !end finish = go dptr sptr
  where
    lix a = return $ w64 (aix (fromIntegral a .&. 0x1f) lut)

    go !dst !src
      | plusPtr src 4 >= end = finish (castPtr dst) src
      | otherwise = do
#ifdef WORDS_BIGENDIAN
        !t <- peek @Word32 (castPtr src)
#else
        !t <- byteSwap32 <$> peek @Word32 (castPtr src)
#endif
        !u <- w32 <$> peek (plusPtr src 4)

        !a <- lix (unsafeShiftR t 27)
        !b <- lix (unsafeShiftR t 22)
        !c <- lix (unsafeShiftR t 17)
        !d <- lix (unsafeShiftR t 12)
        !e <- lix (unsafeShiftR t 7)
        !f <- lix (unsafeShiftR t 2)
        !g <- lix ((unsafeShiftL t 3) .|. (unsafeShiftR u 5))
        !h <- lix u

        w <- return $ a
          .|. (unsafeShiftL b 8)
          .|. (unsafeShiftL c 16)
          .|. (unsafeShiftL d 24)
          .|. (unsafeShiftL e 32)
          .|. (unsafeShiftL f 40)
          .|. (unsafeShiftL g 48)
          .|. (unsafeShiftL h 56)

        poke dst w

        go (plusPtr dst 8) (plusPtr src 5)

innerLoopNoPad
    :: Addr#
    -> Ptr Word64
    -> Ptr Word8
    -> Ptr Word8
    -> (Ptr Word8 -> Ptr Word8 -> Int -> IO ByteString)
    -> IO ByteString
innerLoopNoPad !lut !dptr !sptr !end finish = go dptr sptr 0
  where
    lix a = return $ w64 (aix (fromIntegral a .&. 0x1f) lut)

    go !dst !src !n
      | plusPtr src 4 >= end = finish (castPtr dptr) src n
      | otherwise = do
#ifdef WORDS_BIGENDIAN
        !t <- peek @Word32 (castPtr src)
#else
        !t <- byteSwap32 <$> peek @Word32 (castPtr src)
#endif
        !u <- w32 <$> peek (plusPtr src 4)

        !a <- lix (unsafeShiftR t 27)
        !b <- lix (unsafeShiftR t 22)
        !c <- lix (unsafeShiftR t 17)
        !d <- lix (unsafeShiftR t 12)
        !e <- lix (unsafeShiftR t 7)
        !f <- lix (unsafeShiftR t 2)
        !g <- lix ((unsafeShiftL t 3) .|. (unsafeShiftR u 5))
        !h <- lix u

        !w <- return $ a
          .|. (unsafeShiftL b 8)
          .|. (unsafeShiftL c 16)
          .|. (unsafeShiftL d 24)
          .|. (unsafeShiftL e 32)
          .|. (unsafeShiftL f 40)
          .|. (unsafeShiftL g 48)
          .|. (unsafeShiftL h 56)

        poke dst w

        go (plusPtr dst 8) (plusPtr src 5) (n + 8)

-- ------------------------------------------------------------------------ --
-- Decoding loops
