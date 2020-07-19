{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
module Data.ByteString.Base32.Internal.Loop
( innerLoop
, decodeLoop
) where

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString.Base32.Internal.Utils
import Data.Text (Text)
import qualified Data.Text as T

import Foreign.Ptr
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
    -> (Ptr Word8 -> Ptr Word8 -> IO ByteString)
    -> IO ByteString
innerLoop !lut !dptr !sptr !end finish = go dptr sptr
  where
    lix a = return $ w64 (aix (fromIntegral a .&. 0x1f) lut)
    {-# INLINE lix #-}

    go !dst !src
      | plusPtr src 4 >= end = finish (castPtr dst) src
      | otherwise = do
        !t <- peekWord32BE (castPtr src)
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
{-# INLINE innerLoop #-}

-- ------------------------------------------------------------------------ --
-- Decoding loops

decodeLoop
    :: Addr#
    -> Ptr Word8
    -> Ptr Word64
    -> Ptr Word8
    -> (Ptr Word8 -> Ptr Word8 -> Int -> IO (Either Text ByteString))
    -> IO (Either Text ByteString)
decodeLoop !lut !dptr !sptr !end finish = go dptr sptr 0
  where
    lix a = w64 (aix (fromIntegral a) lut)

    roll !w !acc = (acc `unsafeShiftL` 5) .|. lix w

    err = return . Left . T.pack

    go !dst !src !n
      | plusPtr src 8 == end = finish dst (castPtr src) n
      | otherwise = do
        !t <- peekWord64BE src

        !w <- return
          $ roll (unsafeShiftR t 0)
          $ roll (unsafeShiftR t 8)
          $ roll (unsafeShiftR t 16)
          $ roll (unsafeShiftR t 24)
          $ roll (unsafeShiftR t 32)
          $ roll (unsafeShiftR t 40)
          $ roll (unsafeShiftR t 48)
          $ roll (unsafeShiftR t 56)
          0

        if w /= 0xff
        then do
          poke @Word8 dst (fromIntegral (w `unsafeShiftR` 32))
          poke @Word32 (castPtr (plusPtr dst 1)) (byteSwap32 (fromIntegral w))
          go (plusPtr dst 5) (plusPtr src 8) (n + 5)
        else err
          $ "invalid character at offset: "
          ++ show (src `minusPtr` sptr)
