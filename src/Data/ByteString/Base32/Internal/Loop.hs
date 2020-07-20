{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
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

    err :: Ptr Word64 -> IO (Either Text ByteString)
    err p = return . Left . T.pack
      $ "invalid character at offset: "
      ++ show (p `minusPtr` sptr)

    padErr :: Ptr Word64 -> IO (Either Text ByteString)
    padErr p =  return . Left . T.pack
      $ "invalid padding at offset: "
      ++ show (p `minusPtr` sptr)

    go !dst !src !n
      | plusPtr src 8 >= end = finish dst (castPtr src) n
      | otherwise = do
        !t <- peekWord64BE src

        let !a = lix (unsafeShiftR t 56)
            !b = lix (unsafeShiftR t 48)
            !c = lix (unsafeShiftR t 40)
            !d = lix (unsafeShiftR t 32)
            !e = lix (unsafeShiftR t 24)
            !f = lix (unsafeShiftR t 16)
            !g = lix (unsafeShiftR t 8)
            !h = lix t

        if
          | a == 0x3d -> padErr src
          | b == 0x3d -> padErr (plusPtr src 1)
          | c == 0x3d -> padErr (plusPtr src 2)
          | d == 0x3d -> padErr (plusPtr src 3)
          | e == 0x3d -> padErr (plusPtr src 4)
          | f == 0x3d -> padErr (plusPtr src 5)
          | g == 0x3d -> padErr (plusPtr src 6)
          | h == 0x3d -> padErr (plusPtr src 7)
          | a == 0xff -> err src
          | b == 0xff -> err (plusPtr src 1)
          | c == 0xff -> err (plusPtr src 2)
          | d == 0xff -> err (plusPtr src 3)
          | e == 0xff -> err (plusPtr src 4)
          | f == 0xff -> err (plusPtr src 5)
          | g == 0xff -> err (plusPtr src 6)
          | h == 0xff -> err (plusPtr src 7)
          | otherwise -> do

            let !w = ((unsafeShiftL a 35)
                  .|. (unsafeShiftL b 30)
                  .|. (unsafeShiftL c 25)
                  .|. (unsafeShiftL d 20)
                  .|. (unsafeShiftL e 15)
                  .|. (unsafeShiftL f 10)
                  .|. (unsafeShiftL g 5)
                  .|. h) :: Word64

            poke @Word32 (castPtr dst) (byteSwap32 (fromIntegral (unsafeShiftR w 8)))
            poke @Word8 (plusPtr dst 4) (fromIntegral w)
            go (plusPtr dst 5) (plusPtr src 8) (n + 5)
