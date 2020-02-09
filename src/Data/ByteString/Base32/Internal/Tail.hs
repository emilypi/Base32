{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
module Data.ByteString.Base32.Internal.Tail
( loopTail
, loopTailNoPad
) where


import Data.Bits
import Data.ByteString.Internal
import Data.ByteString.Base32.Internal.Utils

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable

import GHC.Exts
import GHC.Word


-- | Unroll final quantum encoding for base32
--
loopTail
    :: Addr#
    -> Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> IO ()
loopTail !lut !end !dst !src
    | plusPtr src 1 == end = do -- 2 6
      !a <- peek src

      let !t = look (unsafeShiftR (a .&. 0xf8) 3)
          !u = look (unsafeShiftL (a .&. 0x07) 2)

      poke dst t
      poke (plusPtr dst 1) u
      padN (plusPtr dst 2) 6

    | plusPtr src 2 == end = do -- 4 4
      !a <- peek src
      !b <- peek (plusPtr src 1)

      let !t = look (unsafeShiftR (a .&. 0xf8) 3)
          !u = look ((unsafeShiftL (a .&. 0x07) 2) .|. (unsafeShiftR (b .&. 0xc0) 6))
          !v = look (unsafeShiftR (b .&. 0x3e) 1)
          !w = look (unsafeShiftL (b .&. 0x01) 4)

      poke dst t
      poke (plusPtr dst 1) u
      poke (plusPtr dst 2) v
      poke (plusPtr dst 3) w
      padN (plusPtr dst 4) 4

    | plusPtr src 3 == end = do -- 5 3
      !a <- peek src
      !b <- peek (plusPtr src 1)
      !c <- peek (plusPtr src 2)

      let !t = look (unsafeShiftR (a .&. 0xf8) 3)
          !u = look ((unsafeShiftL (a .&. 0x07) 2) .|. (unsafeShiftR (b .&. 0xc0) 6))
          !v = look (unsafeShiftR (b .&. 0x3e) 1)
          !w = look ((unsafeShiftL (b .&. 0x01) 4) .|. (unsafeShiftR (c .&. 0xf0) 4))
          !x = look (unsafeShiftL (c .&. 0x0f) 1)

      poke dst t
      poke (plusPtr dst 1) u
      poke (plusPtr dst 2) v
      poke (plusPtr dst 3) w
      poke (plusPtr dst 4) x
      padN (plusPtr dst 5) 3

    | plusPtr src 4 == end = do -- 7 1
      !a <- peek src
      !b <- peek (plusPtr src 1)
      !c <- peek (plusPtr src 2)
      !d <- peek (plusPtr src 3)

      let !t = look (unsafeShiftR (a .&. 0xf8) 3)
          !u = look ((unsafeShiftL (a .&. 0x07) 2) .|. (unsafeShiftR (b .&. 0xc0) 6))
          !v = look (unsafeShiftR (b .&. 0x3e) 1)
          !w = look ((unsafeShiftL (b .&. 0x01) 4) .|. (unsafeShiftR (c .&. 0xf0) 4))
          !x = look ((unsafeShiftL (c .&. 0x0f) 1) .|. (unsafeShiftR (d .&. 0x80) 7))
          !y = look (unsafeShiftR (d .&. 0x7c) 2)
          !z = look (unsafeShiftL (d .&. 0x03) 3)

      poke dst t
      poke (plusPtr dst 1) u
      poke (plusPtr dst 2) v
      poke (plusPtr dst 3) w
      poke (plusPtr dst 4) x
      poke (plusPtr dst 5) y
      poke (plusPtr dst 6) z
      padN (plusPtr dst 7) 1

    | otherwise = return ()
  where
    look !n = aix n lut

    padN !_ 0 = return ()
    padN !p n = poke @Word8 p 0x3d >> padN (plusPtr p 1) (n - 1)
{-# INLINE loopTail #-}

-- | Unroll final quantum encoding for base32
--
loopTailNoPad
    :: Addr#
    -> ForeignPtr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> Int
    -> IO ByteString
loopTailNoPad !lut !dfp !end !dst !src !n
  | plusPtr src 1 == end = do -- 2 6
      !a <- peek src

      let !t = look (unsafeShiftR (a .&. 0xf8) 3)
          !u = look (unsafeShiftL (a .&. 0x07) 2)

      poke dst t
      poke (plusPtr dst 1) u

      return (PS dfp 0 (n + 2))

    | plusPtr src 2 == end = do -- 4 4
      !a <- peek src
      !b <- peek (plusPtr src 1)

      let !t = look (unsafeShiftR (a .&. 0xf8) 3)
          !u = look ((unsafeShiftL (a .&. 0x07) 2) .|. (unsafeShiftR (b .&. 0xc0) 6))
          !v = look (unsafeShiftR (b .&. 0x3e) 1)
          !w = look (unsafeShiftL (b .&. 0x01) 4)

      poke dst t
      poke (plusPtr dst 1) u
      poke (plusPtr dst 2) v
      poke (plusPtr dst 3) w

      return (PS dfp 0 (n + 4))

    | plusPtr src 3 == end = do -- 5 3
      !a <- peek src
      !b <- peek (plusPtr src 1)
      !c <- peek (plusPtr src 2)

      let !t = look (unsafeShiftR (a .&. 0xf8) 3)
          !u = look ((unsafeShiftL (a .&. 0x07) 2) .|. (unsafeShiftR (b .&. 0xc0) 6))
          !v = look (unsafeShiftR (b .&. 0x3e) 1)
          !w = look ((unsafeShiftL (b .&. 0x01) 4) .|. (unsafeShiftR (c .&. 0xf0) 4))
          !x = look (unsafeShiftL (c .&. 0x0f) 1)

      poke dst t
      poke (plusPtr dst 1) u
      poke (plusPtr dst 2) v
      poke (plusPtr dst 3) w
      poke (plusPtr dst 4) x
      return (PS dfp 0 (n + 5))

    | plusPtr src 4 == end = do -- 7 1
      !a <- peek src
      !b <- peek (plusPtr src 1)
      !c <- peek (plusPtr src 2)
      !d <- peek (plusPtr src 3)

      let !t = look (unsafeShiftR (a .&. 0xf8) 3)
          !u = look ((unsafeShiftL (a .&. 0x07) 2) .|. (unsafeShiftR (b .&. 0xc0) 6))
          !v = look (unsafeShiftR (b .&. 0x3e) 1)
          !w = look ((unsafeShiftL (b .&. 0x01) 4) .|. (unsafeShiftR (c .&. 0xf0) 4))
          !x = look ((unsafeShiftL (c .&. 0x0f) 1) .|. (unsafeShiftR (d .&. 0x80) 7))
          !y = look (unsafeShiftR (d .&. 0x7c) 2)
          !z = look (unsafeShiftL (d .&. 0x03) 3)

      poke dst t
      poke (plusPtr dst 1) u
      poke (plusPtr dst 2) v
      poke (plusPtr dst 3) w
      poke (plusPtr dst 4) x
      poke (plusPtr dst 5) y
      poke (plusPtr dst 6) z
      return (PS dfp 0 (n + 7))

    | otherwise = return (PS dfp 0 n)
  where
    look !n = aix n lut

    padN !_ 0 = return ()
    padN !p n = poke @Word8 p 0x3d >> padN (plusPtr p 1) (n - 1)
{-# INLINE loopTailNoPad #-}
