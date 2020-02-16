{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Data.ByteString.Base32.Internal.Tail
( loopTail
, loopTailNoPad
, decodeTail
) where


import Data.Bits
import Data.ByteString.Internal
import Data.ByteString.Base32.Internal.Utils
import Data.Text (Text)
import qualified Data.Text as T

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

    padN :: Ptr Word8 -> Int -> IO ()
    padN !_ 0 = return ()
    padN !p n = poke p 0x3d >> padN (plusPtr p 1) (n - 1)
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
    look !i = aix i lut
{-# INLINE loopTailNoPad #-}

-- ------------------------------------------------------------------------ --
-- Decoding loops

decodeTail
    :: Addr#
    -> ForeignPtr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> Int
    -> IO (Either Text ByteString)
decodeTail !lut !dfp !end !dptr !sptr !n = go dptr sptr
  where
    lix a = aix a lut
    {-# INLINE lix #-}

    ps !m = return (Right (PS dfp 0 m))
    {-# INLINE ps #-}

    err = return . Left . T.pack
    {-# INLINE err #-}

    decodeOctet (!a,!b,!c,!d,!e,!f,!g,!h) =
      case (lix a, lix b, lix c, lix d, lix e, lix f, lix g, lix h) of
        (0xff,_,_,_,_,_,_,_) -> Left (0 :: Int)
        (_,0xff,_,_,_,_,_,_) -> Left 1
        (_,_,0xff,_,_,_,_,_) -> Left 2
        (_,_,_,0xff,_,_,_,_) -> Left 3
        (_,_,_,_,0xff,_,_,_) -> Left 4
        (_,_,_,_,_,0xff,_,_) -> Left 5
        (_,_,_,_,_,_,0xff,_) -> Left 6
        (_,_,_,_,_,_,_,0xff) -> Left 7
        (ri1,ri2,ri3,ri4,ri5,ri6,ri7,ri8) ->
            let !o1 = (ri1 `unsafeShiftL` 3) .|. (ri2 `unsafeShiftR` 2)
                !o2 = (ri2 `unsafeShiftL` 6) .|. (ri3 `unsafeShiftL` 1) .|. (ri4 `unsafeShiftR` 4)
                !o3 = (ri4 `unsafeShiftL` 4) .|. (ri5 `unsafeShiftR` 1)
                !o4 = (ri5 `unsafeShiftL` 7) .|. (ri6 `unsafeShiftL` 2) .|. (ri7 `unsafeShiftR` 3)
                !o5 = (ri7 `unsafeShiftL` 5) .|. ri8
             in Right (o1, o2, o3, o4, o5)

    go !dst !src
      | src == end = ps n
      | otherwise = do
        !a <- peek @Word8 src
        !b <- peek @Word8 (plusPtr src 1)
        !c <- peek @Word8 (plusPtr src 2)
        !d <- peek @Word8 (plusPtr src 3)
        !e <- peek @Word8 (plusPtr src 4)
        !f <- peek @Word8 (plusPtr src 5)
        !g <- peek @Word8 (plusPtr src 6)
        !h <- peek @Word8 (plusPtr src 7)

        let (!m, !c', !d', !e', !f', !g', !h') = case (c,d,e,f,g,h) of
              (0x3d,0x3d,0x3d,0x3d,0x3d,0x3d) -> (6,0x41,0x41,0x41,0x41,0x41,0x41)
              (_,0x3d,0x3d,0x3d,0x3d,0x3d) -> (5,c,0x41,0x41,0x41,0x41,0x41)
              (_,_,0x3d,0x3d,0x3d,0x3d) -> (4,c,d,0x41,0x41,0x41,0x41)
              (_,_,_,0x3d,0x3d,0x3d) -> (3,c,d,e,0x41,0x41,0x41)
              (_,_,_,_,0x3d,0x3d) -> (2,c,d,e,f,0x41,0x41)
              (_,_,_,_,_,0x3d) -> (1,c,d,e,f,g,0x41)
              _ -> (0 :: Int,c,d,e,f,g,h)

        case decodeOctet (a,b,c',d',e',f',g',h') of
          Left ofs -> err $ "invalid character at offset: " ++ show (n + ofs)
          Right (!v,!w,!x,!y,!z) -> do
            poke dst v
            poke (plusPtr dst 1) w

            if
              | m == 0 -> do
                poke (plusPtr dst 2) x
                poke (plusPtr dst 3) y
                poke (plusPtr dst 4) z
                ps (n + 5)
              | m == 1 -> do
                poke (plusPtr dst 2) x
                poke (plusPtr dst 3) y
                poke (plusPtr dst 4) z
                ps (n + 4)
              | m < 4 -> do
                poke (plusPtr dst 2) x
                poke (plusPtr dst 3) y
                ps (n + 3)
              | m < 5 -> do
                poke (plusPtr dst 2) x
                ps (n + 2)
              | otherwise -> ps (n + 1)
