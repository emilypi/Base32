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
( -- * Encoding
  encodeBase32_
, encodeBase32Nopad_

  -- * Decoding
, decodeBase32_

  -- * Alphabets
, stdAlphabet
, hexAlphabet
, stdDecodeTable
, hexDecodeTable

  -- * Validation
, validateBase32
) where


import Data.Bits
import qualified Data.ByteString as BS
import Data.ByteString.Internal
import Data.Text (Text)
import qualified Data.Text as T

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import GHC.Exts
import GHC.ForeignPtr
import GHC.Word

import System.IO.Unsafe


-- -------------------------------------------------------------------------- --
-- Helpers

-- | Read 'Word8' index off alphabet addr
--
aix :: Word8 -> Addr# -> Word8
aix (W8# i) alpha = W8# (indexWord8OffAddr# alpha (word2Int# i))
{-# INLINE aix #-}

stdAlphabet :: Ptr Word8
stdAlphabet = Ptr "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"#

hexAlphabet :: Ptr Word8
hexAlphabet = Ptr "0123456789ABCDEFGHIJKLMNOPQRSTUV"#

  -- | Allocate and fill @n@ bytes with some data
--
writeNPlainForeignPtrBytes
    :: ( Storable a
       , Storable b
       )
    => Int
    -> [a]
    -> ForeignPtr b
writeNPlainForeignPtrBytes !n as = unsafeDupablePerformIO $ do
    fp <- mallocPlainForeignPtrBytes n
    withForeignPtr fp $ \p -> go p as
    return (castForeignPtr fp)
  where
    go !_ [] = return ()
    go !p (x:xs) = poke p x >> go (plusPtr p 1) xs

-- -------------------------------------------------------------------------- --
-- Validating Base64

validateBase32 :: ByteString -> ByteString -> Bool
validateBase32 !alphabet (PS fp off l) =
    accursedUnutterablePerformIO $ withForeignPtr fp $ \p ->
      go (plusPtr p off) (plusPtr p (l + off))
  where
    go !p !end
      | p == end = return True
      | otherwise = do
        w <- peek p

        let f a
              | a == 0x3d, plusPtr p 1 == end = True
              | a == 0x3d, plusPtr p 2 == end = True
              | a == 0x3d, plusPtr p 3 == end = True
              | a == 0x3d, plusPtr p 4 == end = True
              | a == 0x3d, plusPtr p 5 == end = True
              | a == 0x3d, plusPtr p 6 == end = True
              | a == 0x3d = False
              | otherwise = BS.elem a alphabet

        if f w then go (plusPtr p 1) end else return False
{-# INLINE validateBase32 #-}

-- -------------------------------------------------------------------------- --
-- Encode Base32


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

innerLoop
    :: Addr#
    -> Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> (Ptr Word8 -> Ptr Word8 -> IO ())
    -> IO ()
innerLoop !alpha !dptr !sptr !end finalize = go dptr sptr
  where
    go !dst !src
      | plusPtr src 4 >= end = finalize dst src
      | otherwise = do
        a <- peek (castPtr src)
        b <- peek (plusPtr src 4)

#ifdef WORDS_BIGENDIAN
        encodeOctet alpha dst a b
#else
        encodeOctet alpha dst (byteSwap32 a) b
#endif
        go (plusPtr dst 8) (plusPtr src 5)

-- | Encode an octet from a lower 'Word32' and upper 'Word8'. In Base32 encodings,
-- this amounts to turning 5 bytes into 8 bytes.
--
encodeOctet :: Addr# -> Ptr Word8 -> Word32 -> Word8 -> IO ()
encodeOctet !addr# !dst !lo !hi =
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

encodeBase32_'
    :: Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> IO ()
encodeBase32_' !(Ptr alpha) !dptr !sptr !end =
    innerLoop alpha dptr sptr end finalize
  where
    -- you can't spell -funroll without fun!
    --
    padN !d 0 = return ()
    padN !d !n = poke @Word8 d 0x3d >> padN (plusPtr d 1) (n - 1)

    finalize :: Ptr Word8 -> Ptr Word8 -> IO ()
    finalize !dst !src
      | src == end = return ()
      | otherwise = do
        !a <- peek src

        if
          | plusPtr src 1 == end -> do -- 2 6
            !b <- peek (plusPtr src 1)

            let !x = shiftR (a .&. 0xf8) 3
                !y = shiftL (a .&. 0x07) 2 .|. (shiftR (b .&. 0xc0) 6)

            poke dst (aix x alpha)
            poke (plusPtr dst 1) (aix y alpha)
            padN (plusPtr dst 2) 6

          | plusPtr src 2 == end -> do -- 4 4
            !b <- peek (plusPtr src 1)

            let !w = shiftR (a .&. 0xf8) 3
                !x = (shiftL (a .&. 0x07) 2) .|. (shiftR (b .&. 0xc0) 6)
                !y = shiftR (b .&. 0x3e) 1
                !z = (shiftL (b .&. 0x01) 4)

            poke dst (aix w alpha)
            poke (plusPtr dst 1) (aix x alpha)
            poke (plusPtr dst 2) (aix y alpha)
            poke (plusPtr dst 3) (aix z alpha)
            padN (plusPtr dst 4) 4

          | plusPtr src 3 == end -> do -- 5 3
            !b <- peek (plusPtr src 1)
            !c <- peek (plusPtr src 2)

            let !t = shiftR (a .&. 0xf8) 3
                !u = (shiftL (a .&. 0x07) 2) .|. (shiftR (b .&. 0xc0) 6)
                !v = shiftR (b .&. 0x3e) 1
                !x = (shiftL (b .&. 0x01) 4) .|. (shiftR (c .&. 0xf0) 4)
                !y = (shiftL (c .&. 0x0f) 1)

            poke dst (aix t alpha)
            poke (plusPtr dst 1) (aix u alpha)
            poke (plusPtr dst 2) (aix v alpha)
            poke (plusPtr dst 3) (aix x alpha)
            poke (plusPtr dst 4) (aix y alpha)
            padN (plusPtr dst 5) 3

          | plusPtr src 4 == end -> do -- 7 1
            !b <- peek (plusPtr src 1)
            !c <- peek (plusPtr src 2)
            !d <- peek (plusPtr src 3)

            let !t = shiftR (a .&. 0xf8) 3
                !u = (shiftL (a .&. 0x07) 2) .|. (shiftR (b .&. 0xc0) 6)
                !v = shiftR (b .&. 0x3e) 1
                !w = (shiftL (b .&. 0x01) 4) .|. (shiftR (c .&. 0xf0) 4)
                !x = (shiftL (c .&. 0x0f) 1) .|. (shiftR (d .&. 0x80) 7)
                !y = shiftR (d .&. 0x7c) 2
                !z = shiftL (d .&. 0x03) 3

            poke dst (aix t alpha)
            poke (plusPtr dst 1) (aix u alpha)
            poke (plusPtr dst 2) (aix v alpha)
            poke (plusPtr dst 3) (aix w alpha)
            poke (plusPtr dst 4) (aix x alpha)
            poke (plusPtr dst 5) (aix y alpha)
            poke (plusPtr dst 6) (aix z alpha)
            padN (plusPtr dst 7) 1

-- -------------------------------------------------------------------------- --
-- Encode Base32 (nopad)


encodeBase32Nopad_ :: Ptr Word8 -> ByteString -> ByteString
encodeBase32Nopad_ !alpha (PS !sfp !soff !slen) = unsafeDupablePerformIO $ do
    dfp <- mallocPlainForeignPtrBytes dlen
    withForeignPtr dfp $ \dptr ->
      withForeignPtr sfp $ \sptr ->
        encodeBase32Nopad_'
          alpha
          dptr
          (plusPtr sptr soff)
          (plusPtr sptr (soff + slen))
          dfp
  where
    !dlen = 8 * ((slen + 4) `div` 5)

innerLoopNopad
    :: Addr#
    -> Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> (Ptr Word8 -> Ptr Word8 -> Int -> IO ByteString)
    -> IO ByteString
innerLoopNopad !alpha !dptr !sptr !end finalize = go dptr sptr 0
  where
    go !dst !src !n
      | plusPtr src 4 >= end = finalize dst src n
      | otherwise = do
        a <- peek (castPtr src)
        b <- peek (plusPtr src 4)

#ifdef WORDS_BIGENDIAN
        encodeOctet alpha dst a b
#else
        encodeOctet alpha dst (byteSwap32 a) b
#endif
        go (plusPtr dst 8) (plusPtr src 5) (n + 8)

encodeBase32Nopad_'
    :: Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> ForeignPtr Word8
    -> IO ByteString
encodeBase32Nopad_' !(Ptr alpha) !dptr !sptr !end !dfp =
    innerLoopNopad alpha dptr sptr end finalize
  where
    -- Unroll final quanta
    --
    finalize !dst !src !n
      | src == end = return (PS dfp 0 n)
      | otherwise = do
        !a <- peek src

        if
          | plusPtr src 1 == end -> do -- 2 6
            !b <- peek (plusPtr src 1)

            let !x = shiftR (a .&. 0xf8) 3
                !y = shiftL (a .&. 0x07) 2 .|. (shiftR (b .&. 0xc0) 6)

            poke dst (aix x alpha)
            poke (plusPtr dst 1) (aix y alpha)
            return (PS dfp 0 (n + 2))

          | plusPtr src 2 == end -> do -- 4 4
            !b <- peek (plusPtr src 1)

            let !w = shiftR (a .&. 0xf8) 3
                !x = (shiftL (a .&. 0x07) 2) .|. (shiftR (b .&. 0xc0) 6)
                !y = shiftR (b .&. 0x3e) 1
                !z = (shiftL (b .&. 0x01) 4)

            poke dst (aix w alpha)
            poke (plusPtr dst 1) (aix x alpha)
            poke (plusPtr dst 2) (aix y alpha)
            poke (plusPtr dst 3) (aix z alpha)
            return (PS dfp 0 (n + 4))

          | plusPtr src 3 == end -> do -- 5 3
            !b <- peek (plusPtr src 1)
            !c <- peek (plusPtr src 2)

            let !t = shiftR (a .&. 0xf8) 3
                !u = (shiftL (a .&. 0x07) 2) .|. (shiftR (b .&. 0xc0) 6)
                !v = shiftR (b .&. 0x3e) 1
                !x = (shiftL (b .&. 0x01) 4) .|. (shiftR (c .&. 0xf0) 4)
                !y = (shiftL (c .&. 0x0f) 1)

            poke dst (aix t alpha)
            poke (plusPtr dst 1) (aix u alpha)
            poke (plusPtr dst 2) (aix v alpha)
            poke (plusPtr dst 3) (aix x alpha)
            poke (plusPtr dst 4) (aix y alpha)
            return (PS dfp 0 (n + 5))

          | otherwise -> do -- 7 1
            !b <- peek (plusPtr src 1)
            !c <- peek (plusPtr src 2)
            !d <- peek (plusPtr src 3)

            let !t = shiftR (a .&. 0xf8) 3
                !u = (shiftL (a .&. 0x07) 2) .|. (shiftR (b .&. 0xc0) 6)
                !v = shiftR (b .&. 0x3e) 1
                !w = (shiftL (b .&. 0x01) 4) .|. (shiftR (c .&. 0xf0) 4)
                !x = (shiftL (c .&. 0x0f) 1) .|. (shiftR (d .&. 0x80) 7)
                !y = shiftR (d .&. 0x7c) 2
                !z = shiftL (d .&. 0x03) 3

            poke dst (aix t alpha)
            poke (plusPtr dst 1) (aix u alpha)
            poke (plusPtr dst 2) (aix v alpha)
            poke (plusPtr dst 3) (aix w alpha)
            poke (plusPtr dst 4) (aix x alpha)
            poke (plusPtr dst 5) (aix y alpha)
            poke (plusPtr dst 6) (aix z alpha)
            return (PS dfp 0 (n + 7))

-- -------------------------------------------------------------------------- --
-- Decode Base32 (nopad)

stdDecodeTable :: ForeignPtr Word8
stdDecodeTable = writeNPlainForeignPtrBytes @Word8 256
    [ 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
    , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
    , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
    , 0xff,0xff,0x1a,0x1b,0x1c,0x1d,0x1e,0x1f,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
    , 0xff,0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0a,0x0b,0x0c,0x0d,0x0e
    , 0x0f,0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0xff,0xff,0xff,0xff,0xff
    , 0xff,0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0a,0x0b,0x0c,0x0d,0x0e
    , 0x0f,0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0xff,0xff,0xff,0xff,0xff
    , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
    , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
    , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
    , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
    , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
    , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
    , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
    , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
    ]
{-# NOINLINE stdDecodeTable #-}

hexDecodeTable :: ForeignPtr Word8
hexDecodeTable = writeNPlainForeignPtrBytes @Word8 256
    [ 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
    , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
    , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
    , 0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0xff,0xff,0xff,0xff,0xff,0xff
    , 0xff,0x0a,0x0b,0x0c,0x0d,0x0e,0x0f,0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18
    , 0x19,0x1a,0x1b,0x1c,0x1d,0x1e,0x1f,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
    , 0xff,0x0a,0x0b,0x0c,0x0d,0x0e,0x0f,0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18
    , 0x19,0x1a,0x1b,0x1c,0x1d,0x1e,0x1f,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
    , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
    , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
    , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
    , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
    , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
    , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
    , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
    , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
    ]
{-# NOINLINE hexDecodeTable #-}

decodeBase32_ :: Bool -> ForeignPtr Word8 -> ByteString -> Either Text ByteString
decodeBase32_ !padding !dtfp bs@(PS _ _ !slen)
    | padding = go (BS.append bs (BS.replicate r 0x3d))
    | r /= 0 = Left "invalid padding"
    | otherwise = go bs
  where
    (!q, !r) = divMod slen 8
    !dlen = q * 5

    go (PS !sfp !soff !slen) = unsafeDupablePerformIO $
      withForeignPtr dtfp $ \dtable ->
        withForeignPtr sfp $ \sptr -> do
          dfp <- mallocPlainForeignPtrBytes dlen
          withForeignPtr dfp $ \dptr ->
            decodeBase32_'
              dtable
              (plusPtr sptr soff)
              dptr
              (plusPtr sptr (soff + slen))
              dfp

-- | Decode an octet as 'Word64' by shifting over its contents in 5-bit intervals
--
decodeOctet :: Addr# -> Ptr Word8 -> Word64 -> IO ()
decodeOctet !alpha !dst !w = do
    poke @Word8 dst (fromIntegral (shiftR x 32))
#ifdef WORDS_BIGENDIAN
    poke @Word32 (castPtr (plusPtr dst 1)) (fromIntegral x)
#else
    poke (castPtr (plusPtr dst 1)) (byteSwap32 (fromIntegral x))
#endif

  where
    x = w5 (shiftR w 00)
      $ w5 (shiftR w 08)
      $ w5 (shiftR w 16)
      $ w5 (shiftR w 24)
      $ w5 (shiftR w 32)
      $ w5 (shiftR w 40)
      $ w5 (shiftR w 48)
      $ w5 (shiftR w 56)
      0

    w5 :: Word64 -> Word64 -> Word64
    w5 !a !acc = (shiftL acc 5) .|. fromIntegral (aix (fromIntegral a) alpha)

decodeBase32_'
    :: Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> ForeignPtr Word8
    -> IO (Either Text ByteString)
decodeBase32_' dtable@(Ptr !alpha) !sptr !dptr !end !dfp = go sptr dptr 0
  where
    go !src !dst !n
      | plusPtr src 8 >= end = finalize src dst n
      | otherwise = do
        !w <- peek (castPtr src)
#ifdef WORDS_BIGENDIAN
        decodeOctet alpha dst w
#else
        decodeOctet alpha dst (byteSwap64 w)
#endif
        go (plusPtr src 8) (plusPtr dst 5) (n + 8)

    -- err = return . Left . T.pack
    -- {-# INLINE err #-}

    look :: Ptr Word8 -> IO Word64
    look p = do
      !i <- peekByteOff @Word8 p 0
      !v <- peekByteOff @Word8 dtable (fromIntegral i)
      return (fromIntegral v)

    finish !n = return (Right (PS dfp 0 n))

    finalize !src !dst !n
      | src >= end = finish n
      | otherwise = do
        a <- look src
        b <- look (plusPtr src 1)
        c <- look (plusPtr src 2)
        d <- look (plusPtr src 3)
        e <- look (plusPtr src 4)
        f <- look (plusPtr src 5)
        g <- look (plusPtr src 6)
        h <- look (plusPtr src 7)

        let !w = (shiftL a 35)
              .|. (shiftL b 30)
              .|. (shiftL c 25)
              .|. (shiftL d 20)
              .|. (shiftL e 15)
              .|. (shiftL f 10)
              .|. (shiftL g 5)
              .|. h

        poke @Word8 dst (fromIntegral (shiftR w 32))
        if c == 0xff
        then finish (n + 1)
        else do
          poke @Word8 (plusPtr dst 1) (fromIntegral (shiftR w 24))
          if d == 0xff
          then finish (n + 2)
          else do
            poke @Word8 (plusPtr dst 2) (fromIntegral (shiftR w 16))
            if e == 0xff
            then finish (n + 3)
            else do
              poke @Word8 (plusPtr dst 3) (fromIntegral (shiftR w 8))
              if f == 0xff
              then finish (n + 4)
              else do
                poke @Word8 (plusPtr dst 4) (fromIntegral w)
                finish (n + 5)
