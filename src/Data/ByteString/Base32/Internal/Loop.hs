{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module       : Data.ByteString.Base32.Internal.Loop
-- Copyright    : (c) 2020-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- Encoding loops, checked and typed decoding loops, and quantum validation.
--
-- @since 1.0
--
module Data.ByteString.Base32.Internal.Loop
( innerLoop
, decodeLoop
, decodeLoopTyped
, decodeQuantum
) where

#ifdef BASE32_SIMD
import qualified Data.ByteString.Base32.Internal.SIMD as SIMD
#endif

import Data.Bits
import Data.ByteString.Internal (ByteString(..))
import Data.ByteString.Base32.Internal.Utils
import Data.Text (Text)
import qualified Data.Text as T

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable

import GHC.ByteOrder
import GHC.Exts
import GHC.Word


-- ------------------------------------------------------------------------ --
-- Encoding loops

-- | Encode complete quanta, then run the supplied tail handler.
--
-- @since 1.0
--
innerLoop
    :: Addr#
    -> Ptr Word64
    -> Ptr Word8
    -> Ptr Word8
    -> (Ptr Word8 -> Ptr Word8 -> IO ByteString)
    -> IO ByteString
innerLoop !lut !dptr !sptr !end finish = do
#ifdef BASE32_SIMD
    consumed <- SIMD.encodePrefix lut sptr
      (end `minusPtr` sptr) (castPtr dptr)
    go (plusPtr dptr (consumed `quot` 5 * 8)) (plusPtr sptr consumed)
#else
  go dptr sptr
#endif
  where
    lix a = w64 (aix (fromIntegral a .&. 0x1f) lut)
    {-# INLINE lix #-}

    go !dst !src
      | plusPtr src 4 >= end = finish (castPtr dst) src
      | otherwise = do
        !t <- peekWord32BE (castPtr src)
        !u <- w32 <$> peek (plusPtr src 4)

        let !a = lix (unsafeShiftR t 27)
            !b = lix (unsafeShiftR t 22)
            !c = lix (unsafeShiftR t 17)
            !d = lix (unsafeShiftR t 12)
            !e = lix (unsafeShiftR t 7)
            !f = lix (unsafeShiftR t 2)
            !g = lix (unsafeShiftL t 3 .|. unsafeShiftR u 5)
            !h = lix u

        let !w = a
             .|. unsafeShiftL b 8
             .|. unsafeShiftL c 16
             .|. unsafeShiftL d 24
             .|. unsafeShiftL e 32
             .|. unsafeShiftL f 40
             .|. unsafeShiftL g 48
             .|. unsafeShiftL h 56

        poke dst (case targetByteOrder of
          LittleEndian -> w
          BigEndian -> byteSwap64 w)
        go (plusPtr dst 8) (plusPtr src 5)
{-# INLINE innerLoop #-}

-- ------------------------------------------------------------------------ --
-- Decoding loops

-- | Decode and validate complete quanta, including the final padding.
--
-- @since 1.0
--
decodeLoop
    :: Addr#
    -> ForeignPtr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> IO (Either Text ByteString)
decodeLoop !lut !dfp !dptr !sptr !end = do
#ifdef BASE32_SIMD
    consumed <- SIMD.decodePrefix lut sptr
      (max 0 (end `minusPtr` sptr - 8)) dptr
    go (plusPtr dptr (consumed `quot` 8 * 5)) (plusPtr sptr consumed)
#else
  go dptr sptr
#endif
  where
    look value = w64 (aix (fromIntegral value) lut)
    go !dst !src
      | src == end = pure (Right (BS dfp (dst `minusPtr` dptr)))
      | otherwise = do
          t <- peekWord64BE (castPtr src)
          let !a = look (unsafeShiftR t 56)
              !b = look (unsafeShiftR t 48)
              !c = look (unsafeShiftR t 40)
              !d = look (unsafeShiftR t 32)
              !e = look (unsafeShiftR t 24)
              !f = look (unsafeShiftR t 16)
              !g = look (unsafeShiftR t 8)
              !h = look t
              final = plusPtr src 8 == end
          if not final && (a .|. b .|. c .|. d .|. e .|. f .|. g .|. h) < 32
            then do
              writeFull dst (pack a b c d e f g h)
              go (plusPtr dst 5) (plusPtr src 8)
            else case decodeQuantum final (src `minusPtr` sptr) a b c d e f g h of
              Left err -> pure (Left err)
              Right count -> do
                let clean v = if v == 0x63 then 0 else v
                    w = pack a b (clean c) (clean d) (clean e)
                      (clean f) (clean g) (clean h)
                    write i
                      | i == count = pure ()
                      | otherwise = do
                          poke @Word8 (plusPtr dst i)
                            (fromIntegral (unsafeShiftR w (32 - 8 * i)))
                          write (i + 1)
                write 0
                if final
                  then pure (Right (BS dfp (dst `minusPtr` dptr + count)))
                  else go (plusPtr dst 5) (plusPtr src 8)
    pack a b c d e f g h =
      unsafeShiftL a 35 .|. unsafeShiftL b 30
        .|. unsafeShiftL c 25 .|. unsafeShiftL d 20
        .|. unsafeShiftL e 15 .|. unsafeShiftL f 10
        .|. unsafeShiftL g 5 .|. h
    writeFull dst w = do
      let first = fromIntegral (w `unsafeShiftR` 8) :: Word32
      poke (castPtr dst) (case targetByteOrder of
        LittleEndian -> byteSwap32 first
        BigEndian -> first)
      poke @Word8 (plusPtr dst 4) (fromIntegral w)
{-# INLINE decodeLoop #-}

-- | Validate a decoded quantum before any output is written. Return its
-- payload length. Both pointer and byte-array loops use the same rules.
--
-- @since 1.0
--
decodeQuantum
  :: Bool -> Int
  -> Word64 -> Word64 -> Word64 -> Word64
  -> Word64 -> Word64 -> Word64 -> Word64
  -> Either Text Int
decodeQuantum !final !offset !a !b !c !d !e !f !g !h
  | a == 0x63 = padErr 0
  | b == 0x63 = padErr 1
  | not final, c == 0x63 = padErr 2
  | not final, d == 0x63 = padErr 3
  | not final, e == 0x63 = padErr 4
  | not final, f == 0x63 = padErr 5
  | not final, g == 0x63 = padErr 6
  | not final, h == 0x63 = padErr 7
  | a == 0xff = charErr 0
  | b == 0xff = charErr 1
  | c == 0xff = charErr 2
  | d == 0xff = charErr 3
  | e == 0xff = charErr 4
  | f == 0xff = charErr 5
  | g == 0xff = charErr 6
  | h == 0xff = charErr 7
  | not final = Right 5
  | otherwise = case (c,d,e,f,g,h) of
      (0x63,0x63,0x63,0x63,0x63,0x63) -> canonical b 3 1 1
      (0x63,_,_,_,_,_) -> padErr 3
      (_,0x63,0x63,0x63,0x63,0x63) -> padErr 3
      (_,0x63,_,_,_,_) -> padErr 4
      (_,_,0x63,0x63,0x63,0x63) -> canonical d 15 3 2
      (_,_,0x63,_,_,_) -> padErr 5
      (_,_,_,0x63,0x63,0x63) -> canonical e 1 4 3
      (_,_,_,0x63,_,_) -> padErr 6
      (_,_,_,_,0x63,0x63) -> padErr 6
      (_,_,_,_,0x63,_) -> padErr 7
      (_,_,_,_,_,0x63) -> canonical g 7 6 4
      _ -> Right 5
  where
    failure message i = Left (T.pack (message ++ show (offset + i)))
    padErr = failure "invalid padding at offset: "
    charErr = failure "invalid character at offset: "
    canonical value mask i count
      | value .&. mask == 0 = Right count
      | otherwise = failure "non-canonical encoding at offset: " i
{-# INLINE decodeQuantum #-}

-- | Decode known-valid symbols. Alphabet, size, padding placement and unused
-- bits have already been established by the producer of the typed value.
-- The caller excludes trailing padding from the source length.
--
-- @since 1.0
--
decodeLoopTyped :: Addr# -> Ptr Word8 -> Ptr Word8 -> Int -> IO ()
decodeLoopTyped table dptr sptr slen = do
#ifdef BASE32_SIMD
    consumed <- SIMD.decodeTypedPrefix table sptr slen dptr
#else
    let consumed = 0
#endif
    go (plusPtr dptr (consumed `quot` 8 * 5)) consumed
  where
    look value = w64 (aix (fromIntegral value) table)
    go !dst !i
      | slen - i >= 8 = do
          t <- peekWord64BE (castPtr (plusPtr sptr i))
          let w = pack (look (t `unsafeShiftR` 56)) (look (t `unsafeShiftR` 48))
                (look (t `unsafeShiftR` 40)) (look (t `unsafeShiftR` 32))
                (look (t `unsafeShiftR` 24)) (look (t `unsafeShiftR` 16))
                (look (t `unsafeShiftR` 8)) (look t)
              first = fromIntegral (w `unsafeShiftR` 8) :: Word32
          poke (castPtr dst) (case targetByteOrder of
            LittleEndian -> byteSwap32 first
            BigEndian -> first)
          poke @Word8 (plusPtr dst 4) (fromIntegral w)
          go (plusPtr dst 5) (i + 8)
      | otherwise = tailBytes dst i 0 0 (0 :: Word16)
    tailBytes !dst !i !bits !j !buffer
      | bits >= 8 = do
          let rest = bits - 8
          poke @Word8 (plusPtr dst j) (fromIntegral (buffer `unsafeShiftR` rest))
          tailBytes dst i rest (j + 1) buffer
      | i < slen = do
          symbol <- peek @Word8 (plusPtr sptr i)
          tailBytes dst (i + 1) (bits + 5) j
            (buffer `unsafeShiftL` 5 .|. fromIntegral (aix symbol table))
      | otherwise = pure ()
    pack a b c d e f g h =
      a `unsafeShiftL` 35 .|. b `unsafeShiftL` 30
        .|. c `unsafeShiftL` 25 .|. d `unsafeShiftL` 20
        .|. e `unsafeShiftL` 15 .|. f `unsafeShiftL` 10
        .|. g `unsafeShiftL` 5 .|. h
{-# INLINE decodeLoopTyped #-}
