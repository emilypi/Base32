{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
-- |
-- Module       : Data.ByteString.Base32.Internal.Short
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- Direct byte-array codecs; no conversion through strict ByteString.
--
module Data.ByteString.Base32.Internal.Short
  ( encodeShort
  , decodeShort
  , decodeShortTyped
  ) where

import Control.Monad.ST (runST)
import Data.Base32.Types (Base32, Padding(..), extractBase32)
import Data.Bits
import Data.ByteString.Base32.Internal.Loop (decodeQuantum)
import Data.ByteString.Base32.Internal.Utils (aix, encodedLength)
import Data.ByteString.Short.Internal (ShortByteString(..))
import qualified Data.ByteString.Short as SBS
import Data.Primitive.ByteArray
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word
import GHC.Exts (Addr#)
import GHC.ByteOrder
import GHC.Ptr (Ptr(..))

#ifdef BASE32_SIMD
import qualified Data.ByteString.Base32.Internal.SIMD as SIMD
#endif

-- | Encode with the supplied 32-character ASCII alphabet.
encodeShort :: Addr# -> Bool -> ShortByteString -> ShortByteString
encodeShort alphabet padded input@(SBS source) = runST $ do
    dst <- newByteArray capacity
#ifdef BASE32_SIMD
    consumed <- SIMD.encodeByteArrayPrefix alphabet src len dst
#else
    let consumed = 0
#endif
    let emit !i !j !bits !buffer
          | bits >= 5 = do
              let next = bits - 5
              writeByteArray dst j
                (aix (fromIntegral (buffer `unsafeShiftR` next) .&. 31) alphabet)
              emit i (j + 1) next buffer
          | i < len = emit (i + 1) j (bits + 8)
              ((buffer `unsafeShiftL` 8) .|. fromIntegral (byte i))
          | bits > 0 = do
              writeByteArray dst j
                (aix (fromIntegral (buffer `unsafeShiftL` (5 - bits)) .&. 31)
                  alphabet)
              finish (j + 1)
          | otherwise = finish j
        finish !j
          | padded && j < capacity = do
              writeByteArray dst j (61 :: Word8)
              finish (j + 1)
          | otherwise = do
              shrinkMutableByteArray dst j
              ByteArray result <- unsafeFreezeByteArray dst
              pure (SBS result)
    let full !i !j
          | i + 5 > len = emit i j 0 (0 :: Word16)
          | otherwise = do
              let t = fromIntegral (byte i) `unsafeShiftL` 32
                    .|. fromIntegral (byte (i + 1)) `unsafeShiftL` 24
                    .|. fromIntegral (byte (i + 2)) `unsafeShiftL` 16
                    .|. fromIntegral (byte (i + 3)) `unsafeShiftL` 8
                    .|. fromIntegral (byte (i + 4)) :: Word64
                  at shiftCount = fromIntegral
                    (aix (fromIntegral (t `unsafeShiftR` shiftCount) .&. 31) alphabet)
                  w = at 35 .|. at 30 `unsafeShiftL` 8
                    .|. at 25 `unsafeShiftL` 16 .|. at 20 `unsafeShiftL` 24
                    .|. at 15 `unsafeShiftL` 32 .|. at 10 `unsafeShiftL` 40
                    .|. at 5 `unsafeShiftL` 48 .|. at 0 `unsafeShiftL` 56 :: Word64
              writeByteArray dst (j `quot` 8) (case targetByteOrder of
                LittleEndian -> w
                BigEndian -> byteSwap64 w)
              full (i + 5) (j + 8)
    full consumed (consumed `quot` 5 * 8)
  where
    src = ByteArray source
    len = SBS.length input
    capacity = encodedLength len
    byte i = indexByteArray src i :: Word8
{-# INLINE encodeShort #-}

-- | Decode with an ASCII lookup table and explicit padding policy.
decodeShort
  :: Ptr Word8 -> Padding -> ShortByteString -> Either Text ShortByteString
decodeShort (Ptr table) policy input@(SBS source)
  | len == 0 = Right input
  | remainder == 1 || remainder == 3 || remainder == 6 =
      Left (Text.pack "Base32-encoded bytestring has invalid size")
  | policy == RequirePadding && remainder /= 0 =
      Left (Text.pack "Base32-encoded bytestring requires padding")
  | any isPad [len - checkPads .. len - 1] =
      Left (Text.pack "Base32-encoded bytestring has invalid padding")
  | otherwise = runST $ do
      dst <- newByteArray capacity
#ifdef BASE32_SIMD
      consumed <- SIMD.decodeByteArrayPrefix table
        src (max 0 (len - 8)) dst
#else
      let consumed = 0
#endif
      let go !i !j
            | i + 8 >= len = final i j
            | otherwise = do
                let word = indexByteArray src (i `quot` 8) :: Word64
                    t = case targetByteOrder of
                      LittleEndian -> byteSwap64 word
                      BigEndian -> word
                    from shiftCount = fromIntegral
                      (aix (fromIntegral (t `unsafeShiftR` shiftCount)) table) :: Word64
                    !a = from 56
                    !b = from 48
                    !c = from 40
                    !d = from 32
                    !e = from 24
                    !f = from 16
                    !g = from 8
                    !h = from 0
                if (a .|. b .|. c .|. d .|. e .|. f .|. g .|. h) < 32
                  then do
                    writeFull j (pack a b c d e f g h)
                    go (i + 8) (j + 5)
                  else finishQuantum False i j a b c d e f g h
          final !i !j
            | i >= len = freeze j
            | otherwise = finishQuantum True i j
                (look i) (look (i + 1)) (look (i + 2)) (look (i + 3))
                (look (i + 4)) (look (i + 5)) (look (i + 6)) (look (i + 7))
          finishQuantum lastQuantum i j a b c d e f g h =
            case decodeQuantum lastQuantum i a b c d e f g h of
              Left err -> pure (Left err)
              Right count -> do
                let clean v = if v == 0x63 then 0 else v
                    w = pack a b (clean c) (clean d) (clean e)
                      (clean f) (clean g) (clean h)
                    write !k
                      | k == count = pure ()
                      | otherwise = do
                          writeByteArray dst (j + k)
                            (fromIntegral (w `unsafeShiftR` (32 - 8 * k)) :: Word8)
                          write (k + 1)
                write 0
                if lastQuantum then freeze (j + count) else go (i + 8) (j + count)
          pack a b c d e f g h =
            unsafeShiftL a 35 .|. unsafeShiftL b 30
              .|. unsafeShiftL c 25 .|. unsafeShiftL d 20
              .|. unsafeShiftL e 15 .|. unsafeShiftL f 10
              .|. unsafeShiftL g 5 .|. h
          writeFull j w = do
            writeByteArray dst j (fromIntegral (w `unsafeShiftR` 32) :: Word8)
            writeByteArray dst (j + 1) (fromIntegral (w `unsafeShiftR` 24) :: Word8)
            writeByteArray dst (j + 2) (fromIntegral (w `unsafeShiftR` 16) :: Word8)
            writeByteArray dst (j + 3) (fromIntegral (w `unsafeShiftR` 8) :: Word8)
            writeByteArray dst (j + 4) (fromIntegral w :: Word8)
          freeze n = do
            shrinkMutableByteArray dst n
            ByteArray result <- unsafeFreezeByteArray dst
            pure (Right (SBS result))
      go consumed (consumed `quot` 8 * 5)
  where
    src = ByteArray source
    len = SBS.length input
    remainder = len `rem` 8
    capacity = (len `quot` 8) * 5 + (remainder * 5 `quot` 8)
    isPad i = (indexByteArray src i :: Word8) == 61
    checkPads
      | policy == ForbidPadding && remainder /= 2 = 1
      | policy /= OptionalPadding = 0
      | remainder == 4 = 2
      | remainder == 5 = 3
      | remainder == 7 = 5
      | otherwise = 0
    look i
      | i >= len = 0x63
      | otherwise = fromIntegral (aix (indexByteArray src i) table) :: Word64
{-# INLINE decodeShort #-}

-- | Decode established provenance directly from a byte array, without
-- alphabet, size, padding-policy or canonical-bit validation.
decodeShortTyped
  :: Ptr Word8 -> Base32 alphabet ShortByteString -> ShortByteString
decodeShortTyped (Ptr table) encoded = runST $ do
    dst <- newByteArray capacity
#ifdef BASE32_SIMD
    consumed <- SIMD.decodeByteArrayTypedPrefix table src len dst
#else
    let consumed = 0
#endif
    let go !i !j
          | len - i >= 8 = do
              let word = indexByteArray src (i `quot` 8) :: Word64
                  t = case targetByteOrder of
                    LittleEndian -> byteSwap64 word
                    BigEndian -> word
                  at n = fromIntegral
                    (aix (fromIntegral (t `unsafeShiftR` n)) table) :: Word64
                  w = at 56 `unsafeShiftL` 35 .|. at 48 `unsafeShiftL` 30
                    .|. at 40 `unsafeShiftL` 25 .|. at 32 `unsafeShiftL` 20
                    .|. at 24 `unsafeShiftL` 15 .|. at 16 `unsafeShiftL` 10
                    .|. at 8 `unsafeShiftL` 5 .|. at 0
              writeByteArray dst j (fromIntegral (w `unsafeShiftR` 32) :: Word8)
              writeByteArray dst (j + 1) (fromIntegral (w `unsafeShiftR` 24) :: Word8)
              writeByteArray dst (j + 2) (fromIntegral (w `unsafeShiftR` 16) :: Word8)
              writeByteArray dst (j + 3) (fromIntegral (w `unsafeShiftR` 8) :: Word8)
              writeByteArray dst (j + 4) (fromIntegral w :: Word8)
              go (i + 8) (j + 5)
          | otherwise = tailBytes i j 0 (0 :: Word16)
        tailBytes !i !j !bits !buffer
          | bits >= 8 = do
              let rest = bits - 8
              writeByteArray dst j (fromIntegral (buffer `unsafeShiftR` rest) :: Word8)
              tailBytes i (j + 1) rest buffer
          | i < len = tailBytes (i + 1) j (bits + 5)
              (buffer `unsafeShiftL` 5 .|. fromIntegral (aix (byte i) table))
          | otherwise = pure ()
    go consumed (consumed `quot` 8 * 5)
    ByteArray result <- unsafeFreezeByteArray dst
    pure (SBS result)
  where
    !input@(SBS source) = extractBase32 encoded
    src = ByteArray source
    byte i = indexByteArray src i :: Word8
    payloadEnd n
      | n > 0 && byte (n - 1) == 61 = payloadEnd (n - 1)
      | otherwise = n
    len = payloadEnd (SBS.length input)
    capacity = (len `quot` 8) * 5 + (len `rem` 8) * 5 `quot` 8
{-# INLINE decodeShortTyped #-}
