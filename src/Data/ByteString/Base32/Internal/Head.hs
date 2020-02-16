{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.ByteString.Base32.Internal.Head
( encodeBase32_
, encodeBase32NoPad_
, decodeBase32_
) where


import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Internal
import Data.ByteString.Base32.Internal.Loop
import Data.ByteString.Base32.Internal.Tail
import Data.Text (Text)

import Foreign.Ptr
import Foreign.ForeignPtr

import GHC.Exts
import GHC.ForeignPtr
import GHC.Word

import System.IO.Unsafe


-- | Head of the base32 encoding loop - marshal data, assemble loops
--
encodeBase32_ :: Addr# -> ByteString -> ByteString
encodeBase32_ !lut (PS !sfp !soff !slen) =
    unsafeCreate dlen $ \dptr ->
      withForeignPtr sfp $ \sptr -> do
        let !end = plusPtr sptr (soff + slen)
        innerLoop
          lut
          (castPtr dptr)
          (plusPtr sptr soff)
          end
          (loopTail lut end)
  where
    !dlen = 8 * ((slen + 4) `div` 5)

-- | Head of the unpadded base32 encoding loop - marshal data, assemble loops
--
encodeBase32NoPad_ :: Addr# -> ByteString -> ByteString
encodeBase32NoPad_ !lut (PS !sfp !soff !slen)
    = unsafeDupablePerformIO $ do
      !dfp <- mallocPlainForeignPtrBytes dlen
      withForeignPtr dfp $ \dptr ->
        withForeignPtr sfp $ \sptr -> do
          let !end = plusPtr sptr (soff + slen)
          innerLoopNoPad
            lut
            (castPtr dptr)
            (plusPtr sptr soff)
            end
            (loopTailNoPad lut dfp end)
  where
    !dlen = 8 * ((slen + 4) `div` 5)

-- | Head of the base32 decoding loop - marshal data, assemble loops
--
decodeBase32_ :: Bool -> ForeignPtr Word8 -> ByteString -> Either Text ByteString
decodeBase32_ !pad !alphabet bs@(PS _ _ !l)
    | l == 0 = Right ""
    | r /= 0, pad =
      if
        | r == 2 -> go (BS.append bs (BS.replicate 6 0x3d))
        | r == 4 -> go (BS.append bs (BS.replicate 4 0x3d))
        | r == 5 -> go (BS.append bs (BS.replicate 3 0x3d))
        | r == 7 -> go (BS.append bs (BS.replicate 1 0x3d))
        | otherwise -> Left "invalid bytestring size"
    | r /= 0, not pad = Left "invalid padding"
    | otherwise = go bs
  where
    (!q, !r) = l `divMod` 8
    !dlen = q * 8

    go (PS !sfp !soff !slen) = unsafeDupablePerformIO $ do
      !dfp <- mallocPlainForeignPtrBytes dlen
      withForeignPtr dfp $ \dptr ->
        withForeignPtr alphabet $ \(Ptr lut) ->
        withForeignPtr sfp $ \sptr -> do
          let !end = plusPtr sptr (soff + slen)
          decodeLoop
            lut
            (castPtr dptr)
            (plusPtr sptr soff)
            end
            (decodeTail lut dfp end)
