{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
module Data.ByteString.Base32.Internal.Head
( encodeBase32_
, encodeBase32NoPad_
) where


import Data.ByteString (ByteString)
import Data.ByteString.Internal
import Data.ByteString.Base32.Internal.Loop
import Data.ByteString.Base32.Internal.Tail

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable

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
encodeBase32NoPad_ !alphabet (PS !sfp !soff !slen)
    = unsafeDupablePerformIO $ do
      !dfp <- mallocPlainForeignPtrBytes dlen
      withForeignPtr dfp $ \dptr ->
        withForeignPtr sfp $ \sptr -> do
          let !end = plusPtr sptr (soff + slen)
          innerLoopNoPad
            alphabet
            dptr
            (plusPtr sptr soff)
            end
            (loopTailNoPad alphabet dfp end)
  where
    !dlen = 8 * ((slen + 4) `div` 5)
