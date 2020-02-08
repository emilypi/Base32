{-# LANGUAGE BangPatterns #-}
module Data.ByteString.Base32.Internal.Head
( encodeBase32_

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

encodeBase32_ :: Ptr Word8 -> ByteString -> ByteString
encodeBase32_ lut (PS !sfp !soff !slen) =
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

-- encodeBase32NoPad_ :: Ptr Word8 -> ByteString -> ByteString
-- encodeBase32NoPad_ (Ptr !alphabet) (PS !sfp !soff !slen)
--     = unsafeDupablePerformIO $ do
--       !dfp <- mallocPlainForeignPtrBytes dlen
--       withForeignPtr dfp $ \dptr ->
--         withForeignPtr sfp $ \sptr -> do
--           let !end = plusPtr sptr (soff + slen)
--           innerLoopNopad
--             alphabet
--             dptr
--             (plusPtr sptr soff)
--             end
--             (loopTailNopad alphabet dfp end)
--   where
--     !dlen = 8 * ((slen + 4) `div` 5)
