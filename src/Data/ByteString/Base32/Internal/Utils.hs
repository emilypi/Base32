{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
module Data.ByteString.Base32.Internal.Utils
( aix
, padCeilN
, peekWord32BE
, peekWord64BE
, w32
, w64
, w64_32
, writeNPlainForeignPtrBytes
) where


import Data.Bits

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable

import GHC.ByteOrder
import GHC.Exts
import GHC.ForeignPtr
import GHC.Word

import System.IO.Unsafe


-- | Read 'Word8' index off alphabet addr
--
aix :: Word8 -> Addr# -> Word8
aix (W8# i) alpha = W8# (indexWord8OffAddr# alpha (word2Int# i))
{-# INLINE aix #-}

w32 :: Word8 -> Word32
w32 = fromIntegral
{-# INLINE w32 #-}

w64_32 :: Word32 -> Word64
w64_32 = fromIntegral
{-# INLINE w64_32 #-}

w64 :: Word8 -> Word64
w64 = fromIntegral
{-# INLINE w64 #-}

padCeilN :: Int -> Int -> Int
padCeilN !n !x
    | r == 0 = x
    | otherwise = (x - r) + n
  where
    r = x .&. (n - 1)
{-# INLINE padCeilN #-}

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
{-# INLINE writeNPlainForeignPtrBytes #-}

peekWord32BE :: Ptr Word32 -> IO Word32
peekWord32BE p = case targetByteOrder of
  LittleEndian -> byteSwap32 <$> peek p
  BigEndian    -> peek p
{-# inline peekWord32BE #-}

peekWord64BE :: Ptr Word64 -> IO Word64
peekWord64BE p = case targetByteOrder of
  LittleEndian -> byteSwap64 <$> peek p
  BigEndian    -> peek p
{-# inline peekWord64BE #-}
