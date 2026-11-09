{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
-- |
-- Module       : Data.ByteString.Base32.Internal.SIMD
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- Bounded native prefixes. Callers retain pointer buffers with withForeignPtr.
-- Byte arrays are passed directly to unsafe FFI: GHC cannot collect during
-- the call, and the kernels retain no pointers and make no callbacks.
--
module Data.ByteString.Base32.Internal.SIMD
  ( encodePrefix
  , decodePrefix
  , decodeTypedPrefix
  , encodeByteArrayPrefix
  , decodeByteArrayPrefix
  , decodeByteArrayTypedPrefix
  ) where

import Control.Monad.ST (ST)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Data.ByteString.Base32.Internal.Tables (hexDecodeTable, stdDecodeTable)
import Data.ByteString.Base32.Internal.Utils (aix)
import Data.Primitive.ByteArray (ByteArray (..), MutableByteArray (..))
import Data.Word (Word8)
import Foreign.C.Types (CInt (..), CSize (..))
import GHC.Exts (Addr#, ByteArray#, MutableByteArray#)
import GHC.Ptr (Ptr (..))

foreign import ccall unsafe "hs_base32_simd_encode"
  c_encode :: Ptr Word8 -> CSize -> Ptr Word8 -> CInt -> IO CSize
foreign import ccall unsafe "hs_base32_simd_decode"
  c_decode :: Ptr Word8 -> CSize -> Ptr Word8 -> CInt -> IO CSize
foreign import ccall unsafe "hs_base32_simd_encode"
  c_encodeArray :: ByteArray# -> CSize -> MutableByteArray# s -> CInt -> IO CSize
foreign import ccall unsafe "hs_base32_simd_decode"
  c_decodeArray :: ByteArray# -> CSize -> MutableByteArray# s -> CInt -> IO CSize

foreign import ccall unsafe "hs_base32_simd_decode_typed"
  c_decodeTyped :: Ptr Word8 -> CSize -> Ptr Word8 -> CInt -> IO CSize
foreign import ccall unsafe "hs_base32_simd_decode_typed"
  c_decodeArrayTyped :: ByteArray# -> CSize -> MutableByteArray# s -> CInt -> IO CSize

-- | Encode complete quanta. Destination capacity must be at least len/5*8.
encodePrefix :: Addr# -> Ptr Word8 -> Int -> Ptr Word8 -> IO Int
encodePrefix alphabet src len dst
  | len < 80 = pure 0
  | Just hex <- encodingAlphabet alphabet =
      fromIntegral <$> c_encode src (fromIntegral len) dst hex
  | otherwise = pure 0
{-# INLINE encodePrefix #-}

-- | Decode valid complete quanta; leave padding/invalid blocks for scalar code.
-- Destination capacity must be at least len/8*5.
decodePrefix :: Addr# -> Ptr Word8 -> Int -> Ptr Word8 -> IO Int
decodePrefix table src len dst
  | len < 64 = pure 0
  | Just hex <- decodingAlphabet table =
      fromIntegral <$> c_decode src (fromIntegral len) dst hex
  | otherwise = pure 0
{-# INLINE decodePrefix #-}

-- | Decode known-valid symbols without validation; excludes trailing padding.
-- Destination capacity must be at least len/8*5.
decodeTypedPrefix :: Addr# -> Ptr Word8 -> Int -> Ptr Word8 -> IO Int
decodeTypedPrefix table src len dst
  | len < 64 = pure 0
  | Just hex <- decodingAlphabet table =
      fromIntegral <$> c_decodeTyped src (fromIntegral len) dst hex
  | otherwise = pure 0
{-# INLINE decodeTypedPrefix #-}

-- | Direct unpinned-array encoding into a distinct mutable output buffer.
encodeByteArrayPrefix ::
  Addr# -> ByteArray -> Int -> MutableByteArray s -> ST s Int
encodeByteArrayPrefix alphabet (ByteArray src) len (MutableByteArray dst)
  | len < 40 = pure 0
  | Just hex <- encodingAlphabet alphabet =
      unsafeIOToST $
        fromIntegral <$> c_encodeArray src (fromIntegral len) dst hex
  | otherwise = pure 0
{-# INLINE encodeByteArrayPrefix #-}

-- | Direct unpinned-array decoding into a distinct mutable output buffer.
decodeByteArrayPrefix ::
  Addr# -> ByteArray -> Int -> MutableByteArray s -> ST s Int
decodeByteArrayPrefix table (ByteArray src) len (MutableByteArray dst)
  | len < 64 = pure 0
  | Just hex <- decodingAlphabet table =
      unsafeIOToST $
        fromIntegral <$> c_decodeArray src (fromIntegral len) dst hex
  | otherwise = pure 0
{-# INLINE decodeByteArrayPrefix #-}

-- | Unchecked array decoding of known-valid symbols, excluding padding.
decodeByteArrayTypedPrefix ::
  Addr# -> ByteArray -> Int -> MutableByteArray s -> ST s Int
decodeByteArrayTypedPrefix table (ByteArray src) len (MutableByteArray dst)
  | len < 64 = pure 0
  | Just hex <- decodingAlphabet table =
      unsafeIOToST $
        fromIntegral <$> c_decodeArrayTyped src (fromIntegral len) dst hex
  | otherwise = pure 0
{-# INLINE decodeByteArrayTypedPrefix #-}

-- Arbitrary alphabets are part of the exposed low-level API. Only exact
-- built-in alphabets can use these kernels; other alphabets stay scalar.
encodingAlphabet :: Addr# -> Maybe CInt
encodingAlphabet alphabet
  | matches "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"# = Just 0
  | matches "0123456789ABCDEFGHIJKLMNOPQRSTUV"# = Just 1
  | otherwise = Nothing
 where
  matches expected = go 0
   where
    go i
      | i == 32 = True
      | aix i alphabet == aix i expected = go (i + 1)
      | otherwise = False
{-# INLINE encodingAlphabet #-}

decodingAlphabet :: Addr# -> Maybe CInt
decodingAlphabet table
  | Ptr table == stdDecodeTable = Just 0
  | Ptr table == hexDecodeTable = Just 1
  | otherwise = Nothing
{-# INLINE decodingAlphabet #-}
