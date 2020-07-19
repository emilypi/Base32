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
( validateBase32
, validateLastNPads
) where


import qualified Data.ByteString as BS
import Data.ByteString.Internal
import Data.Text (Text)

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import GHC.Word

import System.IO.Unsafe

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

-- | This function checks that the last char of a bytestring is '='
-- and, if true, fails with a message or completes some io action.
--
-- This is necessary to check when decoding permissively (i.e. filling in padding chars).
-- Consider the following 8 cases of a string of length l:
--
-- l = 0 mod 8: No pad chars are added, since the input is assumed to be good.
-- l = 1 mod 8: Never an admissible length in base32
-- l = 2 mod 8: 6 padding chars are added. If padding chars are present in the string, they will fail as to decode as final quanta
-- l = 3 mod 8: Never an admissible length in base32
-- l = 4 mod 8: 4 padding chars are added. If 2 padding chars are present in the string this can be "completed" in the sense that
--              it now acts like a string `l == 2 mod 8` with 6 padding chars, and could potentially form corrupted data.
-- l = 5 mod 8: 3 padding chars are added. If 3 padding chars are present in the string, this could form corrupted data like in the
--              previous case.
-- l = 6 mod 8: Never an admissible length in base32
-- l = 7 mod 8: 1 padding char is added. If 5 padding chars are present in the string, this could form corrupted data like the
--              previous cases.
--
-- Hence, permissive decodes should only fill in padding chars when it makes sense to add them. That is,
-- if an input is degenerate, it should never succeed when we add padding chars. We need the following invariant to hold:
--
-- @
--   B32.decodeUnpadded <|> B32.decodePadded ~ B32.decode
-- @
--
validateLastNPads
    :: Int
    -> ByteString
    -> IO (Either Text ByteString)
    -> Either Text ByteString
validateLastNPads !n (PS !fp !o !l) io
    | not valid = Left "Base64-encoded bytestring has invalid padding"
    | otherwise = unsafeDupablePerformIO io
  where
    !lo = l + o
    valid = accursedUnutterablePerformIO $ withForeignPtr fp $ \p -> do
      let end = plusPtr p lo

      let go :: Ptr Word8 -> IO Bool
          go !q
            | q == end = return True
            | otherwise = do
              a <- peek q
              if a == 0x3d then return False else go (plusPtr q 1)

      go (plusPtr p (lo - n))
{-# INLINE validateLastNPads #-}
