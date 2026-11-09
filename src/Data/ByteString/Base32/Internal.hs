{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module       : Data.ByteString.Base32.Internal
-- Copyright 	: (c) 2020-2026 Emily Pillmore
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
( encodeBase32_
, encodeBase32NoPad_
, decodeBase32_
, decodeBase32Typed_
, validateLastNPads
) where


import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base32.Internal.Head
import Data.Text (Text)

import System.IO.Unsafe

-- -------------------------------------------------------------------------- --
-- Validating Base32

-- | Reject '=' in the final @n@ bytes before running the decoder.
-- The caller selects @n@ from the input length and padding policy, preventing
-- existing padding from being accepted when missing padding is added.
validateLastNPads
    :: Int
    -> ByteString
    -> IO (Either Text ByteString)
    -> Either Text ByteString
validateLastNPads n bs io
    | BS.elem 0x3d (BS.drop (BS.length bs - n) bs) =
        Left "Base32-encoded bytestring has invalid padding"
    | otherwise = unsafeDupablePerformIO io
{-# INLINE validateLastNPads #-}
