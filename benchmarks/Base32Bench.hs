{-# LANGUAGE CPP #-}
-- |
-- Module       : Main
-- Copyright    : (c) 2020-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- Build once with -f-simd and once with -fsimd to compare the same cases.
-- Sizes describe raw input bytes; sliced inputs also apply to decode buffers.
--
module Main (main) where

import Criterion.Main
import Data.Base32.Types (assertBase32, extractBase32)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base32 as B32
import qualified Data.ByteString.Base32.Hex as H32
import qualified Data.ByteString.Short as S
import qualified Data.ByteString.Short.Base32 as S32
import qualified Data.ByteString.Short.Base32.Hex as SH32
import qualified Data.Text.Short as T
import qualified Data.Text.Short.Encoding.Base32 as T32

main :: IO ()
main = defaultMain
  [ bgroup implementation
      [ bgroup (show size)
          [ bgroup "strict"
              [ env (inputs id size offset) $
                \ ~(raw, std, stdUnpadded, hex, hexUnpadded) ->
                  bgroup ("offset-" ++ show offset)
                    [ bgroup "standard-padded"
                        [ bench "encode" $ nf B32.encodeBase32' raw
                        , bench "decode-typed" $
                            nf (B32.decodeBase32Padded . assertBase32) std
                        , bench "decode-untyped" $ nf B32.decodeBase32PaddedUntyped std
                        , bench "parse" $ nf B32.parseBase32Padded std
                        ]
                    , bgroup "standard-unpadded"
                        [ bench "encode" $ nf B32.encodeBase32Unpadded' raw
                        , bench "decode-typed" $
                            nf (B32.decodeBase32Unpadded . assertBase32) stdUnpadded
                        , bench "decode-untyped" $
                            nf B32.decodeBase32UnpaddedUntyped stdUnpadded
                        , bench "parse" $ nf B32.parseBase32Unpadded stdUnpadded
                        ]
                    , bgroup "hex-padded"
                        [ bench "encode" $ nf H32.encodeBase32' raw
                        , bench "decode-typed" $
                            nf (H32.decodeBase32Padded . assertBase32) hex
                        , bench "decode-untyped" $ nf H32.decodeBase32PaddedUntyped hex
                        , bench "parse" $ nf H32.parseBase32Padded hex
                        ]
                    , bgroup "hex-unpadded"
                        [ bench "encode" $ nf H32.encodeBase32Unpadded' raw
                        , bench "decode-typed" $
                            nf (H32.decodeBase32Unpadded . assertBase32) hexUnpadded
                        , bench "decode-untyped" $
                            nf H32.decodeBase32UnpaddedUntyped hexUnpadded
                        , bench "parse" $ nf H32.parseBase32Unpadded hexUnpadded
                        ]
                    ]
              | offset <- [0, 1]
              ]
          , env (inputs S.toShort size 0) $
            \ ~(raw, std, stdUnpadded, hex, hexUnpadded) ->
              bgroup "short"
                [ bgroup "standard-padded"
                    [ bench "encode" $ nf S32.encodeBase32' raw
                    , bench "decode-typed" $
                        nf (S32.decodeBase32Padded . assertBase32) std
                    , bench "decode-untyped" $ nf S32.decodeBase32PaddedUntyped std
                    , bench "parse" $ nf S32.parseBase32Padded std
                    ]
                , bgroup "standard-unpadded"
                    [ bench "encode" $ nf S32.encodeBase32Unpadded' raw
                    , bench "decode-typed" $
                        nf (S32.decodeBase32Unpadded . assertBase32) stdUnpadded
                    , bench "decode-untyped" $
                        nf S32.decodeBase32UnpaddedUntyped stdUnpadded
                    , bench "parse" $ nf S32.parseBase32Unpadded stdUnpadded
                    ]
                , bgroup "hex-padded"
                    [ bench "encode" $ nf SH32.encodeBase32' raw
                    , bench "decode-typed" $
                        nf (SH32.decodeBase32Padded . assertBase32) hex
                    , bench "decode-untyped" $ nf SH32.decodeBase32PaddedUntyped hex
                    , bench "parse" $ nf SH32.parseBase32Padded hex
                    ]
                , bgroup "hex-unpadded"
                    [ bench "encode" $ nf SH32.encodeBase32Unpadded' raw
                    , bench "decode-typed" $
                        nf (SH32.decodeBase32Unpadded . assertBase32) hexUnpadded
                    , bench "decode-untyped" $
                        nf SH32.decodeBase32UnpaddedUntyped hexUnpadded
                    , bench "parse" $ nf SH32.parseBase32Unpadded hexUnpadded
                    ]
                ]
          ]
      | size <- [25, 39, 40, 41, 79, 80, 81, 4096, 1048576]
      ]
  , env (pure (T.pack "Base32 benchmark λ")) $ \shortText ->
      bgroup implementation [bench "short-text" $ nf T32.encodeBase32 shortText]
  ]

implementation :: String
#ifdef BASE32_SIMD
implementation = "simd"
#else
implementation = "scalar"
#endif

-- Criterion forces this environment before timing. Both builds use the same
-- deterministic data; encoding, slicing, and conversion are excluded from timing.
inputs :: (B.ByteString -> a) -> Int -> Int -> IO (a, a, a, a, a)
inputs convert size offset = pure
  ( prepared raw
  , prepared (extractBase32 (B32.encodeBase32' raw))
  , prepared (extractBase32 (B32.encodeBase32Unpadded' raw))
  , prepared (extractBase32 (H32.encodeBase32' raw))
  , prepared (extractBase32 (H32.encodeBase32Unpadded' raw))
  )
  where
    raw = B.pack (take size (cycle [0 .. 255]))
    prepared bytes = convert . B.take (B.length bytes) . B.drop offset $
      B.replicate offset 0xe5 <> bytes <> B.replicate 32 0xff
