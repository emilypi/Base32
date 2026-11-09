{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Criterion.Main
import Data.Base32.Types (Alphabet(..), assertBase32, extractBase32)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base32 as B32
import qualified Data.ByteString.Base32.Hex as H32
import qualified Data.ByteString.Short as S
import qualified Data.ByteString.Short.Base32 as S32
import qualified Data.ByteString.Short.Base32.Hex as SH32
import qualified Data.Text.Short as T
import qualified Data.Text.Short.Encoding.Base32 as T32
import qualified "memory" Data.ByteArray.Encoding as Mem

main :: IO ()
main =
  defaultMain
    [ env (pure (B.pack (take n (cycle [0 .. 255])))) $ \input ->
      bgroup
        (show n)
        [ bench "encode/strict" $ nf B32.encodeBase32' input
        , bench "encode/hex" $ nf H32.encodeBase32' input
        , bench "encode/memory" $ nf memoryEncode input
        , env (pure (S.toShort input)) $ \short ->
            bgroup
              "short"
              [ bench "encode" $ nf S32.encodeBase32' short
              , bench "encode-hex" $ nf SH32.encodeBase32' short
              ]
        , env (pure (extractBase32 (B32.encodeBase32' input))) $ \encoded ->
            bgroup
              "decode"
              [ bench "strict" $ nf B32.decodeBase32Untyped encoded
              , env (pure (assertBase32 @'StdPadded encoded)) $ \typed ->
                  bench "strict-typed" $ nf B32.decodeBase32 typed
              , bench "parse" $ nf B32.parseBase32Padded encoded
              , env (pure (S.toShort encoded)) $ \short ->
                  bgroup "short"
                    [ bench "checked" $ nf S32.decodeBase32Untyped short
                    , env (pure (assertBase32 @'StdPadded short)) $ \typed ->
                        bench "typed" $ nf S32.decodeBase32 typed
                    ]
              ]
        , env (pure (T.fromText "Base32 benchmark λ")) $ \shortText ->
            bench "short-text" $ nf T32.encodeBase32 shortText
        ]
    | n <- [25, 39, 40, 41, 79, 80, 81, 1000, 1000000]
    ]
 where
  memoryEncode :: B.ByteString -> B.ByteString
  memoryEncode = Mem.convertToBase Mem.Base32
