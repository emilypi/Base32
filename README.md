# Base32

![Build Status](https://github.com/emilypi/base32/workflows/ci/badge.svg)
[![Hackage](https://img.shields.io/hackage/v/base32.svg)](https://hackage.haskell.org/package/base32)

RFC 4648 standard and extended-hex Base32 encoding, with padded and unpadded
variants for strict, lazy, and short `ByteString` and `Text`.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Base32.Types
import qualified Data.ByteString.Base32 as B32

encoded = B32.encodeBase32' "Sun"
-- encoded :: Base32 'Std ByteString
-- extractBase32 encoded == "KN2W4==="
-- B32.decodeBase32 encoded == "Sun"

parsed = B32.parseBase32 "KN2W4"
-- Right "KN2W4"; the original encoded representation is retained

raw = B32.decodeBase32Untyped "KN2W4"
-- Right "Sun"
```

Use `.Hex` modules for the extended-hex alphabet. Its values carry the `'Hex`
marker, preventing accidental mixing with standard Base32. See
[MIGRATION-1.0.md](MIGRATION-1.0.md) for the changes from 0.4.

## Validation

`parseBase32` accepts padded or unpadded input. `parseBase32Padded` and
`parseBase32Unpadded` enforce a specific padding policy. All three check the
alphabet, input length, padding placement, and zero unused trailing bits.
They accept upper- and lowercase letters, preserve the original case and
representation, and accept empty input. Whitespace is rejected.

The wrapper identifies the alphabet; it does not record the padding policy or
prove that decoded bytes form UTF-8. Use matching padded/unpadded operations.
Checked Text decoders return `Left` for invalid UTF-8; `decodeBase32With` and
its variants let you choose a character conversion. For arbitrary binary
payloads, use ByteString decoders.

`assertBase32`, record updates, and the mapping instances
can bypass validation. Typed decoding requires a truthful encoded value and,
for Text, valid UTF-8 payload bytes. Use parsers and checked decoders for
external input.

Lazy encoding preserves streaming through rechunking. Checked lazy decoding
and parsing validate a complete finite input; they are not streaming decoders.

## Optional SIMD

```sh
cabal build -fsimd
cabal test -fsimd
```

The `simd` flag defaults to **off**. It enables bundled NEON kernels on AArch64
and runtime-dispatched SSSE3/AVX2 kernels on x86-64, with scalar fallback on
other targets. Small inputs and final quanta use Haskell scalar code. No
external native library is required.

Strict and short values use SIMD directly. ShortByteString and ShortText use
byte arrays without an intermediate strict ByteString copy or pinning. Lazy
variants reach the same kernels through their strict chunks or checked input.

## Development

```sh
cabal test -f-simd
cabal test -fsimd --ghc-options='-fcheck-prim-bounds -fno-ignore-asserts'
cabal bench -fsimd
cabal haddock -fsimd
cabal check
```

CI tests both flag configurations across GHC 9.6–9.14, builds from the source
archive, and runs native sanitizer and guard-page tests on x86-64 and ARM64.
The benchmarks cover strict/short codecs, both alphabets, parsing, and sizes
around the dispatch thresholds.

The companion [base32-lens](https://hackage.haskell.org/package/base32-lens)
package needs an update for the 1.0 API before it can be used with this release.
