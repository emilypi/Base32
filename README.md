# Base32

![Build Status](https://github.com/emilypi/base32/workflows/ci/badge.svg)
[![Hackage](https://img.shields.io/hackage/v/base32.svg)](https://hackage.haskell.org/package/base32)

RFC 4648 standard and extended-hex Base32 encoding, with padded and unpadded
variants for strict, lazy, and short `ByteString` and `Text`.

```haskell
import Data.Base32.Types
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base32 as B32

encoded = B32.encodeBase32' (BS.pack "Sun")
-- encoded :: Base32 'StdPadded ByteString
-- extractBase32 encoded == BS.pack "KN2W4==="
-- B32.decodeBase32 encoded == BS.pack "Sun"

parsed = B32.parseBase32Unpadded (BS.pack "KN2W4")
-- Right "KN2W4"; the original encoded representation is retained

raw = B32.decodeBase32Untyped (BS.pack "KN2W4")
-- Right "Sun"
```

Use `.Hex` modules for the extended-hex alphabet. Values carry `'StdPadded`,
`'StdUnpadded`, `'HexPadded`, or `'HexUnpadded`, preventing accidental mixing of
alphabets or padding-specific APIs. See
[MIGRATION-1.0.md](MIGRATION-1.0.md) for the changes from 0.4.

## Validation

`parseBase32Padded` and `parseBase32Unpadded` establish the corresponding padding
tag. Both check the alphabet, input length, padding placement, and zero unused
trailing bits.
They accept upper- and lowercase letters, preserve the original case and
representation, and accept empty input. Whitespace is rejected.

Typed decoders use dedicated unchecked loops: provenance establishes validity,
so decoding does not repeat alphabet, length, padding, or canonical-bit checks.
`decodeBase32` uses the `StdAlphabet` or `HexAlphabet` constraint to accept both
padding forms of its alphabet. `decodeBase32Padded` and `decodeBase32Unpadded`
require their exact tags; calling them with the wrong padding form is a type error.

The wrapper identifies the alphabet; it does not prove that decoded bytes form UTF-8.
Checked Text decoders return `Left` for invalid UTF-8; `decodeBase32With` and
its variants let you choose a character conversion. For arbitrary binary
payloads, use ByteString decoders.

`assertBase32`, record updates, and the mapping instances
can bypass validation. Typed decoding requires a truthful encoded value and,
for Text, valid UTF-8 payload bytes. Use parsers and checked decoders for
external input.

Lazy encoding and typed decoding preserve streaming through rechunking.
Checked lazy decoding and parsing validate a complete finite input.

## Optional SIMD

```sh
cabal build -fsimd
cabal test -fsimd
```

The `simd` flag defaults to **off**. It enables bundled NEON kernels on AArch64
and runtime-dispatched SSSE3/AVX2 kernels on x86-64, with scalar fallback on
other targets. Small inputs and final quanta use Haskell scalar code. No
external native library is required.

Strict and short values use SIMD directly. Short codecs use byte arrays without
pinning; typed ShortText additionally converts the decoded UTF-8 payload. Lazy
variants reach the same kernels through their strict chunks or checked input.

## Development

```sh
cabal test -f-simd
cabal test -fsimd --ghc-options='-fcheck-prim-bounds -fno-ignore-asserts'
cabal exec -- sh test/typecheck.sh
cabal haddock -fsimd
cabal check
```

CI tests both flag configurations across GHC 9.6–9.14, builds from the source
archive, and runs native sanitizer and guard-page tests on x86-64 and ARM64.

Compare optimized scalar and SIMD benchmark builds separately:

```sh
cabal bench -f-simd --builddir=dist-scalar -O2 --benchmark-options='--csv scalar.csv'
cabal bench -fsimd --builddir=dist-simd -O2 --benchmark-options='--csv simd.csv'
```

Benchmark names identify the selected flag configuration. Both builds measure
the same deterministic inputs, with setup excluded from timing. The suite covers
strict and short codecs, both alphabets and padding forms, encoding, typed and
untyped decoding, parsing, sizes around dispatch thresholds, large buffers, and
sliced strict inputs.

The companion [base32-lens](https://hackage.haskell.org/package/base32-lens)
package needs an update for the 1.0 API before it can be used with this release.
