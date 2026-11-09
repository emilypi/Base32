# Revision history for base32

## 1.0

* Introduce alphabet-tagged `Base32` values in `Data.Base32.Types`.
  Encoders return wrapped values; typed decoders consume them. Checked decoders
  now use the `Untyped` suffix; custom text conversion functions retain their names.
* Replace `isBase32`, `isValidBase32`, `isBase32Hex`, and `isValidBase32Hex` with
  `parseBase32`, `parseBase32Padded`, and `parseBase32Unpadded` in all twelve APIs.
  Parsers preserve input, accept the decoder's existing case-insensitive alphabets,
  and check unused pad bits. Noncanonical pad bits are now rejected by decoders too.
* Add an optional, default-off `simd` flag with NEON, SSSE3, and AVX2 prefixes
  and a portable scalar fallback, for both standard and extended-hex alphabets.
* Encode and decode short values directly through byte arrays, including SIMD.
* Report invalid UTF-8 through checked Text decoding, instead of throwing or
  constructing invalid ShortText. Base32 parsing itself accepts binary payloads.
* Use checked integer output capacities, write only the actual decoded tail,
  and correct scalar byte order on big-endian targets.
* Support GHC 9.6–9.14; update copyright through 2026, CI, bounds, and benchmarks.

See [MIGRATION-1.0.md](MIGRATION-1.0.md) for source compatibility changes.

## 0.4

* Support for GHC 9.8
* Drop support for GHC <9.x
* Bump `bytestring` lower bound to 0.11 (thanks @vlix!)

## 0.3.1.0

* Small bug fix
* Doc updates

## 0.3

* Bump lower bounds for `bytestring` and `text` to latest significant major/major version
* Update CI for 8.10.x thru 9.6 coverage
* Correct the update the latin1 decode default for text modules to `utf8` (which is now pure for `text-2.0`).

## 0.2.2.0

* Add support for GHC 9.2.x
* Drop support for GHC <8.10
* Drop dependency on `ghc-byteorder`
* Bump bounds for `deepseq`

## 0.2.1.0

* Expose `Data.ByteString.Base32.Internal` API
* Use closed-form encoding/decoding length calculations, reducing branching and improving performance
* Update tests:
  * Migrate to using lower heap-footprint concrete dict passing
  * Support for 8.10.5 testing


## 0.2.0.0

* Bugfix: fix unpadded base32 encoding ([#4](https://github.com/emilypi/Base32/pull/4))
* Bugfix: Use `decodeLatin1` when decoding, defer to `decodeWith*` primitives for finer-grained
          `Text` to `ByteString` conversions. ([#5](https://github.com/emilypi/Base32/pull/5))
* Add short and lazy variants for `Text` and `ByteString` ([#6](https://github.com/emilypi/Base32/pull/6))
* Expanded test coverage to 96% ([#7](https://github.com/emilypi/Base32/pull/7))
* Improved performance and better error reporting in final quanta ([#9](https://github.com/emilypi/Base32/pull/9))
* Improved docs + trustworthy/safe annotations ([#10](https://github.com/emilypi/Base32/pull/10))

## 0.1.1.1 -- 2020-02-16

* Documentation now references correct RFC section

## 0.1.1 -- 2020-02-16

* Textual interface now uses correct unpadded version

## 0.1.0.0 -- 2020-02-16

* First version. Released on an unsuspecting world.
