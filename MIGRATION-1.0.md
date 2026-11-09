# Migrating from base32 0.4 to 1.0

## Encoded values carry their alphabet

Import `Data.Base32.Types` for the opaque `Base32` wrapper, `Alphabet(..)`,
`extractBase32`, and `assertBase32`. Standard encoders return `Base32 'Std a`;
extended-hex encoders return `Base32 'Hex a`. Add `DataKinds` when writing these
signatures explicitly. Padding is selected by the function name, not the type.

```haskell
-- Before
wire = B32.encodeBase32' bytes

-- After, when the surrounding API expects a plain ByteString
wire = extractBase32 (B32.encodeBase32' bytes)

-- A typed roundtrip no longer returns Either
bytesAgain = B32.decodeBase32 (B32.encodeBase32' bytes)
```

## Checked decoding keeps an explicit name

| 0.4 | 1.0 checked equivalent |
| --- | --- |
| `decodeBase32` | `decodeBase32Untyped` |
| `decodeBase32Padded` | `decodeBase32PaddedUntyped` |
| `decodeBase32Unpadded` | `decodeBase32UnpaddedUntyped` |

These replacements apply to all twelve ByteString/Text modules. The old names
now consume `Base32` values and return the decoded result directly. The
`decodeBase32With`, `decodeBase32PaddedWith`, and `decodeBase32UnpaddedWith`
functions keep their checked signatures and names.

Typed padded/unpadded decoders still require the corresponding padding policy;
the alphabet tag alone does not prove it. For external input, pair each typed
decoder with its matching parser. False assertions, representation updates, or
incompatible padding policies cause typed decoding to fail.

## Replace predicates with parsers

`isBase32`, `isValidBase32`, `isBase32Hex`, and `isValidBase32Hex` are removed.
Use `parseBase32` from the corresponding standard or `.Hex` module, or a
padding-specific parser. `Data.Either.isRight . parseBase32` provides a Boolean
when needed. Prefer retaining the successful parsed value:

```haskell
case B32.parseBase32 wire of
  Left err -> handleError err
  Right encoded -> consumeBytes (B32.decodeBase32 encoded)
```

Parsers return the original encoded representation, including case, padding,
and lazy chunk structure. Unlike the old shape predicates, they perform full
validation. The decoders' existing lowercase acceptance is preserved; encoders
produce uppercase. Inputs with nonzero unused bits, such as `MZ======`, are now
rejected instead of silently decoding as `f`.

## Text decoding and binary payloads

The wrapper establishes Base32 syntax, not the decoded payload's character
encoding. `parseBase32 "74======"` succeeds, since it encodes byte `0xff`.
Checked Text decoding now reports invalid UTF-8 as `Left`, including short and
lazy Text. Typed Text decoding requires valid UTF-8 and fails otherwise. Use
ByteString decoding for binary data, or a `decode*With` function for explicit
character conversion.

## Build and compatibility

The compiler range is GHC 9.6–9.14. Dependencies include `primitive` for direct
short byte-array codecs. SIMD is optional and remains disabled unless selected
with `-fsimd`; both modes use identical validation rules. The source archive
contains the bundled C sources, headers, tests, and this migration guide.

Update downstream libraries, including `base32-lens`, before widening their
bounds to accept 1.0.
