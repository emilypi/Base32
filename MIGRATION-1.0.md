# Migrating from base32 0.4 to 1.0

## Encoded values carry their alphabet and padding

Encoding now produces type-tagged values that carry the alphabet and padding provenance 
carried out by the encoding. You can extract these bytes or assert their provenance by importing `Data.Base32.Types` and manipulating the wrapper through the supplied functions. `Alphabet(..)` is also included, and can be used in conjunction with `-XDataKinds` to assert what you need. Standard encoders return `Base32 'StdPadded a`
or `Base32 'StdUnpadded a`, while extended-hex encoders return `Base32 'HexPadded a` or
`Base32 'HexUnpadded a`. For example:

```haskell
-- Before
rawb32bytes = B32.encodeBase32' bytes

-- After, when the surrounding API expects a plain ByteString
rawb32bytes = extractBase32 (B32.encodeBase32' unencodedbytes)

-- A typed roundtrip no longer returns Either
unencodedBytesAgain = B32.decodeBase32 (B32.encodeBase32' unencodedbytes)
```

The reasoning behind this wrapper is clear from a performance perspective: by shoving the information about the bytes into the typed wrapper and shunting the contract to the user 
or the program, we have all the info we need to forego validity checks during the decode 
process. This removes most branching from the procedure and hotpath detection is both 
faster and easier, resulting in a significant speedup. Go Haskell!

## Checked decoding keeps an explicit name

| 0.4 | 1.0 checked equivalent |
| --- | --- |
| `decodeBase32` | `decodeBase32Untyped` |
| `decodeBase32Padded` | `decodeBase32PaddedUntyped` |
| `decodeBase32Unpadded` | `decodeBase32UnpaddedUntyped` |

These replacements apply to all twelve ByteString/Text modules. The old names
now consume `Base32` values and return the decoded result directly. The
`decodeBase32With`, `decodeBase32PaddedWith`, and `decodeBase32UnpaddedWith`
functions keep their checked signatures and names, and are `Text` only.

Typed decoders use dedicated unchecked loops. They trust the established
provenance and do not check the alphabet, input length, padding, or unused bits.
`decodeBase32` accepts both padding forms within its alphabet through the
`StdAlphabet k` or `HexAlphabet k` constraint. The padding-specific decoders
require the corresponding exact tag, so mixing their API paths is a type error.
For example, `decodeBase32Padded (encodeBase32Unpadded' bytes)` cannot compile.
Use `TypeFamilies` when writing signatures with these constraints.
Callers using `assertBase32`, record updates, or mapping instances must preserve
validity themselves.

Typed lazy decoding streams through complete quanta. Checked lazy decoding and
parsing still validate a complete finite input.

## Replace predicates with parsers

`isBase32`, `isValidBase32`, `isBase32Hex`, and `isValidBase32Hex` are removed.
Use `parseBase32Padded` or `parseBase32Unpadded` from the corresponding standard
or `.Hex` module. Each returns the precise padding tag; there is no permissive
`parseBase32`. `Data.Either.isRight . parseBase32Padded` provides a Boolean check
for padded input when needed. Prefer retaining the successful parsed value:

```haskell
case B32.parseBase32Padded wire of
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
encoding. `parseBase32Padded (Text.pack "74======")` succeeds, since it encodes
byte `0xff`.
Checked Text decoding now reports invalid UTF-8 as `Left`, including short and
lazy Text. Typed Text decoding requires valid UTF-8 and fails otherwise. Use
ByteString decoding for binary data, or a `decode*With` function for explicit
character conversion.

## Build and compatibility

`Data.ByteString.Base32.Internal` is removed. Import codec entry points from
`Data.ByteString.Base32.Internal.Head` and `validateLastNPads` from
`Data.ByteString.Base32.Internal.Utils`.

The compiler range is GHC 9.6–9.14. Dependencies include `primitive` for direct
short byte-array codecs. SIMD is optional and remains disabled unless selected
with `-fsimd`; both modes use identical validation rules. The source archive
contains the bundled C sources, headers, tests, and this migration guide.

Update downstream libraries, including `base32-lens`, before widening their
bounds to accept 1.0.
