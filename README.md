# Base32

[![Build Status](https://travis-ci.com/emilypi/base32.svg?branch=master)](https://travis-ci.com/emilypi/base32)
[![Hackage](https://img.shields.io/hackage/v/base32.svg)](https://hackage.haskell.org/package/base32)

Padded and unpadded base32 and base32hex encoding and decoding for `Text` and `ByteString` values.

For the companion optics and pattern synonyms, see [base32-lens](https://hackage.haskell.org/package/base32-lens).


### Summary

What does this library provide? Here is the summary:

- Support for padded and unpadded Base32 and Base32hex
- Support for `Text` encodings and decodings
- Optics for handling more complex structures with Base32 representations via the `base32-lens` package
- Checks for both valid Base32 and correct Base32 and Base32hex encodings

There are no dependencies aside from those bundled with GHC:

![base32 dependencies](https://i.imgur.com/8CdVsey.png)
