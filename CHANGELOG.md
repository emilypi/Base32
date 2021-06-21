# Revision history for base32

## 0.2.1.0

* Expose `Data.ByteString.Base32.Internal` API
* Switched to using a closed-form for length calculations, reducing branching and improving performance
*

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
