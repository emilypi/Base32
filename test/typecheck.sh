#!/bin/sh
# Run inside the package environment: cabal exec -- sh test/typecheck.sh
set -eu

compiler=${GHC:-ghc}
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT HUP INT TERM

compile() {
    "$compiler" -v0 -fno-code -fforce-recomp -package base32 \
        -odir "$work" -hidir "$work" "$work/Api.hs" > "$work/result" 2>&1
}

reject() {
    cat "$work/header" > "$work/Api.hs"
    printf '%s\n' "$1" >> "$work/Api.hs"
    if compile; then
        printf 'Unexpectedly accepted in %s: %s\n' "$api" "$1" >&2
        exit 1
    fi
    if ! grep -F "$2" "$work/result" > /dev/null; then
        cat "$work/result" >&2
        exit 1
    fi
}

while read -r api representation value suffix; do
    [ "$suffix" != - ] || suffix=
    padded="encodeBase32$suffix"
    unpadded="encodeBase32Unpadded$suffix"
    cat > "$work/header" <<EOF
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Api where
import Data.Base32.Types
import Data.Coerce (coerce)
import qualified $representation as R
import qualified Data.Text as Error
import qualified $api as S
import qualified $api.Hex as H
type Value = R.$value
EOF
    cat "$work/header" > "$work/Api.hs"
    cat >> "$work/Api.hs" <<EOF
standard :: StdAlphabet k => Base32 k Value -> Value
standard = S.decodeBase32
hexadecimal :: HexAlphabet k => Base32 k Value -> Value
hexadecimal = H.decodeBase32
roundtrips :: Value -> [Value]
roundtrips value =
  [ standard (S.$padded value), standard (S.$unpadded value)
  , S.decodeBase32Padded (S.$padded value)
  , S.decodeBase32Unpadded (S.$unpadded value)
  , hexadecimal (H.$padded value), hexadecimal (H.$unpadded value)
  , H.decodeBase32Padded (H.$padded value)
  , H.decodeBase32Unpadded (H.$unpadded value)
  ]
stdPadded :: Value -> Either Error.Text (Base32 'StdPadded Value)
stdPadded = S.parseBase32Padded
stdUnpadded :: Value -> Either Error.Text (Base32 'StdUnpadded Value)
stdUnpadded = S.parseBase32Unpadded
hexPadded :: Value -> Either Error.Text (Base32 'HexPadded Value)
hexPadded = H.parseBase32Padded
hexUnpadded :: Value -> Either Error.Text (Base32 'HexUnpadded Value)
hexUnpadded = H.parseBase32Unpadded
EOF
    if ! compile; then
        cat "$work/result" >&2
        exit 1
    fi
    for encoder in "$padded" "$unpadded"; do
        reject "bad = S.decodeBase32 . H.$encoder" 'standard alphabet'
        reject "bad = H.decodeBase32 . S.$encoder" 'extended-hex alphabet'
    done
    for alphabet in S H; do
        reject "bad = $alphabet.decodeBase32Padded . $alphabet.$unpadded" \
            "Couldn't match type"
        reject "bad = $alphabet.decodeBase32Unpadded . $alphabet.$padded" \
            "Couldn't match type"
        reject "bad = fmap $alphabet.decodeBase32Padded . $alphabet.parseBase32Unpadded" \
            "Couldn't match type"
        reject "bad = fmap $alphabet.decodeBase32Unpadded . $alphabet.parseBase32Padded" \
            "Couldn't match type"
    done
    reject "bad :: Base32 'StdPadded Value -> Base32 'StdUnpadded Value
bad = coerce" "Couldn't match type"
    reject "bad :: Base32 'StdPadded Value -> Base32 'HexPadded Value
bad = coerce" "Couldn't match type"
    printf 'Type checks passed: %s\n' "$api"
done <<'EOF'
Data.ByteString.Base32 Data.ByteString ByteString '
Data.ByteString.Lazy.Base32 Data.ByteString.Lazy ByteString '
Data.ByteString.Short.Base32 Data.ByteString.Short ShortByteString '
Data.Text.Encoding.Base32 Data.Text Text -
Data.Text.Lazy.Encoding.Base32 Data.Text.Lazy Text -
Data.Text.Short.Encoding.Base32 Data.Text.Short ShortText -
EOF
