{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_base32 (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "base32"
version :: Version
version = Version [0,2,2,0] []

synopsis :: String
synopsis = "Fast RFC 4648-compliant Base32 encoding"
copyright :: String
copyright = "(c) 2020-2023 Emily Pillmore"
homepage :: String
homepage = "https://github.com/emilypi/base32"
