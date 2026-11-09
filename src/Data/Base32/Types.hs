{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Module       : Data.Base32.Types
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- Encoded values tagged with their alphabet and padding policy.
--
module Data.Base32.Types
  ( Alphabet (..)
  , StdAlphabet
  , HexAlphabet
  , Padding (..)
  , Base32
  , extractBase32
  , assertBase32
  ) where

import Control.DeepSeq (NFData)
import Data.Functor.Identity (Identity (..))
import Data.Kind (Constraint)
import GHC.TypeLits (ErrorMessage (..), TypeError)

-- | The alphabets and padding forms from RFC 4648 sections 6 and 7.
--
data Alphabet = StdPadded | StdUnpadded | HexPadded | HexUnpadded
  deriving (Eq, Ord, Show)

-- | Both padding forms of the standard alphabet.
--
-- >>> let standard :: StdAlphabet k => Base32 k a -> a; standard = extractBase32
-- >>> standard (assertBase32 @'StdPadded "KN2W4===")
-- "KN2W4==="
-- >>> standard (assertBase32 @'StdUnpadded "KN2W4")
-- "KN2W4"
type family StdAlphabet (k :: Alphabet) :: Constraint where
  StdAlphabet 'StdPadded = ()
  StdAlphabet 'StdUnpadded = ()
  StdAlphabet _ = TypeError
    ( 'Text "Cannot prove base32 value is encoded using the standard alphabet. "
      ':<>: 'Text "Use the standard encoders or padding-specific parsers."
    )

-- | Both padding forms of the extended-hex alphabet.
--
-- >>> let hex :: HexAlphabet k => Base32 k a -> a; hex = extractBase32
-- >>> hex (assertBase32 @'HexPadded "ADQMS===")
-- "ADQMS==="
-- >>> hex (assertBase32 @'HexUnpadded "ADQMS")
-- "ADQMS"
type family HexAlphabet (k :: Alphabet) :: Constraint where
  HexAlphabet 'HexPadded = ()
  HexAlphabet 'HexUnpadded = ()
  HexAlphabet _ = TypeError
    ( 'Text "Cannot prove base32 value is encoded using the extended-hex alphabet. "
      ':<>: 'Text "Use the hex encoders or padding-specific parsers."
    )

-- | The padding policy enforced by a checked decoder or parser.
--
-- >>> [OptionalPadding, RequirePadding, ForbidPadding]
-- [OptionalPadding,RequirePadding,ForbidPadding]
data Padding = OptionalPadding | RequirePadding | ForbidPadding
  deriving (Eq, Ord, Show)

-- | A value encoded with the given alphabet and padding. Encoding and parsing establish
-- this provenance. 'assertBase32', record updates, and the derived mapping
-- instances do not validate their results.
--
-- >>> assertBase32 "KN2W4===" :: Base32 'StdPadded String
-- "KN2W4==="
type role Base32 nominal representational

newtype Base32 (alphabet :: Alphabet) a = Base32
  { extractBase32 :: a
  -- ^ Extract the encoded representation without decoding it.
  --
  -- >>> extractBase32 (assertBase32 @'StdPadded "KN2W4===")
  -- "KN2W4==="
  }
  deriving (Eq, Ord, Show, NFData) via a
  deriving (Functor, Applicative, Monad) via Identity

-- | Assert provenance without validation. Prefer a parser for external input.
-- The caller must establish validity; typed decoding performs no validation.
--
-- >>> assertBase32 @'HexUnpadded "ADQMS"
-- "ADQMS"
assertBase32 :: forall alphabet a. a -> Base32 alphabet a
assertBase32 = Base32
{-# INLINE assertBase32 #-}

-- $setup
-- >>> :seti -XOverloadedStrings
-- >>> :seti -XDataKinds -XTypeApplications -XTypeFamilies -XFlexibleContexts
