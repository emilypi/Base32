{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module       : Data.Base32.Types
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-style
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
--
-- Encoded values with an alphabet tag. Padding policy is selected by the
-- padded, unpadded, or permissive parser in each representation module.
module Data.Base32.Types
  ( Alphabet (..)
  , Padding (..)
  , Base32
  , extractBase32
  , assertBase32
  , base32
  ) where

import Control.DeepSeq (NFData)
import Data.Functor.Identity (Identity (..))

-- | The alphabets from RFC 4648 sections 6 and 7.
data Alphabet = Std | Hex
  deriving (Eq, Ord, Show)

-- | The padding policy enforced by a checked decoder or parser.
data Padding = OptionalPadding | RequirePadding | ForbidPadding
  deriving (Eq, Ord, Show)

-- | A value encoded with the given alphabet. Encoding and parsing establish
-- this provenance. 'assertBase32', record updates, and the derived mapping
-- instances do not validate their results.
type role Base32 nominal representational

newtype Base32 (alphabet :: Alphabet) a = Base32
  { extractBase32 :: a
  -- ^ Extract the encoded representation without decoding it.
  }
  deriving (Eq, Ord, Show, NFData) via a
  deriving (Functor, Applicative, Monad) via Identity

-- | Assert provenance without validation. Prefer a parser for external input.
-- A false assertion can cause typed decoding to fail.
assertBase32 :: a -> Base32 alphabet a
assertBase32 = Base32
{-# INLINE assertBase32 #-}

-- | A representation-preserving lens. Updating through it does not validate
-- the replacement; callers must preserve the encoded-value invariant.
base32 ::
  (Functor f) =>
  (a -> f b) ->
  Base32 alphabet a ->
  f (Base32 alphabet b)
base32 f = fmap Base32 . f . extractBase32
{-# INLINE base32 #-}
