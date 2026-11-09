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
-- @since 1.0
--
module Data.Base32.Types
  ( Alphabet (..)
  , StdAlphabet
  , HexAlphabet
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
-- @since 1.0
--
data Alphabet = StdPadded | StdUnpadded | HexPadded | HexUnpadded
  deriving
    ( Eq   -- ^ @since 1.0
    , Ord  -- ^ @since 1.0
    , Show -- ^ @since 1.0
    )

-- | Padded and unpadded forms of the standard alphabet.
--
-- @since 1.0
--
type family StdAlphabet (k :: Alphabet) :: Constraint where
  StdAlphabet 'StdPadded = ()
  StdAlphabet 'StdUnpadded = ()
  StdAlphabet _ = TypeError
    ( 'Text "Cannot prove base32 value is encoded using the standard alphabet. "
      ':<>: 'Text "Use the standard encoders or padding-specific parsers."
    )

-- | Padded and unpadded forms of the extended-hex alphabet.
--
-- >>> let hex :: HexAlphabet k => Base32 k a -> a; hex = extractBase32
-- >>> hex (assertBase32 @'HexPadded "ADQMS===")
-- "ADQMS==="
-- >>> hex (assertBase32 @'HexUnpadded "ADQMS")
-- "ADQMS"
--
-- @since 1.0
--
type family HexAlphabet (k :: Alphabet) :: Constraint where
  HexAlphabet 'HexPadded = ()
  HexAlphabet 'HexUnpadded = ()
  HexAlphabet _ = TypeError
    ( 'Text "Cannot prove base32 value is encoded using the extended-hex alphabet. "
      ':<>: 'Text "Use the hex encoders or padding-specific parsers."
    )

-- | A value encoded with the given alphabet and padding. Encoding and parsing establish
-- this provenance. 'assertBase32', record updates, and the derived mapping
-- instances do not validate their results.
--
-- @since 1.0
--
type role Base32 nominal representational
newtype Base32 (alphabet :: Alphabet) a = Base32
  { extractBase32 :: a
    -- ^ Extract the encoded representation without decoding it.
    --
    -- @since 1.0
  }
  deriving
    ( Eq     -- ^ @since 1.0
    , Ord    -- ^ @since 1.0
    , Show   -- ^ @since 1.0
    , NFData -- ^ @since 1.0
    ) via a
  deriving
    ( Functor     -- ^ @since 1.0
    , Applicative -- ^ @since 1.0
    , Monad       -- ^ @since 1.0
    ) via Identity

-- | Assert that a value is Base32-encoded.  
--
-- @since 1.0
--
assertBase32 :: forall alphabet a. a -> Base32 alphabet a
assertBase32 = Base32
{-# INLINE assertBase32 #-}

-- $setup
-- >>> :seti -XOverloadedStrings
-- >>> :seti -XDataKinds -XTypeApplications -XTypeFamilies -XFlexibleContexts
