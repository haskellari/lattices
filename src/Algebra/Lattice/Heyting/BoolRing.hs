{-# LANGUAGE CPP #-}
module Algebra.Lattice.Heyting.BoolRing
  ( BoolRing (..)
  , Semiring (..)
  , (<+>)
  ) where

import Prelude hiding (not)

#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup (Semigroup (..))
#endif

import Algebra.Lattice (bottom, top, (/\), (\/))
import Data.Semiring (Semiring (..), (<+>))

import Algebra.Lattice.Heyting

-- |
-- Newtype wraper which captures Boolean ring structure, which holds for every
-- Heyting algebra.  A Boolean ring is a ring which satisfies:
--
-- prop> a <.> a = a
--
-- Some other properties:
--
-- prop> a <+> a = mempty                  -- thus it is a ring of characteristic 2
-- prop> a <.> b = b <.> a                 -- hence it is a commutative ring
-- prop> a <+> (b <+> c) = (a <+> b) <+> c -- multiplicative associativity
newtype BoolRing a = BoolRing { getBoolRing :: a }

-- | Sum is [symmetric differnce](https://en.wikipedia.org/wiki/Symmetric_difference).
instance HeytingAlgebra a => Semigroup (BoolRing a) where
  (BoolRing a) <> (BoolRing b) = BoolRing $ (not a /\ b) \/ (a /\ not b)

-- | In a Boolean ring @a + a = 0@, hence @negate = id@.
instance HeytingAlgebra a => Monoid (BoolRing a) where
  mempty = BoolRing bottom

#if __GLASGOW_HASKELL__ <= 804
  mappend = (<>)
#endif

-- |  Multiplication is given by @'/\'@
instance HeytingAlgebra a => Semiring (BoolRing a) where
  BoolRing a <.> BoolRing b = BoolRing (a \/ b)
  one = BoolRing top
