{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe       #-}

----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.Lattice.Free
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------

module Algebra.Lattice.Free (
   -- * Free lattices
    FreeLattice,
    liftFreeLattice,
    lowerFreeLattice,
    retractFreeLattice,
    ) where

import Prelude ()
import Prelude.Compat

import Algebra.Lattice

import Data.Universe.Class (Finite (..), Universe (..))

--
-- Free lattices
--

newtype FreeLattice a = FreeLattice
  { lowerFreeLattice :: forall b. Lattice b =>
                                    (a -> b) -> b
  }

instance Functor FreeLattice where
  fmap f (FreeLattice g) = FreeLattice (\inj -> g (inj . f))
  a <$ FreeLattice f = FreeLattice (\inj -> f (const (inj a)))

liftFreeLattice :: a -> FreeLattice a
liftFreeLattice a = FreeLattice (\inj -> inj a)

retractFreeLattice :: Lattice a => FreeLattice a -> a
retractFreeLattice a = lowerFreeLattice a id

instance Lattice (FreeLattice a) where
  FreeLattice f \/ FreeLattice g = FreeLattice (\inj -> f inj \/ g inj)
  FreeLattice f /\ FreeLattice g = FreeLattice (\inj -> f inj /\ g inj)


instance BoundedJoinSemiLattice a =>
         BoundedJoinSemiLattice (FreeLattice a) where
  bottom = FreeLattice (\inj -> inj bottom)

instance BoundedMeetSemiLattice a =>
         BoundedMeetSemiLattice (FreeLattice a) where
  top = FreeLattice (\inj -> inj top)

instance Universe a => Universe (FreeLattice a) where
  universe = fmap liftFreeLattice universe

instance Finite a => Finite (FreeLattice a) where
  universeF = fmap liftFreeLattice universeF
