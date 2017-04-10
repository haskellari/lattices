{-# LANGUAGE RankNTypes #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.Lattice.Free
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------

module Algebra.Lattice.Free
  ( -- * Free join-semilattices
    FreeJoinSemiLattice
  , liftFreeJoinSemiLattice
  , lowerFreeJoinSemiLattice
  , retractFreeJoinSemiLattice

   -- * Free meet-semilattices
  , FreeMeetSemiLattice
  , liftFreeMeetSemiLattice
  , lowerFreeMeetSemiLattice
  , retractFreeMeetSemiLattice

   -- * Free lattices
  , FreeLattice
  , liftFreeLattice
  , lowerFreeLattice
  , retractFreeLattice
  ) where

import Algebra.Lattice
import Data.Universe.Class

--
-- Free join-semilattices
--

newtype FreeJoinSemiLattice a = FreeJoinSemiLattice
  { lowerFreeJoinSemiLattice :: forall b. JoinSemiLattice b =>
                                            (a -> b) -> b
  }

liftFreeJoinSemiLattice :: a -> FreeJoinSemiLattice a
liftFreeJoinSemiLattice a = FreeJoinSemiLattice (\inj -> inj a)

retractFreeJoinSemiLattice :: JoinSemiLattice a => FreeJoinSemiLattice a -> a
retractFreeJoinSemiLattice a = lowerFreeJoinSemiLattice a id

instance Functor FreeJoinSemiLattice where
  fmap f (FreeJoinSemiLattice g) = FreeJoinSemiLattice (\inj -> g (inj . f))
  a <$ FreeJoinSemiLattice f = FreeJoinSemiLattice (\inj -> f (const (inj a)))

instance JoinSemiLattice (FreeJoinSemiLattice a) where
  FreeJoinSemiLattice f \/ FreeJoinSemiLattice g =
    FreeJoinSemiLattice (\inj -> f inj \/ g inj)

instance BoundedJoinSemiLattice a =>
         BoundedJoinSemiLattice (FreeJoinSemiLattice a) where
  bottom = FreeJoinSemiLattice (\inj -> inj bottom)

instance Universe a => Universe (FreeJoinSemiLattice a) where
  universe = fmap liftFreeJoinSemiLattice universe

instance Finite a => Finite (FreeJoinSemiLattice a) where
  universeF = fmap liftFreeJoinSemiLattice universeF


--
-- Free meet-semilattices
--

newtype FreeMeetSemiLattice a = FreeMeetSemiLattice
  { lowerFreeMeetSemiLattice :: forall b. MeetSemiLattice b =>
                                            (a -> b) -> b
  }

instance Functor FreeMeetSemiLattice where
  fmap f (FreeMeetSemiLattice g) = FreeMeetSemiLattice (\inj -> g (inj . f))
  a <$ FreeMeetSemiLattice f = FreeMeetSemiLattice (\inj -> f (const (inj a)))

liftFreeMeetSemiLattice :: a -> FreeMeetSemiLattice a
liftFreeMeetSemiLattice a = FreeMeetSemiLattice (\inj -> inj a)

retractFreeMeetSemiLattice :: MeetSemiLattice a => FreeMeetSemiLattice a -> a
retractFreeMeetSemiLattice a = lowerFreeMeetSemiLattice a id

instance MeetSemiLattice (FreeMeetSemiLattice a) where
  FreeMeetSemiLattice f /\ FreeMeetSemiLattice g =
    FreeMeetSemiLattice (\inj -> f inj /\ g inj)

instance BoundedMeetSemiLattice a =>
         BoundedMeetSemiLattice (FreeMeetSemiLattice a) where
  top = FreeMeetSemiLattice (\inj -> inj top)

instance Universe a => Universe (FreeMeetSemiLattice a) where
  universe = fmap liftFreeMeetSemiLattice universe

instance Finite a => Finite (FreeMeetSemiLattice a) where
  universeF = fmap liftFreeMeetSemiLattice universeF


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

instance JoinSemiLattice (FreeLattice a) where
  FreeLattice f \/ FreeLattice g = FreeLattice (\inj -> f inj \/ g inj)

instance MeetSemiLattice (FreeLattice a) where
  FreeLattice f /\ FreeLattice g = FreeLattice (\inj -> f inj /\ g inj)

instance Lattice (FreeLattice a)

instance BoundedJoinSemiLattice a =>
         BoundedJoinSemiLattice (FreeLattice a) where
  bottom = FreeLattice (\inj -> inj bottom)

instance BoundedMeetSemiLattice a =>
         BoundedMeetSemiLattice (FreeLattice a) where
  top = FreeLattice (\inj -> inj top)

instance BoundedLattice a =>
         BoundedLattice (FreeLattice a)

instance Universe a => Universe (FreeLattice a) where
  universe = fmap liftFreeLattice universe

instance Finite a => Finite (FreeLattice a) where
  universeF = fmap liftFreeLattice universeF
