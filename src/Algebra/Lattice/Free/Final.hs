{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE Safe            #-}

----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.Lattice.Free
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------

module Algebra.Lattice.Free.Final (
   -- * Free Lattice
    FLattice,
    liftFLattice,
    lowerFLattice,
    retractFLattice,
   -- * Free BoundedLattice
    FBoundedLattice,
    liftFBoundedLattice,
    lowerFBoundedLattice,
    retractFBoundedLattice,
    ) where

import Algebra.Lattice

import Data.Universe.Class (Finite (..), Universe (..))

-------------------------------------------------------------------------------
-- Lattice
-------------------------------------------------------------------------------

newtype FLattice a = FLattice
  { lowerFLattice :: forall b. Lattice b =>
                                    (a -> b) -> b
  }

instance Functor FLattice where
  fmap f (FLattice g) = FLattice (\inj -> g (inj . f))
  a <$ FLattice f = FLattice (\inj -> f (const (inj a)))

liftFLattice :: a -> FLattice a
liftFLattice a = FLattice (\inj -> inj a)

retractFLattice :: Lattice a => FLattice a -> a
retractFLattice a = lowerFLattice a id

instance Lattice (FLattice a) where
  FLattice f \/ FLattice g = FLattice (\inj -> f inj \/ g inj)
  FLattice f /\ FLattice g = FLattice (\inj -> f inj /\ g inj)


instance BoundedJoinSemiLattice a =>
         BoundedJoinSemiLattice (FLattice a) where
  bottom = FLattice (\inj -> inj bottom)

instance BoundedMeetSemiLattice a =>
         BoundedMeetSemiLattice (FLattice a) where
  top = FLattice (\inj -> inj top)

instance Universe a => Universe (FLattice a) where
  universe = fmap liftFLattice universe

instance Finite a => Finite (FLattice a) where
  universeF = fmap liftFLattice universeF

-------------------------------------------------------------------------------
-- BoundedLattice
-------------------------------------------------------------------------------

newtype FBoundedLattice a = FBoundedLattice
  { lowerFBoundedLattice :: forall b. BoundedLattice b =>
                                    (a -> b) -> b
  }

instance Functor FBoundedLattice where
  fmap f (FBoundedLattice g) = FBoundedLattice (\inj -> g (inj . f))
  a <$ FBoundedLattice f = FBoundedLattice (\inj -> f (const (inj a)))

liftFBoundedLattice :: a -> FBoundedLattice a
liftFBoundedLattice a = FBoundedLattice (\inj -> inj a)

retractFBoundedLattice :: BoundedLattice a => FBoundedLattice a -> a
retractFBoundedLattice a = lowerFBoundedLattice a id

instance Lattice (FBoundedLattice a) where
  FBoundedLattice f \/ FBoundedLattice g = FBoundedLattice (\inj -> f inj \/ g inj)
  FBoundedLattice f /\ FBoundedLattice g = FBoundedLattice (\inj -> f inj /\ g inj)


instance BoundedJoinSemiLattice (FBoundedLattice a) where
  bottom = FBoundedLattice (\_ -> bottom)

instance BoundedMeetSemiLattice (FBoundedLattice a) where
  top = FBoundedLattice (\_ -> top)

instance Universe a => Universe (FBoundedLattice a) where
  universe = fmap liftFBoundedLattice universe

instance Finite a => Finite (FBoundedLattice a) where
  universeF = fmap liftFBoundedLattice universeF
