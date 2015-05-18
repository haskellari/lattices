{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
module Algebra.Lattice.Lifted (
    Lifted(..)
  ) where

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

import Algebra.Lattice

#if MIN_VERSION_base(4,8,0)
#else
import Data.Monoid (Monoid(..))
import Data.Foldable
import Data.Traversable
#endif

import Control.Applicative
import Control.DeepSeq
import Data.Data
import Data.Hashable
import GHC.Generics

--
-- Lifted
--

-- | Graft a distinct bottom onto an otherwise unbounded lattice.
-- As a bonus, the bottom will be an absorbing element for the meet.
data Lifted a = Lift a
              | Bottom
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic
#if __GLASGOW_HASKELL__ >= 706
           , Generic1
#endif
           )

instance Functor Lifted where
  fmap _ Bottom   = Bottom
  fmap f (Lift a) = Lift (f a)

instance Foldable Lifted where
  foldMap _ Bottom   = mempty
  foldMap f (Lift a) = f a

instance Traversable Lifted where
  traverse _ Bottom   = pure Bottom
  traverse f (Lift a) = Lift <$> f a

instance NFData a => NFData (Lifted a) where
  rnf Bottom   = ()
  rnf (Lift a) = rnf a

instance Hashable a => Hashable (Lifted a)

instance JoinSemiLattice a => JoinSemiLattice (Lifted a) where
    Lift x `join` Lift y = Lift (x `join` y)
    Bottom `join` lift_y = lift_y
    lift_x `join` Bottom = lift_x

instance MeetSemiLattice a => MeetSemiLattice (Lifted a) where
    Lift x `meet` Lift y = Lift (x `meet` y)
    Bottom `meet` _      = Bottom
    _      `meet` Bottom = Bottom

instance Lattice a => Lattice (Lifted a) where

instance JoinSemiLattice a => BoundedJoinSemiLattice (Lifted a) where
    bottom = Bottom

instance BoundedMeetSemiLattice a => BoundedMeetSemiLattice (Lifted a) where
    top = Lift top

instance BoundedLattice a => BoundedLattice (Lifted a) where
