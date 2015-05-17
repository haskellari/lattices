{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Algebra.Lattice.Dropped (
    Dropped(..)
  ) where

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
import GHC.Generics

--
-- Dropped
--

-- | Graft a distinct top onto an otherwise unbounded lattice.
-- As a bonus, the top will be an absorbing element for the join.
data Dropped a = Top
               | Drop a
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic
#if __GLASGOW_HASKELL__ >= 706
           , Generic1
#endif
           )

instance Functor Dropped where
  fmap _ Top      = Top
  fmap f (Drop a) = Drop (f a)

instance Foldable Dropped where
  foldMap _ Top      = mempty
  foldMap f (Drop a) = f a

instance Traversable Dropped where
  traverse _ Top      = pure Top
  traverse f (Drop a) = Drop <$> f a

instance NFData a => NFData (Dropped a) where
  rnf Top      = ()
  rnf (Drop a) = rnf a

instance JoinSemiLattice a => JoinSemiLattice (Dropped a) where
    Top    `join` _      = Top
    _      `join` Top    = Top
    Drop x `join` Drop y = Drop (x `join` y)

instance MeetSemiLattice a => MeetSemiLattice (Dropped a) where
    Top    `meet` drop_y = drop_y
    drop_x `meet` Top    = drop_x
    Drop x `meet` Drop y = Drop (x `meet` y)

instance Lattice a => Lattice (Dropped a) where

instance BoundedJoinSemiLattice a => BoundedJoinSemiLattice (Dropped a) where
    bottom = Drop bottom

instance MeetSemiLattice a => BoundedMeetSemiLattice (Dropped a) where
    top = Top

instance BoundedLattice a => BoundedLattice (Dropped a) where
