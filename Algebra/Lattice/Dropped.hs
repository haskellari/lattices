{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Trustworthy #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.Lattice.Dropped
-- Copyright   :  (C) 2010-2015 Maximilian Bolingbroke, 2015 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Algebra.Lattice.Dropped (
    Dropped(..)
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

instance Hashable a => Hashable (Dropped a)

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
