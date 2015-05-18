{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Trustworthy #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.Lattice.Levitated
-- Copyright   :  (C) 2010-2015 Maximilian Bolingbroke, 2015 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Algebra.Lattice.Levitated (
    Levitated(..)
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
-- Levitated
--

-- | Graft a distinct top and bottom onto an otherwise unbounded lattice.
-- The top is the absorbing element for the join, and the bottom is the absorbing
-- element for the meet.
data Levitated a = Top
                 | Levitate a
                 | Bottom
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic
#if __GLASGOW_HASKELL__ >= 706
           , Generic1
#endif
           )
instance Functor Levitated where
  fmap _ Bottom       = Bottom
  fmap _ Top          = Top
  fmap f (Levitate a) = Levitate (f a)

instance Foldable Levitated where
  foldMap _ Bottom       = mempty
  foldMap _ Top          = mempty
  foldMap f (Levitate a) = f a

instance Traversable Levitated where
  traverse _ Bottom       = pure Bottom
  traverse _ Top          = pure Top
  traverse f (Levitate a) = Levitate <$> f a

instance NFData a => NFData (Levitated a) where
  rnf Top          = ()
  rnf Bottom       = ()
  rnf (Levitate a) = rnf a

instance Hashable a => Hashable (Levitated a)

instance JoinSemiLattice a => JoinSemiLattice (Levitated a) where
    Top        `join` _          = Top
    _          `join` Top        = Top
    Levitate x `join` Levitate y = Levitate (x `join` y)
    Bottom     `join` lev_y      = lev_y
    lev_x      `join` Bottom     = lev_x

instance MeetSemiLattice a => MeetSemiLattice (Levitated a) where
    Top        `meet` lev_y      = lev_y
    lev_x      `meet` Top        = lev_x
    Levitate x `meet` Levitate y = Levitate (x `meet` y)
    Bottom     `meet` _          = Bottom
    _          `meet` Bottom     = Bottom

instance Lattice a => Lattice (Levitated a) where

instance JoinSemiLattice a => BoundedJoinSemiLattice (Levitated a) where
    bottom = Bottom

instance MeetSemiLattice a => BoundedMeetSemiLattice (Levitated a) where
    top = Top

instance Lattice a => BoundedLattice (Levitated a) where
