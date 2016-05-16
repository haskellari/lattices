{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if __GLASGOW_HASKELL__ < 709
{-# LANGUAGE Trustworthy #-}
#else
{-# LANGUAGE Safe #-}
#endif
----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.Lattice.Ordered
-- Copyright   :  (C) 2010-2015 Maximilian Bolingbroke, 2015 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Algebra.Lattice.Ordered (
    Ordered(..)
  ) where

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

import Algebra.Lattice
import Algebra.PartialOrd

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
import Data.Foldable
import Data.Traversable
#endif

import Control.DeepSeq
import Control.Monad
import Data.Data
import Data.Hashable
import GHC.Generics

--
-- Ordered
--

-- | A total order gives rise to a lattice. Join is
-- max, meet is min.
newtype Ordered a = Ordered { getOrdered :: a }
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic
#if __GLASGOW_HASKELL__ >= 706
           , Generic1
#endif
           )

instance Foldable Ordered where
  foldMap f (Ordered a) = f a

instance Traversable Ordered where
  traverse f (Ordered a) = Ordered <$> f a

instance Functor Ordered where
  fmap f (Ordered a) = Ordered (f a)

instance Applicative Ordered where
  pure = return
  (<*>) = ap

instance Monad Ordered where
  return           = Ordered
  Ordered x >>= f  = f x

instance NFData a => NFData (Ordered a) where
  rnf (Ordered a) = rnf a

instance Hashable a => Hashable (Ordered a)

instance Ord a => JoinSemiLattice (Ordered a) where
  Ordered x \/ Ordered y = Ordered (max x y)

instance Ord a => MeetSemiLattice (Ordered a) where
  Ordered x /\ Ordered y = Ordered (min x y)

instance Ord a => Lattice (Ordered a) where

instance (Ord a, Bounded a) => BoundedJoinSemiLattice (Ordered a) where
  bottom = Ordered minBound

instance (Ord a, Bounded a) => BoundedMeetSemiLattice (Ordered a) where
  top = Ordered maxBound

instance (Ord a, Bounded a) => BoundedLattice (Ordered a) where

instance Ord a => PartialOrd (Ordered a) where
    leq = (<=)
    comparable _ _ = True
