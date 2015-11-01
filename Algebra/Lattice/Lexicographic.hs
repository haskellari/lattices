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
-- Module      :  Algebra.Lattice.Lexicographic
-- Copyright   :  (C) 2010-2015 Maximilian Bolingbroke, 2015 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Algebra.Lattice.Lexicographic (
    Lexicographic(..)
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
-- Lexicographic
--

-- | A pair lattice with a lexicographic ordering.  This means in 
-- a join the second component of the resulting pair is the second
-- component of the pair with the larger first component.  If the
-- first components are equal, then the second components will be
-- joined.  The meet is similar only it prefers the smaller first
-- component.
--
-- An application of this type is versioning.  For example, a
-- Last-Writer-Wins register would look like 
-- 'Lexicographc (Ordered Timestamp) v' where the lattice 
-- structure handles the, presumably rare, case of matching
-- 'Timestamps'.  Typically this is done in an arbitary, but
-- deterministic manner.
data Lexicographic k v = Lexicographic !k !v
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic
#if __GLASGOW_HASKELL__ >= 706
           , Generic1
#endif
           )

instance Foldable (Lexicographic k) where
  foldMap f (Lexicographic _ v) = f v

instance Traversable (Lexicographic k) where
  traverse f (Lexicographic k v) = Lexicographic k <$> f v

instance Functor (Lexicographic k) where
  fmap f (Lexicographic k v) = Lexicographic k (f v)

instance BoundedJoinSemiLattice k => Applicative (Lexicographic k) where
  pure = return
  (<*>) = ap

-- Essentially the Writer monad.
instance BoundedJoinSemiLattice k => Monad (Lexicographic k) where
  return                   =  Lexicographic bottom
  Lexicographic k v >>= f  =
    case f v of
      Lexicographic k' v' -> Lexicographic (k \/ k') v'

instance (NFData k, NFData v) => NFData (Lexicographic k v) where
  rnf (Lexicographic k v) = rnf k `seq` rnf v

instance (Hashable k, Hashable v) => Hashable (Lexicographic k v)

instance (PartialOrd k, JoinSemiLattice k, JoinSemiLattice v) => JoinSemiLattice (Lexicographic k v) where
  l@(Lexicographic k1 v1) \/ r@(Lexicographic k2 v2)
    | k1 `leq` k2 = r
    | k2 `leq` k1 = l
    | otherwise   = Lexicographic (k1 \/ k2) (v1 \/ v2)

instance (PartialOrd k, MeetSemiLattice k, MeetSemiLattice v) => MeetSemiLattice (Lexicographic k v) where
  l@(Lexicographic k1 v1) /\ r@(Lexicographic k2 v2)
    | k1 `leq` k2 = l
    | k2 `leq` k1 = r
    | otherwise   = Lexicographic (k1 /\ k2) (v1 /\ v2)

instance (PartialOrd k, Lattice k, Lattice v) => Lattice (Lexicographic k v) where

instance (PartialOrd k, BoundedJoinSemiLattice k, BoundedJoinSemiLattice v) => BoundedJoinSemiLattice (Lexicographic k v) where
  bottom = Lexicographic bottom bottom

instance (PartialOrd k, BoundedMeetSemiLattice k, BoundedMeetSemiLattice v) => BoundedMeetSemiLattice (Lexicographic k v) where
  top = Lexicographic top top

instance (PartialOrd k, BoundedLattice k, BoundedLattice v) => BoundedLattice (Lexicographic k v) where

instance (PartialOrd k, PartialOrd v) => PartialOrd (Lexicographic k v) where
  Lexicographic k1 v1 `leq` Lexicographic k2 v2
    | k1 `leq` k2 = True
    | k1   ==  k1 = v1 `leq` v2
    | otherwise   = False -- Incomparable or k2 `leq` k1
