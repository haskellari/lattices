{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Trustworthy #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.Lattice
-- Copyright   :  (C) 2010-2015 Maximilian Bolingbroke
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- In mathematics, a lattice is a partially ordered set in which every
-- two elements have a unique supremum (also called a least upper bound
-- or @join@) and a unique infimum (also called a greatest lower bound or
-- @meet@).
--
-- In this module lattices are defined using `meet` and `join` operators,
-- as it's constructive one.
--
----------------------------------------------------------------------------
module Algebra.Lattice (
    -- * Unbounded lattices
    JoinSemiLattice(..), MeetSemiLattice(..), Lattice,
    joinLeq, joins1, meetLeq, meets1,

    -- * Bounded lattices
    BoundedJoinSemiLattice(..), BoundedMeetSemiLattice(..), BoundedLattice,
    joins, meets,

    -- * Fixed points of chains in lattices
    lfp, lfpFrom, unsafeLfp,
    gfp, gfpFrom, unsafeGfp,
  ) where

import Algebra.Enumerable
import qualified Algebra.PartialOrd as PO

import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.IntMap as IM

import Data.Hashable
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM

infixr 6 /\
infixr 5 \/

-- | A algebraic structure with element joins: <http://en.wikipedia.org/wiki/Semilattice>
--
-- @
-- Associativity: x `join` (y `join` z) == (x `join` y) `join` z
-- Commutativity: x `join` y == y `join` x
-- Idempotency:   x `join` x == x
-- @
class JoinSemiLattice a where
    (\/) :: a -> a -> a

-- | The partial ordering induced by the join-semilattice structure
joinLeq :: (Eq a, JoinSemiLattice a) => a -> a -> Bool
joinLeq x y = (x \/ y) == y

-- | The join of at a list of join-semilattice elements (of length at least one)
joins1 :: JoinSemiLattice a => [a] -> a
joins1 = foldr1 (\/)

-- | A algebraic structure with element meets: <http://en.wikipedia.org/wiki/Semilattice>
--
-- @
-- Associativity: x `meet` (y `meet` z) == (x `meet` y) `meet` z
-- Commutativity: x `meet` y == y `meet` x
-- Idempotency:   x `meet` x == x
-- @
class MeetSemiLattice a where
    (/\) :: a -> a -> a

-- | The partial ordering induced by the meet-semilattice structure
meetLeq :: (Eq a, MeetSemiLattice a) => a -> a -> Bool
meetLeq x y = (x /\ y) == x

-- | The meet of at a list of meet-semilattice elements (of length at least one)
meets1 :: MeetSemiLattice a => [a] -> a
meets1 = foldr1 (/\)

-- | The combination of two semi lattices makes a lattice if the absorption law holds:
-- see <http://en.wikipedia.org/wiki/Absorption_law> and <http://en.wikipedia.org/wiki/Lattice_(order)>
--
-- @
-- Absorption: a `join` (a `meet` b) == a `meet` (a `join` b) == a
-- @
class (JoinSemiLattice a, MeetSemiLattice a) => Lattice a where

-- | A join-semilattice with some element |bottom| that `join` approaches.
--
-- @
-- Identity: x `join` bottom == x
-- @
class JoinSemiLattice a => BoundedJoinSemiLattice a where
    bottom :: a

-- | The join of a list of join-semilattice elements
joins :: BoundedJoinSemiLattice a => [a] -> a
joins = foldr (\/) bottom

-- | A meet-semilattice with some element |top| that `meet` approaches.
--
-- @
-- Identity: x `meet` top == x
-- @
class MeetSemiLattice a => BoundedMeetSemiLattice a where
    top :: a

-- | The meet of a list of meet-semilattice elements
meets :: BoundedMeetSemiLattice a => [a] -> a
meets = foldr (/\) top


-- | Lattices with both bounds
class (Lattice a, BoundedJoinSemiLattice a, BoundedMeetSemiLattice a) => BoundedLattice a where


--
-- Sets
--

instance Ord a => JoinSemiLattice (S.Set a) where
    (\/) = S.union

instance (Ord a, Enumerable a) => MeetSemiLattice (S.Set (Enumerated a)) where
    (/\) = S.intersection

instance (Ord a, Enumerable a) => Lattice (S.Set (Enumerated a)) where

instance Ord a => BoundedJoinSemiLattice (S.Set a) where
    bottom = S.empty

instance (Ord a, Enumerable a) => BoundedMeetSemiLattice (S.Set (Enumerated a)) where
    top = S.fromList universe

instance (Ord a, Enumerable a) => BoundedLattice (S.Set (Enumerated a)) where

--
-- IntSets
--

instance JoinSemiLattice IS.IntSet where
    (\/) = IS.union

instance BoundedJoinSemiLattice IS.IntSet where
    bottom = IS.empty

--
-- HashSet
--

instance (Eq a, Hashable a) => JoinSemiLattice (HS.HashSet a) where
    (\/) = HS.union

instance (Eq a, Hashable a) => MeetSemiLattice (HS.HashSet a) where
    (/\) = HS.intersection

instance (Eq a, Hashable a) => BoundedJoinSemiLattice (HS.HashSet a) where
    bottom = HS.empty

--
-- Maps
--

instance (Ord k, JoinSemiLattice v) => JoinSemiLattice (M.Map k v) where
    (\/) = M.unionWith (\/)

instance (Ord k, Enumerable k, MeetSemiLattice v) => MeetSemiLattice (M.Map (Enumerated k) v) where
    (/\) = M.intersectionWith (/\)

instance (Ord k, Enumerable k, Lattice v) => Lattice (M.Map (Enumerated k) v) where

instance (Ord k, JoinSemiLattice v) => BoundedJoinSemiLattice (M.Map k v) where
    bottom = M.empty

instance (Ord k, Enumerable k, BoundedMeetSemiLattice v) => BoundedMeetSemiLattice (M.Map (Enumerated k) v) where
    top = M.fromList (universe `zip` repeat top)

instance (Ord k, Enumerable k, BoundedLattice v) => BoundedLattice (M.Map (Enumerated k) v) where

--
-- IntMaps
--

instance JoinSemiLattice v => JoinSemiLattice (IM.IntMap v) where
    (\/) = IM.unionWith (\/)

instance JoinSemiLattice v => BoundedJoinSemiLattice (IM.IntMap v) where
    bottom = IM.empty

--
-- HashMaps
--

instance (Eq k, Hashable k) => JoinSemiLattice (HM.HashMap k v) where
    (\/) = HM.union

instance (Eq k, Hashable k) => MeetSemiLattice (HM.HashMap k v) where
    (/\) = HM.intersection

instance (Eq k, Hashable k) => BoundedJoinSemiLattice (HM.HashMap k v) where
    bottom = HM.empty

--
-- Functions
--

instance JoinSemiLattice v => JoinSemiLattice (k -> v) where
    f \/ g = \x -> f x \/ g x

instance MeetSemiLattice v => MeetSemiLattice (k -> v) where
    f /\ g = \x -> f x /\ g x

instance Lattice v => Lattice (k -> v) where

instance BoundedJoinSemiLattice v => BoundedJoinSemiLattice (k -> v) where
    bottom = const bottom

instance BoundedMeetSemiLattice v => BoundedMeetSemiLattice (k -> v) where
    top = const top

instance BoundedLattice v => BoundedLattice (k -> v) where

--
-- Tuples
--

instance (JoinSemiLattice a, JoinSemiLattice b) => JoinSemiLattice (a, b) where
    (x1, y1) \/ (x2, y2) = (x1 \/ x2, y1 \/ y2)

instance (MeetSemiLattice a, MeetSemiLattice b) => MeetSemiLattice (a, b) where
    (x1, y1) /\ (x2, y2) = (x1 /\ x2, y1 /\ y2)

instance (Lattice a, Lattice b) => Lattice (a, b) where

instance (BoundedJoinSemiLattice a, BoundedJoinSemiLattice b) => BoundedJoinSemiLattice (a, b) where
    bottom = (bottom, bottom)

instance (BoundedMeetSemiLattice a, BoundedMeetSemiLattice b) => BoundedMeetSemiLattice (a, b) where
    top = (top, top)

instance (BoundedLattice a, BoundedLattice b) => BoundedLattice (a, b) where

--
-- Bools
--

instance JoinSemiLattice Bool where
    (\/) = (||)

instance MeetSemiLattice Bool where
    (/\) = (&&)

instance Lattice Bool where

instance BoundedJoinSemiLattice Bool where
    bottom = False

instance BoundedMeetSemiLattice Bool where
    top = True

instance BoundedLattice Bool where


-- | Implementation of Kleene fixed-point theorem <http://en.wikipedia.org/wiki/Kleene_fixed-point_theorem>.
-- Assumes that the function is monotone and does not check if that is correct.
{-# INLINE unsafeLfp #-}
unsafeLfp :: (Eq a, BoundedJoinSemiLattice a) => (a -> a) -> a
unsafeLfp = PO.unsafeLfpFrom bottom

-- | Implementation of Kleene fixed-point theorem <http://en.wikipedia.org/wiki/Kleene_fixed-point_theorem>.
-- Forces the function to be monotone.
{-# INLINE lfp #-}
lfp :: (Eq a, BoundedJoinSemiLattice a) => (a -> a) -> a
lfp = lfpFrom bottom

-- | Implementation of Kleene fixed-point theorem <http://en.wikipedia.org/wiki/Kleene_fixed-point_theorem>.
-- Forces the function to be monotone.
{-# INLINE lfpFrom #-}
lfpFrom :: (Eq a, BoundedJoinSemiLattice a) => a -> (a -> a) -> a
lfpFrom init_x f = PO.unsafeLfpFrom init_x (\x -> f x \/ x)


-- | Implementation of Kleene fixed-point theorem <http://en.wikipedia.org/wiki/Kleene_fixed-point_theorem>.
-- Assumes that the function is antinone and does not check if that is correct.
{-# INLINE unsafeGfp #-}
unsafeGfp :: (Eq a, BoundedMeetSemiLattice a) => (a -> a) -> a
unsafeGfp = PO.unsafeGfpFrom top

-- | Implementation of Kleene fixed-point theorem <http://en.wikipedia.org/wiki/Kleene_fixed-point_theorem>.
-- Forces the function to be antinone.
{-# INLINE gfp #-}
gfp :: (Eq a, BoundedMeetSemiLattice a) => (a -> a) -> a
gfp = gfpFrom top

-- | Implementation of Kleene fixed-point theorem <http://en.wikipedia.org/wiki/Kleene_fixed-point_theorem>.
-- Forces the function to be antinone.
{-# INLINE gfpFrom #-}
gfpFrom :: (Eq a, BoundedMeetSemiLattice a) => a -> (a -> a) -> a
gfpFrom init_x f = PO.unsafeGfpFrom init_x (\x -> f x /\ x)
