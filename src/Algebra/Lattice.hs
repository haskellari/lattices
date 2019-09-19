{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE Safe               #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.Lattice
-- Copyright   :  (C) 2010-2015 Maximilian Bolingbroke, 2015-2019 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- In mathematics, a lattice is a partially ordered set in which every
-- two elements have a unique supremum (also called a least upper bound
-- or @join@) and a unique infimum (also called a greatest lower bound or
-- @meet@).
--
-- In this module lattices are defined using 'meet' and 'join' operators,
-- as it's constructive one.
--
----------------------------------------------------------------------------
module Algebra.Lattice (
    -- * Unbounded lattices
    Lattice (..),
    joinLeq, joins1, meetLeq, meets1,

    -- * Bounded lattices
    BoundedJoinSemiLattice(..), BoundedMeetSemiLattice(..),
    joins, meets,
    fromBool,
    BoundedLattice,

    -- * Monoid wrappers
    Meet(..), Join(..),

    -- * Fixed points of chains in lattices
    lfp, lfpFrom, unsafeLfp,
    gfp, gfpFrom, unsafeGfp,
  ) where

import Prelude ()
import Prelude.Compat

import qualified Algebra.PartialOrd as PO

import Control.Applicative     (Const (..))
import Control.Monad.Zip       (MonadZip (..))
import Data.Data               (Data, Typeable)
import Data.Functor.Identity   (Identity (..))
import Data.Hashable           (Hashable (..))
import Data.Proxy              (Proxy (..))
import Data.Semigroup          (All (..), Any (..), Endo (..), Semigroup (..))
import Data.Semigroup.Foldable (Foldable1 (..))
import Data.Tagged             (Tagged (..))
import Data.Universe.Class     (Finite (..), Universe (..))
import Data.Void               (Void)
import GHC.Generics            (Generic)

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet      as HS
import qualified Data.IntMap       as IM
import qualified Data.IntSet       as IS
import qualified Data.Map          as Map
import qualified Data.Set          as Set
import qualified Test.QuickCheck   as QC

infixr 6 /\ -- This comment needed because of CPP
infixr 5 \/

-- | An algebraic structure with joins and meets.
--
-- See <http://en.wikipedia.org/wiki/Lattice_(order)> and <http://en.wikipedia.org/wiki/Absorption_law>.
--
-- 'Lattice' is very symmetric, which is seen from the laws:
--
-- /Associativity/
--
-- @
-- x '\/' (y '\/' z) ≡ (x '\/' y) '\/' z
-- x '/\' (y '/\' z) ≡ (x '/\' y) '/\' z
-- @
--
-- /Commputativity/
--
-- @
-- x '\/' y ≡ y '\/' x
-- x '/\' y ≡ y '/\' x
-- @
--
-- /Idempotency/
--
-- @
-- x '\/' x ≡ x
-- x '/\' x ≡ x
-- @
--
-- /Absorption/
--
-- @
-- a '\/' (a '/\' b) ≡ a
-- a '/\' (a '\/' b) ≡ a
-- @
class Lattice a where
    -- | join
    (\/) :: a -> a -> a

    -- | meet
    (/\) :: a -> a -> a

-- | The partial ordering induced by the join-semilattice structure
joinLeq :: (Eq a, Lattice a) => a -> a -> Bool
joinLeq x y = (x \/ y) == y

meetLeq :: (Eq a, Lattice a) => a -> a -> Bool
meetLeq x y = (x /\ y) == x

-- | A join-semilattice with an identity element 'bottom' for '\/'.
--
-- /Laws/
--
-- @
-- x '\/' 'bottom' ≡ x
-- @
--
-- /Corollary/
--
-- @
-- x '/\' 'bottom'
--   ≡⟨ identity ⟩
-- (x '/\' 'bottom') '\/' 'bottom'
--   ≡⟨ absorption ⟩
-- 'bottom'
-- @
class Lattice a => BoundedJoinSemiLattice a where
    bottom :: a

-- | The join of a list of join-semilattice elements
joins :: (BoundedJoinSemiLattice a, Foldable f) => f a -> a
joins = getJoin . foldMap Join

-- | The join of at a list of join-semilattice elements (of length at least one)
joins1 :: (Lattice a, Foldable1 f) => f a -> a
joins1 =  getJoin . foldMap1 Join

-- | A meet-semilattice with an identity element 'top' for '/\'.
--
-- /Laws/
--
-- @
-- x '/\' 'top' ≡ x
-- @
--
-- /Corollary/
--
-- @
-- x '\/' 'top'
--   ≡⟨ identity ⟩
-- (x '\/' 'top') '/\' 'top'
--   ≡⟨ absorption ⟩
-- 'top'
-- @
--
class Lattice a => BoundedMeetSemiLattice a where
    top :: a

-- | The meet of a list of meet-semilattice elements
meets :: (BoundedMeetSemiLattice a, Foldable f) => f a -> a
meets = getMeet . foldMap Meet
--
-- | The meet of at a list of meet-semilattice elements (of length at least one)
meets1 :: (Lattice a, Foldable1 f) => f a -> a
meets1 = getMeet . foldMap1 Meet

type BoundedLattice a = (BoundedMeetSemiLattice a, BoundedJoinSemiLattice a)

-- | 'True' to 'top' and 'False' to 'bottom'
fromBool :: BoundedLattice a => Bool -> a
fromBool True  = top
fromBool False = bottom

--
-- Sets
--

instance Ord a => Lattice (Set.Set a) where
    (\/) = Set.union
    (/\) = Set.intersection

instance Ord a => BoundedJoinSemiLattice (Set.Set a) where
    bottom = Set.empty

instance (Ord a, Finite a) => BoundedMeetSemiLattice (Set.Set a) where
    top = Set.fromList universeF

--
-- IntSets
--

instance Lattice IS.IntSet where
    (\/) = IS.union
    (/\) = IS.intersection

instance BoundedJoinSemiLattice IS.IntSet where
    bottom = IS.empty

--
-- HashSet
--


instance (Eq a, Hashable a) => Lattice (HS.HashSet a) where
    (\/) = HS.union
    (/\) = HS.intersection

instance (Eq a, Hashable a) => BoundedJoinSemiLattice (HS.HashSet a) where
    bottom = HS.empty

instance (Eq a, Hashable a, Finite a) => BoundedMeetSemiLattice (HS.HashSet a) where
    top = HS.fromList universeF

--
-- Maps
--

instance (Ord k, Lattice v) => Lattice (Map.Map k v) where
    (\/) = Map.unionWith (\/)
    (/\) = Map.intersectionWith (/\)

instance (Ord k, Lattice v) => BoundedJoinSemiLattice (Map.Map k v) where
    bottom = Map.empty

instance (Ord k, Finite k, BoundedMeetSemiLattice v) => BoundedMeetSemiLattice (Map.Map k v) where
    top = Map.fromList (universeF `zip` repeat top)

--
-- IntMaps
--

instance Lattice v => Lattice (IM.IntMap v) where
    (\/) = IM.unionWith (\/)
    (/\) = IM.intersectionWith (/\)

instance Lattice v => BoundedJoinSemiLattice (IM.IntMap v) where
    bottom = IM.empty

--
-- HashMaps
--

instance (Eq k, Hashable k, Lattice v) => BoundedJoinSemiLattice (HM.HashMap k v) where
    bottom = HM.empty

instance (Eq k, Hashable k, Lattice v) => Lattice (HM.HashMap k v) where
    (\/) = HM.unionWith (\/)
    (/\) = HM.intersectionWith (/\)

instance (Eq k, Hashable k, Finite k, BoundedMeetSemiLattice v) => BoundedMeetSemiLattice (HM.HashMap k v) where
    top = HM.fromList (universeF `zip` repeat top)

--
-- Functions
--

instance Lattice v => Lattice (k -> v) where
    f \/ g = \x -> f x \/ g x
    f /\ g = \x -> f x /\ g x

instance BoundedJoinSemiLattice v => BoundedJoinSemiLattice (k -> v) where
    bottom = const bottom

instance BoundedMeetSemiLattice v => BoundedMeetSemiLattice (k -> v) where
    top = const top

--
-- Unit
--


instance Lattice () where
    _ \/ _ = ()
    _ /\ _ = ()

instance BoundedJoinSemiLattice () where
  bottom = ()

instance BoundedMeetSemiLattice () where
  top = ()

--
-- Tuples
--

instance (Lattice a, Lattice b) => Lattice (a, b) where
    (x1, y1) \/ (x2, y2) = (x1 \/ x2, y1 \/ y2)
    (x1, y1) /\ (x2, y2) = (x1 /\ x2, y1 /\ y2)

instance (BoundedJoinSemiLattice a, BoundedJoinSemiLattice b) => BoundedJoinSemiLattice (a, b) where
    bottom = (bottom, bottom)

instance (BoundedMeetSemiLattice a, BoundedMeetSemiLattice b) => BoundedMeetSemiLattice (a, b) where
    top = (top, top)

--
-- Bools
--

instance Lattice Bool where
    (\/) = (||)
    (/\) = (&&)

instance BoundedJoinSemiLattice Bool where
    bottom = False

instance BoundedMeetSemiLattice Bool where
    top = True

--- Monoids

-- | Monoid wrapper for join-'Lattice'
newtype Join a = Join { getJoin :: a }
  deriving (Eq, Ord, Read, Show, Bounded, Typeable, Data, Generic)

instance Lattice a => Semigroup (Join a) where
  Join a <> Join b = Join (a \/ b)

instance BoundedJoinSemiLattice a => Monoid (Join a) where
  mempty = Join bottom
  Join a `mappend` Join b = Join (a \/ b)

instance (Eq a, Lattice a) => PO.PartialOrd (Join a) where
  leq (Join a) (Join b) = joinLeq a b

instance Functor Join where
  fmap f (Join x) = Join (f x)

instance Applicative Join where
  pure = Join
  Join f <*> Join x = Join (f x)
  _ *> x = x

instance Monad Join where
  return = pure
  Join m >>= f = f m
  (>>) = (*>)

instance MonadZip Join where
  mzip (Join x) (Join y) = Join (x, y)

instance Universe a => Universe (Join a) where
  universe = fmap Join universe

instance Finite a => Finite (Join a) where
  universeF = fmap Join universeF

-- | Monoid wrapper for meet-'Lattice'
newtype Meet a = Meet { getMeet :: a }
  deriving (Eq, Ord, Read, Show, Bounded, Typeable, Data, Generic)

instance Lattice a => Semigroup (Meet a) where
  Meet a <> Meet b = Meet (a /\ b)

instance BoundedMeetSemiLattice a => Monoid (Meet a) where
  mempty = Meet top
  Meet a `mappend` Meet b = Meet (a /\ b)

instance (Eq a, Lattice a) => PO.PartialOrd (Meet a) where
  leq (Meet a) (Meet b) = meetLeq a b

instance Functor Meet where
  fmap f (Meet x) = Meet (f x)

instance Applicative Meet where
  pure = Meet
  Meet f <*> Meet x = Meet (f x)
  _ *> x = x

instance Monad Meet where
  return = pure
  Meet m >>= f = f m
  (>>) = (*>)

instance MonadZip Meet where
  mzip (Meet x) (Meet y) = Meet (x, y)

instance Universe a => Universe (Meet a) where
  universe = fmap Meet universe

instance Finite a => Finite (Meet a) where
  universeF = fmap Meet universeF

-- All

instance Lattice All where
  All a \/ All b = All $ a \/ b
  All a /\ All b = All $ a /\ b

instance BoundedJoinSemiLattice All where
  bottom = All False

instance BoundedMeetSemiLattice All where
  top = All True

-- Any
instance Lattice Any where
  Any a \/ Any b = Any $ a \/ b
  Any a /\ Any b = Any $ a /\ b

instance BoundedJoinSemiLattice Any where
  bottom = Any False

instance BoundedMeetSemiLattice Any where
  top = Any True

-- Endo
instance Lattice a => Lattice (Endo a) where
  Endo a \/ Endo b = Endo $ a \/ b
  Endo a /\ Endo b = Endo $ a /\ b

instance BoundedJoinSemiLattice a => BoundedJoinSemiLattice (Endo a) where
  bottom = Endo bottom

instance BoundedMeetSemiLattice a => BoundedMeetSemiLattice (Endo a) where
  top = Endo top

-- Tagged

instance Lattice a => Lattice (Tagged t a) where
  Tagged a \/ Tagged b = Tagged $ a \/ b
  Tagged a /\ Tagged b = Tagged $ a /\ b

instance BoundedJoinSemiLattice a => BoundedJoinSemiLattice (Tagged t a) where
  bottom = Tagged bottom

instance BoundedMeetSemiLattice a => BoundedMeetSemiLattice (Tagged t a) where
  top = Tagged top

-- Proxy
instance Lattice (Proxy a) where
  _ \/ _ = Proxy
  _ /\ _ = Proxy

instance BoundedJoinSemiLattice (Proxy a) where
  bottom = Proxy

instance BoundedMeetSemiLattice (Proxy a) where
  top = Proxy

-- Identity

instance Lattice a => Lattice (Identity a) where
  Identity a \/ Identity b = Identity (a \/ b)
  Identity a /\ Identity b = Identity (a /\ b)

instance BoundedMeetSemiLattice a => BoundedMeetSemiLattice (Identity a) where
  top = Identity top

instance BoundedJoinSemiLattice a => BoundedJoinSemiLattice (Identity a) where
  bottom = Identity bottom

-- Const
instance Lattice a => Lattice (Const a b) where
  Const a \/ Const b = Const (a \/ b)
  Const a /\ Const b = Const (a /\ b)

instance BoundedJoinSemiLattice a => BoundedJoinSemiLattice (Const a b) where
  bottom = Const bottom

instance BoundedMeetSemiLattice a => BoundedMeetSemiLattice (Const a b) where
  top = Const top

-------------------------------------------------------------------------------
-- Void
-------------------------------------------------------------------------------

instance Lattice Void where
  a \/ _ = a
  a /\ _ = a

-------------------------------------------------------------------------------
-- QuickCheck
-------------------------------------------------------------------------------

instance Lattice QC.Property where
  (\/) = (QC..||.)
  (/\) = (QC..&&.)

instance BoundedJoinSemiLattice QC.Property where bottom = QC.property False
instance BoundedMeetSemiLattice QC.Property where top = QC.property True

-------------------------------------------------------------------------------
-- Theorems
-------------------------------------------------------------------------------

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
