{-# LANGUAGE Safe #-}
{-# LANGUAGE DefaultSignatures #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.PartialOrd
-- Copyright   :  (C) 2010-2015 Maximilian Bolingbroke, 2015-2019 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Algebra.PartialOrd (
    -- * Partial orderings
    PartialOrd(..),
    partialOrdEq,

    -- * Fixed points of chains in partial orders
    lfpFrom, unsafeLfpFrom,
    gfpFrom, unsafeGfpFrom
  ) where

import           Data.Foldable     (Foldable (..))
import           Data.Hashable     (Hashable (..))
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet      as HS
import qualified Data.IntMap       as IM
import qualified Data.IntSet       as IS
import qualified Data.List.Compat  as L
import qualified Data.Map          as Map
import           Data.Monoid       (All (..), Any (..))
import qualified Data.Set          as Set
import           Data.Void         (Void)

-- | A partial ordering on sets
-- (<http://en.wikipedia.org/wiki/Partially_ordered_set>) is a set equipped
-- with a binary relation, `leq`, that obeys the following laws
--
-- @
-- Reflexive:     a ``leq`` a
-- Antisymmetric: a ``leq`` b && b ``leq`` a ==> a == b
-- Transitive:    a ``leq`` b && b ``leq`` c ==> a ``leq`` c
-- @
--
-- Two elements of the set are said to be `comparable` when they are are
-- ordered with respect to the `leq` relation. So
--
-- @
-- `comparable` a b ==> a ``leq`` b || b ``leq`` a
-- @
--
-- If `comparable` always returns true then the relation `leq` defines a
-- total ordering (and an `Ord` instance may be defined). Any `Ord` instance is
-- trivially an instance of `PartialOrd`. 'Algebra.Lattice.Ordered' provides a
-- convenient wrapper to satisfy 'PartialOrd' given 'Ord'.
--
-- As an example consider the partial ordering on sets induced by set
-- inclusion.  Then for sets `a` and `b`,
--
-- @
-- a ``leq`` b
-- @
--
-- is true when `a` is a subset of `b`.  Two sets are `comparable` if one is a
-- subset of the other. Concretely
--
-- @
-- a = {1, 2, 3}
-- b = {1, 3, 4}
-- c = {1, 2}
--
-- a ``leq`` a = `True`
-- a ``leq`` b = `False`
-- a ``leq`` c = `False`
-- b ``leq`` a = `False`
-- b ``leq`` b = `True`
-- b ``leq`` c = `False`
-- c ``leq`` a = `True`
-- c ``leq`` b = `False`
-- c ``leq`` c = `True`
--
-- `comparable` a b = `False`
-- `comparable` a c = `True`
-- `comparable` b c = `False`
-- @
class Eq a => PartialOrd a where
    -- | The relation that induces the partial ordering
    leq :: a -> a -> Bool
    {-# INLINE leq #-}
    default leq :: Ord a => a -> a -> Bool
    leq = (<=)
    -- | Whether two elements are ordered with respect to the relation. A
    -- default implementation is given by
    --
    -- @
    -- 'comparable' x y = 'leq' x y '||' 'leq' y x
    -- @
    comparable :: a -> a -> Bool
    comparable x y = leq x y || leq y x

-- | The equality relation induced by the partial-order structure. It satisfies
-- the laws of an equivalence relation:
-- @
-- Reflexive:  a == a
-- Symmetric:  a == b ==> b == a
-- Transitive: a == b && b == c ==> a == c
-- @
partialOrdEq :: PartialOrd a => a -> a -> Bool
partialOrdEq x y = leq x y && leq y x

instance PartialOrd () where
    leq _ _ = True

-- | @since 2
instance PartialOrd Bool where
    leq = (<=)

instance PartialOrd Any where
    leq = (<=)

instance PartialOrd All where
    leq = (<=)

instance PartialOrd Void where
    leq _ _ = True

-- | @'leq' = 'Data.List.isSequenceOf'@.
instance Eq a => PartialOrd [a] where
    leq = L.isSubsequenceOf

instance Ord a => PartialOrd (Set.Set a) where
    leq = Set.isSubsetOf

instance PartialOrd IS.IntSet where
    leq = IS.isSubsetOf

instance (Eq k, Hashable k) => PartialOrd (HS.HashSet k) where
    leq a b = HS.null (HS.difference a b)

instance (Ord k, PartialOrd v) => PartialOrd (Map.Map k v) where
    leq = Map.isSubmapOfBy leq

instance PartialOrd v => PartialOrd (IM.IntMap v) where
    leq = IM.isSubmapOfBy leq

instance (Eq k, Hashable k, PartialOrd v) => PartialOrd (HM.HashMap k v) where
    x `leq` y = {- wish: HM.isSubmapOfBy leq -}
        HM.null (HM.difference x y) && getAll (fold $ HM.intersectionWith (\vx vy -> All (vx `leq` vy)) x y)

instance (PartialOrd a, PartialOrd b) => PartialOrd (a, b) where
    -- NB: *not* a lexical ordering. This is because for some component partial orders, lexical
    -- ordering is incompatible with the transitivity axiom we require for the derived partial order
    (x1, y1) `leq` (x2, y2) = x1 `leq` x2 && y1 `leq` y2

    comparable (x1, y1) (x2, y2) = comparable x1 x2 && comparable y1 y2

-- | @since 2.0.1
instance (PartialOrd a, PartialOrd b) => PartialOrd (Either a b) where
    Left x  `leq` Left y  = leq x y
    Right x `leq` Right y = leq x y
    leq _ _ = False

    comparable (Left x)  (Left y)  = comparable x y
    comparable (Right x) (Right y) = comparable x y
    comparable _ _ = False

-- | Least point of a partially ordered monotone function. Checks that the function is monotone.
lfpFrom :: PartialOrd a => a -> (a -> a) -> a
lfpFrom = lfpFrom' leq

-- | Least point of a partially ordered monotone function. Does not checks that the function is monotone.
unsafeLfpFrom :: Eq a => a -> (a -> a) -> a
unsafeLfpFrom = lfpFrom' (\_ _ -> True)

{-# INLINE lfpFrom' #-}
lfpFrom' :: Eq a => (a -> a -> Bool) -> a -> (a -> a) -> a
lfpFrom' check init_x f = go init_x
  where go x | x' == x      = x
             | x `check` x' = go x'
             | otherwise    = error "lfpFrom: non-monotone function"
          where x' = f x


-- | Greatest fixed point of a partially ordered antinone function. Checks that the function is antinone.
{-# INLINE gfpFrom #-}
gfpFrom :: PartialOrd a => a -> (a -> a) -> a
gfpFrom = gfpFrom' leq

-- | Greatest fixed point of a partially ordered antinone function. Does not check that the function is antinone.
{-# INLINE unsafeGfpFrom #-}
unsafeGfpFrom :: Eq a => a -> (a -> a) -> a
unsafeGfpFrom = gfpFrom' (\_ _ -> True)

{-# INLINE gfpFrom' #-}
gfpFrom' :: Eq a => (a -> a -> Bool) -> a -> (a -> a) -> a
gfpFrom' check init_x f = go init_x
  where go x | x' == x      = x
             | x' `check` x = go x'
             | otherwise    = error "gfpFrom: non-antinone function"
          where x' = f x
