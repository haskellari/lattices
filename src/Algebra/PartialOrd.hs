{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeOperators      #-}
#if __GLASGOW_HASKELL__ < 709
{-# LANGUAGE Trustworthy        #-}
#else
{-# LANGUAGE Safe               #-}
#endif

----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.PartialOrd
-- Copyright   :  (C) 2010-2015 Maximilian Bolingbroke
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
    gfpFrom, unsafeGfpFrom,

    -- * Newtypes
    Partial(Partial, runPartial)
  ) where

import           Control.Applicative
import           Data.Data
import           Data.Foldable     (Foldable (..))
import           Data.Hashable     (Hashable (..))
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet      as HS
import qualified Data.IntMap       as IM
import qualified Data.IntSet       as IS
import qualified Data.List         as L
import qualified Data.Map          as M
import           Data.Monoid       (All (..))
import qualified Data.Set          as S
import           Data.Traversable
import           Data.Void         (Void)
import           GHC.Generics

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

    -- | Whether two elements are ordered with respect to the relation. A
    -- default implementation is given by
    --
    -- > comparable x y = leq x y || leq y x
    comparable :: a -> a -> Bool
    comparable x y = leq x y || leq y x

-- | The equality relation induced by the partial-order structure. It must obey
-- the laws
-- @
-- Reflexive:  a == a
-- Transitive: a == b && b == c ==> a == c
-- @
partialOrdEq :: PartialOrd a => a -> a -> Bool
partialOrdEq x y = leq x y && leq y x

instance PartialOrd () where
    leq _ _ = True

instance PartialOrd Void where
    leq _ _ = True

-- | @'leq' = 'Data.List.isInfixOf'@.
instance Eq a => PartialOrd [a] where
    leq = L.isInfixOf

instance Ord a => PartialOrd (S.Set a) where
    leq = S.isSubsetOf

instance PartialOrd IS.IntSet where
    leq = IS.isSubsetOf

instance (Eq k, Hashable k) => PartialOrd (HS.HashSet k) where
    leq a b = HS.null (HS.difference a b)

instance (Ord k, PartialOrd v) => PartialOrd (M.Map k v) where
    leq = M.isSubmapOfBy leq

instance PartialOrd v => PartialOrd (IM.IntMap v) where
    leq = IM.isSubmapOfBy leq

instance (Eq k, Hashable k, PartialOrd v) => PartialOrd (HM.HashMap k v) where
    x `leq` y = {- wish: HM.isSubmapOfBy leq -}
        HM.null (HM.difference x y) && getAll (fold $ HM.intersectionWith (\vx vy -> All (vx `leq` vy)) x y)

instance (PartialOrd a, PartialOrd b) => PartialOrd (a, b) where
    -- NB: *not* a lexical ordering. This is because for some component partial orders, lexical
    -- ordering is incompatible with the transitivity axiom we require for the derived partial order
    (x1, y1) `leq` (x2, y2) = x1 `leq` x2 && y1 `leq` y2

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

newtype Partial a = Partial { runPartial :: a }
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic, Functor, Foldable, Traversable
#if __GLASGOW_HASKELL__ >= 706
           , Generic1
#endif
           )

instance Ord a => PartialOrd (Partial a) where
  leq = (<=)
  comparable _ _ = True

instance Applicative Partial where
  pure = Partial
  Partial f <*> Partial a = Partial (f a)

instance Monad Partial where
  return = Partial
  Partial a >>= f = f a

