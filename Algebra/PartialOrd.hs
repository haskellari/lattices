{-# LANGUAGE Safe #-}
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
    gfpFrom, unsafeGfpFrom
  ) where

import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.IntMap as IM


-- | A partial ordering on sets: <http://en.wikipedia.org/wiki/Partially_ordered_set>
--
-- This can be defined using either 'joinLeq' or 'meetLeq', or a more efficient definition
-- can be derived directly.
--
-- @
-- Reflexive:     a `leq` a
-- Antisymmetric: a `leq` b && b `leq` a ==> a == b
-- Transitive:    a `leq` b && b `leq` c ==> a `leq` c
-- @
--
-- The superclass equality (which can be defined using 'partialOrdEq') must obey these laws:
--
-- @
-- Reflexive:  a == a
-- Transitive: a == b && b == c ==> a == c
-- @
class Eq a => PartialOrd a where
    leq :: a -> a -> Bool

    comparable :: a -> a -> Bool
    comparable x y = leq x y || leq y x

-- | The equality relation induced by the partial-order structure
partialOrdEq :: PartialOrd a => a -> a -> Bool
partialOrdEq x y = leq x y && leq y x


instance Ord a => PartialOrd (S.Set a) where
    leq = S.isSubsetOf

instance PartialOrd IS.IntSet where
    leq = IS.isSubsetOf

instance (Ord k, PartialOrd v) => PartialOrd (M.Map k v) where
    m1 `leq` m2 = m1 `M.isSubmapOf` m2 && M.fold (\(x1, x2) b -> b && x1 `leq` x2) True (M.intersectionWith (,) m1 m2)

instance PartialOrd v => PartialOrd (IM.IntMap v) where
    im1 `leq` im2 = im1 `IM.isSubmapOf` im2 && IM.fold (\(x1, x2) b -> b && x1 `leq` x2) True (IM.intersectionWith (,) im1 im2)

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
