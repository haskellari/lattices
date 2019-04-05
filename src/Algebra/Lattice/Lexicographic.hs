{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE Safe               #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.Lattice.Lexicographic
-- Copyright   :  (C) 2010-2015 Maximilian Bolingbroke, 2015-2019 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Algebra.Lattice.Lexicographic (
    Lexicographic(..)
  ) where

import Prelude ()
import Prelude.Compat

import Algebra.Lattice
import Algebra.PartialOrd

import Control.DeepSeq     (NFData (..))
import Control.Monad       (ap)
import Data.Data           (Data, Typeable)
import Data.Hashable       (Hashable (..))
import Data.Universe.Class (Finite (..), Universe (..))
import Data.Universe.Instances.Base ()
import GHC.Generics        (Generic, Generic1)

import qualified Test.QuickCheck as QC

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
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic, Functor, Foldable, Traversable
           , Generic1
           )

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

-- Why we have 'bottom', and not @v1 \\/ v2@ in the @otherwise@ clause?
--
-- For example what is the join of @(2, 1)@ and @(3, 2)@
-- in lexicographic divisibility divisibility lattice.
--
-- With @v1 \\/ v2@, we get the upper bound, but not least!
--
-- @
-- (2, 1) `leq` (6, 2)
-- (3, 2) `leq` (6, 2)
-- @
--
-- But @(6, 1) `leq` (6, 2)@, and
--
-- @
-- (2, 1) `leq` (6, 1)
-- (3, 2) `leq` (6, 1)
-- @
--
instance (PartialOrd k, Lattice k, BoundedJoinSemiLattice v, BoundedMeetSemiLattice v) => Lattice (Lexicographic k v) where
  l@(Lexicographic k1 v1) \/ r@(Lexicographic k2 v2)
    | k1 == k2 = Lexicographic k1 (v1 \/ v2)
    | k1 `leq` k2 = r
    | k2 `leq` k1 = l
    | otherwise   = Lexicographic (k1 \/ k2) bottom

  l@(Lexicographic k1 v1) /\ r@(Lexicographic k2 v2)
    | k1 == k2 = Lexicographic k1 (v1 /\ v2)
    | k1 `leq` k2 = l
    | k2 `leq` k1 = r
    | otherwise   = Lexicographic (k1 /\ k2) top

instance (PartialOrd k, BoundedJoinSemiLattice k, BoundedJoinSemiLattice v, BoundedMeetSemiLattice v) => BoundedJoinSemiLattice (Lexicographic k v) where
  bottom = Lexicographic bottom bottom

instance (PartialOrd k, BoundedMeetSemiLattice k, BoundedJoinSemiLattice v, BoundedMeetSemiLattice v) => BoundedMeetSemiLattice (Lexicographic k v) where
  top = Lexicographic top top

instance (PartialOrd k, PartialOrd v) => PartialOrd (Lexicographic k v) where
  Lexicographic k1 v1 `leq` Lexicographic k2 v2
    | k1   ==  k2 = v1 `leq` v2
    | k1 `leq` k2 = True
    | otherwise   = False -- Incomparable or k2 `leq` k1
  comparable (Lexicographic k1 v1) (Lexicographic k2 v2)
    | k1 == k2 = comparable v1 v2
    | otherwise = comparable k1 k2

instance (Universe k, Universe v) => Universe (Lexicographic k v) where
    universe = map (uncurry Lexicographic) universe
instance (Finite k, Finite v) => Finite (Lexicographic k v) where
    universeF = map (uncurry Lexicographic) universeF

instance (QC.Arbitrary k, QC.Arbitrary v) => QC.Arbitrary (Lexicographic k v) where
    arbitrary = uncurry Lexicographic <$> QC.arbitrary
    shrink (Lexicographic k v) = uncurry Lexicographic <$> QC.shrink (k, v)

instance (QC.CoArbitrary k, QC.CoArbitrary v) => QC.CoArbitrary (Lexicographic k v) where
    coarbitrary (Lexicographic k v) = QC.coarbitrary (k, v)

instance (QC.Function k, QC.Function v) => QC.Function (Lexicographic k v) where
    function = QC.functionMap (\(Lexicographic k v) -> (k,v)) (uncurry Lexicographic)
