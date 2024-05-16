{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE Safe               #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.Lattice.ZeroHalfOne
-- Copyright   :  (C) 2019 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Algebra.Lattice.ZeroHalfOne (
    ZeroHalfOne (..),
    ) where

import Control.DeepSeq     (NFData (..))
import Data.Data           (Data, Typeable)
import Data.Hashable       (Hashable (..))
import Data.Universe.Class (Finite (..), Universe (..))
import GHC.Generics        (Generic)

import qualified Test.QuickCheck as QC

import Algebra.Heyting
import Algebra.Lattice
import Algebra.PartialOrd

-- | The simplest Heyting algebra that is not already a Boolean algebra is the
-- totally ordered set \(\{ 0, \frac{1}{2}, 1 \}\).
--
data ZeroHalfOne = Zero | Half | One
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Data, Generic)

instance PartialOrd ZeroHalfOne where
    leq = (<=)

instance Lattice ZeroHalfOne where
    (\/) = max
    (/\) = min

instance BoundedJoinSemiLattice ZeroHalfOne where
    bottom = Zero

instance BoundedMeetSemiLattice ZeroHalfOne where
    top = One

-- | Not boolean: @'neg' 'Half' '\/' 'Half' = 'Half' /= 'One'@
instance Heyting ZeroHalfOne where
    Zero ==> _    = One
    One  ==> x    = x
    Half ==> Zero = Zero
    Half ==> _    = One

    neg Zero = One
    neg One  = Zero
    neg Half = Zero

instance QC.Arbitrary ZeroHalfOne where
    arbitrary = QC.arbitraryBoundedEnum
    shrink x | x == minBound = []
             | otherwise     = [minBound .. pred x]

instance QC.CoArbitrary ZeroHalfOne where
    coarbitrary = QC.coarbitraryEnum

instance QC.Function ZeroHalfOne where
    function = QC.functionBoundedEnum

instance Universe ZeroHalfOne where universe = [minBound .. maxBound]
instance Finite ZeroHalfOne where cardinality = 3

instance NFData ZeroHalfOne where
    rnf x = x `seq` ()

instance Hashable ZeroHalfOne where
    hashWithSalt salt = hashWithSalt salt . fromEnum
