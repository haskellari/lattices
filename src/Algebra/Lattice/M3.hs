{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE Safe               #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.Lattice.M3
-- Copyright   :  (C) 2019 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Algebra.Lattice.M3 (
    M3 (..),
    ) where

import Control.DeepSeq     (NFData (..))
import Data.Data           (Data, Typeable)
import Data.Hashable       (Hashable (..))
import Data.Universe.Class (Finite (..), Universe (..))
import GHC.Generics        (Generic)

import qualified Test.QuickCheck as QC

import Algebra.Lattice
import Algebra.PartialOrd

-- | \(M_3\), is smallest non-distributive, yet modular lattice.
--
-- <<m3.png>>
--
data M3 = M3o | M3a | M3b | M3c | M3i
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Data, Generic)

instance PartialOrd M3 where
    M3o `leq` _   = True
    _   `leq` M3i = True
    M3a `leq` M3a = True
    M3b `leq` M3b = True
    M3c `leq` M3c = True
    _   `leq` _   = False

instance Lattice M3 where
    M3o \/ y   = y
    M3i \/ _   = M3i
    x   \/ M3o = x
    _   \/ M3i = M3i
    M3a \/ M3a = M3a
    M3b \/ M3b = M3b
    M3c \/ M3c = M3c
    _   \/ _   = M3i

    M3o /\ _   = M3o
    M3i /\ y   = y
    _   /\ M3o = M3o
    x   /\ M3i = x
    M3a /\ M3a = M3a
    M3b /\ M3b = M3b
    M3c /\ M3c = M3c
    _   /\ _   = M3o

instance BoundedJoinSemiLattice M3 where
    bottom = M3o

instance BoundedMeetSemiLattice M3 where
    top = M3i

instance QC.Arbitrary M3 where
    arbitrary = QC.arbitraryBoundedEnum
    shrink x | x == minBound = []
             | otherwise     = [minBound .. pred x]

instance QC.CoArbitrary M3 where
    coarbitrary = QC.coarbitraryEnum

instance QC.Function M3 where
    function = QC.functionBoundedEnum

instance Universe M3 where universe = [minBound .. maxBound]
instance Finite M3 where cardinality = 5

instance NFData M3 where
    rnf x = x `seq` ()

instance Hashable M3 where
    hashWithSalt salt = hashWithSalt salt . fromEnum
