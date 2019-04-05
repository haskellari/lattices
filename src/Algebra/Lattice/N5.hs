{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE Safe               #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.Lattice.N5
-- Copyright   :  (C) 2019 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Algebra.Lattice.N5 (
    N5 (..),
    ) where

import Prelude ()
import Prelude.Compat

import Control.DeepSeq     (NFData (..))
import Data.Data           (Data, Typeable)
import Data.Hashable       (Hashable (..))
import Data.Universe.Class (Finite (..), Universe (..))
import GHC.Generics        (Generic)

import qualified Test.QuickCheck as QC

import Algebra.Lattice
import Algebra.PartialOrd

-- | \(N_5\), is smallest non-modular (and non-distributive) lattice.
--
-- <<n5.png>>
--
data N5 = N5o | N5a | N5b | N5c | N5i
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Data, Generic)

instance PartialOrd N5 where
    N5o `leq` _   = True
    _   `leq` N5i = True
    N5a `leq` N5a = True
    N5b `leq` N5a = True
    N5b `leq` N5b = True
    N5c `leq` N5c = True
    _   `leq` _   = False

instance Lattice N5 where
    N5o \/ y   = y
    N5i \/ _   = N5i
    x   \/ N5o = x
    _   \/ N5i = N5i
    N5a \/ N5a = N5a
    N5a \/ N5b = N5a
    N5b \/ N5a = N5a
    N5b \/ N5b = N5b
    N5c \/ N5c = N5c
    _   \/ _   = N5i

    N5o /\ _   = N5o
    N5i /\ y   = y
    _   /\ N5o = N5o
    x   /\ N5i = x
    N5a /\ N5a = N5a
    N5b /\ N5b = N5b
    N5a /\ N5b = N5b
    N5b /\ N5a = N5b
    N5c /\ N5c = N5c
    _   /\ _   = N5o

instance BoundedJoinSemiLattice N5 where
    bottom = N5o

instance BoundedMeetSemiLattice N5 where
    top = N5i

instance QC.Arbitrary N5 where
    arbitrary = QC.arbitraryBoundedEnum
    shrink x | x == minBound = []
             | otherwise     = [minBound .. pred x]

instance QC.CoArbitrary N5 where
    coarbitrary = QC.coarbitraryEnum

instance QC.Function N5 where
    function = QC.functionBoundedEnum

instance Universe N5 where universe = [minBound .. maxBound]
instance Finite N5

instance NFData N5 where
    rnf x = x `seq` ()

instance Hashable N5 where
    hashWithSalt salt = hashWithSalt salt . fromEnum
