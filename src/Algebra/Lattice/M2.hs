{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE Safe               #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.Lattice.M2
-- Copyright   :  (C) 2019 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Algebra.Lattice.M2 (
    M2 (..),
    toSetBool,
    fromSetBool,
    ) where

import Prelude ()
import Prelude.Compat

import Control.DeepSeq     (NFData (..))
import Data.Data           (Data, Typeable)
import Data.Hashable       (Hashable (..))
import Data.Universe.Class (Finite (..), Universe (..))
import GHC.Generics        (Generic)

import qualified Test.QuickCheck as QC

import Algebra.Heyting
import Algebra.Lattice
import Algebra.PartialOrd

import           Data.Set (Set)
import qualified Data.Set as Set

-- | \(M_2\) is isomorphic to \(\mathcal{P}\{\mathbb{B}\}\), i.e. powerset of 'Bool'.
--
-- <<m2.png>>
--
data M2 = M2o | M2a | M2b | M2i
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Data, Generic)

instance PartialOrd M2 where
    M2o `leq` _   = True
    _   `leq` M2i = True
    M2a `leq` M2a = True
    M2b `leq` M2b = True
    _   `leq` _   = False

instance Lattice M2 where
    M2o \/ y   = y
    M2i \/ _   = M2i
    x   \/ M2o = x
    _   \/ M2i = M2i
    M2a \/ M2a = M2a
    M2b \/ M2b = M2b
    _   \/ _   = M2i

    M2o /\ _   = M2o
    M2i /\ y   = y
    _   /\ M2o = M2o
    x   /\ M2i = x
    M2a /\ M2a = M2a
    M2b /\ M2b = M2b
    _   /\ _   = M2o

instance BoundedJoinSemiLattice M2 where
    bottom = M2o

instance BoundedMeetSemiLattice M2 where
    top = M2i

instance Heyting M2 where
    M2o ==> _   = M2i
    M2i ==> x   = x

    M2a ==> M2o = M2b
    M2a ==> M2a = M2i
    M2a ==> M2b = M2b
    M2a ==> M2i = M2i

    M2b ==> M2o = M2a
    M2b ==> M2a = M2a
    M2b ==> M2b = M2i
    M2b ==> M2i = M2i

    neg M2o = M2i
    neg M2a = M2b
    neg M2b = M2a
    neg M2i = M2o

toSetBool :: M2 -> Set Bool
toSetBool M2o = mempty
toSetBool M2a = Set.singleton False
toSetBool M2b = Set.singleton True
toSetBool M2i = Set.fromList [True, False]

fromSetBool :: Set Bool -> M2
fromSetBool x = case Set.toList x of
    [False,True] -> M2i
    [False]      -> M2a
    [True]       -> M2b
    _            -> M2o

instance QC.Arbitrary M2 where
    arbitrary = QC.arbitraryBoundedEnum
    shrink x | x == minBound = []
             | otherwise     = [minBound .. pred x]

instance QC.CoArbitrary M2 where
    coarbitrary = QC.coarbitraryEnum

instance QC.Function M2 where
    function = QC.functionBoundedEnum

instance Universe M2 where universe = [minBound .. maxBound]
instance Finite M2 where cardinality = 4

instance NFData M2 where
    rnf x = x `seq` ()

instance Hashable M2 where
    hashWithSalt salt = hashWithSalt salt . fromEnum
