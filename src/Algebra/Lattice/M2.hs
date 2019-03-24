{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
#if __GLASGOW_HASKELL__ < 709
{-# LANGUAGE Trustworthy        #-}
#else
{-# LANGUAGE Safe               #-}
#endif
module Algebra.Lattice.M2 (
    M2 (..),
    toSetBool,
    fromSetBool,
    ) where

import Prelude ()
import Prelude.Compat

import Algebra.Lattice
import Algebra.PartialOrd

import Data.Data
import GHC.Generics

import Data.Set (Set)
import qualified Data.Set as Set

-- | \(M_2\) is isomorphic to \(\mathcal{P}\{\mathbb{B}\}\), i.e. powerset of 'Bool'.
--
-- TODO: picture
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
