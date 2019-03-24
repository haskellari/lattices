{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
#if __GLASGOW_HASKELL__ < 709
{-# LANGUAGE Trustworthy        #-}
#else
{-# LANGUAGE Safe               #-}
#endif
module Algebra.Lattice.M3 (
    M3 (..),
    ) where

import Prelude ()
import Prelude.Compat

import Algebra.Lattice
import Algebra.PartialOrd

import Data.Data
import GHC.Generics

-- | \(M_3\), is smallest non-distributive, yet modular lattice.
--
-- TODO: picture
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
