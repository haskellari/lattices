{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Algebra.Lattice.Replicated (
    Replicated(..)
  ) where

import Prelude ()
import Prelude.Compat

import Algebra.Lattice
import Algebra.PartialOrd

import Control.DeepSeq       (NFData (..))
import Control.Monad         (ap, liftM2)
import Data.Data             (Data, Typeable)
import Data.Hashable         (Hashable (..))
import Data.Universe.Class   (Finite (..), Universe (..))
import Data.Universe.Helpers (Natural, Tagged, retag, (+++))
import GHC.Generics          (Generic, Generic1)

import qualified Test.QuickCheck as QC

--
-- Replicated
--

-- | Replicated lattice according to another lattice. 
-- | All corresponding elements of first lattice replicas are ordered according to second lattice.
data Replicated a b = Replicated a b
    deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic, Functor, Foldable, Traversable
            , Generic1
            )

instance (NFData a, NFData b) => NFData (Replicated a b) where
    rnf (Replicated a b) = rnf (rnf a, rnf b) -- TODO

instance (Hashable a, Hashable b) => Hashable (Replicated a b)

instance (PartialOrd a, PartialOrd b) => PartialOrd (Replicated a b) where
    leq (Replicated xa xb) (Replicated ya yb) = leq xa ya && leq xb yb

instance (Lattice a, Lattice b) => Lattice (Replicated a b) where
    (Replicated xa xb) \/ (Replicated ya yb) = Replicated (xa \/ ya) (xb \/ yb)
    (Replicated xa xb) /\ (Replicated ya yb) = Replicated (xa /\ ya) (xb /\ yb)

instance (BoundedJoinSemiLattice a, BoundedJoinSemiLattice b) => BoundedJoinSemiLattice (Replicated a b) where
    bottom = Replicated bottom bottom

instance (BoundedMeetSemiLattice a, BoundedMeetSemiLattice b) => BoundedMeetSemiLattice (Replicated a b) where
    top = Replicated top top

instance (Universe a, Universe b) => Universe (Replicated a b) where
    universe = [Replicated a b | a <- universe, b <- universe]

instance (Finite a, Finite b) => Finite (Replicated a b) where
    -- ?

instance (QC.Arbitrary a, QC.Arbitrary b) => QC.Arbitrary (Replicated a b) where
    arbitrary = Replicated <$> QC.arbitrary <*> QC.arbitrary
    shrink (Replicated a b) = Replicated <$> QC.shrink a <*> QC.shrink b
