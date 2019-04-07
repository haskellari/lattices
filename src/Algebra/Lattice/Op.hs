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
-- Module      :  Algebra.Lattice.Op
-- Copyright   :  (C) 2010-2015 Maximilian Bolingbroke, 2015-2019 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Algebra.Lattice.Op (
    Op(..)
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
import GHC.Generics        (Generic, Generic1)

import qualified Test.QuickCheck as QC

--
-- Op
--

-- | The opposite lattice of a given lattice.  That is, switch
-- meets and joins.
newtype Op a = Op { getOp :: a }
  deriving ( Eq, Show, Read, Data, Typeable, Generic, Functor, Foldable, Traversable
           , Generic1
           )

instance Ord a => Ord (Op a) where
  compare (Op a) (Op b) = compare b a

instance Applicative Op where
  pure = return
  (<*>) = ap

instance Monad Op where
  return      = Op
  Op x >>= f  = f x

instance NFData a => NFData (Op a) where
  rnf (Op a) = rnf a

instance Hashable a => Hashable (Op a)

instance Lattice a => Lattice (Op a) where
  Op x \/ Op y = Op (x /\ y)
  Op x /\ Op y = Op (x \/ y)

instance BoundedMeetSemiLattice a => BoundedJoinSemiLattice (Op a) where
  bottom = Op top

instance BoundedJoinSemiLattice a => BoundedMeetSemiLattice (Op a) where
  top = Op bottom

instance PartialOrd a => PartialOrd (Op a) where
    Op a `leq` Op b = b `leq` a -- Note swap.
    comparable (Op a) (Op b) = comparable a b

instance Universe a => Universe (Op a) where
    universe = map Op universe
instance Finite a => Finite (Op a) where
    universeF = map Op universeF

instance QC.Arbitrary a => QC.Arbitrary (Op a) where
    arbitrary = Op <$> QC.arbitrary
    shrink    = QC.shrinkMap getOp Op

instance QC.CoArbitrary a => QC.CoArbitrary (Op a) where
    coarbitrary = QC.coarbitrary . getOp

instance QC.Function a => QC.Function (Op a) where
    function = QC.functionMap getOp Op
