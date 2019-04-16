{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.Lattice.Ordered
-- Copyright   :  (C) 2010-2015 Maximilian Bolingbroke, 2015-2019 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Algebra.Lattice.Ordered (
    Ordered(..)
  ) where

import Prelude ()
import Prelude.Compat

import Algebra.Heyting
import Algebra.Lattice
import Algebra.PartialOrd

import Control.DeepSeq       (NFData (..))
import Control.Monad         (ap)
import Data.Data             (Data, Typeable)
import Data.Hashable         (Hashable (..))
import Data.Universe.Class   (Finite (..), Universe (..))
import Data.Universe.Helpers (Natural, Tagged, retag)
import GHC.Generics          (Generic, Generic1)

import qualified Test.QuickCheck as QC

--
-- Ordered
--

-- | A total order gives rise to a lattice. Join is
-- max, meet is min.
newtype Ordered a = Ordered { getOrdered :: a }
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic, Functor, Foldable, Traversable
           , Generic1
           )

instance Applicative Ordered where
  pure = return
  (<*>) = ap

instance Monad Ordered where
  return           = Ordered
  Ordered x >>= f  = f x

instance NFData a => NFData (Ordered a) where
  rnf (Ordered a) = rnf a

instance Hashable a => Hashable (Ordered a)

instance Ord a => Lattice (Ordered a) where
  Ordered x \/ Ordered y = Ordered (max x y)
  Ordered x /\ Ordered y = Ordered (min x y)

instance (Ord a, Bounded a) => BoundedJoinSemiLattice (Ordered a) where
  bottom = Ordered minBound

instance (Ord a, Bounded a) => BoundedMeetSemiLattice (Ordered a) where
  top = Ordered maxBound

-- | This is interesting logic, as it satisfies both de Morgan laws;
-- but isn't Boolean: i.e. law of exluded middle doesn't hold.
--
-- Negation "smashes" value into 'minBound' or 'maxBound'.
instance (Ord a, Bounded a) => Heyting (Ordered a) where
    x ==> y | x > y     = y
            | otherwise = top

instance Ord a => PartialOrd (Ordered a) where
    leq = (<=)
    comparable _ _ = True

instance Universe a => Universe (Ordered a) where
    universe = map Ordered universe
instance Finite a => Finite (Ordered a) where
    universeF = map Ordered universeF
    cardinality = retag (cardinality :: Tagged a Natural)

instance QC.Arbitrary a => QC.Arbitrary (Ordered a) where
    arbitrary = Ordered <$> QC.arbitrary
    shrink    = QC.shrinkMap Ordered getOrdered

instance QC.CoArbitrary a => QC.CoArbitrary (Ordered a) where
    coarbitrary = QC.coarbitrary . getOrdered

instance QC.Function a => QC.Function (Ordered a) where
    function = QC.functionMap getOrdered Ordered
