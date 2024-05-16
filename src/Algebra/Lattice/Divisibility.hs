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
-- Module      :  Algebra.Lattice.Divisibility
-- Copyright   :  (C) 2010-2015 Maximilian Bolingbroke, 2015-2019 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Algebra.Lattice.Divisibility (
    Divisibility(..)
  ) where

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
-- Divisibility
--

-- | A divisibility lattice. @'join' = 'lcm'@, @'meet' = 'gcd'@.
newtype Divisibility a = Divisibility { getDivisibility :: a }
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic, Functor, Foldable, Traversable
           , Generic1
           )

instance Applicative Divisibility where
  pure = return
  (<*>) = ap

instance Monad Divisibility where
  return           = Divisibility
  Divisibility x >>= f  = f x

instance NFData a => NFData (Divisibility a) where
  rnf (Divisibility a) = rnf a

instance Hashable a => Hashable (Divisibility a)

instance Integral a => Lattice (Divisibility a) where
  Divisibility x \/ Divisibility y = Divisibility (lcm x y)

  Divisibility x /\ Divisibility y = Divisibility (gcd x y)

instance Integral a => BoundedJoinSemiLattice (Divisibility a) where
  bottom = Divisibility 1

instance (Eq a, Integral a) => PartialOrd (Divisibility a) where
    leq (Divisibility a) (Divisibility b) = b `mod` a == 0

instance Universe a => Universe (Divisibility a) where
    universe = map Divisibility universe
instance Finite a => Finite (Divisibility a) where
    universeF = map Divisibility universeF
    cardinality = retag (cardinality :: Tagged a Natural)

instance (QC.Arbitrary a, Num a, Ord a) => QC.Arbitrary (Divisibility a) where
    arbitrary = divisibility <$> QC.arbitrary
    shrink d = filter (<d) . map divisibility . QC.shrink . getDivisibility $ d

instance QC.CoArbitrary a => QC.CoArbitrary (Divisibility a) where
    coarbitrary = QC.coarbitrary . getDivisibility

instance QC.Function a => QC.Function (Divisibility a) where
    function = QC.functionMap getDivisibility Divisibility

divisibility :: (Ord a, Num a) => a -> Divisibility a
divisibility x | x < (-1)  = Divisibility (abs x)
               | x < 1     = Divisibility 1
               | otherwise = Divisibility x
