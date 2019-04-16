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
-- Module      :  Algebra.Lattice.Fuzzy
-- Copyright   :  (C) 2010-2015 Maximilian Bolingbroke, 2015-2019 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Algebra.Lattice.Fuzzy (
    Fuzzy,
    getFuzzy,
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
-- Fuzzy
--

-- | Fuzzy logic.
--
-- Negation 'neg' is @1 - x@.
newtype Fuzzy a = Fuzzy { getFuzzy :: a }
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic, Functor, Foldable, Traversable
           , Generic1
           )

mkFuzzy :: (Ord a, Num a) => a -> Fuzzy a
mkFuzzy x | x < 0     = Fuzzy 0
          | x > 1     = Fuzzy 1
          | otherwise = Fuzzy x

instance Applicative Fuzzy where
  pure = return
  (<*>) = ap

instance Monad Fuzzy where
  return           = Fuzzy
  Fuzzy x >>= f  = f x

instance NFData a => NFData (Fuzzy a) where
  rnf (Fuzzy a) = rnf a

instance Hashable a => Hashable (Fuzzy a)

instance Ord a => Lattice (Fuzzy a) where
  Fuzzy x \/ Fuzzy y = Fuzzy (max x y)
  Fuzzy x /\ Fuzzy y = Fuzzy (min x y)

instance (Ord a, Num a) => BoundedJoinSemiLattice (Fuzzy a) where
  bottom = Fuzzy 0

instance (Ord a, Num a) => BoundedMeetSemiLattice (Fuzzy a) where
  top = Fuzzy 1

instance (Ord a, Num a) => Heyting (Fuzzy a) where
    x ==> y = neg x \/ y

    neg (Fuzzy x) = Fuzzy (1 - x)

instance Ord a => PartialOrd (Fuzzy a) where
    leq = (<=)
    comparable _ _ = True

instance Universe a => Universe (Fuzzy a) where
    universe = map Fuzzy universe
instance Finite a => Finite (Fuzzy a) where
    universeF = map Fuzzy universeF
    cardinality = retag (cardinality :: Tagged a Natural)

instance (QC.Arbitrary a, Ord a, Num a) => QC.Arbitrary (Fuzzy a) where
    arbitrary = mkFuzzy <$> QC.arbitrary
    shrink (Fuzzy x) =
        [ Fuzzy y
        | y <- QC.shrink x
        , y >= 0 && y <= 1
        ]

instance QC.CoArbitrary a => QC.CoArbitrary (Fuzzy a) where
    coarbitrary = QC.coarbitrary . getFuzzy

instance QC.Function a => QC.Function (Fuzzy a) where
    function = QC.functionMap getFuzzy Fuzzy
