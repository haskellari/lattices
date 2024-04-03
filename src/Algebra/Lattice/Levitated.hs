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
-- Module      :  Algebra.Lattice.Levitated
-- Copyright   :  (C) 2010-2015 Maximilian Bolingbroke, 2015-2019 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Algebra.Lattice.Levitated (
    Levitated(..)
  , retractLevitated
  , foldLevitated
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
-- Levitated
--

-- | Graft a distinct top and bottom onto an otherwise unbounded lattice.
-- The top is the absorbing element for the join, and the bottom is the absorbing
-- element for the meet.
data Levitated a = Bottom
                 | Levitate a
                 | Top
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic, Functor, Foldable, Traversable
           , Generic1
           )

instance Applicative Levitated where
  pure = return
  (<*>) = ap

instance Monad Levitated where
  return            = Levitate
  Top >>= _         = Top
  Bottom >>= _      = Bottom
  Levitate x >>= f  = f x

instance NFData a => NFData (Levitated a) where
  rnf Top          = ()
  rnf Bottom       = ()
  rnf (Levitate a) = rnf a

instance Hashable a => Hashable (Levitated a)

instance PartialOrd a => PartialOrd (Levitated a) where
  leq _ Top = True
  leq Top _ = False
  leq Bottom _ = True
  leq _ Bottom = False
  leq (Levitate x) (Levitate y) = leq x y
  comparable Top _ = True
  comparable _ Top = True
  comparable Bottom _ = True
  comparable _ Bottom = True
  comparable (Levitate x) (Levitate y) = comparable x y

instance Lattice a => Lattice (Levitated a) where
    Top        \/ _          = Top
    _          \/ Top        = Top
    Levitate x \/ Levitate y = Levitate (x \/ y)
    Bottom     \/ lev_y      = lev_y
    lev_x      \/ Bottom     = lev_x

    Top        /\ lev_y      = lev_y
    lev_x      /\ Top        = lev_x
    Levitate x /\ Levitate y = Levitate (x /\ y)
    Bottom     /\ _          = Bottom
    _          /\ Bottom     = Bottom

instance Lattice a => BoundedJoinSemiLattice (Levitated a) where
    bottom = Bottom

instance Lattice a => BoundedMeetSemiLattice (Levitated a) where
    top = Top

instance (Eq a, Heyting a) => Heyting (Levitated a) where
  (Levitate a) ==> (Levitate b) | Meet a `leq` Meet b
                                = Top
                                | otherwise
                                = Levitate (a ==> b)
  Top          ==> a            = a
  _            ==> Top          = Top
  Bottom       ==> _            = Top
  _            ==> Bottom       = Bottom

-- | Interpret @'Levitated' a@ using the 'BoundedLattice' of @a@.
retractLevitated :: (BoundedMeetSemiLattice a, BoundedJoinSemiLattice a) => Levitated a -> a
retractLevitated = foldLevitated bottom id top

-- | Fold 'Levitated'.
foldLevitated :: b -> (a -> b) -> b -> Levitated a -> b
foldLevitated b _ _ Bottom       = b
foldLevitated _ f _ (Levitate x) = f x
foldLevitated _ _ t Top          = t

instance Universe a => Universe (Levitated a) where
    universe = Top : Bottom : map Levitate universe
instance Finite a => Finite (Levitated a) where
    universeF = Top : Bottom : map Levitate universeF
    cardinality = fmap (2 +) (retag (cardinality :: Tagged a Natural))

instance QC.Arbitrary a => QC.Arbitrary (Levitated a) where
    arbitrary = QC.frequency
        [ (1, pure Top)
        , (1, pure Bottom)
        , (9, Levitate <$> QC.arbitrary)
        ]

    shrink Top          = []
    shrink Bottom       = []
    shrink (Levitate x) = Top : Bottom : map Levitate (QC.shrink x)

instance QC.CoArbitrary a => QC.CoArbitrary (Levitated a) where
    coarbitrary Top          = QC.variant (0 :: Int)
    coarbitrary Bottom       = QC.variant (0 :: Int)
    coarbitrary (Levitate x) = QC.variant (0 :: Int) . QC.coarbitrary x

instance QC.Function a => QC.Function (Levitated a) where
    function = QC.functionMap fromLevitated toLevitated where
        fromLevitated Top          = Left True
        fromLevitated Bottom       = Left False
        fromLevitated (Levitate x) = Right x

        toLevitated (Left True)  = Top
        toLevitated (Left False) = Bottom
        toLevitated (Right x)    = Levitate x
