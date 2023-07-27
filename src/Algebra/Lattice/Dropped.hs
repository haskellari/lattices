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
-- Module      :  Algebra.Lattice.Dropped
-- Copyright   :  (C) 2010-2015 Maximilian Bolingbroke, 2015-2019 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Algebra.Lattice.Dropped (
    Dropped(..)
  , retractDropped
  , foldDropped
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
-- Dropped
--

-- | Graft a distinct top onto an otherwise unbounded lattice.
-- As a bonus, the top will be an absorbing element for the join.
data Dropped a = Drop a
               | Top
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic, Functor, Foldable, Traversable
           , Generic1
           )

instance Applicative Dropped where
  pure = return
  (<*>) = ap

instance Monad Dropped where
  return        = Drop
  Top >>= _     = Top
  Drop x >>= f  = f x

instance NFData a => NFData (Dropped a) where
  rnf Top      = ()
  rnf (Drop a) = rnf a

instance Hashable a => Hashable (Dropped a)

instance PartialOrd a => PartialOrd (Dropped a) where
  leq _ Top = True
  leq Top _ = False
  leq (Drop x) (Drop y) = leq x y
  comparable Top _ = True
  comparable _ Top = True
  comparable (Drop x) (Drop y) = comparable x y

instance Lattice a => Lattice (Dropped a) where
    Top    \/ _      = Top
    _      \/ Top    = Top
    Drop x \/ Drop y = Drop (x \/ y)

    Top    /\ drop_y = drop_y
    drop_x /\ Top    = drop_x
    Drop x /\ Drop y = Drop (x /\ y)

instance BoundedJoinSemiLattice a => BoundedJoinSemiLattice (Dropped a) where
    bottom = Drop bottom

instance Lattice a => BoundedMeetSemiLattice (Dropped a) where
    top = Top

instance (Eq a, Heyting a) => Heyting (Dropped a) where
    (Drop a) ==> (Drop b) | Meet a `leq` Meet b = Top
                          | otherwise           = Drop (a ==> b)
    Top      ==> a        = a
    _        ==> Top      = Top

-- | Interpret @'Dropped' a@ using the 'BoundedMeetSemiLattice' of @a@.
retractDropped :: BoundedMeetSemiLattice a => Dropped a -> a
retractDropped = foldDropped top id

-- | Similar to @'maybe'@, but for @'Dropped'@ type.
foldDropped :: b -> (a -> b) -> Dropped a -> b
foldDropped _ f (Drop x) = f x
foldDropped y _ Top      = y

instance Universe a => Universe (Dropped a) where
    universe = Top : map Drop universe
instance Finite a => Finite (Dropped a) where
    universeF = Top : map Drop universeF
    cardinality = fmap succ (retag (cardinality :: Tagged a Natural))

instance QC.Arbitrary a => QC.Arbitrary (Dropped a) where
    arbitrary = QC.frequency
        [ (1, pure Top)
        , (9, Drop <$> QC.arbitrary)
        ]

    shrink Top      = []
    shrink (Drop x) = Top : map Drop (QC.shrink x)

instance QC.CoArbitrary a => QC.CoArbitrary (Dropped a) where
    coarbitrary Top      = QC.variant (0 :: Int)
    coarbitrary (Drop x) = QC.variant (1 :: Int) . QC.coarbitrary x

instance QC.Function a => QC.Function (Dropped a) where
    function = QC.functionMap fromDropped toDropped where
        fromDropped = foldDropped Nothing Just
        toDropped   = maybe Top Drop
