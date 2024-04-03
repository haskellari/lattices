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
-- Module      :  Algebra.Lattice.Lifted
-- Copyright   :  (C) 2010-2015 Maximilian Bolingbroke, 2015-2019 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Algebra.Lattice.Lifted (
    Lifted(..)
  , retractLifted
  , foldLifted
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
-- Lifted
--

-- | Graft a distinct bottom onto an otherwise unbounded lattice.
-- As a bonus, the bottom will be an absorbing element for the meet.
data Lifted a = Bottom
              | Lift a
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic, Functor, Foldable, Traversable
           , Generic1
           )

instance Applicative Lifted where
  pure = return
  (<*>) = ap

instance Monad Lifted where
  return        = Lift
  Bottom >>= _  = Bottom
  Lift x >>= f  = f x

instance NFData a => NFData (Lifted a) where
  rnf Bottom   = ()
  rnf (Lift a) = rnf a

instance Hashable a => Hashable (Lifted a)

instance PartialOrd a => PartialOrd (Lifted a) where
  leq Bottom _ = True
  leq _ Bottom = False
  leq (Lift x) (Lift y) = leq x y
  comparable Bottom _ = True
  comparable _ Bottom = True
  comparable (Lift x) (Lift y) = comparable x y

instance Lattice a => Lattice (Lifted a) where
    Lift x \/ Lift y = Lift (x \/ y)
    Bottom \/ lift_y = lift_y
    lift_x \/ Bottom = lift_x

    Lift x /\ Lift y = Lift (x /\ y)
    Bottom /\ _      = Bottom
    _      /\ Bottom = Bottom

instance Lattice a => BoundedJoinSemiLattice (Lifted a) where
    bottom = Bottom

instance BoundedMeetSemiLattice a => BoundedMeetSemiLattice (Lifted a) where
    top = Lift top

instance Heyting a => Heyting (Lifted a) where
    (Lift a) ==> (Lift b) = Lift (a ==> b)
    Bottom   ==> _        = Lift top
    _        ==> Bottom   = Bottom

-- | Interpret @'Lifted' a@ using the 'BoundedJoinSemiLattice' of @a@.
retractLifted :: BoundedJoinSemiLattice a => Lifted a -> a
retractLifted = foldLifted bottom id

-- | Similar to @'maybe'@, but for @'Lifted'@ type.
foldLifted :: b -> (a -> b) -> Lifted a -> b
foldLifted _ f (Lift x) = f x
foldLifted y _ Bottom   = y

instance Universe a => Universe (Lifted a) where
    universe = Bottom : map Lift universe
instance Finite a => Finite (Lifted a) where
    universeF = Bottom : map Lift universeF
    cardinality = fmap succ (retag (cardinality :: Tagged a Natural))

instance QC.Arbitrary a => QC.Arbitrary (Lifted a) where
    arbitrary = QC.frequency
        [ (1, pure Bottom)
        , (9, Lift <$> QC.arbitrary)
        ]
    shrink Bottom   = []
    shrink (Lift x) = Bottom : map Lift (QC.shrink x)

instance QC.CoArbitrary a => QC.CoArbitrary (Lifted a) where
    coarbitrary Bottom      = QC.variant (0 :: Int)
    coarbitrary (Lift x) = QC.variant (1 :: Int) . QC.coarbitrary x

instance QC.Function a => QC.Function (Lifted a) where
    function = QC.functionMap fromLifted toLifted where
        fromLifted = foldLifted Nothing Just
        toLifted   = maybe Bottom Lift
