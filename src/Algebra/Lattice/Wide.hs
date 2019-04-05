{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE Safe               #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.Lattice.Wide
-- Copyright   :  (C) 2010-2015 Maximilian Bolingbroke, 2015-2019 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Algebra.Lattice.Wide (
    Wide(..)
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
-- Wide
--

-- | Graft a distinct top and bottom onto any type.
-- The 'Top' is identity for '/\' and the absorbing element for '\/'.
-- The 'Bottom' is the identity for '\/' and and the absorbing element for '/\'.
-- Two 'Middle' values join to top, unless they are equal.
--
-- <<wide.png>>
--
data Wide a
    = Top
    | Middle a
    | Bottom
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic, Functor, Foldable, Traversable
           , Generic1
           )

instance Applicative Wide where
  pure = return
  (<*>) = ap

instance Monad Wide where
  return       = Middle
  Top >>= _    = Top
  Bottom >>= _ = Bottom
  Middle x >>= f = f x

instance NFData a => NFData (Wide a) where
  rnf Top      = ()
  rnf Bottom   = ()
  rnf (Middle a) = rnf a

instance Hashable a => Hashable (Wide a)

instance Eq a => Lattice (Wide a) where
  Top      \/ _        = Top
  Bottom   \/ x        = x
  Middle _ \/ Top      = Top
  Middle x \/ Bottom   = Middle x
  Middle x \/ Middle y = if x == y then Middle x else Top

  Bottom   /\ _        = Bottom
  Top      /\ x        = x
  Middle _ /\ Bottom   = Bottom
  Middle x /\ Top      = Middle x
  Middle x /\ Middle y = if x == y then Middle x else Bottom

instance Eq a => BoundedJoinSemiLattice (Wide a) where
  bottom = Bottom

instance Eq a => BoundedMeetSemiLattice (Wide a) where
  top = Top

instance Eq a => PartialOrd (Wide a) where
  leq Bottom _              = True
  leq Top Bottom            = False
  leq Top (Middle _)        = False
  leq Top Top               = True
  leq (Middle _) Bottom     = False
  leq (Middle _) Top        = True
  leq (Middle x) (Middle y) = x == y

  comparable Bottom _              = True
  comparable Top _                 = True
  comparable (Middle _) Bottom     = True
  comparable (Middle _) Top        = True
  comparable (Middle x) (Middle y) = x == y

instance Universe a => Universe (Wide a) where
    universe = Top : Bottom : map Middle universe
instance Finite a => Finite (Wide a) where
    universeF = Top : Bottom : map Middle universeF

instance QC.Arbitrary a => QC.Arbitrary (Wide a) where
    arbitrary = QC.frequency
        [ (1, pure Top)
        , (1, pure Bottom)
        , (9, Middle <$> QC.arbitrary)
        ]

    shrink Top        = []
    shrink Bottom     = []
    shrink (Middle x) = Top : Bottom : map Middle (QC.shrink x)

instance QC.CoArbitrary a => QC.CoArbitrary (Wide a) where
    coarbitrary Top        = QC.variant (0 :: Int)
    coarbitrary Bottom     = QC.variant (0 :: Int)
    coarbitrary (Middle x) = QC.variant (0 :: Int) . QC.coarbitrary x

instance QC.Function a => QC.Function (Wide a) where
    function = QC.functionMap fromWide toWide where
        fromWide Top        = Left True
        fromWide Bottom     = Left False
        fromWide (Middle x) = Right x

        toWide (Left True)  = Top
        toWide (Left False) = Bottom
        toWide (Right x)    = Middle x
