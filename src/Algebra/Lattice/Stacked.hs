{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Algebra.Lattice.Stacked (
    Stacked(..)
  , foldStacked
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
-- Stacked
--

-- | Stacked two lattices, one on top of another. All minimal elements of upper lattice cover all maximal elements of lower lattice.
data Stacked a b = Lower a
              | Upper b
    deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic, Functor, Foldable, Traversable
            , Generic1
            )

foldStacked :: (l -> r) -> (u -> r) -> Stacked l u -> r
foldStacked f _ (Lower l) = f l
foldStacked _ f (Upper u) = f u

instance Applicative (Stacked a) where
    pure = Upper
    (<*>) = ap

instance Monad (Stacked a) where
    return = pure

    Lower x >>= _ = Lower x
    Upper x >>= f = f x

instance (NFData a, NFData b) => NFData (Stacked a b) where
    rnf (Upper x) = rnf x
    rnf (Lower x) = rnf x

instance (Hashable a, Hashable b) => Hashable (Stacked a b)

instance (PartialOrd a, PartialOrd b) => PartialOrd (Stacked a b) where
    leq (Upper x) (Upper y) = leq x y
    leq (Upper _) _ = False
    leq _ (Upper _) = True
    leq (Lower x) (Lower y) = leq x y
    comparable (Upper x) (Upper y) = comparable x y
    comparable (Upper _) _ = True
    comparable _ (Upper _) = True
    comparable (Lower x) (Lower y) = comparable x y

instance (Lattice a, Lattice b) => Lattice (Stacked a b) where
    Upper x \/ Upper y = Upper (x \/ y)
    u@(Upper _) \/ _ = u
    _ \/ u@(Upper _) = u
    Lower x \/ Lower y = Lower (x \/ y)
    Lower x /\ Lower y = Lower (x /\ y)
    l@(Lower _) /\ _ = l
    _ /\ l@(Lower _) = l
    Upper x /\ Upper y = Upper (x /\ y)

instance (BoundedJoinSemiLattice a, Lattice b) => BoundedJoinSemiLattice (Stacked a b) where
    bottom = Lower bottom

instance (Lattice a, BoundedMeetSemiLattice b) => BoundedMeetSemiLattice (Stacked a b) where
    top = Upper top

instance (Universe a, Universe b) => Universe (Stacked a b) where
    universe = (Lower <$> universe) +++ (Upper <$> universe)

instance (Finite a, Finite b) => Finite (Stacked a b) where
    universeF = (Lower <$> universe) ++ (Upper <$> universe)
    cardinality = liftM2 (+)
        (retag (cardinality :: Tagged a Natural))
        (retag (cardinality :: Tagged b Natural))

instance (QC.Arbitrary a, QC.Arbitrary b) => QC.Arbitrary (Stacked a b) where
    arbitrary = QC.oneof [Upper <$> QC.arbitrary, Lower <$> QC.arbitrary]
    shrink (Lower x) = Lower <$> QC.shrink x
    shrink (Upper y) = Upper <$> QC.shrink y
