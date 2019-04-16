{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE Safe                #-}
module Algebra.Heyting.Free (
    Free (..),
    liftFree,
    substFree,
    retractFree,
    ) where

import Prelude ()
import Prelude.Compat

import Algebra.Heyting
import Algebra.Lattice
import Algebra.PartialOrd

import Control.Applicative          (liftA2)
import Data.Data                    (Data, Typeable)
import GHC.Generics                 (Generic, Generic1)
import Math.NumberTheory.Logarithms (intLog2)
import Control.Monad (ap)

import qualified Algebra.Heyting.Free.Expr as E
import qualified Test.QuickCheck           as QC

-------------------------------------------------------------------------------
-- Free
-------------------------------------------------------------------------------

data Free a
    = Var a
    | Bottom
    | Top
    | Free a :/\: Free a
    | Free a :\/: Free a
    | Free a :=>: Free a
  deriving (Show, Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable)

infixr 6 :/\:
infixr 5 :\/:
infixr 4 :=>:

liftFree :: a -> Free a
liftFree = Var

substFree :: Free a -> (a -> Free b) -> Free b
substFree z k = go z where
    go (Var x)    = k x
    go Bottom     = Bottom
    go Top        = Top
    go (x :/\: y) = go x /\ go y
    go (x :\/: y) = go x \/ go y
    go (x :=>: y) = go x ==> go y

retractFree :: Heyting b => (a -> b) -> Free a -> b
retractFree f = go where
    go (Var x)    = f x
    go Bottom     = bottom
    go Top        = top
    go (x :/\: y) = go x /\ go y
    go (x :\/: y) = go x \/ go y
    go (x :=>: y) = go x ==> go y

toE :: Free a -> E.Expr a
toE (Var a)    = E.Var a
toE Bottom     = E.Bottom
toE Top        = E.Top
toE (x :/\: y) = toE x E.:/\: toE y
toE (x :\/: y) = toE x E.:\/: toE y
toE (x :=>: y) = toE x E.:=>: toE y

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

instance Applicative Free where
    pure = liftFree
    (<*>) = ap

instance Monad Free where
    return = pure
    (>>=)  = substFree

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- instances do small local optimisations.

instance Lattice (Free a) where
    Top    /\ y      = y
    Bottom /\ _      = Bottom
    x      /\ Top    = x
    _      /\ Bottom = Bottom
    x      /\ y      = x :/\: y

    Top    \/ _      = Top
    Bottom \/ y      = y
    _      \/ Top    = Top
    x      \/ Bottom = x
    x      \/ y      = x :\/: y

instance BoundedJoinSemiLattice (Free a) where
    bottom = Bottom

instance BoundedMeetSemiLattice (Free a) where
    top = Top

instance Heyting (Free a) where
    Bottom ==> _   = Top
    Top    ==> y   = y
    _      ==> Top = Top
    x      ==> y   = x :=>: y

instance Ord a => Eq (Free a) where
    x == y = E.proofSearch (toE (x <=> y))

instance Ord a => PartialOrd (Free a) where
    leq x y = E.proofSearch (toE (x ==> y))

-------------------------------------------------------------------------------
-- Other instances
-------------------------------------------------------------------------------

instance QC.Arbitrary a => QC.Arbitrary (Free a) where
    arbitrary = QC.sized arb where
        arb n | n <= 0    = prim
              | otherwise = QC.oneof (prim : compound)
          where
            arb' = arb (intLog2 (max 1 n))

            compound =
                [ liftA2 (:/\:) arb' arb'
                , liftA2 (:\/:) arb' arb'
                , liftA2 (:=>:) arb' arb'
                ]

        prim = QC.frequency
            [ (20, Var <$> QC.arbitrary)
            , (1, pure Bottom)
            , (2, pure Top)
            ]

    shrink (Var c)    = Top : map Var (QC.shrink c)
    shrink Bottom     = []
    shrink Top        = [Bottom]
    shrink (x :/\: y) = x : y : map (uncurry (:/\:)) (QC.shrink (x, y))
    shrink (x :\/: y) = x : y : map (uncurry (:\/:)) (QC.shrink (x, y))
    shrink (x :=>: y) = x : y : map (uncurry (:=>:)) (QC.shrink (x, y))
