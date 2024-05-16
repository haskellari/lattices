{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Algebra.Heyting.Free (
    Free (..),
    liftFree,
    lowerFree,
    retractFree,
    substFree,
    toExpr,
    ) where

import Algebra.Heyting
import Algebra.Lattice
import Algebra.PartialOrd

import Control.Applicative          (liftA2)
import Control.Monad                (ap)
import Data.Data                    (Data, Typeable)
import GHC.Generics                 (Generic, Generic1)
import Math.NumberTheory.Logarithms (intLog2)

import qualified Algebra.Heyting.Free.Expr as E
import qualified Test.QuickCheck           as QC

-- $setup
-- >>> import Algebra.Lattice
-- >>> import Algebra.PartialOrd
-- >>> import Algebra.Heyting

-------------------------------------------------------------------------------
-- Free
-------------------------------------------------------------------------------

-- | Free Heyting algebra.
--
-- Note: `Eq` and `PartialOrd` instances aren't structural.
--
-- >>> Top == (Var 'x' ==> Var 'x')
-- True
--
-- >>> Var 'x' == Var 'y'
-- False
--
-- You can test for taulogogies:
--
-- >>> leq Top $ (Var 'A' /\ Var 'B' ==> Var 'C') <=>  (Var 'A' ==> Var 'B' ==> Var 'C')
-- True
--
-- >>> leq Top $ (Var 'A' /\ neg (Var 'A')) <=> Bottom
-- True
--
-- >>> leq Top $ (Var 'A' \/ neg (Var 'A')) <=> Top
-- False
--
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

retractFree :: Heyting a => Free a -> a
retractFree = lowerFree id

lowerFree :: Heyting b => (a -> b) -> Free a -> b
lowerFree f = go where
    go (Var x)    = f x
    go Bottom     = bottom
    go Top        = top
    go (x :/\: y) = go x /\ go y
    go (x :\/: y) = go x \/ go y
    go (x :=>: y) = go x ==> go y

toExpr :: Free a -> E.Expr a
toExpr (Var a)    = E.Var a
toExpr Bottom     = E.Bottom
toExpr Top        = E.Top
toExpr (x :/\: y) = toExpr x E.:/\: toExpr y
toExpr (x :\/: y) = toExpr x E.:\/: toExpr y
toExpr (x :=>: y) = toExpr x E.:=>: toExpr y

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
    x == y = E.proofSearch (toExpr (x <=> y))

instance Ord a => PartialOrd (Free a) where
    leq x y = E.proofSearch (toExpr (x ==> y))

-------------------------------------------------------------------------------
-- Other instances
-------------------------------------------------------------------------------

instance QC.Arbitrary a => QC.Arbitrary (Free a) where
    arbitrary = QC.sized arb where
        arb n | n <= 0    = prim
              | otherwise = QC.oneof (prim : compound)
          where
            arb' = arb (sc n)
            arb'' = arb (sc (sc n)) -- make domains be smaller.

            sc = intLog2 . max 1

            compound =
                [ liftA2 (:/\:) arb' arb'
                , liftA2 (:\/:) arb' arb'
                , liftA2 (:=>:) arb'' arb'
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
