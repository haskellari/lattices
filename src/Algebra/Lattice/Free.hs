{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Algebra.Lattice.Free (
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
import Control.Monad                (ap)
import Data.Data                    (Data, Typeable)
import GHC.Generics                 (Generic, Generic1)
import Math.NumberTheory.Logarithms (intLog2)

import qualified Algebra.Heyting.Free.Expr as E
import qualified Test.QuickCheck           as QC

-------------------------------------------------------------------------------
-- Free
-------------------------------------------------------------------------------

-- | Free distributive lattice.
data Free a
    = Var a
    | Free a :/\: Free a
    | Free a :\/: Free a
  deriving (Show, Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable)

infixr 6 :/\:
infixr 5 :\/:

liftFree :: a -> Free a
liftFree = Var

substFree :: Free a -> (a -> Free b) -> Free b
substFree z k = go z where
    go (Var x)    = k x
    go (x :/\: y) = go x /\ go y
    go (x :\/: y) = go x \/ go y

retractFree :: Heyting b => (a -> b) -> Free a -> b
retractFree f = go where
    go (Var x)    = f x
    go (x :/\: y) = go x /\ go y
    go (x :\/: y) = go x \/ go y

toE :: Free a -> E.Expr a
toE (Var a)    = E.Var a
toE (x :/\: y) = toE x E.:/\: toE y
toE (x :\/: y) = toE x E.:\/: toE y

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

instance Lattice (Free a) where
    x /\ y = x :/\: y
    x \/ y = x :\/: y

instance Ord a => Eq (Free a) where
    (==) = partialOrdEq

instance Ord a => PartialOrd (Free a) where
    leq x y = E.proofSearch (toE x E.:=>: toE y)

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
                ]

        prim = Var <$> QC.arbitrary

    shrink (Var c)    = map Var (QC.shrink c)
    shrink (x :/\: y) = x : y : map (uncurry (:/\:)) (QC.shrink (x, y))
    shrink (x :\/: y) = x : y : map (uncurry (:\/:)) (QC.shrink (x, y))
