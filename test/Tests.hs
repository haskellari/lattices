{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import Prelude ()
import Prelude.Compat

import Data.Monoid ((<>))
import Control.Monad (ap)
import Test.QuickCheck.Function
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Algebra.Lattice
import Algebra.PartialOrd

import qualified Algebra.Lattice.Divisibility as Div
import qualified Algebra.Lattice.Dropped as D
import qualified Algebra.Lattice.Levitated as L
import qualified Algebra.Lattice.Lexicographic as LO
import qualified Algebra.Lattice.Lifted as U
import qualified Algebra.Lattice.Op as Op
import qualified Algebra.Lattice.Ordered as O

import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Set (Set)

import Data.Universe.Instances.Base ()
import Test.QuickCheck.Instances ()

-- For old GHC to work
data Proxy (a :: *) = Proxy
data Proxy1 (a :: * -> *) = Proxy1

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ latticeLaws "Map" True (Proxy :: Proxy (Map Int (O.Ordered Int)))
  , latticeLaws "IntMap" True (Proxy :: Proxy (IntMap (O.Ordered Int)))
  , latticeLaws "Set" True (Proxy :: Proxy (Set Int))
  , latticeLaws "IntSet" True (Proxy :: Proxy IntSet)
  , latticeLaws "Ordered" True (Proxy :: Proxy (O.Ordered Int))
  , latticeLaws "Divisibility" True (Proxy :: Proxy (Div.Divisibility Int))
  , latticeLaws "LexOrdered" True (Proxy :: Proxy (LO.Lexicographic (O.Ordered Int) (O.Ordered Int)))
  , latticeLaws "Lexicographic" False (Proxy :: Proxy (LO.Lexicographic (Set Bool) (Set Bool)))
  , monadLaws "Dropped" (Proxy1 :: Proxy1 D.Dropped)
  , monadLaws "Levitated" (Proxy1 :: Proxy1 L.Levitated)
  , monadLaws "Lexicographic" (Proxy1 :: Proxy1 (LO.Lexicographic Bool))
  , monadLaws "Lifted" (Proxy1 :: Proxy1 U.Lifted)
  , monadLaws "Op" (Proxy1 :: Proxy1 Op.Op)
  , monadLaws "Ordered" (Proxy1 :: Proxy1 O.Ordered)
  ]

monadLaws :: forall (m :: * -> *). ( Monad m
#if !MIN_VERSION_base(4, 8, 0)
                                   , Applicative m
#endif
                                   , Arbitrary (m Int)
                                   , Eq (m Int)
                                   , Show (m Int)
                                   , Arbitrary (m (Fun Int Int))
                                   , Show (m (Fun Int Int)))
          => String
          -> Proxy1 m
          -> TestTree
monadLaws name _ = testGroup ("Monad laws: " <> name)
  [ QC.testProperty "left identity" leftIdentityProp
  , QC.testProperty "right identity" rightIdentityProp
  , QC.testProperty "composition" compositionProp
  , QC.testProperty "Applicative pure" pureProp
  , QC.testProperty "Applicative ap" apProp
  ]
  where
    leftIdentityProp :: Int -> Fun Int (m Int) -> Property
    leftIdentityProp x (Fun _ k) = (return x >>= k) === k x

    rightIdentityProp :: m Int -> Property
    rightIdentityProp m = (m >>= return) === m

    compositionProp :: m Int -> Fun Int (m Int) -> Fun Int (m Int) -> Property
    compositionProp m (Fun _ k) (Fun _ h) = (m >>= (\x -> k x >>= h)) === ((m >>= k) >>= h)

    pureProp :: Int -> Property
    pureProp x = pure x === (return x :: m Int)

    apProp :: m (Fun Int Int) -> m Int -> Property
    apProp f x = (f' <*> x) === ap f' x
       where f' = apply <$> f

-------------------------------------------------------------------------------
-- Lattice distributive
-------------------------------------------------------------------------------

latticeLaws
    :: forall a. (Eq a, Show a, Arbitrary a,  Lattice a, PartialOrd a)
    => String
    -> Bool -- ^ distributive
    -> Proxy a
    -> TestTree
latticeLaws name distr _ = testGroup ("Lattice laws: " <> name) $
    [ QC.testProperty "leq = joinLeq" joinLeqProp
    , QC.testProperty "leq = meetLeq" meetLeqProp
    , QC.testProperty "meet is lower bound" meetLower
    , QC.testProperty "join is upper bound" joinUpper
    , QC.testProperty "meet commutes" meetComm
    , QC.testProperty "join commute" joinComm
    , QC.testProperty "meet associative" meetAssoc
    , QC.testProperty "join associative" joinAssoc
    , QC.testProperty "absorbtion 1" meetAbsorb
    , QC.testProperty "absorbtion 2" joinAbsorb
    , QC.testProperty "meet idempontent" meetIdemp
    , QC.testProperty "join idempontent" joinIdemp
    , QC.testProperty "comparableDef" comparableDef
    ] ++ if not distr then [] else
    -- Not all lattices are distributive!
    [ QC.testProperty "x ∧ (y ∨ z) = (x ∧ y) ∨ (x ∧ z)" distrProp
    , QC.testProperty "x ∨ (y ∧ z) = (x ∨ y) ∧ (x ∨ z)" distr2Prop
    ]
  where
    joinLeqProp :: a -> a -> Property
    joinLeqProp x y = leq x y === joinLeq x y

    meetLeqProp :: a -> a -> Property
    meetLeqProp x y = leq x y === meetLeq x y

    meetLower :: a -> a -> Property
    meetLower x y = (m `leq` x) QC..&&. (m `leq` y)
      where
        m = x /\ y

    joinUpper :: a -> a -> Property
    joinUpper x y = (x `leq` j) QC..&&. (y `leq` j)
      where
        j = x \/ y

    meetComm :: a -> a -> Property
    meetComm x y = x /\ y === y /\ x

    joinComm :: a -> a -> Property
    joinComm x y = x \/ y === y \/ x

    meetAssoc :: a -> a -> a -> Property
    meetAssoc x y z = x /\ (y /\ z) === (x /\ y) /\ z

    joinAssoc :: a -> a -> a -> Property
    joinAssoc x y z = x \/ (y \/ z) === (x \/ y) \/ z

    meetAbsorb :: a -> a -> Property
    meetAbsorb x y = x /\ (x \/ y) === x

    joinAbsorb :: a -> a -> Property
    joinAbsorb x y = x \/ (x /\ y) === x

    meetIdemp :: a -> Property
    meetIdemp x = x /\ x === x

    joinIdemp :: a -> Property
    joinIdemp x = x \/ x === x

    comparableDef :: a -> a -> Property
    comparableDef x y = (leq x y || leq y x) === comparable x y

    distrProp :: a -> a -> a -> Property
    distrProp x y z = lhs === rhs
      where
        lhs = x /\ (y \/ z)
        rhs = (x /\ y) \/ (x /\ z)

    distr2Prop :: a -> a -> a -> Property
    distr2Prop x y z = lhs === rhs
      where
        lhs = x \/ (y /\ z)
        rhs = (x \/ y) /\ (x \/ z)

-------------------------------------------------------------------------------
-- Orphans
-------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (D.Dropped a) where
  arbitrary = frequency [ (1, pure D.Top)
                        , (9, D.Drop <$> arbitrary)
                        ]

instance Arbitrary a => Arbitrary (U.Lifted a) where
  arbitrary = frequency [ (1, pure U.Bottom)
                        , (9, U.Lift <$> arbitrary)
                        ]

instance Arbitrary a => Arbitrary (L.Levitated a) where
  arbitrary = frequency [ (1, pure L.Top)
                        , (1, pure L.Bottom)
                        , (9, L.Levitate <$> arbitrary)
                        ]

instance Arbitrary a => Arbitrary (O.Ordered a) where
  arbitrary = O.Ordered <$> arbitrary
  shrink = map O.Ordered . shrink . O.getOrdered

instance (Arbitrary a, Num a, Ord a) => Arbitrary (Div.Divisibility a) where
  arbitrary = divisibility <$> arbitrary
  shrink d = filter (<d) . map divisibility . shrink . Div.getDivisibility $ d

divisibility :: (Ord a, Num a) => a -> Div.Divisibility a
divisibility x | x < (-1)  = Div.Divisibility (abs x)
               | x < 1     = Div.Divisibility 1
               | otherwise = Div.Divisibility x


instance Arbitrary a => Arbitrary (Op.Op a) where
  arbitrary = Op.Op <$> arbitrary

instance (Arbitrary k, Arbitrary v) => Arbitrary (LO.Lexicographic k v) where
    arbitrary = uncurry LO.Lexicographic <$> arbitrary
    shrink (LO.Lexicographic k v) = uncurry LO.Lexicographic <$> shrink (k, v)
