{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import Prelude ()
import Prelude.Compat

import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Monoid ((<>))
import Data.Traversable
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

import Test.QuickCheck.Instances ()

-- For old GHC to work
data Proxy (a :: *) = Proxy
data Proxy1 (a :: * -> *) = Proxy1

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [theseProps]

theseProps :: TestTree
theseProps = testGroup "These"
  [ functorLaws "Dropped" (Proxy1 :: Proxy1 D.Dropped)
  , functorLaws "Levitated" (Proxy1 :: Proxy1 L.Levitated)
  , functorLaws "Lexicographic" (Proxy1 :: Proxy1 (LO.Lexicographic Bool))
  , functorLaws "Lifted" (Proxy1 :: Proxy1 U.Lifted)
  , functorLaws "Op" (Proxy1 :: Proxy1 Op.Op)
  , functorLaws "Ordered" (Proxy1 :: Proxy1 O.Ordered)
  , monadLaws "Dropped" (Proxy1 :: Proxy1 D.Dropped)
  , monadLaws "Levitated" (Proxy1 :: Proxy1 L.Levitated)
  , monadLaws "Lexicographic" (Proxy1 :: Proxy1 (LO.Lexicographic Bool))
  , monadLaws "Lifted" (Proxy1 :: Proxy1 U.Lifted)
  , monadLaws "Op" (Proxy1 :: Proxy1 Op.Op)
  , monadLaws "Ordered" (Proxy1 :: Proxy1 O.Ordered)
  , traversableLaws "Dropped" (Proxy1 :: Proxy1 D.Dropped)
  , traversableLaws "Levitated" (Proxy1 :: Proxy1 L.Levitated)
  , traversableLaws "Lexicographic" (Proxy1 :: Proxy1 (LO.Lexicographic Bool))
  , traversableLaws "Lifted" (Proxy1 :: Proxy1 U.Lifted)
  , traversableLaws "Op" (Proxy1 :: Proxy1 Op.Op)
  , traversableLaws "Ordered" (Proxy1 :: Proxy1 O.Ordered)
  , latticeLaws "Map" (Proxy :: Proxy (Map Int (O.Ordered Int)))
  , latticeLaws "IntMap" (Proxy :: Proxy (IntMap (O.Ordered Int)))
  , latticeLaws "Set" (Proxy :: Proxy (Set Int))
  , latticeLaws "IntSet" (Proxy :: Proxy IntSet)
  , latticeLaws "Ordered" (Proxy :: Proxy (O.Ordered Int))
  , latticeLaws "Divisibility " (Proxy :: Proxy (Div.Divisibility Int))
  ]

functorLaws :: forall (f :: * -> *). ( Functor f
                                     , Arbitrary (f Int)
                                     , Eq (f Int)
                                     , Show (f Int))
            => String
            -> Proxy1 f
            -> TestTree
functorLaws name _ = testGroup ("Functor laws: " <> name)
  [ QC.testProperty "identity" identityProp
  , QC.testProperty "composition" compositionProp
  ]
  where
    identityProp :: f Int -> Property
    identityProp x = fmap id x === x

    compositionProp :: f Int -> Fun Int Int -> Fun Int Int -> Property
    compositionProp x (Fun _ f) (Fun _ g) = fmap g (fmap f x) === fmap (g . f) x

traversableLaws :: forall (t :: * -> *). ( Traversable t
                                         , Arbitrary (t Int)
                                         , Eq (t Int)
                                         , Show (t Int))
                => String
                -> Proxy1 t
                -> TestTree
traversableLaws name _ = testGroup ("Traversable laws: " <> name)
  [ QC.testProperty "identity" identityProp
  , QC.testProperty "composition" compositionProp
  , QC.testProperty "functor" functorProp
  , QC.testProperty "foldable" foldableProp
  ]
  where
    identityProp :: t Int -> Property
    identityProp x = traverse Identity x === Identity x

    compositionProp :: t Int -> Fun Int (Maybe Int) -> Fun Int ([] Int) -> Property
    compositionProp x (Fun _ f) (Fun _ g) = traverse (Compose . fmap g . f) x === (Compose . fmap (traverse g) . traverse f $ x)

    functorProp :: t Int -> Fun Int Int -> Property
    functorProp x (Fun _ f) = fmap f x === fmapDefault f x

    foldableProp :: t Int -> Fun Int [Int] -> Property
    foldableProp x (Fun _ f) = foldMap f x === foldMapDefault f x

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
    -> Proxy a
    -> TestTree
latticeLaws name _ = testGroup ("Lattice laws: " <> name)
    [ QC.testProperty "leq = joinLeq" joinLeqProp
    , QC.testProperty "x ∧ (y ∨ z) = (x ∧ y) ∨ (x ∧ z)" distrProp
    , QC.testProperty "x ∨ (y ∧ z) = (x ∨ y) ∧ (x ∨ z)" distr2Prop
    ]
  where
    joinLeqProp :: a -> a -> Property
    joinLeqProp x y = leq x y === joinLeq x y

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
  arbitrary = LO.Lexicographic <$> arbitrary <*> arbitrary
