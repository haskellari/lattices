{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative
import Data.Foldable
#endif

import Data.Functor.Compose
import Data.Functor.Identity
import Data.Monoid
import Data.Traversable
import Control.Monad (ap)
import Test.QuickCheck.Function
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import qualified Algebra.Lattice.Dropped as D
import qualified Algebra.Lattice.Lifted as U
import qualified Algebra.Lattice.Levitated as L
import qualified Algebra.Lattice.Op as Op

-- For old GHC to work
data Proxy1 (a :: * -> *) = Proxy1

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [theseProps]

theseProps :: TestTree
theseProps = testGroup "These"
  [ functorLaws "Dropped" (Proxy1 :: Proxy1 D.Dropped)
  , functorLaws "Lifted" (Proxy1 :: Proxy1 U.Lifted)
  , functorLaws "Leviated" (Proxy1 :: Proxy1 L.Levitated)
  , functorLaws "Op" (Proxy1 :: Proxy1 Op.Op)
  , traversableLaws "Dropped" (Proxy1 :: Proxy1 D.Dropped)
  , traversableLaws "Lifted" (Proxy1 :: Proxy1 U.Lifted)
  , traversableLaws "Levitated" (Proxy1 :: Proxy1 L.Levitated)
  , traversableLaws "Op" (Proxy1 :: Proxy1 Op.Op)
  , monadLaws "Dropped" (Proxy1 :: Proxy1 D.Dropped)
  , monadLaws "Lifted" (Proxy1 :: Proxy1 U.Lifted)
  , monadLaws "Levitated" (Proxy1 :: Proxy1 L.Levitated)
  , monadLaws "Op" (Proxy1 :: Proxy1 Op.Op)
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


-- Orphan instances

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
instance Arbitrary a => Arbitrary (Op.Op a) where
  arbitrary = Op.Op <$> arbitrary
