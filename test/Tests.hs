{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import Prelude ()
import Prelude.Compat

import Data.Maybe (listToMaybe, isJust)
import Data.Monoid ((<>))
import Control.Monad (ap, guard)
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
import qualified Algebra.Lattice.Wide as W

import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Set (Set)
import Data.HashMap.Lazy (HashMap)
import Data.HashSet (HashSet)

import Data.Universe.Instances.Base ()
import Test.QuickCheck.Instances ()

-- For old GHC to work
data Proxy (a :: *) = Proxy
data Proxy1 (a :: * -> *) = Proxy1

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ latticeLaws "M3" False (Proxy :: Proxy M3) -- non distributive lattice!
  , latticeLaws "M2" True (Proxy :: Proxy M2) -- M2
  , latticeLaws "Map" True (Proxy :: Proxy (Map Int (O.Ordered Int)))
  , latticeLaws "IntMap" True (Proxy :: Proxy (IntMap (O.Ordered Int)))
  , latticeLaws "HashMap" True (Proxy :: Proxy (HashMap Int (O.Ordered Int)))
  , latticeLaws "Set" True (Proxy :: Proxy (Set Int))
  , latticeLaws "IntSet" True (Proxy :: Proxy IntSet)
  , latticeLaws "HashSet" True (Proxy :: Proxy (HashSet Int))
  , latticeLaws "Ordered" True (Proxy :: Proxy (O.Ordered Int))
  , latticeLaws "Divisibility" True (Proxy :: Proxy (Div.Divisibility Int))
  , latticeLaws "LexOrdered" True (Proxy :: Proxy (LO.Lexicographic (O.Ordered Int) (O.Ordered Int)))
  , latticeLaws "Wide" False (Proxy :: Proxy (W.Wide Int))
  , latticeLaws "Lexicographic" False (Proxy :: Proxy (LO.Lexicographic (Set Bool) (Set Bool)))
  , latticeLaws "Lexicographic" False (Proxy :: Proxy (LO.Lexicographic M2 M2)) -- non distributive!
  , testProperty "Lexicographic M2 M2 contains M3" $ QC.property $
      isJust searchM3LexM2
  , monadLaws "Dropped" (Proxy1 :: Proxy1 D.Dropped)
  , monadLaws "Levitated" (Proxy1 :: Proxy1 L.Levitated)
  , monadLaws "Lexicographic" (Proxy1 :: Proxy1 (LO.Lexicographic Bool))
  , monadLaws "Lifted" (Proxy1 :: Proxy1 U.Lifted)
  , monadLaws "Op" (Proxy1 :: Proxy1 Op.Op)
  , monadLaws "Ordered" (Proxy1 :: Proxy1 O.Ordered)
  , monadLaws "Wide" (Proxy1 :: Proxy1 W.Wide)
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

instance Arbitrary a => Arbitrary (W.Wide a) where
  arbitrary = frequency [ (1, pure W.Top)
                        , (1, pure W.Bottom)
                        , (9, W.Middle <$> arbitrary)
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

-------------------------------------------------------------------------------
-- Examples
-------------------------------------------------------------------------------

-- | Non-distributive lattice
data M3 = M3_0 | M3_a | M3_b | M3_c | M3_1
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Arbitrary M3 where
    arbitrary = QC.arbitraryBoundedEnum

instance PartialOrd M3 where
    x    `leq` y    | x == y = True
    M3_0 `leq` _             = True
    _    `leq` M3_1          = True
    _    `leq` _             = False

instance JoinSemiLattice M3 where
    x    \/ M3_0          = x
    M3_0 \/ y             = y
    _    \/ M3_1          = M3_1
    M3_1 \/ _             = M3_1
    x    \/ y | x == y    = x
              | otherwise = M3_1

instance MeetSemiLattice M3 where
    x    /\ M3_1          = x
    M3_1 /\ y             = y
    _    /\ M3_0          = M3_0
    M3_0 /\ _             = M3_0
    x    /\ y | x == y    = x
              | otherwise = M3_0

instance Lattice M3 where

-- | Set Bool, M2
data M2 = M2_0 | M2_T | M2_F | M2_1
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Arbitrary M2 where
    arbitrary = QC.arbitraryBoundedEnum

instance PartialOrd M2 where
    x    `leq` y   | x == y  = True
    M2_0 `leq` _             = True
    _    `leq` M2_1          = True
    _    `leq` _             = False

instance JoinSemiLattice M2 where
    x    \/ M2_0          = x
    M2_0 \/ y             = y
    _    \/ M2_1          = M2_1
    M2_1 \/ _             = M2_1
    x    \/ y | x == y    = x
              | otherwise = M2_1

instance MeetSemiLattice M2 where
    x    /\ M2_1          = x
    M2_1 /\ y             = y
    _    /\ M2_0          = M2_0
    M2_0 /\ _             = M2_0
    x    /\ y | x == y    = x
              | otherwise = M2_0

instance Lattice M2 where

instance BoundedJoinSemiLattice M2 where
    bottom = M2_0

instance BoundedMeetSemiLattice M2 where
    top = M2_1

instance BoundedLattice M2 where

-------------------------------------------------------------------------------
-- Lexicographic M2 search
-------------------------------------------------------------------------------

searchM3 :: (Eq a, PartialOrd a, Lattice a) => [a] -> Maybe (a,a,a,a,a)
searchM3 xs = listToMaybe $ do
    x0 <- xs
    xa <- xs
    guard (xa `notElem` [x0])
    guard (x0 `leq` xa)
    xb <- xs
    guard (xb `notElem` [x0,xa])
    guard (x0 `leq` xb)
    guard (not $ comparable xa xb)
    xc <- xs
    guard (xc `notElem` [x0,xa,xb])
    guard (x0 `leq` xc)
    guard (not $ comparable xa xc)
    guard (not $ comparable xb xc)
    x1 <- xs
    guard (x1 `notElem` [x0,xa,xb,xc])
    guard (x0 `leq` x1)
    guard (xa `leq` x1)
    guard (xb `leq` x1)
    guard (xc `leq` x1)

    -- homomorphism
    let f M3_0 = x1
        f M3_a = xa
        f M3_b = xb
        f M3_c = xc
        f M3_1 = x1

    ma <- [minBound .. maxBound]
    mb <- [minBound .. maxBound]
    guard (f (ma /\ mb) == f ma /\ f mb)
    guard (f (ma \/ mb) == f ma \/ f mb)

    return (x0,xa,xb,xc,x1)

type L2 = LO.Lexicographic M2 M2

searchM3LexM2 :: Maybe (L2,L2,L2,L2,L2)
searchM3LexM2 = searchM3 xs
  where
    xs = [ LO.Lexicographic x y | x <- ys, y <- ys ]
    ys = [minBound .. maxBound]
