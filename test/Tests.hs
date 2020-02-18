{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Prelude ()
import Prelude.Compat

import Control.Monad            (ap, guard)
import Data.Int                 (Int8)
import Data.List                (genericLength, nub)
import Data.Maybe               (isJust, listToMaybe)
import Data.Semigroup           (All, Any, Endo (..), (<>))
import Data.Typeable            (Typeable, typeOf)
import Data.Universe.Class      (Finite (..), Universe (..))
import Data.Universe.Helpers    (Natural, Tagged (..))
import Test.QuickCheck
       (Arbitrary (..), Property, discard, label, (=/=), (===))
import Test.QuickCheck.Function
import Test.Tasty
import Test.Tasty.QuickCheck    (testProperty)

import qualified Test.QuickCheck as QC

import Algebra.Heyting
import Algebra.Lattice
import Algebra.PartialOrd

import Algebra.Lattice.M2          (M2 (..))
import Algebra.Lattice.M3          (M3 (..))
import Algebra.Lattice.N5          (N5 (..))
import Algebra.Lattice.ZeroHalfOne (ZeroHalfOne (..))

import qualified Algebra.Heyting.Free          as HF
import qualified Algebra.Lattice.Divisibility  as Div
import qualified Algebra.Lattice.Dropped       as D
import qualified Algebra.Lattice.Free          as F
import qualified Algebra.Lattice.Levitated     as L
import qualified Algebra.Lattice.Lexicographic as LO
import qualified Algebra.Lattice.Lifted        as U
import qualified Algebra.Lattice.Op            as Op
import qualified Algebra.Lattice.Ordered       as O
import qualified Algebra.Lattice.Stacked       as S
import qualified Algebra.Lattice.Wide          as W

import Data.HashMap.Lazy (HashMap)
import Data.HashSet      (HashSet)
import Data.IntMap       (IntMap)
import Data.IntSet       (IntSet)
import Data.Map          (Map)
import Data.Set          (Set)

import Algebra.PartialOrd.Instances ()
import Data.Universe.Instances.Eq ()
import Data.Universe.Instances.Ord ()
import Data.Universe.Instances.Show ()
import Test.QuickCheck.Instances ()

-- For old GHC to work
data Proxy (a :: *) = Proxy
data Proxy1 (a :: * -> *) = Proxy1

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ allLatticeLaws (LBounded Partial Modular)          (Proxy :: Proxy M3) -- non distributive lattice!
    , allLatticeLaws (LHeyting Partial IsBoolean)        (Proxy :: Proxy M2) -- M2
    , allLatticeLaws (LHeyting Partial IsBoolean)        (Proxy :: Proxy (Set Bool)) -- isomorphic to M2
    , allLatticeLaws (LBounded Partial NonModular)       (Proxy :: Proxy N5)
    , allLatticeLaws (LHeyting Total IsBoolean)          (Proxy :: Proxy ())
    , allLatticeLaws (LHeyting Total IsBoolean)          (Proxy :: Proxy Bool)
    , allLatticeLaws (LHeyting Total DeMorgan)           (Proxy :: Proxy ZeroHalfOne)
    , allLatticeLaws (LNormal Partial Distributive)      (Proxy :: Proxy (Map Int (O.Ordered Int)))
    , allLatticeLaws (LNormal Partial Distributive)      (Proxy :: Proxy (IntMap (O.Ordered Int)))
    , allLatticeLaws (LNormal Partial Distributive)      (Proxy :: Proxy (HashMap Int (O.Ordered Int)))
    , allLatticeLaws (LHeyting     Partial IsBoolean)    (Proxy :: Proxy (Set Int8))
    , allLatticeLaws (LHeyting     Partial IsBoolean)    (Proxy :: Proxy (HashSet Int8))
    , allLatticeLaws (LBoundedJoin Partial Distributive) (Proxy :: Proxy (Set Int))
    , allLatticeLaws (LBoundedJoin Partial Distributive) (Proxy :: Proxy IntSet)
    , allLatticeLaws (LBoundedJoin Partial Distributive) (Proxy :: Proxy (HashSet Int))
    , allLatticeLaws (LHeyting Total DeMorgan)           (Proxy :: Proxy (O.Ordered Int8))
    , allLatticeLaws (LBoundedJoin Partial Distributive) (Proxy :: Proxy (Div.Divisibility Int))
    , allLatticeLaws (LNormal Total Distributive)        (Proxy :: Proxy (LO.Lexicographic (O.Ordered Int) (O.Ordered Int)))
    , allLatticeLaws (LBounded Partial Modular)          (Proxy :: Proxy (W.Wide Int))
    , allLatticeLaws (LBounded Partial NonModular)       (Proxy :: Proxy (LO.Lexicographic (Set Bool) (Set Bool)))
    , allLatticeLaws (LBounded Partial NonModular)       (Proxy :: Proxy (LO.Lexicographic M2 M2)) -- non distributive!
    , allLatticeLaws (LBounded Partial Distributive)     (Proxy :: Proxy (S.Stacked M2 M2))
    , allLatticeLaws (LBounded Partial NonModular)       (Proxy :: Proxy (S.Stacked M3 N5)) -- non modular, though it takes QC time to find

    , allLatticeLaws LNotLattice                         (Proxy :: Proxy String)

    , allLatticeLaws (LHeyting Total   IsBoolean)        (Proxy :: Proxy All)
    , allLatticeLaws (LHeyting Total   IsBoolean)        (Proxy :: Proxy Any)
    , allLatticeLaws (LHeyting Partial IsBoolean)        (Proxy :: Proxy (Endo Bool)) -- note: it's partial!
    , allLatticeLaws (LBounded Partial Modular)          (Proxy :: Proxy (Endo M3))

    , allLatticeLaws (LHeyting Partial IsBoolean)        (Proxy :: Proxy (Int8 -> Bool))
    , allLatticeLaws (LHeyting Partial IsBoolean)        (Proxy :: Proxy (Int8 -> M2))
    , allLatticeLaws (LBounded Partial Modular)          (Proxy :: Proxy (Int8 -> M3))

    , allLatticeLaws (LNormal  Partial Distributive)     (Proxy :: Proxy (F.Free Int8))
    , allLatticeLaws (LHeyting Partial NonBoolean)       (Proxy :: Proxy (HF.Free Var))

    , allLatticeLaws (LBoundedMeet Total Distributive)   (Proxy :: Proxy (D.Dropped (O.Ordered Int)))
    , allLatticeLaws (LBounded     Total Distributive)   (Proxy :: Proxy (L.Levitated (O.Ordered Int)))
    , allLatticeLaws (LBoundedJoin Total Distributive)   (Proxy :: Proxy (U.Lifted (O.Ordered Int)))
    , allLatticeLaws (LNormal      Total Distributive )  (Proxy :: Proxy (Op.Op (O.Ordered Int)))

    , testProperty "Lexicographic M2 M2 contains M3" $ QC.property $
        isJust searchM3LexM2

    , monadLaws "Dropped" (Proxy1 :: Proxy1 D.Dropped)
    , monadLaws "Levitated" (Proxy1 :: Proxy1 L.Levitated)
    , monadLaws "Lexicographic" (Proxy1 :: Proxy1 (LO.Lexicographic Bool))
    , monadLaws "Lifted" (Proxy1 :: Proxy1 U.Lifted)
    , monadLaws "Op" (Proxy1 :: Proxy1 Op.Op)
    , monadLaws "Ordered" (Proxy1 :: Proxy1 O.Ordered)
    , monadLaws "Wide" (Proxy1 :: Proxy1 W.Wide)
    , monadLaws "Stacked" (Proxy1 :: Proxy1 (S.Stacked N5))
    , monadLaws "Heyting.Free" (Proxy1 :: Proxy1 HF.Free)

    , finiteLaws (Proxy :: Proxy M2)
    , finiteLaws (Proxy :: Proxy M3)
    , finiteLaws (Proxy :: Proxy N5)
    , finiteLaws (Proxy :: Proxy ZeroHalfOne)

    , finiteLaws (Proxy :: Proxy OInt8)
    , finiteLaws (Proxy :: Proxy (Div.Divisibility Int8))
    , finiteLaws (Proxy :: Proxy (W.Wide Int8))
    , finiteLaws (Proxy :: Proxy (D.Dropped OInt8))
    , finiteLaws (Proxy :: Proxy (L.Levitated OInt8))
    , finiteLaws (Proxy :: Proxy (U.Lifted OInt8))
    , finiteLaws (Proxy :: Proxy (LO.Lexicographic OInt8 OInt8))
    , finiteLaws (Proxy :: Proxy (S.Stacked OInt8 OInt8))
    ]

type OInt8 = O.Ordered Int8

-------------------------------------------------------------------------------
-- Monad laws
-------------------------------------------------------------------------------

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
    [ testProperty "left identity" leftIdentityProp
    , testProperty "right identity" rightIdentityProp
    , testProperty "composition" compositionProp
    , testProperty "Applicative pure" pureProp
    , testProperty "Applicative ap" apProp
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
-- Partial ord laws
-------------------------------------------------------------------------------

data IsTotal a where
    Total :: Ord a          => IsTotal a
    Partial :: PartialOrd a => IsTotal a

partialOrdLaws
    :: forall a. (Eq a, Show a, Arbitrary a, PartialOrd a)
    => IsTotal a
    -> Proxy a
    -> TestTree
partialOrdLaws total _ = testGroup "PartialOrd" $
    [ testProperty "reflexive" reflProp
    , testProperty "anti-symmetric" antiSymProp
    , testProperty "transitive" transitiveProp
    ] ++ case total of
        Partial -> []
        Total ->
            [ testProperty "total" totalProp
            , testProperty "leq/compare agree" leqCompareProp
            ]
  where
    reflProp :: a -> Property
    reflProp x = QC.property $ leq x x

    antiSymProp :: a -> a -> Property
    antiSymProp x y
        | leq x y && leq y x = label "same" $ x === y
        | otherwise          = label "diff" $ x =/= y

    transitiveProp :: a -> a -> a -> Property
    transitiveProp x y z = case p of
        []                -> label "non-related" $ QC.property True
        ((x', _, z') : _) -> label "related" $ QC.property $ leq x' z'
      where
        p = [ (x', y', z')
            | (x', y', z') <- [(x,y,z),(y,x,z),(z,y,x),(y,z,x),(z,x,y),(x,z,y)]
            , leq x' y'
            , leq y' z'
            ]

    totalProp :: a -> a -> Property
    totalProp x y = QC.property $ leq x y || leq y x

    leqCompareProp :: Ord a => a -> a -> Property
    leqCompareProp x y = agree (leq x y) (leq y x) (compare x y)
      where
        agree True True = (=== EQ)
        agree True False = (=== LT)
        agree False True = (=== GT)
        agree False False = discard

-------------------------------------------------------------------------------
-- Lattice
-------------------------------------------------------------------------------

-- | Lattice Kind
data LKind a where
    LNotLattice   :: LKind a
    LNormal       :: Lattice a => IsTotal a -> Distr ->  LKind a
    LBoundedMeet  :: BoundedMeetSemiLattice a => IsTotal a -> Distr -> LKind a
    LBoundedJoin  :: BoundedJoinSemiLattice a => IsTotal a -> Distr -> LKind a
    LBounded      :: BoundedLattice a => IsTotal a -> Distr -> LKind a
    LHeyting      :: Heyting a => IsTotal a -> IsBoolean -> LKind a

data Distr
    = NonModular
    | Modular
    | Distributive
  deriving (Eq, Ord)

data IsBoolean
    = NonBoolean
    | DeMorgan
    | IsBoolean
  deriving (Eq, Ord)

allLatticeLaws
    :: forall a. (Eq a, Show a, Arbitrary a, Typeable a, PartialOrd a)
    => LKind a
    -> Proxy a
    -> TestTree
allLatticeLaws ki pr = case ki of
    LNotLattice -> testGroup name $
        [partialOrdLaws Partial pr]
    LNormal t d -> testGroup name $
        partialOrdLaws t pr : allLatticeLaws' d pr
    LBoundedMeet t d -> testGroup name $
        partialOrdLaws t pr : allLatticeLaws' d pr ++
        [ boundedMeetLaws pr ]
    LBoundedJoin t d -> testGroup name $
        partialOrdLaws t pr :  allLatticeLaws' d pr ++
        [ boundedJoinLaws pr ]
    LBounded t d -> testGroup name $
        partialOrdLaws t pr : allLatticeLaws' d pr ++
        [ boundedMeetLaws pr
        , boundedJoinLaws pr
        ]
    LHeyting t b -> testGroup name $
        partialOrdLaws t pr : allLatticeLaws' Distributive pr ++
        [ boundedMeetLaws pr
        , boundedJoinLaws pr
        , heytingLaws pr
        ] ++
        [ deMorganLaws pr | b >= DeMorgan ] ++
        [ booleanLaws pr | b >= IsBoolean ]
  where
    name = show (typeOf (undefined :: a))

allLatticeLaws'
    :: forall a. (Eq a, Show a, Arbitrary a, Lattice a, PartialOrd a)
    => Distr
    -> Proxy a
    -> [TestTree]
allLatticeLaws' distr pr =
    [ latticeLaws pr ] ++
    [ modularLaws pr | distr >= Modular ] ++
    [ distributiveLaws pr | distr >= Distributive ]

-------------------------------------------------------------------------------
-- Lattice laws
-------------------------------------------------------------------------------

latticeLaws
    :: forall a. (Eq a, Show a, Arbitrary a, Lattice a, PartialOrd a)
    => Proxy a
    -> TestTree
latticeLaws _ = testGroup "Lattice"
    [ testProperty "leq = joinLeq" joinLeqProp
    , testProperty "leq = meetLeq" meetLeqProp
    , testProperty "meet is lower bound" meetLower
    , testProperty "join is upper bound" joinUpper
    , testProperty "meet commutes" meetComm
    , testProperty "join commute" joinComm
    , testProperty "meet associative" meetAssoc
    , testProperty "join associative" joinAssoc
    , testProperty "absorbtion 1" meetAbsorb
    , testProperty "absorbtion 2" joinAbsorb
    , testProperty "meet idempontent" meetIdemp
    , testProperty "join idempontent" joinIdemp
    , testProperty "comparableDef" comparableDef
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

-------------------------------------------------------------------------------
-- Modular
-------------------------------------------------------------------------------

modularLaws
    :: forall a. (Eq a, Show a, Arbitrary a, Lattice a, PartialOrd a)
    => Proxy a
    -> TestTree
modularLaws _ = testGroup "Modular"
    [ testProperty "(y ∧ (x ∨ z)) ∨ z = (y ∨ z) ∧ (x ∨ z)" modularProp
    ]
  where
    modularProp :: a -> a -> a -> Property
    modularProp x y z = lhs === rhs where
        lhs = (y /\ (x \/ z)) \/ z
        rhs = (y \/ z) /\ (x \/ z)

-------------------------------------------------------------------------------
-- Distributive
-------------------------------------------------------------------------------

distributiveLaws
    :: forall a. (Eq a, Show a, Arbitrary a, Lattice a, PartialOrd a)
    => Proxy a
    -> TestTree
distributiveLaws _ = testGroup "Distributive"
    [ testProperty "x ∧ (y ∨ z) = (x ∧ y) ∨ (x ∧ z)" distrProp
    , testProperty "x ∨ (y ∧ z) = (x ∨ y) ∧ (x ∨ z)" distr2Prop
    ]
  where
    distrProp :: a -> a -> a -> Property
    distrProp x y z = lhs === rhs where
        lhs = x /\ (y \/ z)
        rhs = (x /\ y) \/ (x /\ z)

    distr2Prop :: a -> a -> a -> Property
    distr2Prop x y z = lhs === rhs where
        lhs = x \/ (y /\ z)
        rhs = (x \/ y) /\ (x \/ z)

-------------------------------------------------------------------------------
-- Bounded lattice laws
-------------------------------------------------------------------------------

boundedMeetLaws
    :: forall a. (Eq a, Show a, Arbitrary a, BoundedMeetSemiLattice a)
    => Proxy a
    -> TestTree
boundedMeetLaws _ = testGroup "BoundedMeetSemiLattice"
    [ testProperty "top /\\ x = x" identityLeftProp
    , testProperty "x /\\ top = x" identityRightProp
    , testProperty "top \\/ x = top" annihilationLeftProp
    , testProperty "x \\/ top = top" annihilationRightProp
    ]
  where
    identityLeftProp :: a -> Property
    identityLeftProp x = lhs === rhs where
        lhs = top /\ x
        rhs = x

    identityRightProp :: a -> Property
    identityRightProp x = lhs === rhs where
        lhs = x /\ top
        rhs = x

    annihilationLeftProp :: a -> Property
    annihilationLeftProp x = lhs === rhs where
        lhs = top \/ x
        rhs = top

    annihilationRightProp :: a -> Property
    annihilationRightProp x = lhs === rhs where
        lhs = x \/ top
        rhs = top

boundedJoinLaws
    :: forall a. (Eq a, Show a, Arbitrary a, BoundedJoinSemiLattice a)
    => Proxy a
    -> TestTree
boundedJoinLaws _ = testGroup "BoundedJoinSemiLattice"
    [ testProperty "bottom \\/ x = x" identityLeftProp
    , testProperty "x \\/ bottom = x" identityRightProp
    , testProperty "bottom /\\ x = bottom" annihilationLeftProp
    , testProperty "x /\\ bottom = bottom" annihilationRightProp
    ]
  where
    identityLeftProp :: a -> Property
    identityLeftProp x = lhs === rhs where
        lhs = bottom \/ x
        rhs = x

    identityRightProp :: a -> Property
    identityRightProp x = lhs === rhs where
        lhs = x \/ bottom
        rhs = x

    annihilationLeftProp :: a -> Property
    annihilationLeftProp x = lhs === rhs where
        lhs = bottom /\ x
        rhs = bottom

    annihilationRightProp :: a -> Property
    annihilationRightProp x = lhs === rhs where
        lhs = x /\ bottom
        rhs = bottom

-------------------------------------------------------------------------------
-- Heyting laws
-------------------------------------------------------------------------------

heytingLaws
    :: forall a. (Eq a, Show a, Arbitrary a, Heyting a, Typeable a)
    => Proxy a
    -> TestTree
heytingLaws _ = testGroup "Heyting"
    [ testProperty "neg default" negDefaultProp
    , testProperty "<=> default" equivDefaultProp
    , testProperty "x ==> x = top" idIsTopProp
    , testProperty "a /\\ (a ==> b) = a /\\ b" andDomainProp
    , testProperty "b /\\ (a ==> b) = b" andCodomainProp
    , testProperty "a ==> (b /\\ c) = (a ==> b) /\\ (a ==> c)" implDistrProp
    , testProperty "de Morgan 1" deMorganProp1
    , testProperty "weak de Morgan 2" deMorganProp2weak
    ]
  where
    negDefaultProp :: a -> Property
    negDefaultProp x = lhs === rhs where
        lhs = neg x
        rhs = x ==> bottom

    equivDefaultProp :: a -> a -> Property
    equivDefaultProp x y = lhs === rhs where
        lhs = x <=> y
        rhs = (x ==> y) /\ (y ==> x)

    idIsTopProp :: a -> Property
    idIsTopProp x = lhs === rhs where
        lhs = x ==> x
        rhs = top

    andDomainProp :: a -> a -> Property
    andDomainProp x y = lhs === rhs where
        lhs = x /\ (x ==> y)
        rhs = x /\ y

    andCodomainProp :: a -> a -> Property
    andCodomainProp x y = lhs === rhs where
        lhs = y /\ (x ==> y)
        rhs = y

    implDistrProp :: a -> a -> a -> Property
    implDistrProp x y z
        | typeOf (undefined :: a) == typeOf (undefined :: HF.Free Var)
            = QC.mapSize (min 16) $ implDistrProp' x y z
        | otherwise
            = implDistrProp' x y z

    implDistrProp' :: a -> a -> a -> Property
    implDistrProp' x y z = lhs === rhs where
        lhs = x ==> (y /\ z)
        rhs = (x ==> y) /\ (x ==> z)

    deMorganProp1 :: a -> a -> Property
    deMorganProp1 x y = lhs === rhs where
        lhs = neg (x \/ y)
        rhs = neg x /\ neg y

    deMorganProp2weak :: a -> a -> Property
    deMorganProp2weak x y = lhs === rhs where
        lhs = neg (x /\ y)
        rhs = neg (neg (neg x \/ neg y))

-------------------------------------------------------------------------------
-- De morgan
-------------------------------------------------------------------------------

deMorganLaws
    :: forall a. (Eq a, Show a, Arbitrary a, Heyting a)
    => Proxy a
    -> TestTree
deMorganLaws _ = testGroup "de Morgan"
    [ testProperty "de Morgan 2" deMorganProp2
    ]
  where
    deMorganProp2 :: a -> a -> Property
    deMorganProp2 x y = lhs === rhs where
        lhs = neg (x /\ y)
        rhs = neg x \/ neg y

-------------------------------------------------------------------------------
-- Boolean laws
-------------------------------------------------------------------------------

booleanLaws
    :: forall a. (Eq a, Show a, Arbitrary a, Heyting a)
    => Proxy a
    -> TestTree
booleanLaws _ = testGroup "Boolean"
    [ testProperty "LEM: neg x \\/ x = top" lemProp
    , testProperty "DN: neg (neg x) = x" dnProp
    ]
  where
    lemProp :: a -> Property
    lemProp x = lhs === rhs where
        lhs = neg x \/ x
        rhs = top

    -- every element is regular, i.e. either of following equivalend conditions hold:
    -- * neg (neg x) = x
    -- * x = neg y, for some y in H -- I don't know example of this
    dnProp :: a -> Property
    dnProp x = lhs === rhs where
        lhs = neg (neg x)
        rhs = x

-------------------------------------------------------------------------------
-- Universe / Finite laws
-------------------------------------------------------------------------------

finiteLaws
    :: forall a. (Eq a, Show a, Arbitrary a, Typeable a, Finite a)
    => Proxy a
    -> TestTree
finiteLaws _ = testGroup name
    [ testProperty "elem x universe" elemProp
    , testProperty "length pfx = length (nub pfx)" prefixProp

    , testProperty "elem x universeF" elemFProp
    , testProperty "length (filter (== x) universeF) = 1" singleProp
    , testProperty "cardinality = Tagged (genericLength universeF)" cardinalityProp
    ]
  where
    name = show (typeOf (undefined :: a))

    elemProp :: a -> Property
    elemProp x = QC.property $ elem x universe

    elemFProp :: a -> Property
    elemFProp x = QC.property $ elem x universeF

    prefixProp :: Int -> Property
    prefixProp n =
        let pfx = take n (universe :: [a])
        in QC.counterexample (show pfx) $ length pfx === length (nub pfx)

    singleProp :: a -> Property
    singleProp x = length (filter (== x) universeF) === 1

    cardinalityProp :: Property
    cardinalityProp = cardinality === (Tagged (genericLength (universeF :: [a])) :: Tagged a Natural)

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
    let f M3o = x1
        f M3a = xa
        f M3b = xb
        f M3c = xc
        f M3i = x1

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

-------------------------------------------------------------------------------
-- Variable (for Free)
-------------------------------------------------------------------------------

-- | The less variables we have, the quicker tests will be :)
data Var = A | B | C | D
  deriving (Eq, Ord, Show, Enum, Bounded, Typeable)

instance Arbitrary Var where
    arbitrary = QC.arbitraryBoundedEnum

    shrink A = []
    shrink x = [ minBound .. pred x ]
