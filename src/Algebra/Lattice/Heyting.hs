{-# LANGUAGE CPP #-}
module Algebra.Lattice.Heyting
  ( HeytingAlgebra (..)
  , iff
  , iff'
  , toBoolean

    -- * QuickCheck Properties
    --
    -- $properties
#ifdef EXPORT_PROPERTIES
  , prop_BoundedMeetSemiLattice
  , prop_BoundedJoinSemiLattice
  , prop_DistributiveLattice
  , prop_HeytingAlgebra
  , prop_implies
#endif
  )
  where

import Prelude hiding (not)
import qualified Prelude

import Control.Applicative    (Const (..))
#ifdef EXPORT_PROPERTIES
import Data.List              (intersperse)
#endif
import Data.Functor.Identity  (Identity (..))
import Data.Hashable          (Hashable)
import Data.Proxy             (Proxy (..))
import Data.Semigroup         (All (..), Any (..), Endo (..))
import Data.Tagged            (Tagged (..))
import Data.Universe.Class    (Finite, universe)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet      as HS

#ifdef EXPORT_PROPERTIES
import Algebra.Lattice ( BoundedJoinSemiLattice (..)
                       , BoundedMeetSemiLattice (..)
                       , Lattice
                       , Join (..)
                       )
#endif
import Algebra.Lattice ( BoundedLattice
                       , Meet (..)
                       , bottom
                       , top
                       , (/\)
                       , (\/)
                       )
import Algebra.Lattice.Dropped    (Dropped    ( ..))
import Algebra.Lattice.Lifted     (Lifted     ( ..))
import Algebra.Lattice.Levitated  (Levitated)
import qualified Algebra.Lattice.Levitated as L
import Algebra.Lattice.Ordered    (Ordered    ( ..))
import Algebra.PartialOrd         (leq)
#ifdef EXPORT_PROPERTIES
import Test.QuickCheck            (Blind (..), Property, counterexample, (.&&.), (===))
import qualified Test.QuickCheck as QC
#endif

-- |
-- Heyting algebra is a bounded semi-lattice with implication which should
-- satisfy the following law:
--
-- prop> x ∧ a ≤ b ⇔ x ≤ (a ⇒ b)
--
-- We also require that a Heyting algebra is a distributive lattice, which
-- means any of the two equialent conditions holds:
--
-- prop> a ∧ (b ∨ c) = a ∧ b ∨ a ∧ c
-- prop> a ∨ (b ∧ c) = (a ∨ b) ∧ (a ∨ c)
--
-- This means @a ⇒ b@ is an
-- [exponential object](https://ncatlab.org/nlab/show/exponential%2Bobject),
-- which makes any Heyting algebra
-- a [cartesian
-- closed category](https://ncatlab.org/nlab/show/cartesian%2Bclosed%2Bcategory).
-- This means that Curry isomorphism holds (which takes a form of an actual
-- equality):
--
-- prop> a ⇒ (b ⇒ c) = (a ∧ b) ⇒ c
--
-- Some other useful properties of Heyting algebras:
-- 
-- prop> (a ⇒ b) ∧ a ≤ b
-- prop> b ≤ a ⇒ a ∧ b
-- prop> a ≤ b  iff a ⇒ b = ⊤
-- prop> b ≤ b' then a ⇒ b ≤ a ⇒ b'
-- prop> a'≤ a  then a' ⇒ b ≤ a ⇒ b
-- prop> not (a ∧ b) = not (a ∨ b)
-- prop> not (a ∨ b) = not a ∧ not b
class BoundedLattice a => HeytingAlgebra a where
  -- |
  -- Default implementation: @a ==> b = not a \/ b@, it requires @not@ to
  -- satisfy Boolean axioms, which will make it into a Boolean algebra.
  (==>) :: a -> a -> a
  (==>) a b = not a \/ b

  -- |
  -- Default implementation: @not a = a '==>' 'bottom'@
  --
  -- It is useful for optimisation reasons.
  not :: a -> a
  not a = a ==> bottom

  {-# MINIMAL (==>) | not #-}

-- |
-- Less than fixity of both @'\/'@ and @'/\'@.
infixr 4 ==>

iff :: HeytingAlgebra a => a -> a -> a
iff a b = (a ==> b) /\ (b ==> a)

iff' :: (Eq a, HeytingAlgebra a) => a -> a -> Bool
iff' a b = Meet top `leq` Meet (iff a b)
  
-- |
-- Every Heyting algebra contains a Boolean algebra. @'toBoolean'@ maps onto it;
-- moreover it is a monad (Heyting algebra is a category as every poset is) which
-- preserves finite infima.
toBoolean :: HeytingAlgebra a => a -> a
toBoolean = not . not

instance HeytingAlgebra Bool where
  not     = Prelude.not

instance HeytingAlgebra All where
  All a ==> All b = All (a ==> b)
  not (All a)     = All (not a)

instance HeytingAlgebra Any where
  Any a ==> Any b = Any (a ==> b)
  not (Any a)     = Any (not a)

instance HeytingAlgebra () where
  _ ==> _ = ()

instance HeytingAlgebra (Proxy a) where
  _ ==> _ = Proxy

instance HeytingAlgebra a => HeytingAlgebra (Tagged t a) where
  Tagged a ==> Tagged b = Tagged (a ==> b)

instance HeytingAlgebra b => HeytingAlgebra (a -> b) where
  f ==> g = \a -> f a ==> g a

instance HeytingAlgebra a => HeytingAlgebra (Identity a) where
  (Identity a) ==> (Identity b) = Identity (a ==> b)

instance HeytingAlgebra a => HeytingAlgebra (Const a b) where
  (Const a) ==> (Const b) = Const (a ==> b)

instance HeytingAlgebra a => HeytingAlgebra (Endo a) where
  (Endo f) ==> (Endo g) = Endo (f ==> g)

instance (HeytingAlgebra a, HeytingAlgebra b) => HeytingAlgebra (a, b) where
  (a0, b0) ==> (a1, b1) = (a0 ==> a1, b0 ==> b1)

--
-- Dropped, Lifted, Levitated, Ordered
--

-- |
-- Subdirectly irreducible Heyting algebra.
instance (Eq a, HeytingAlgebra a) => HeytingAlgebra (Dropped a) where
  (Drop a) ==> (Drop b) | Meet a `leq` Meet b = Top
                        | otherwise           = Drop (a ==> b)
  Top      ==> a        = a
  _        ==> Top      = Top

instance HeytingAlgebra a => HeytingAlgebra (Lifted a) where
  (Lift a) ==> (Lift b) = Lift (a ==> b)
  Bottom   ==> _        = Lift top
  _        ==> Bottom   = Bottom

instance (Eq a, HeytingAlgebra a) => HeytingAlgebra (Levitated a) where
  (L.Levitate a) ==> (L.Levitate b) | Meet a `leq` Meet b = L.Top
                                    | otherwise           = L.Levitate (a ==> b)
  L.Top          ==> a              = a
  _              ==> L.Top          = L.Top
  L.Bottom       ==> _              = L.Top
  _              ==> L.Bottom       = L.Bottom

instance (Ord a, Bounded a) => HeytingAlgebra (Ordered a) where
  Ordered a ==> Ordered b | a <= b    = top
                          | otherwise = Ordered b

--
-- containers
--

-- |
-- Power set: the cannoical example of a Boolean algebra
instance (Ord a, Finite a) => HeytingAlgebra (S.Set a) where
  not a = S.fromList universe `S.difference` a

instance (Eq a, Finite a, Hashable a) => HeytingAlgebra (HS.HashSet a) where
  not a = HS.fromList universe `HS.difference` a

-- |
-- It is not a boolean algebra (even if values are).
instance (Ord k, Finite k, HeytingAlgebra v) => HeytingAlgebra (M.Map k v) where
  -- _xx__
  -- __yy_
  -- T_iTT where i = x ==> y; T = top; _ missing (or removed key)
#if __GLASOW_HASKELL__ >= 804
  a ==> b =
    Merge.merge
      Merge.dropMissing                    -- drop if an element is missing in @b@
      (Merge.mapMissing (\_ _ -> top))     -- put @top@ if an element is missing in @a@
      (Merge.zipWithMatched (\_ -> (==>))) -- merge  matching elements with @==>@
      a b

    \/ M.fromList [(k, top) | k <- universe, not (M.member k a), not (M.member k b) ] 
                            -- for elements which are not in a, nor in b add
                            -- a @top@ key
#else
  a ==> b =
    M.intersectionWith (==>) a b
      `M.union` M.map (const top) (M.difference b a)
      `M.union` M.fromList [(k, top) | k <- universe, not (M.member k a), not (M.member k b)]
#endif

instance (Eq k, Finite k, Hashable k, HeytingAlgebra v) => HeytingAlgebra (HM.HashMap k v) where
  a ==> b = HM.intersectionWith (==>) a b
    `HM.union` HM.map (const top) (HM.difference b a)
    `HM.union` HM.fromList [(k, top) | k <- universe, not (HM.member k a), not (HM.member k b)]

--
-- $properties
--
-- /Properties are exported only if @export-properties@ cabal flag is defined./
#ifdef EXPORT_PROPERTIES

withBlinds :: Show a => String -> [a] -> String
withBlinds s bs = s ++ "\n\t" ++ foldMap id (intersperse "\n\t" (map show bs))

-- |
-- Verifies bounded meet semilattice laws.
prop_BoundedMeetSemiLattice :: (BoundedMeetSemiLattice a, Eq a, Show a)
                            => Blind a -> Blind a -> Blind a -> Property
prop_BoundedMeetSemiLattice (Blind a) (Blind b) (Blind c) =
       counterexample (withBlinds "meet associativity" [a, b, c])
        ((a /\ (b /\ c)) === ((a /\ b) /\ c))
  .&&. counterexample (withBlinds "meet commutativity" [a, b])
        ((a /\ b) === (b /\ a))
  .&&. counterexample (withBlinds "meet idempotent" [a])
        ((a /\ a) === a)
  .&&. counterexample (withBlinds "meet identity" [a])
        ((top /\ a) === a)
  .&&. counterexample (withBlinds "meet order" [a, b])
        (Meet (a /\ b) `leq` Meet a)

-- |
-- Verfifies bounded join semilattice laws.
prop_BoundedJoinSemiLattice :: (BoundedJoinSemiLattice a, Eq a, Show a)
                            => Blind a -> Blind a -> Blind a -> Property
prop_BoundedJoinSemiLattice (Blind a) (Blind b) (Blind c) =
       counterexample (withBlinds "join associativity" [a, b, c])
        ((a \/ (b \/ c)) === ((a \/ b) \/ c))
  .&&. counterexample (withBlinds "join commutativity" [a, b])
        ((a \/ b) === (b \/ a))
  .&&. counterexample (withBlinds "join idempotent" [a])
        ((a \/ a) === a)
  .&&. counterexample (withBlinds "join identity" [a])
        ((bottom \/ a) === a)
  .&&. counterexample (withBlinds "join order" [a, b])
        (Join a `leq` Join (a \/ b))

-- |
-- Distributivity laws for a lattice.
prop_DistributiveLattice :: (Lattice a, Eq a, Show a)
                         => a -> a -> a -> Property
prop_DistributiveLattice a b c =
       counterexample "a ∧ (b ∨ c) ≠ a ∧ b ∨ a ∧ c"
        ((a /\ (b \/ c)) === ((a /\ b) \/ (a /\ c)))
  .&&.
       counterexample "a ∨ (b ∧ c) ≠ (a ∨ b) ∧ (a ∨ c)"
        ((a \/ (b /\ c)) === ((a \/ b) /\ (a \/ c)))

-- |
-- Verifies the Heyting algebra law for @==>@:
-- for all @a@: @_ /\ a@ is left adjoint to  @a ==>@
-- and some other properties that are a consequence of that.
prop_implies :: (HeytingAlgebra a, Eq a, Show a)
             => Blind a -> Blind a -> Blind a -> Property
prop_implies (Blind x) (Blind a) (Blind b) =
  counterexample
    (withBlinds "Failed: x ≤ (a ⇒ b) then x ∧ a ≤ b" [x, a, b])
    (Meet x `leq` Meet (a ==> b) QC.==> (Meet (x /\ a) `leq` Meet b))
  .&&.
  counterexample
    (withBlinds "Failed: x ∧ a ≤ b then x ≤ (a ⇒ b)" [x, a, b])
    (Meet (x /\ a) `leq` Meet b QC.==> (Meet x `leq` Meet (a ==> b)))
  .&&.
  counterexample
    (withBlinds "Failed: a ≤ b ⇏ not a" [a, b])
    (Meet a `leq` Meet b QC.==> (Meet (not b) `leq` Meet (not a)))
  .&&.
  counterexample
    (withBlinds "Failed: not (a ∧ b) ≠ not a ∨ not b" [a, b])
    (not (a /\  b) === not a \/ not b)
  .&&.
  counterexample
    (withBlinds "Failed: not (a ∨ b) ≠ not a ∧ not b" [a, b])
    (not (a \/  b) === not a /\ not b)
  .&&.
  counterexample
    (withBlinds "Failed: (a ⇒ b) ∧ a ≰ b" [a, b])
    (Meet ((a ==> b) /\ a) `leq` Meet b)


-- |
-- Usefull for testing valid instances of @'HeytingAlgebra'@ type class. It
-- validates:
--
-- * bounded lattice laws
-- * @'prop_implies'@
prop_HeytingAlgebra :: (HeytingAlgebra a, Eq a, Show a)
                    => Blind a -> Blind a -> Blind a -> Property
prop_HeytingAlgebra (Blind a) (Blind b) (Blind c) = 
       prop_BoundedJoinSemiLattice (Blind a) (Blind b) (Blind c)
  .&&. prop_BoundedMeetSemiLattice (Blind a) (Blind b) (Blind c)
  .&&. prop_DistributiveLattice a b c
  .&&. prop_implies (Blind a) (Blind b) (Blind c)
#endif
