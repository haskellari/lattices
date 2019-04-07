{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Safe            #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.Heyting
-- Copyright   :  (C) 2019 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Algebra.Heyting where

import Prelude ()
import Prelude.Compat

import Algebra.Lattice
import Control.Applicative   (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Hashable         (Hashable (..))
import Data.Proxy            (Proxy (..))
import Data.Semigroup        (All (..), Any (..), Endo (..))
import Data.Tagged           (Tagged (..))
import Data.Universe.Class   (Finite (..))

import qualified Data.HashSet as HS
import qualified Data.Set     as S

-- | A Heyting algebra is a bounded lattice equipped with a
-- binary operation \(a \to b\) of implication.
--
-- /Laws/
--
-- @
-- x '==>' x        ≡ 'top'
-- x '/\' (x '==>' y) ≡ x '/\' y
-- y '/\' (x '==>' y) ≡ y
-- x '==>' (y '/\' z) ≡ (x '==>' y) '/\' (x '==>' z)
-- @
--
class BoundedLattice a => Heyting a where
    -- | Implication.
    (==>) :: a -> a -> a

    -- | Negation.
    --
    -- @
    -- 'neg' x = x '==>' 'bottom'
    -- @
    neg :: a -> a
    neg x = x ==> bottom

    -- | Equivalence.
    --
    -- @
    -- x '<=>' y = (x '==>' y) '/\' (y '==>' x)
    -- @
    (<=>) :: a -> a -> a
    x <=> y = (x ==> y) /\ (y ==> x)

-------------------------------------------------------------------------------
-- base
-------------------------------------------------------------------------------

instance Heyting () where
    _ ==> _ = ()
    neg _   = ()
    _ <=> _ = ()

instance Heyting Bool where
    False ==> _ = True
    True  ==> y = y

    neg   = not
    (<=>) = (==)

instance Heyting a => Heyting (b -> a) where
    f ==> g = \x -> f x ==> g x
    f <=> g = \x -> f x <=> g x
    neg f   = neg . f

-------------------------------------------------------------------------------
-- All, Any, Endo
-------------------------------------------------------------------------------

instance Heyting All where
    All a ==> All b = All (a ==> b)
    neg (All a)     = All (neg a)
    All a <=> All b = All (a <=> b)

instance Heyting Any where
    Any a ==> Any b = Any (a ==> b)
    neg (Any a)     = Any (neg a)
    Any a <=> Any b = Any (a <=> b)

instance Heyting a => Heyting (Endo a) where
    Endo a ==> Endo b = Endo (a ==> b)
    neg (Endo a)      = Endo (neg a)
    Endo a <=> Endo b = Endo (a <=> b)

-------------------------------------------------------------------------------
-- Proxy, Tagged, Const, Identity
-------------------------------------------------------------------------------

instance Heyting (Proxy a) where
    _ ==> _ = Proxy
    neg _   = Proxy
    _ <=> _ = Proxy

instance Heyting a => Heyting (Identity a) where
    Identity a ==> Identity b = Identity (a ==> b)
    neg (Identity a)          = Identity (neg a)
    Identity a <=> Identity b = Identity (a <=> b)

instance Heyting a => Heyting (Tagged b a) where
    Tagged a ==> Tagged b = Tagged (a ==> b)
    neg (Tagged a)          = Tagged (neg a)
    Tagged a <=> Tagged b = Tagged (a <=> b)

instance Heyting a => Heyting (Const a b) where
    Const a ==> Const b = Const (a ==> b)
    neg (Const a)       = Const (neg a)
    Const a <=> Const b = Const (a <=> b)

-------------------------------------------------------------------------------
-- Sets
-------------------------------------------------------------------------------

instance (Ord a, Finite a) => Heyting (S.Set a) where
    x ==> y = S.union (neg x) y

    neg xs = S.fromList [ x | x <- universeF, S.notMember x xs]

    x <=> y = S.fromList
        [ z
        | z <- universeF
        , S.member z x <=> S.member z y
        ]

instance (Eq a, Hashable a, Finite a) => Heyting (HS.HashSet a) where
    x ==> y = HS.union (neg x) y

    neg xs = HS.fromList [ x | x <- universeF, not $ HS.member x xs]

    x <=> y = HS.fromList
        [ z
        | z <- universeF
        , HS.member z x <=> HS.member z y
        ]
