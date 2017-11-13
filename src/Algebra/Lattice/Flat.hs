{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeOperators      #-}
#if __GLASGOW_HASKELL__ < 709
{-# LANGUAGE Trustworthy        #-}
#else
{-# LANGUAGE Safe               #-}
#endif
----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.Lattice.Flat
-- Copyright   :  (C) 2010-2015 Maximilian Bolingbroke, 2015 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Algebra.Lattice.Flat (
    Flat(..)
  ) where

import Prelude ()
import Prelude.Compat

import Algebra.Lattice
import Algebra.PartialOrd

import Control.DeepSeq
import Control.Monad
import Data.Data
import Data.Hashable
import GHC.Generics

--
-- Flat
--

-- | Graft a distinct top and bottom onto any type.
-- The top is the absorbing element for the join.
-- The bottom is the identity for the join.
-- Two non-top, non-bottom values join to top, unless they are equal.
data Flat a = Top
            | Flat a
            | Bottom
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic, Functor, Foldable, Traversable
#if __GLASGOW_HASKELL__ >= 706
           , Generic1
#endif
           )

instance Applicative Flat where
  pure = return
  (<*>) = ap

instance Monad Flat where
  return            = Flat
  Top >>= _         = Top
  Bottom >>= _      = Bottom
  Flat x >>= f  = f x

instance NFData a => NFData (Flat a) where
  rnf Top          = ()
  rnf Bottom       = ()
  rnf (Flat a) = rnf a

instance Hashable a => Hashable (Flat a)

instance Eq a => JoinSemiLattice (Flat a) where
  Top    \/ _      = Top
  _      \/ Top    = Top
  Bottom \/ flat_y = flat_y
  flat_x \/ Bottom = flat_x
  Flat x \/ Flat y = if x == y then Flat x else Top

instance Eq a => BoundedJoinSemiLattice (Flat a) where
  bottom = Bottom

instance Eq a => PartialOrd (Flat a) where
  leq Bottom Bottom = True
  leq Bottom Top = True
  leq Top Bottom = False
  leq Top Top = True
  leq Bottom (Flat _) = True
  leq (Flat _) Bottom = False
  leq Top (Flat _) = False
  leq (Flat _) Top = True
  leq (Flat x) (Flat y) = x == y

