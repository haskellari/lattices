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
-- Module      :  Algebra.Lattice.Divisibility
-- Copyright   :  (C) 2010-2015 Maximilian Bolingbroke, 2015 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Algebra.Lattice.Divisibility (
    Divisibility(..)
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
-- Divisibility
--

-- | A divisibility lattice. @'join' = 'lcm'@, @'meet' = 'gcd'@.
newtype Divisibility a = Divisibility { getDivisibility :: a }
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic, Functor, Foldable, Traversable
#if __GLASGOW_HASKELL__ >= 706
           , Generic1
#endif
           )

instance Applicative Divisibility where
  pure = return
  (<*>) = ap

instance Monad Divisibility where
  return           = Divisibility
  Divisibility x >>= f  = f x

instance NFData a => NFData (Divisibility a) where
  rnf (Divisibility a) = rnf a

instance Hashable a => Hashable (Divisibility a)

instance Integral a => Lattice (Divisibility a) where
  Divisibility x \/ Divisibility y = Divisibility (lcm x y)

  Divisibility x /\ Divisibility y = Divisibility (gcd x y)

instance Integral a => BoundedJoinSemiLattice (Divisibility a) where
  bottom = Divisibility 1

instance (Eq a, Integral a) => PartialOrd (Divisibility a) where
    leq (Divisibility a) (Divisibility b) = b `mod` a == 0
