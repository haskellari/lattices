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
-- Module      :  Algebra.Lattice.Ordered
-- Copyright   :  (C) 2010-2015 Maximilian Bolingbroke, 2015 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Algebra.Lattice.Ordered (
    Ordered(..)
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
-- Ordered
--

-- | A total order gives rise to a lattice. Join is
-- max, meet is min.
newtype Ordered a = Ordered { getOrdered :: a }
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic, Functor, Foldable, Traversable
#if __GLASGOW_HASKELL__ >= 706
           , Generic1
#endif
           )

instance Applicative Ordered where
  pure = return
  (<*>) = ap

instance Monad Ordered where
  return           = Ordered
  Ordered x >>= f  = f x

instance NFData a => NFData (Ordered a) where
  rnf (Ordered a) = rnf a

instance Hashable a => Hashable (Ordered a)

instance Ord a => Lattice (Ordered a) where
  Ordered x \/ Ordered y = Ordered (max x y)
  Ordered x /\ Ordered y = Ordered (min x y)

instance (Ord a, Bounded a) => BoundedJoinSemiLattice (Ordered a) where
  bottom = Ordered minBound

instance (Ord a, Bounded a) => BoundedMeetSemiLattice (Ordered a) where
  top = Ordered maxBound

instance Ord a => PartialOrd (Ordered a) where
    leq = (<=)
    comparable _ _ = True
