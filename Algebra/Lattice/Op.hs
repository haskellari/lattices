{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if __GLASGOW_HASKELL__ < 709
{-# LANGUAGE Trustworthy #-}
#else
{-# LANGUAGE Safe #-}
#endif
----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.Lattice.Op
-- Copyright   :  (C) 2010-2015 Maximilian Bolingbroke, 2015 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Algebra.Lattice.Op (
    Op(..)
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
-- Op
--

-- | The opposite lattice of a given lattice.  That is, switch
-- meets and joins.
newtype Op a = Op { getOp :: a }
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic, Functor, Foldable, Traversable
#if __GLASGOW_HASKELL__ >= 706
           , Generic1
#endif
           )

instance Applicative Op where
  pure = return
  (<*>) = ap

instance Monad Op where
  return      = Op
  Op x >>= f  = f x

instance NFData a => NFData (Op a) where
  rnf (Op a) = rnf a

instance Hashable a => Hashable (Op a)

instance MeetSemiLattice a => JoinSemiLattice (Op a) where
  Op x \/ Op y = Op (x /\ y)

instance JoinSemiLattice a => MeetSemiLattice (Op a) where
  Op x /\ Op y = Op (x \/ y)

instance Lattice a => Lattice (Op a) where

instance BoundedMeetSemiLattice a => BoundedJoinSemiLattice (Op a) where
  bottom = Op top

instance BoundedJoinSemiLattice a => BoundedMeetSemiLattice (Op a) where
  top = Op bottom

instance BoundedLattice a => BoundedLattice (Op a) where

instance PartialOrd a => PartialOrd (Op a) where
    Op a `leq` Op b = b `leq` a -- Note swap.
    comparable (Op a) (Op b) = comparable a b
