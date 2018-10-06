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
-- Module      :  Algebra.Lattice.Dropped
-- Copyright   :  (C) 2010-2015 Maximilian Bolingbroke, 2015 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Algebra.Lattice.Dropped (
    Dropped(..)
  , retractDropped
  , dropped
  ) where

import Prelude ()
import Prelude.Compat

import Algebra.Lattice

import Control.DeepSeq
import Control.Monad
import Data.Data
import Data.Hashable
import GHC.Generics

--
-- Dropped
--

-- | Graft a distinct top onto an otherwise unbounded lattice.
-- As a bonus, the top will be an absorbing element for the join.
data Dropped a = Top
               | Drop a
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic, Functor, Foldable, Traversable
#if __GLASGOW_HASKELL__ >= 706
           , Generic1
#endif
           )

instance Applicative Dropped where
  pure = return
  (<*>) = ap

instance Monad Dropped where
  return        = Drop
  Top >>= _     = Top
  Drop x >>= f  = f x

instance NFData a => NFData (Dropped a) where
  rnf Top      = ()
  rnf (Drop a) = rnf a

instance Hashable a => Hashable (Dropped a)

instance JoinSemiLattice a => JoinSemiLattice (Dropped a) where
    Top    \/ _      = Top
    _      \/ Top    = Top
    Drop x \/ Drop y = Drop (x \/ y)

instance MeetSemiLattice a => MeetSemiLattice (Dropped a) where
    Top    /\ drop_y = drop_y
    drop_x /\ Top    = drop_x
    Drop x /\ Drop y = Drop (x /\ y)

instance Lattice a => Lattice (Dropped a) where

instance BoundedJoinSemiLattice a => BoundedJoinSemiLattice (Dropped a) where
    bottom = Drop bottom

instance MeetSemiLattice a => BoundedMeetSemiLattice (Dropped a) where
    top = Top

instance BoundedLattice a => BoundedLattice (Dropped a) where

-- | Interpret @'Dropped' a@ using the 'BoundedMeetSemiLattice' of @a@.
retractDropped :: BoundedMeetSemiLattice a => Dropped a -> a
retractDropped = dropped top id

-- | Similar to @'maybe'@, but for @'Dropped'@ type.
dropped :: b -> (a -> b) -> Dropped a -> b
dropped _ f (Drop x) = f x
dropped y _ Top      = y
