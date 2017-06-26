{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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
-- Module      :  Algebra.Lattice.Levitated
-- Copyright   :  (C) 2010-2015 Maximilian Bolingbroke, 2015 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Algebra.Lattice.Levitated (
    Levitated(..)
  , retractLevitated
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
-- Levitated
--

-- | Graft a distinct top and bottom onto an otherwise unbounded lattice.
-- The top is the absorbing element for the join, and the bottom is the absorbing
-- element for the meet.
data Levitated a = Top
                 | Levitate a
                 | Bottom
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic
#if __GLASGOW_HASKELL__ >= 706
           , Generic1
#endif
           )
instance Functor Levitated where
  fmap _ Bottom       = Bottom
  fmap _ Top          = Top
  fmap f (Levitate a) = Levitate (f a)

instance Foldable Levitated where
  foldMap _ Bottom       = mempty
  foldMap _ Top          = mempty
  foldMap f (Levitate a) = f a

instance Traversable Levitated where
  traverse _ Bottom       = pure Bottom
  traverse _ Top          = pure Top
  traverse f (Levitate a) = Levitate <$> f a

instance Applicative Levitated where
  pure = return
  (<*>) = ap

instance Monad Levitated where
  return            = Levitate
  Top >>= _         = Top
  Bottom >>= _      = Bottom
  Levitate x >>= f  = f x

instance NFData a => NFData (Levitated a) where
  rnf Top          = ()
  rnf Bottom       = ()
  rnf (Levitate a) = rnf a

instance Hashable a => Hashable (Levitated a)

instance JoinSemiLattice a => JoinSemiLattice (Levitated a) where
    Top        \/ _          = Top
    _          \/ Top        = Top
    Levitate x \/ Levitate y = Levitate (x \/ y)
    Bottom     \/ lev_y      = lev_y
    lev_x      \/ Bottom     = lev_x

instance MeetSemiLattice a => MeetSemiLattice (Levitated a) where
    Top        /\ lev_y      = lev_y
    lev_x      /\ Top        = lev_x
    Levitate x /\ Levitate y = Levitate (x /\ y)
    Bottom     /\ _          = Bottom
    _          /\ Bottom     = Bottom

instance Lattice a => Lattice (Levitated a) where

instance JoinSemiLattice a => BoundedJoinSemiLattice (Levitated a) where
    bottom = Bottom

instance MeetSemiLattice a => BoundedMeetSemiLattice (Levitated a) where
    top = Top

instance Lattice a => BoundedLattice (Levitated a) where

-- | Interpret @'Levitated' a@ using the 'BoundedLattice' of @a@.
retractLevitated :: BoundedLattice a => Levitated a -> a
retractLevitated Top           = top
retractLevitated Bottom        = bottom
retractLevitated (Levitate x)  = x
