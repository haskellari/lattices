{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
#if __GLASGOW_HASKELL__ < 709
{-# LANGUAGE Trustworthy #-}
#else
{-# LANGUAGE Safe #-}
#endif
----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.Lattice.Lifted
-- Copyright   :  (C) 2010-2015 Maximilian Bolingbroke, 2015 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Algebra.Lattice.Lifted (
    Lifted(..)
  , retractLifted
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
-- Lifted
--

-- | Graft a distinct bottom onto an otherwise unbounded lattice.
-- As a bonus, the bottom will be an absorbing element for the meet.
data Lifted a = Lift a
              | Bottom
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic
#if __GLASGOW_HASKELL__ >= 706
           , Generic1
#endif
           )

instance Functor Lifted where
  fmap _ Bottom   = Bottom
  fmap f (Lift a) = Lift (f a)

instance Foldable Lifted where
  foldMap _ Bottom   = mempty
  foldMap f (Lift a) = f a

instance Traversable Lifted where
  traverse _ Bottom   = pure Bottom
  traverse f (Lift a) = Lift <$> f a

instance Applicative Lifted where
  pure = return
  (<*>) = ap

instance Monad Lifted where
  return        = Lift
  Bottom >>= _  = Bottom
  Lift x >>= f  = f x

instance NFData a => NFData (Lifted a) where
  rnf Bottom   = ()
  rnf (Lift a) = rnf a

instance Hashable a => Hashable (Lifted a)

instance JoinSemiLattice a => JoinSemiLattice (Lifted a) where
    Lift x \/ Lift y = Lift (x \/ y)
    Bottom \/ lift_y = lift_y
    lift_x \/ Bottom = lift_x

instance MeetSemiLattice a => MeetSemiLattice (Lifted a) where
    Lift x /\ Lift y = Lift (x /\ y)
    Bottom /\ _      = Bottom
    _      /\ Bottom = Bottom

instance Lattice a => Lattice (Lifted a) where

instance JoinSemiLattice a => BoundedJoinSemiLattice (Lifted a) where
    bottom = Bottom

instance BoundedMeetSemiLattice a => BoundedMeetSemiLattice (Lifted a) where
    top = Lift top

instance BoundedLattice a => BoundedLattice (Lifted a) where

-- | Interpret @'Lifted' a@ using the 'BoundedJoinSemiLattice' of @a@.
retractLifted :: BoundedJoinSemiLattice a => Lifted a -> a
retractLifted Bottom    = bottom
retractLifted (Lift x)  = x
