{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleContexts   #-}
#if __GLASGOW_HASKELL__ < 709
{-# LANGUAGE Trustworthy        #-}
#else
{-# LANGUAGE Safe               #-}
#endif
----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.Lattice.Wide
-- Copyright   :  (C) 2010-2015 Maximilian Bolingbroke, 2015 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Algebra.Lattice.Wide (
    Wide(..)
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
-- Wide
--

-- | Graft a distinct top and bottom onto any type.
-- The 'Top' is identity for 'meet' and the absorbing element for 'join'.
-- The 'Bottom' is the identity for 'join' and and the absorbing element for 'meet'.
-- Two 'Middle' values join to top, unless they are equal.
--
-- TODO: picture
data Wide a = Top
            | Middle a
            | Bottom
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic, Functor, Foldable, Traversable
#if __GLASGOW_HASKELL__ >= 706
           , Generic1
#endif
           )

instance Applicative Wide where
  pure = return
  (<*>) = ap

instance Monad Wide where
  return       = Middle
  Top >>= _    = Top
  Bottom >>= _ = Bottom
  Middle x >>= f = f x

instance NFData a => NFData (Wide a) where
  rnf Top      = ()
  rnf Bottom   = ()
  rnf (Middle a) = rnf a

instance Hashable a => Hashable (Wide a)

instance Eq a => Lattice (Wide a) where
  Top      \/ _        = Top
  Bottom   \/ x        = x
  Middle _ \/ Top      = Top
  Middle x \/ Bottom   = Middle x
  Middle x \/ Middle y = if x == y then Middle x else Top

  Bottom   /\ _        = Bottom
  Top      /\ x        = x
  Middle _ /\ Bottom   = Bottom
  Middle x /\ Top      = Middle x
  Middle x /\ Middle y = if x == y then Middle x else Bottom

instance Eq a => BoundedJoinSemiLattice (Wide a) where
  bottom = Bottom

instance Eq a => BoundedMeetSemiLattice (Wide a) where
  top = Top

instance Eq a => PartialOrd (Wide a) where
  leq Bottom _              = True
  leq Top Bottom            = False
  leq Top (Middle _)        = False
  leq Top Top               = True
  leq (Middle _) Bottom     = False
  leq (Middle _) Top        = True
  leq (Middle x) (Middle y) = x == y

  comparable Bottom _              = True
  comparable Top _                 = True
  comparable (Middle _) Bottom     = True
  comparable (Middle _) Top        = True
  comparable (Middle x) (Middle y) = x == y
