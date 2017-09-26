{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.PartialOrd.Instances
-- Copyright   :  (C) 2010-2015 Maximilian Bolingbroke, 2015 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- This module re-exports orphan instances from 'Data.Universe.Instances.Eq'
-- module, and @(PartialOrd v, Finite k) => PartialOrd (k -> v)@ instance.
----------------------------------------------------------------------------
module Algebra.PartialOrd.Instances () where

import Algebra.PartialOrd         (PartialOrd (..))
import Data.Universe.Class        (Finite (..))
import Data.Universe.Instances.Eq ()

-- | @Eq (k -> v)@ is from 'Data.Universe.Instances.Eq'
instance (PartialOrd v, Finite k) => PartialOrd (k -> v) where
    f `leq` g = all (\k -> f k `leq` g k) universeF
