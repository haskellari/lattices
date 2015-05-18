{-# LANGUAGE Safe #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Algebra.Enumerable
-- Copyright   :  (C) 2010-2015 Maximilian Bolingbroke
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
----------------------------------------------------------------------------
module Algebra.Enumerable (
    Enumerable(..), universeBounded,
    Enumerated(..)
  ) where

-- | Finitely enumerable things
class Enumerable a where
    universe :: [a]

universeBounded :: (Enum a, Bounded a) => [a]
universeBounded = enumFromTo minBound maxBound


-- | Wrapper used to mark where we expect to use the fact that something is Enumerable
newtype Enumerated a = Enumerated { unEnumerated :: a }
                     deriving (Eq, Ord)

instance Enumerable a => Enumerable (Enumerated a) where
    universe = map Enumerated universe


-- TODO: add to this rather sorry little set of instances. Can we exploit commonality with lazy-smallcheck?

instance Enumerable Bool where
    universe = universeBounded

instance Enumerable Int where
    universe = universeBounded

instance Enumerable a => Enumerable (Maybe a) where
    universe = Nothing : map Just universe

instance (Enumerable a, Enumerable b) => Enumerable (Either a b) where
    universe = map Left universe ++ map Right universe

instance Enumerable () where
    universe = [()]

instance (Enumerable a, Enumerable b) => Enumerable (a, b) where
    universe = [(a, b) | a <- universe, b <- universe]
