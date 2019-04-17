-- | This module provides Unicode variants of the operators.
--
-- Unfortunately, ⊤, ⊥, and ¬ don't fit into Haskell lexical structure well.
--
module Algebra.Lattice.Unicode where

import Algebra.Heyting
import Algebra.Lattice

infixr 6 ∧
infixr 5 ∨
infixr 4 ⟹
infix 4 ⟺

-- | Meet, alias for '/\'.
(∧) :: Lattice a => a -> a -> a
(∧) = (/\)

-- | Join, alias for '\/'.
(∨) :: Lattice a => a -> a -> a
(∨) = (\/)

-- | Implication, alias for '==>'.
(⟹) :: Heyting a => a -> a -> a
(⟹) = (==>)

-- | Equivalence, alias for '<=>'.
(⟺) :: Heyting a => a -> a -> a
(⟺) = (<=>)
