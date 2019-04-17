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

(∧) :: Lattice a => a -> a -> a
(∧) = (/\)

(∨) :: Lattice a => a -> a -> a
(∨) = (\/)

(⟹) :: Heyting a => a -> a -> a
(⟹) = (==>)

(⟺) :: Heyting a => a -> a -> a
(⟺) = (<=>)
