module Algebra.Lattice.Heyting.Layered
  ( Layered (..)
  , layer
  ) where

import Prelude

import Algebra.Lattice
  ( BoundedJoinSemiLattice (..)
  , JoinSemiLattice (..)
  , BoundedMeetSemiLattice (..)
  , MeetSemiLattice (..)
  , BoundedLattice
  , Lattice
  )

import Algebra.Lattice.Heyting (HeytingAlgebra (..))

-- |
-- Layer one Heyting algebra on top of the other.  Note: this is not
-- a categorial sum.
data Layered a b
  = Lower a
  | Upper b
  deriving (Show, Eq, Ord)

instance (JoinSemiLattice a, JoinSemiLattice b) => JoinSemiLattice (Layered a b) where
  Lower _   \/ u@Upper{} = u
  u@Upper{} \/ Lower _   = u
  Lower l   \/ Lower l'  = Lower $ l \/ l'
  Upper u   \/ Upper u'  = Upper $ u \/ u'

instance (BoundedJoinSemiLattice a, JoinSemiLattice b) => BoundedJoinSemiLattice (Layered a b) where
  bottom = Lower bottom

instance (MeetSemiLattice a, MeetSemiLattice b) => MeetSemiLattice (Layered a b) where
  l@Lower{} /\ Upper _   = l
  Upper _   /\ l@Lower{} = l
  Lower l   /\ Lower l'  = Lower $ l /\ l'
  Upper u   /\ Upper u'  = Upper $ u /\ u'

instance (MeetSemiLattice a, BoundedMeetSemiLattice b) => BoundedMeetSemiLattice (Layered a b) where
  top = Upper top

instance ( Lattice a
         , Lattice b
         ) => Lattice (Layered a b)

instance ( Lattice a
         , Lattice b
         , BoundedJoinSemiLattice a
         , BoundedMeetSemiLattice b
         ) => BoundedLattice (Layered a b)

instance ( HeytingAlgebra a
         , HeytingAlgebra b
         , Eq a
         ) => HeytingAlgebra (Layered a b) where
  Lower _   ==> Upper _      = Upper top
  Upper _   ==> l@Lower{}    = l
  Upper u   ==> Upper u'     = Upper $ u ==> u'
  Lower l   ==> Lower l'     = case l ==> l' of
    ll' | ll' == top -> Upper top
        | otherwise  -> Lower ll'

layer :: (a -> c) -> (b -> c) -> Layered a b -> c
layer f _ (Lower a) = f a
layer _ g (Upper b) = g b
