module Algebra.Lattice.Lifted (
    Lifted(..)
  ) where

import Algebra.Lattice

--
-- Lifted
--

-- | Graft a distinct bottom onto an otherwise unbounded lattice.
-- As a bonus, the bottom will be an absorbing element for the meet.
data Lifted a = Lift a
              | Bottom

instance JoinSemiLattice a => JoinSemiLattice (Lifted a) where
    Lift x `join` Lift y = Lift (x `join` y)
    Bottom `join` lift_y = lift_y
    lift_x `join` Bottom = lift_x

instance MeetSemiLattice a => MeetSemiLattice (Lifted a) where
    Lift x `meet` Lift y = Lift (x `meet` y)
    Bottom `meet` _      = Bottom
    _      `meet` Bottom = Bottom

instance Lattice a => Lattice (Lifted a) where

instance JoinSemiLattice a => BoundedJoinSemiLattice (Lifted a) where
    bottom = Bottom

instance BoundedMeetSemiLattice a => BoundedMeetSemiLattice (Lifted a) where
    top = Lift top

instance BoundedLattice a => BoundedLattice (Lifted a) where
