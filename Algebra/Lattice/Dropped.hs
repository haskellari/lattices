module Algebra.Lattice.Dropped (
    Dropped(..)
  ) where

import Algebra.Lattice

--
-- Dropped
--

-- | Graft a distinct top onto an otherwise unbounded lattice.
-- As a bonus, the top will be an absorbing element for the join.
data Dropped a = Top
               | Drop a

instance JoinSemiLattice a => JoinSemiLattice (Dropped a) where
    Top    `join` _      = Top
    _      `join` Top    = Top
    Drop x `join` Drop y = Drop (x `join` y)

instance MeetSemiLattice a => MeetSemiLattice (Dropped a) where
    Top    `meet` drop_y = drop_y
    drop_x `meet` Top    = drop_x
    Drop x `meet` Drop y = Drop (x `meet` y)

instance Lattice a => Lattice (Dropped a) where

instance BoundedJoinSemiLattice a => BoundedJoinSemiLattice (Dropped a) where
    bottom = Drop bottom

instance MeetSemiLattice a => BoundedMeetSemiLattice (Dropped a) where
    top = Top

instance BoundedLattice a => BoundedLattice (Dropped a) where
