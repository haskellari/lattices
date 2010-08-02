module Algebra.Lattice.Levitated (
    Levitated(..)
  ) where

import Algebra.Lattice

--
-- Levitated
--

-- | Graft a distinct top and bottom onto an otherwise unbounded lattice.
-- The top is the absorbing element for the join, and the bottom is the absorbing
-- element for the meet.
data Levitated a = Top
                 | Levitate a
                 | Bottom

instance JoinSemiLattice a => JoinSemiLattice (Levitated a) where
    Top        `join` _          = Top
    _          `join` Top        = Top
    Levitate x `join` Levitate y = Levitate (x `join` y)
    Bottom     `join` lev_y      = lev_y
    lev_x      `join` Bottom     = lev_x

instance MeetSemiLattice a => MeetSemiLattice (Levitated a) where
    Top        `meet` lev_y      = lev_y
    lev_x      `meet` Top        = lev_x
    Levitate x `meet` Levitate y = Levitate (x `meet` y)
    Bottom     `meet` _          = Bottom
    _          `meet` Bottom     = Bottom

instance Lattice a => Lattice (Levitated a) where

instance JoinSemiLattice a => BoundedJoinSemiLattice (Levitated a) where
    bottom = Bottom

instance MeetSemiLattice a => BoundedMeetSemiLattice (Levitated a) where
    top = Top

instance BoundedLattice a => BoundedLattice (Levitated a) where
