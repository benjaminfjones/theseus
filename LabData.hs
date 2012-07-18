--
-- LabData.hs
-- Copyright 2012 Benjamin Jones <benjaminfjones@gmail.com>
--

-- | This module contains several concrete labyrinth data structures
module LabData where

import Labyrinth

-- | An example labyrinth
l0 :: Node (Int, Int)
l0 = Fork (0,2) 
     (Fork (-2, 0) 
           (DeadEnd (0,-2)) 
           (DeadEnd (-1,0)))
     (Passage (2,0) 
              (Fork (1,0)
                    (Passage (0,1)
                             (DeadEnd (0,0)))
                    (DeadEnd (0,-1))))
