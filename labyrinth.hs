--
-- labyrinth.hs
-- Copyright 2012 Benjamin Jones <benjaminfjones@gmail.com>
--
module Labyrinth
( Node(..)
, Thread(..)
, Branch(..)
, Zipper(..)
, get
, put
, turnRight
, turnLeft
, straight
, back
, retreive
, update
) where

-- | Labyrinth node data structure
data Node a = DeadEnd a
            | Passage a (Node a)
            | Fork a (Node a) (Node a)

instance (Show a) => Show (Node a) where
    show = show' 0 where
        show' n (DeadEnd x) = (pad n) ++ "DeadEnd " ++ (show x)
        show' n (Passage x node) = (pad n) ++ "Passage " ++ (show x) ++ "\n" ++
                                   (show' (n+1) node)
        show' n (Fork x node1 node2) = (pad n) ++ "Fork " ++ (show x) ++ "\n" ++
                                   (show' (n+1) node1) ++ "\n" ++
                                   (show' (n+1) node2)
        pad n = concat . take n . repeat $ "   "

-- Example:
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

-- | We keep track of the player using a list of branches. There are three types of 
-- branches.
data Branch a = Straight a            -- ^ A straight passage has no sub-tree attached
              | TurnLeft a (Node a)   -- ^ After turning left, we save the right-tree
              | TurnRight a (Node a)  -- ^ After turning right, we save the left-tree

-- | A Thread is a list of branches. Each branch contains a value of type `a` and
-- possibly a sub-tree
type Thread a = [Branch a]

-- | A Zipper consists of a thread of branches (with attaches sub-trees) as well
-- as the current sub-tree (the focus). The idea is that the whole tree can be 
-- reconstructed from any Zipper by starting at the Node and working backwards
-- through the thread attaching sub-trees.
type Zipper a = (Thread a, Node a)

-- | Get the data stored at the given Zipper
get :: Zipper a -> a
get (_, DeadEnd x) = x
get (_, Passage x _) = x
get (_, Fork x _ _)  = x

-- | Change the value at the given node
put :: a -> Zipper a -> Zipper a
put x (t, DeadEnd _) = (t, DeadEnd x)
put x (t, Passage _ y) = (t, Passage x y)
put x (t, Fork _ y z) = (t, Fork x y z)

--------------------------------------------------------------------------------
-- Moving the player through the labyrinth

turnRight :: Zipper a -> Maybe (Zipper a)
turnRight (t, Fork x l r) = Just (TurnRight x l : t, r) -- you can only turn right at
turnRight _ = Nothing -- failed movement                -- a fork

turnLeft :: Zipper a -> Maybe (Zipper a)
turnLeft (t, Fork x l r) = Just (TurnLeft x r : t, l)
turnLeft _ = Nothing

straight :: Zipper a -> Maybe (Zipper a)
straight (t, Passage x n) = Just (Straight x:t, n)
straight _ = Nothing

back :: Zipper a -> Maybe (Zipper a)
back (Straight x:t, n)    = Just (t, Passage x n)
back (TurnLeft x r:t, l)  = Just (t, Fork x l r)
back (TurnRight x l:t, r) = Just (t, Fork x l r)
back ([], _)              = Nothing -- can't go back if there's nothing there

--------------------------------------------------------------------------------
-- Accessing and updating the player's environment

-- | Retreive the extra data at a player's location (given by a thread and a base node)
retreive :: Zipper a -- ^ zipper tree structure giving the player's current position
         -> a        -- ^ extra data at the player's position
retreive = get

-- | Update the extra data at a player's location using the function f
update :: (a -> a) -- ^ Function to apply to extra data at player's location
       -> Zipper a -- ^ Player's current location
       -> Zipper a -- ^ Location with new data
update f (t, DeadEnd x)      = (t, DeadEnd (f x))
update f (t, Passage x p)    = (t, Passage (f x) p)
update f (t, Fork x l r)     = (t, Fork (f x) l r)
