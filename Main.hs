--
-- Main.hs
-- Copyright 2012 Benjamin F Jones <benjaminfjones@gmail.com>
--
-- A Labyrinth game inspired by the Monad.Reader special issue. See README.md.
--
{-# OPTIONS_GHC -Wall -Werror #-}
module Main where

import Labyrinth

-- | main, print a boilerplate
main :: IO ()
main = do
    putStrLn "Coming soon..."
    print l0

-- Example labyrinth
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
