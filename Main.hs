--
-- Main.hs
-- Copyright 2012 Benjamin F Jones <benjaminfjones@gmail.com>
--
-- A Labyrinth game inspired by the Monad.Reader special issue. See README.md.
--
{-# OPTIONS_GHC -Wall -Werror #-}
module Main where

import Labyrinth ()
import LabData (l0)

-- | main, print a boilerplate
main :: IO ()
main = do
    putStrLn "Coming soon..."
    print l0
