--
-- Tests.hs
-- Copyright 2012 Benjamin Jones <benjaminfjones@gmail.com>
--

import Labyrinth
import LabData
import Control.Monad
import Test.QuickCheck
import Text.Printf
 
main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests
 
instance (Arbitrary a) => Arbitrary (Node a) where
    arbitrary = genNode

-- | randomly generate labyrinths with bounded size
genNode :: (Arbitrary a) => Gen (Node a)
genNode = sized genNode'
-- | randomly generate labryinths with at most n branches
genNode' :: (Arbitrary a) => Int -> Gen (Node a)
genNode' 0 = liftM DeadEnd arbitrary
genNode' n | n>0 = 
    oneof [liftM DeadEnd arbitrary,
           liftM2 Passage arbitrary subLab,
           liftM3 Fork arbitrary subLab subLab]
    where subLab = genNode' (n `div` 2)

instance (Arbitrary a) => Arbitrary (Branch a) where
    -- | Choose one of three branches with equal probibility
    arbitrary = oneof [ liftM Straight arbitrary, 
                        liftM2 TurnLeft arbitrary arbitrary,
                        liftM2 TurnRight arbitrary arbitrary ]

-- Trivial tests

prop_id_get z = (id . get) z == get z
    where _ = z :: Zipper Int
prop_get_id z = (get . id) z == get z
    where _ = z :: Zipper Int

-- Get / Put tests

prop_get_put x z = get (put x z) == x
    where _ = x :: Int
          _ = z :: Zipper Int
prop_put_get z = put (get z) z == z
    where _ = z :: Zipper Int

-- | list of tests
tests  = [ ("id.get/get", quickCheck prop_id_get)
         , ("get.id/get", quickCheck prop_get_id)
         , ("get.put", quickCheck prop_get_put) 
         , ("put.get", quickCheck prop_put_get) ]
