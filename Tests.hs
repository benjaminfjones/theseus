--
-- Tests.hs
-- Copyright 2012 Benjamin Jones <benjaminfjones@gmail.com>
--

import Labyrinth
import LabData
import Control.Monad
import Test.QuickCheck
import Text.Printf
import Text.Show.Functions
 
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

prop_update_id z = (update id z) == z
    where _ = z :: Zipper Int

-- | Check if applying a random function using `update` is compatible with 
-- `get` and `put`
prop_update_f :: Int -> (Int -> Int) -> Zipper Int -> Bool
prop_update_f x f z = (get . update f $ put x z) == f x

-- Forward / Back tests

prop_right_back :: Zipper Int -> Bool
prop_right_back z@(t, Fork x l r) = (turnRight z >>= back) == Just z
prop_right_back _ = True

prop_left_back :: Zipper Int -> Bool
prop_left_back z@(t, Fork x l r) = (turnLeft z >>= back) == Just z
prop_left_back _ = True

prop_straight_back :: Zipper Int -> Bool
prop_straight_back z@(t, Passage x n) = (straight z >>= back) == Just z
prop_straight_back _ = True

-- | list of tests
tests  = [ ("id.get/get", quickCheck prop_id_get)
         , ("get.id/get", quickCheck prop_get_id)
         , ("get.put", quickCheck prop_get_put) 
         , ("put.get", quickCheck prop_put_get) 
         , ("right.back", quickCheck prop_right_back)  
         , ("left.back", quickCheck prop_left_back)
         , ("straight.back", quickCheck prop_straight_back)
         , ("update.id/id", quickCheck prop_update_id)
         , ("get.update_f.put/f", quickCheck prop_update_f)  ] 
