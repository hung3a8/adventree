module Adventree.Tree where

import System.Random
import Adventree.Types
import Adventree.Bin


-- Function to generate a random binary tree
generateRandomTree :: (RandomGen g) => g -> Bin Int
generateRandomTree gen = fst $ generateTree gen
  where
    generateTree :: (RandomGen g) => g -> (Bin Int, g)
    generateTree g =
      let (val, g1) = random g
          (choice, g2) = randomR (0, 4 :: Int) g1
      in case choice of
           0 -> (L (val `mod` 1000), g2)
           1 -> (L (val `mod` 1000), g2)
           _ -> let (leftTree, g3) = generateTree g2
                    (rightTree, g4) = generateTree g3
                in (B leftTree rightTree, g4)

-- Function to generate a tree with a deterministic seed
generateTreeWithSeed :: Int -> Bin Int
generateTreeWithSeed seed = generateRandomTree (mkStdGen seed)
