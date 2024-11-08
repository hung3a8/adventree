module Adventree.Tree where

import System.Random
import Adventree.Types
import Adventree.Bin
import Adventree.Birds

minDepth :: Int
minDepth = 8
maxDepth :: Int
maxDepth = 12

-- Function to generate a random binary tree, cannot generate trees with depth > 10
generateRandomTree :: (RandomGen g) => g -> TreeLevel -> Bin NodeType
generateRandomTree gen level = fst $ generateTree gen 0 level
  where
    generateTree :: (RandomGen g) => g -> Int -> TreeLevel -> (Bin NodeType, g)
    generateTree g depth level =
      if depth >= maxDepth
        then (L (NodeType Empty False), g)
      else
        let (bird, g1) = getRandomBird (birdRarityByLevel level) g
            (nodeTypeChoice, g2) = randomR (0, 2 :: Int) g1
            nodeType = case nodeTypeChoice of
              0 -> Bird bird
              1 -> Store "Google"
              2 -> Empty
            (choice, g3) = randomR (0, 1 :: Int) g2
        in if depth < minDepth
          then let
              (leftTree, gLeft) = generateTree g3 (depth + 1) level
              (rightTree, gRight) = generateTree gLeft (depth + 1) level
            in (B (NodeType nodeType False) leftTree rightTree, gRight)
          else case choice of
            0 -> (L (NodeType nodeType False), g3)
            1 -> let
                (leftTree, gLeft) = generateTree g3 (depth + 1) level
                (rightTree, gRight) = generateTree gLeft (depth + 1) level
              in (B (NodeType nodeType False) leftTree rightTree, gRight)

generateTreeWithSeed :: Int -> TreeLevel -> Bin NodeType
generateTreeWithSeed seed level =
  let (B node left right) = generateRandomTree (mkStdGen seed) level
  in B (NodeType (Portal level) True) left right

getTreeLevel :: Bin NodeType -> Int
getTreeLevel (B (NodeType (Portal level) _) _ _) = level
getTreeLevel _ = 0
