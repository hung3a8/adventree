module Adventree.Tree where

import System.Random
import Adventree.Types
import Adventree.Bin
import Adventree.Birds
import Adventree.Stores

minDepth :: Int
minDepth = 8
maxDepth :: Int
maxDepth = 12
storeDepth :: Int
storeDepth = 3

generateRandomTree :: (RandomGen g) => g -> TreeLevel -> (Bin NodeType, g)
generateRandomTree gen level = generateTree gen 0 level
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
              1 -> Empty
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

insertStore :: (RandomGen g) => g -> TreeLevel -> Bin NodeType -> Bin NodeType
insertStore gen level tree = fst $ traverseAndInsert gen tree 0
  where
    traverseAndInsert :: (RandomGen g) => g -> Bin NodeType -> Int -> (Bin NodeType, g)
    traverseAndInsert g (L _) depth = (L (NodeType (Store (getStoreByLevel level)) False), g)
    traverseAndInsert g (B node t1 t2) depth
      | depth >= storeDepth = (B (NodeType (Store (getStoreByLevel level)) False) t1 t2, g)
      | otherwise =
          let (choice, g1) = randomR (0, 1 :: Int) g
          in if choice == 0
             then let (newLeft, gLeft) = traverseAndInsert g1 t1 (depth + 1)
                  in (B node newLeft t2, gLeft)
             else let (newRight, gRight) = traverseAndInsert g1 t2 (depth + 1)
                  in (B node t1 newRight, gRight)

generateTreeWithSeed :: (RandomGen g) => TreeLevel -> g -> (Bin NodeType, g)
generateTreeWithSeed level g = do
    let (tree, g') = generateRandomTree g level
    let treeWithStore = insertStore g' level tree
    (B (NodeType (Portal level) True) (getLeft treeWithStore) (getRight treeWithStore), g)

getLeft :: Bin a -> Bin a
getLeft (B _ left _) = left
getLeft _ = error "No left subtree"

getRight :: Bin a -> Bin a
getRight (B _ _ right) = right
getRight _ = error "No right subtree"

getTreeLevel :: Bin NodeType -> Int
getTreeLevel (B (NodeType (Portal level) _) _ _) = level
getTreeLevel _ = 0
