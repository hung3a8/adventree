module Adventree.Bin where

import Data.Tree
import Data.Tree.Pretty ( drawVerticalTree )
import Data.List (intercalate)

import Adventree.Types

-- reveal the current node
reveal :: Bin NodeType -> Bin NodeType
reveal (L (NodeType node _)) = L (NodeType node True)
reveal (B (NodeType node _) t1 t2) = B (NodeType node True) t1 t2

revealAll :: Bin NodeType -> Bin NodeType
revealAll (L (NodeType node _)) = L (NodeType node True)
revealAll (B (NodeType node _) t1 t2) = B (NodeType node True) (revealAll t1) (revealAll t2)

treeFromBin :: Show a => Bin a -> Tree String
treeFromBin (L node) = Node (show node) []
treeFromBin (B node t1 t2) = Node (show node) [treeFromBin t1, treeFromBin t2]

displayNode :: NodeType -> String
displayNode node = case node of
  NodeType _ False -> "You see a mysterious node."
  NodeType (Bird bird) True -> "You see a bird named " ++ show node ++ "."
  NodeType (Store name) True -> "You see a store named " ++ name ++ "."
  NodeType Empty True -> "You see an empty node."
  NodeType (Portal dest) True -> "You see a portal to tree " ++ show dest ++ "."


treeCxtFromBinCxt :: Show a => BinCxt a -> Tree String -> Tree String
treeCxtFromBinCxt Hole t = t
treeCxtFromBinCxt (B0 c (L node) pnode) t = treeCxtFromBinCxt c (Node (show pnode) [t, treeFromBin (L node)])
treeCxtFromBinCxt (B0 c (B node t1 t2) pnode) t = treeCxtFromBinCxt c (Node (show pnode) [t, treeFromBin (B node t1 t2)])
treeCxtFromBinCxt (B1 (L node) c pnode) t = treeCxtFromBinCxt c (Node (show pnode) [treeFromBin (L node), t])
treeCxtFromBinCxt (B1 (B node t1 t2) c pnode) t = treeCxtFromBinCxt c (Node (show pnode) [treeFromBin (B node t1 t2), t])

treeFromBinZip :: Show a => BinZip a -> Tree String
treeFromBinZip (c,t) = treeCxtFromBinCxt c (t'{rootLabel=rootLabel t' ++ marker})
  where
    t' = treeFromBin t
    marker = "@"

-- Helper function to limit the depth of a tree
limitDepth :: Int -> Tree a -> Tree a
limitDepth 0 (Node label _) = Node label []
limitDepth n (Node label subtrees) = Node label (map (limitDepth (n - 1)) subtrees)

-- Helper function to reverse a tree
reverseTree :: String -> String
reverseTree = intercalate "\n" . reverse . lines

-- Function to draw a Bin tree with the root at the bottom
drawBin :: Show a => Bin a -> String
drawBin = drawVerticalTree . treeFromBin

-- Function to draw a Bin zipper with the root at the bottom
drawBinZip :: Show a => BinZip a -> String
drawBinZip = drawVerticalTree . treeFromBinZip

-- Function to draw the current subtree with a depth limit of 5 and return two booleans
drawCurrentSubTreeBinZip :: Show a => BinZip a -> (String, Bool, Bool)
drawCurrentSubTreeBinZip (c, t) = (drawVerticalTree limitedTree, hasParents c, hasMoreBranches t)
  where
    limitedTree = limitDepth 5 $ treeFromBin t
    hasParents Hole = False
    hasParents _ = True
    hasMoreBranches (L _) = False
    hasMoreBranches (B _ t1 t2) = depthExceedsLimit 5 t1 || depthExceedsLimit 5 t2

-- Helper function to check if the depth of a Bin tree exceeds a limit
depthExceedsLimit :: Int -> Bin a -> Bool
depthExceedsLimit 0 _ = True
depthExceedsLimit _ (L _) = False
depthExceedsLimit n (B _ t1 t2) = depthExceedsLimit (n - 1) t1 || depthExceedsLimit (n - 1) t2

replaceBinZipCurrentNode :: BinZip a -> a -> BinZip a
replaceBinZipCurrentNode (c, L _) node = (c, L node)
replaceBinZipCurrentNode (c, B _ t1 t2) node = (c, B node t1 t2)

getBinZipCurrentNode :: BinZip a -> a
getBinZipCurrentNode (_, L node) = node
getBinZipCurrentNode (_, B node _ _) = node

isNodeType :: BaseNodeType -> BaseNodeType -> Bool
isNodeType (Bird _) (Bird _) = True
isNodeType (Store _) (Store _) = True
isNodeType Empty Empty = True
isNodeType (Portal _) (Portal _) = True
isNodeType _ _ = False
