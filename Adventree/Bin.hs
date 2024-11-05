module Adventree.Bin where

import Data.Tree
import Data.Tree.Pretty ( drawVerticalTree )

import Adventree.Types

plug :: BinCxt a -> Bin a -> Bin a
plug Hole      t = t
plug (B0 c t2) t = plug c (B t t2)
plug (B1 t1 c) t = plug c (B t1 t)

type BinZip a = (BinCxt a,Bin a)

goLeft :: BinZip a -> Maybe (BinZip a)
goLeft (c,B t1 t2) = Just (B0 c t2,t1)
goLeft (c,L _)     = Nothing

goRight :: BinZip a -> Maybe (BinZip a)
goRight (c,B t1 t2) = Just (B1 t1 c,t2)
goRight (c,L _)     = Nothing

goDown :: BinZip a -> Maybe (BinZip a)
goDown (B0 c t2,t) = Just (c,B t t2)
goDown (B1 t1 c,t) = Just (c,B t1 t)
goDown (Hole,t)    = Nothing

graftLeft :: Bin a -> BinZip a -> BinZip a
graftLeft  g (c,t) = (c,B g t)
graftRight :: Bin a -> BinZip a -> BinZip a
graftRight g (c,t) = (c,B t g)


treeFromBin :: Show a => Bin a -> Tree String
treeFromBin (L x)     = Node (show x) []
treeFromBin (B t1 t2) = Node "*" [treeFromBin t1,treeFromBin t2]

treeCxtFromBinCxt :: Show a => BinCxt a -> Tree String -> Tree String
treeCxtFromBinCxt Hole t = t
treeCxtFromBinCxt (B0 c t2) t = treeCxtFromBinCxt c (Node "*" [t, treeFromBin t2])
treeCxtFromBinCxt (B1 t1 c) t = treeCxtFromBinCxt c (Node "*" [treeFromBin t1, t])

treeFromBinZip :: Show a => BinZip a -> Tree String
treeFromBinZip (c,t) = treeCxtFromBinCxt c (t'{rootLabel=rootLabel t' ++ marker})
  where
    t' = treeFromBin t
    marker = "@"

drawBin :: Show a => Bin a -> String
drawBin = drawVerticalTree . treeFromBin

drawBinZip :: Show a => BinZip a -> String
drawBinZip = drawVerticalTree . treeFromBinZip
