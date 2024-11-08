module Adventree.Types where

data Cmd = GoLeft | GoRight | GoDown | Display | DisplayCheat | Quit
  deriving (Show, Read)

-- Generic Binary Tree Type, but a branch node can have a value and two children
data Bin a = L a | B a (Bin a) (Bin a)
  deriving (Show, Eq)

-- Binary Tree Context, used for tree navigation
-- Maybe NodeType is used to store the type of the parent node
data BinCxt a = Hole
              | B0 (BinCxt a) (Bin a) NodeType
              | B1 (Bin a) (BinCxt a) NodeType
  deriving (Show, Eq)

type BinZip a = (BinCxt a, Bin a)

data PlayerState = Idle | InAction deriving (Show, Read)

type GameState = (BinZip NodeType, PlayerState)

type TreeLevel = Int

-- BirdType = (birdName, birdDescription, birdRarity)
type BirdType = (String, String, Float, BirdRarity)

data BirdRarity = VeryCommon | Common | Uncommon | Rare | VeryRare | Mythological
  deriving (Show, Eq)

data BaseNodeType = Bird BirdType
              | Store String
              | Empty
              | Portal Int  -- Portal to a destination tree
              deriving (Show, Eq)

-- make data NodeType a combination of type and a boolean of revealed or not
data NodeType = NodeType BaseNodeType Bool
  deriving (Eq)

instance Show NodeType where
  show (NodeType _ False) = "???"
  show (NodeType (Bird (name, _, _, rarity)) True) = name ++ " (" ++ show rarity ++ ")"
  show (NodeType baseNodeType True) = show baseNodeType
