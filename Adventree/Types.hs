module Adventree.Types where

data IdleCmd = GoLeft | GoRight | GoDown | IntoAction | ShowCapturePouch | ShowGoldPouch | Display | DisplayCheat | Quit
  deriving (Show, Read)

data ActionCmd =
  BirdCapture -- Capture a bird
  | BirdFlee -- Flee from a bird
  | BirdFeed -- Feed a bird
  | BirdDisplay -- Display a bird information

  | TreeDisplay

  | QuitAction -- Quit the action
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

type CapturePouch = [BirdType]

type GoldPouch = Int

data PlayerState = Idle | InAction deriving (Show, Read)

type GameState = (BinZip NodeType, PlayerState, CapturePouch, GoldPouch)

type TreeLevel = Int

data BirdName = Pigeon
  | Sparrow
  | Crow
  | Seagull
  | Robin
  | BlueJay
  | Starling
  | Finch
  | Duck
  | Goose
  | Cardinal
  | Swallow
  | Woodpecker
  | Magpie
  | Mockingbird
  | Hummingbird
  | Swan
  | Heron
  | Eagle
  | Owl
  | Kingfisher
  | Pelican
  | Crane
  | Toucan
  | Cockatoo
  | Albatross
  | Flamingo
  | Peacock
  | Penguin
  | Kiwi
  | HarpyEagle
  | Kakapo
  | Phoenix
  | Griffin
  | Roc
  deriving (Show, Eq)

type BirdType = (BirdName, String, Float, BirdRarity)

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
  show (NodeType (Bird (name, _, _, rarity)) True) = show name ++ " (" ++ show rarity ++ ")"
  show (NodeType baseNodeType True) = show baseNodeType
