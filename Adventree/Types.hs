module Adventree.Types where

data IdleCmd = GoLeft
  | GoRight
  | GoDown
  | IntoAction
  | ShowCapturePouch
  | ShowGoldPouch
  | ShowItemPouch
  | Sleep
  | Display
  | DisplayCheat
  | Quit
  deriving (Show, Read)

data ActionCmd =
  BirdCapture -- Capture a bird
  | BirdFlee -- Flee from a bird
  | BirdFeed -- Feed a bird
  | BirdDisplay -- Display a bird information

  | StoreBuy -- Buy from a store
  | StoreSell -- Sell to a store
  | StoreDisplay -- Display a store

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

type Stamina = Int

type ItemPouch = [(ItemName, Int)] -- Item name and quantity

type GameState = (BinZip NodeType, PlayerState, Stamina, CapturePouch, GoldPouch, ItemPouch)

data PlayerState = Idle | InAction deriving (Read)

instance Show PlayerState where
  show Idle = "Idle"
  show InAction = "In Action"

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

data StoreName = PigeonClub
  | DuckHouse
  | OwlChurch
  | EagleDC
  | FlamingoResort
  | PhoenixHall
  deriving (Show, Eq)

data ItemName =
  BirdSeed
  | BirdCage
  | EnergyDrink
  deriving (Show, Eq)

type Price = Int

-- store name + description + items
type StoreType = (StoreName, String, [(ItemName, Price)])

data BirdRarity = VeryCommon | Common | Uncommon | Rare | VeryRare | Mythological
  deriving (Show, Eq)

data BaseNodeType = Bird BirdType
              | Store StoreType -- Store of a specific level
              | Empty
              | Portal Int  -- Portal to a destination tree
              deriving (Show, Eq)

-- make data NodeType a combination of type and a boolean of revealed or not
data NodeType = NodeType BaseNodeType Bool
  deriving (Eq)

instance Show NodeType where
  show (NodeType _ False) = "???"
  show (NodeType (Bird (name, _, _, rarity)) True) = show name ++ " (" ++ show rarity ++ ")"
  show (NodeType (Store (name, _, _)) True) = show name
  show (NodeType baseNodeType True) = show baseNodeType
