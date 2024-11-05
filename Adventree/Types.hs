module Adventree.Types where

data Cmd = GoLeft | GoRight | GoDown | Display | Quit
  deriving (Show, Read)

data Bin a = L a | B (Bin a) (Bin a)
  deriving (Show, Eq)

data BinCxt a = Hole
              | B0 (BinCxt a) (Bin a)
              | B1 (Bin a) (BinCxt a)
  deriving (Show, Eq)

data PlayerState = Idle | InAction deriving (Show, Read)

-- data GameState = GameState
--   {
--     playerState :: PlayerState,
--     tree :: Bin Int
--   } deriving (Show)
