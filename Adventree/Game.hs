module Adventree.Game where

import System.Random
import Adventree.Types
import Adventree.Birds

togglePlayerState :: GameState -> GameState
togglePlayerState (z, state, stamina, capturePouch, goldPouch, itemPouch) = case state of
  Idle -> (z, InAction, stamina, capturePouch, goldPouch, itemPouch)
  InAction -> (z, Idle, stamina, capturePouch, goldPouch, itemPouch)

updateStamina :: GameState -> Int -> GameState
updateStamina (z, state, stamina, capturePouch, goldPouch, itemPouch) delta =
  (z, state, stamina + delta, capturePouch, goldPouch, itemPouch)

setStamina :: GameState -> Int -> GameState
setStamina (z, state, _, capturePouch, goldPouch, itemPouch) stamina =
  (z, state, stamina, capturePouch, goldPouch, itemPouch)

addBirdToCapturePouch :: GameState -> BirdType -> GameState
addBirdToCapturePouch (z, state, stamina, capturePouch, goldPouch, itemPouch) bird =
  (z, state, stamina, bird : capturePouch, goldPouch, itemPouch)

addGoldToGoldPouch :: GameState -> Int -> GameState
addGoldToGoldPouch (z, state, stamina, capturePouch, goldPouch, itemPouch) gold =
  (z, state, stamina, capturePouch, goldPouch + gold, itemPouch)

addItemToItemPouch :: GameState -> ItemName -> Int -> GameState
addItemToItemPouch (z, state, stamina, capturePouch, goldPouch, itemPouch) item quantity =
  (z, state, stamina, capturePouch, goldPouch, updateItemPouch item quantity itemPouch)
  where
    updateItemPouch :: ItemName -> Int -> ItemPouch -> ItemPouch
    -- find the item in the item pouch, increase the quantity if it exists
    updateItemPouch item quantity [] = [(item, quantity)]
    updateItemPouch item quantity ((itemName, itemQuantity):rest) =
      if itemName == item
        then (itemName, itemQuantity + quantity) : rest
        else (itemName, itemQuantity) : updateItemPouch item quantity rest

updateBinZip :: GameState -> BinZip NodeType -> GameState
updateBinZip (z, state, stamina, capturePouch, goldPouch, itemPouch) z' = (z', state, stamina, capturePouch, goldPouch, itemPouch)

-- Choose a random Empty node and spawn a bird. If on a branch and the branch is empty, randomly choose to spawn the bird on the branch node or left/right child.
-- If the branch is not empty, randomly choose to spawn the bird on the left or right child.
-- If leaf node, spawn the bird on the leaf node.
-- Return Maybe Bin NodeType to indicate if the tree has been updated.
spawnBird :: (RandomGen g) => Bin NodeType -> g -> (Maybe (Bin NodeType), g)
spawnBird (L (NodeType Empty False)) g =
  let (bird, g') = getRandomBird [VeryCommon, Common, Uncommon, Rare, VeryRare, Mythological] g
  in (Just (L (NodeType (Bird bird) True)), g')
spawnBird (L _) g = (Nothing, g)
spawnBird (B (NodeType Empty False) t1 t2) g =
  let (choice, g') = randomR (0, 2 :: Int) g
  in case choice of
        0 -> let (bird, g'') = getRandomBird [VeryCommon, Common, Uncommon, Rare, VeryRare, Mythological] g'
            in (Just (B (NodeType (Bird bird) True) t1 t2), g'')
        1 -> let (newLeft, g'') = spawnBird t1 g'
            in case newLeft of
                  Just t1' -> (Just (B (NodeType Empty False) t1' t2), g'')
                  Nothing -> (Nothing, g'')
        2 -> let (newRight, g'') = spawnBird t2 g'
            in case newRight of
                  Just t2' -> (Just (B (NodeType Empty False) t1 t2'), g'')
                  Nothing -> (Nothing, g'')
spawnBird (B node t1 t2) g =
  let (choice, g') = randomR (0, 1 :: Int) g
  in case choice of
        0 -> let (newLeft, g'') = spawnBird t1 g'
            in case newLeft of
                  Just t1' -> (Just (B node t1' t2), g'')
                  Nothing -> (Nothing, g'')
        1 -> let (newRight, g'') = spawnBird t2 g'
            in case newRight of
                  Just t2' -> (Just (B node t1 t2'), g'')
                  Nothing -> (Nothing, g'')
