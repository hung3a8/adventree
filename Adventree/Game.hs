module Adventree.Game where

import Adventree.Types

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
