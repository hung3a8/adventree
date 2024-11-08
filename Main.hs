{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
import Adventree.Bin
import Adventree.Types
import Adventree.Parser
import Adventree.Tree
import Adventree.Birds
import Adventree.Game

import System.IO
import System.Random
import Control.Concurrent (threadDelay)
import Data.Tree (drawTree)
import System.Posix.Internals (puts)
import Data.List (unfoldr)
import Control.Monad.Accum (MonadAccum(add))

seed :: Int
seed = 75

-- Function to unfold trees using the previous tree's random generator
unfoldTrees :: (RandomGen g) => g -> TreeLevel -> [Bin NodeType]
unfoldTrees gen level = unfoldr (\(g, lvl) -> let (tree, g') = generateTreeWithSeed lvl g in Just (tree, (g', lvl + 1))) (gen, level)

-- Generate 11 different trees with levels from 0 to 10
trees :: [Bin NodeType]
trees = take 11 $ unfoldTrees (mkStdGen seed) 0

maxStamina :: Int
maxStamina = 50

initialState :: GameState
-- initial state is the list of all trees a
initialState = (map (Hole,) trees, 0, Idle, maxStamina, [], 1000000, [])

repl :: IO ()
repl = do
  putStrLn "Welcome to Binary Tree World.\n"

  go initialState (mkStdGen seed) where
    go :: (RandomGen g) => GameState -> g -> IO ()
    go gameState g = do
      -- let (z, state, stamina, capturePouch, goldPouch, itemPouch) = gameState
      let (zs, level, state, stamina, capturePouch, goldPouch, itemPouch) = gameState
      let z = zs !! level
      putStrLn ""

      case snd z of
        L node -> do
          putStrLn (displayNode node)
        B node _ _  -> do
          putStrLn (displayNode node)

      putStr ("(Stamina: " ++ show stamina ++  ") " ++ "(Level: " ++ show level ++ ") " ++ show state ++ "> ")

      case state of
        Idle -> do
          hFlush stdout
          line <- getLine

          case parseInput parseIdleCmd line of
            Nothing -> do
              putStrLn "I'm sorry, I do not understand."
              go gameState g

            Just GoLeft ->
              -- if stamina < 1, do not allow the player to move
              if stamina < 1
                then do
                  putStrLn "You are too tired to move."
                  go gameState g
                else
                  case z of
                    (c, B node t1 t2) ->
                      go (updateStamina (updateBinZipAtLevel gameState (B0 c (reveal t2) node, reveal t1) level) (-1)) g
                    (c, L _) -> do
                      putStrLn "You cannot climb any further."
                      go gameState g

            Just GoRight -> do
              if stamina < 1
                then do
                  putStrLn "You are too tired to move."
                  go gameState g
                else
                  case z of
                    (c, B node t1 t2) ->
                      go (updateStamina (updateBinZipAtLevel gameState (B1 (reveal t1) c node, reveal t2) level) (-1)) g
                    (c, L _) -> do
                      putStrLn "You cannot climb any further."
                      go gameState g

            Just GoDown ->
              if stamina < 1
                then do
                  putStrLn "You are too tired to move."
                  go gameState g
                else
                  case z of
                    (B0 c t2 node, t) -> go (updateStamina (updateBinZipAtLevel gameState (c, B node t t2) level) (-1)) g
                    (B1 t1 c node, t) -> go (updateStamina (updateBinZipAtLevel gameState (c, B node t1 t) level) (-1)) g
                    (Hole, t) -> do
                      putStrLn "You are already at the root."
                      putStrLn "You cannot climb down any further."
                      go gameState g

            Just Display -> do
              let (s, hasParents, hasMoreBranches) = drawCurrentSubTreeBinZip z
              if hasMoreBranches
                then putStrLn "There are more branches, but your eyes cannot see that far."
                else putStrLn ""
              putStrLn $ reverseTree s
              if hasParents
                then putStrLn "\n You can climb down, but looking down is too scary for you."
                else putStrLn "\nYou are at the root. Lets climb up."
              go gameState g

            Just IntoAction -> do
              putStrLn "You prepare for some actions."
              go (togglePlayerState gameState) g

            Just Sleep -> do
              putStrLn "You decided to sleep for a while."
              putStrLn "You fell asleep..."
              threadDelay 6000000

              -- Random between 0 and 1, if 0 then recover the stamina, if 1 then both recover stamina and spawn a bird randomly
              let (choice, g') = randomR (0, 10 :: Int) g
              if choice == 0
                then do
                  let (newTree, g'') = spawnBird (snd z) g'
                  case newTree of
                    Just t -> do
                      putStrLn "You woke up feeling refreshed."
                      putStrLn "You feel the branch above you is shaking. Better go check it out."
                      let newz = (fst z, t)
                      go (updateStamina (updateBinZipAtLevel gameState newz level) (max maxStamina stamina)) g''
                    Nothing -> do
                      putStrLn "You woke up feeling refreshed."
                      putStrLn "You did not find any bird nearby."
                      go (setStamina gameState (max maxStamina stamina)) g''
                else do
                  putStrLn "You woke up feeling refreshed."
                  go (setStamina gameState (max maxStamina stamina)) g'

            Just ShowCapturePouch -> do
              putStrLn "You have captured the following birds:"
              mapM_ (\(index, (name, _, _, rarity, _)) -> putStrLn (show index ++ ": " ++ show name ++ " (" ++ show rarity ++ ")")) (zip [1..] capturePouch)
              go gameState g

            Just ShowGoldPouch -> do
              putStrLn ("You have " ++ show goldPouch ++ " gold.")
              go gameState g

            Just ShowItemPouch -> do
              putStrLn "You have the following items:"
              mapM_ (\(index, (name, quantity)) -> putStrLn (show index ++ ": " ++ show name ++ " (" ++ show quantity ++ ")")) (zip [1..] itemPouch)
              go gameState g

            Just DisplayCheat -> do
              -- reveal z tree
              let (s, _, _) = drawCurrentSubTreeBinZip (fst z, revealAll (snd z))
              putStrLn $ reverseTree s
              go gameState g

            Just Quit -> do
              putStrLn "Okay."
              putStrLn "You ended the game over here:\n"
              let (s, _, _) = drawCurrentSubTreeBinZip z
              putStrLn $ reverseTree s
              putStrLn "Goodbye."
              return ()

        InAction -> do
          let node = getBinZipCurrentNode z

          case node of
            NodeType (Bird bird) _ -> do
              hFlush stdout
              line <- getLine

              let (name, description, chance, rarity, difficulty) = bird
              case parseInput parseActionCmd line of
                Nothing -> do
                  putStrLn "I'm sorry, I do not understand."
                  go gameState g

                Just BirdCapture -> do
                  if stamina < 5
                    then do
                      putStrLn "You are too tired to capture the bird."
                      go gameState g
                    else do
                      -- Random between 0 and 100, if the random number is larger than the difficulty, the bird is captured
                      -- putStrLn "Nice! You captured the bird."
                      -- putStrLn ("[ " ++ show node ++ " ] was added to your capture pouch.")
                      -- let newTree = replaceBinZipCurrentNode z (NodeType Empty True)
                      -- go (updateStamina (updateBinZipAtLevel (addBirdToCapturePouch gameState bird) newTree level) (-5)) g
                      let (randomNumber, g') = randomR (0, 100 :: Int) g
                      if randomNumber >= difficulty
                        then do
                          putStrLn "Nice! You captured the bird."
                          putStrLn ("[ " ++ show node ++ " ] was added to your capture pouch.")
                          let newTree = replaceBinZipCurrentNode z (NodeType Empty True)
                          go (updateStamina (updateBinZipAtLevel (addBirdToCapturePouch gameState bird) newTree level) (-5)) g'
                        else do
                          putStrLn "The bird is too quick for you. You missed the chance. But the bird stays there."
                          go (updateStamina gameState (-5)) g'


                Just UseItem -> do
                  -- Show list of items, ask for item index
                  putStrLn "You look at your item pouch to see if there are any items you want to use."
                  if null itemPouch
                    then do
                      putStrLn "You do not have any items to use."
                      go gameState g
                    else do
                      putStrLn "You have the following items:"
                      mapM_ (\(index, (name, quantity)) -> putStrLn (show index ++ ": " ++ show name ++ " (" ++ show quantity ++ ")")) (zip [1..] itemPouch)
                      putStr "Which item would you like to use? "
                      hFlush stdout
                      itemIndex <- getLine
                      let index = read itemIndex :: Int
                      if index < 1 || index > length itemPouch
                        then do
                          putStrLn "Invalid item index."
                          go gameState g
                        else do
                          let (itemName, quantity) = itemPouch !! (index - 1)
                          putStrLn ("You used [ " ++ show itemName ++ " ].")

                          -- in case the item is a bird seed, bird cage, or energy drink, update the game state
                          -- else do nothing
                          case itemName of
                            -- Update current bird using feedBird
                            BirdSeed -> do
                              let birdAfterFed = feedBird bird
                              putStrLn "You fed the bird. The bird looks happier."
                              let newBinZip = replaceBinZipCurrentNode z (NodeType (Bird birdAfterFed) True)
                              go (updateStamina (removeItemFromItemPouch (updateBinZipAtLevel gameState newBinZip level) itemName 1) (-5)) g

                            -- Bird cage can immediately capture a bird, but cost 30 stamina
                            BirdCage -> do
                              if stamina < 30
                                then do
                                  putStrLn "You are too tired to capture the bird."
                                  go gameState g
                                else do
                                  let (bird, g') = getRandomBird [VeryCommon, Common, Uncommon, Rare, VeryRare, Mythological] g
                                  putStrLn "You used the bird cage and captured a bird."
                                  putStrLn ("[ " ++ show bird ++ " ] was added to your capture pouch.")
                                  go (updateStamina (removeItemFromItemPouch (addBirdToCapturePouch gameState bird) itemName 1) (-30)) g'

                            -- recover 50 stamina, exceed the max stamina
                            EnergyDrink -> do
                              putStrLn "You used the energy drink and feel refreshed."
                              go (updateStamina (removeItemFromItemPouch gameState itemName 1) 50) g

                            _ -> do
                              putStrLn "You cannot use this item here."
                              go gameState g

                Just BirdDisplay -> do
                  putStrLn ("Bird name: " ++ show name)
                  putStrLn ("Rarity: " ++ show rarity)
                  putStrLn ("Description: " ++ description)
                  putStrLn ("Chance to find one: " ++ show (chance * 100) ++ "%")
                  putStrLn ("Difficulty: " ++ show difficulty)
                  go gameState g

                Just TreeDisplay -> do
                  let (s, hasParents, hasMoreBranches) = drawCurrentSubTreeBinZip z
                  if hasMoreBranches
                    then putStrLn "There are more branches, but your eyes cannot see that far."
                    else putStrLn ""
                  putStrLn $ reverseTree s
                  if hasParents
                    then putStrLn "\n You can climb down, but looking down is too scary for you."
                    else putStrLn "\nYou are at the root. Lets climb up."
                  go gameState g

                Just QuitAction -> do
                  putStrLn "You ended the action."
                  go (togglePlayerState gameState) g

                _ -> do
                  putStrLn "I'm sorry, I do not understand."
                  go gameState g

            NodeType (Store (name, description, items)) _ -> do
              hFlush stdout
              line <- getLine

              case parseInput parseActionCmd line of
                Nothing -> do
                  putStrLn "I'm sorry, I do not understand."
                  go gameState g

                Just StoreBuy -> do
                  -- ask for item index
                  putStrLn "Items:"
                  -- map item and price with index and show them
                  mapM_ (\(index, (itemName, price)) -> putStrLn (show index ++ ": " ++ show itemName ++ " (" ++ show price ++ " gold)")) (zip [1..] items)
                  putStr "Which item would you like to buy? "
                  hFlush stdout
                  itemIndex <- getLine
                  let index = read itemIndex :: Int
                  if index < 1 || index > length items
                    then do
                      putStrLn "Invalid item index."
                      go gameState g
                    else do
                      let (itemName, price) = items !! (index - 1)
                      if goldPouch < price
                        then do
                          putStrLn "You do not have enough gold."
                          go gameState g
                        else do
                          putStrLn ("You bought [ " ++ show itemName ++ " ] for " ++ show price ++ " gold. The item is added to your item pouch.")
                          go (addItemToItemPouch (addGoldToGoldPouch gameState (-price)) itemName 1) g

                Just StoreSell -> do
                  putStrLn "You look at your bird cages to see if there are any birds you want to sell."
                  -- if capture pouch is empty, tell the player
                  if null capturePouch
                    then do
                      putStrLn "You do not have any birds to sell."
                      go gameState g
                    else do
                      putStrLn "You have the following birds:"
                      mapM_ (\(index, (name, _, _, rarity, _)) -> putStrLn (show index ++ ": " ++ show name ++ " (" ++ show rarity ++ ")")) (zip [1..] capturePouch)
                      putStr "Which bird would you like to sell? "
                      hFlush stdout
                      birdIndex <- getLine
                      let index = read birdIndex :: Int
                      if index < 1 || index > length capturePouch
                        then do
                          putStrLn "Invalid bird index."
                          go gameState g
                        else do
                          let bird = capturePouch !! (index - 1)
                          -- make price deviation based on chance
                          let (price, gNext) = getBirdPriceQuote bird g
                          putStrLn ("You sold [ " ++ show node ++ " ] for " ++ show price ++ " gold.")
                          go (addGoldToGoldPouch (addBirdToCapturePouch gameState (capturePouch !! (index - 1))) price) gNext

                Just StoreDisplay -> do
                  putStrLn ("Store name: " ++ show name)
                  putStrLn ("Description: " ++ description)
                  putStrLn "Items:"
                  -- map item and price with index and show them
                  mapM_ (\(index, (itemName, price)) -> putStrLn (show index ++ ": " ++ show itemName ++ " (" ++ show price ++ " gold)")) (zip [1..] items)
                  go gameState g

                Just TreeDisplay -> do
                  let (s, hasParents, hasMoreBranches) = drawCurrentSubTreeBinZip z
                  if hasMoreBranches
                    then putStrLn "There are more branches, but your eyes cannot see that far."
                    else putStrLn ""
                  putStrLn $ reverseTree s
                  if hasParents
                    then putStrLn "\n You can climb down, but looking down is too scary for you."
                    else putStrLn "\nYou are at the root. Lets climb up."
                  go gameState g

                Just QuitAction -> do
                  putStrLn "You ended the action."
                  go (togglePlayerState gameState) g

                _ -> do
                  putStrLn "I'm sorry, I do not understand."
                  go gameState g

            NodeType (Portal level) _ -> do
              hFlush stdout
              line <- getLine

              case parseInput parseActionCmd line of
                Nothing -> do
                  putStrLn "I'm sorry, I do not understand."
                  go gameState g

                Just JumpPortal -> do
                  putStr "Choose a level to jump to: "
                  hFlush stdout
                  newLevel <- getLine
                  let newLevel' = read newLevel :: Int
                  if newLevel' < 0 || newLevel' > 10
                    then do
                      putStrLn "Invalid level."
                      go gameState g
                    else do
                      putStrLn ("You jumped to level " ++ show newLevel' ++ ".")

                  -- check if user has the portal key to jump to the level
                  if PortalKey newLevel' `elem` map fst itemPouch
                    then do
                      putStrLn "You used the portal key to jump to the new level."
                      go (changeLevel gameState newLevel') g
                    else do
                      putStrLn "You do not have the portal key to jump to the new level."
                      go gameState g

                Just QuitAction -> do
                  putStrLn "You ended the action."
                  go (togglePlayerState gameState) g

                _ -> do
                  putStrLn "I'm sorry, I do not understand."
                  go gameState g

            NodeType _ _ -> do
              hFlush stdout
              line <- getLine

              case parseInput parseActionCmd line of
                Nothing -> do
                  putStrLn "I'm sorry, I do not understand."
                  go gameState g

                Just UseItem -> do
                  -- still shows all the items and ask for item index, but only allow energy drink to be used
                  putStrLn "You look at your item pouch to see if there are any items you want to use."
                  if null itemPouch
                    then do
                      putStrLn "You do not have any items to use."
                      go gameState g
                    else do
                      putStrLn "You have the following items:"
                      mapM_ (\(index, (name, quantity)) -> putStrLn (show index ++ ": " ++ show name ++ " (" ++ show quantity ++ ")")) (zip [1..] itemPouch)
                      putStr "Which item would you like to use? "
                      hFlush stdout
                      itemIndex <- getLine
                      let index = read itemIndex :: Int
                      if index < 1 || index > length itemPouch
                        then do
                          putStrLn "Invalid item index."
                          go gameState g
                        else do
                          let (itemName, quantity) = itemPouch !! (index - 1)
                          putStrLn ("You used [ " ++ show itemName ++ " ].")

                          case itemName of
                            EnergyDrink -> do
                              putStrLn "You used the energy drink and feel refreshed."
                              go (updateStamina (removeItemFromItemPouch gameState itemName 1) 50) g

                            _ -> do
                              putStrLn "You cannot use this item here."
                              go gameState g


                Just TreeDisplay -> do
                  let (s, hasParents, hasMoreBranches) = drawCurrentSubTreeBinZip z
                  if hasMoreBranches
                    then putStrLn "There are more branches, but your eyes cannot see that far."
                    else putStrLn ""
                  putStrLn $ reverseTree s
                  if hasParents
                    then putStrLn "\n You can climb down, but looking down is too scary for you."
                    else putStrLn "\nYou are at the root. Lets climb up."
                  go gameState g

                Just QuitAction -> do
                  putStrLn "You ended the action."
                  go (togglePlayerState gameState) g

                _ -> do
                  putStrLn "I'm sorry, I do not understand."
                  go gameState g

main = repl
