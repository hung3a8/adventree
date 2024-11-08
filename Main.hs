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
import Control.Monad.Accum (MonadAccum(add))

seed :: Int
seed = 75

tree :: Bin NodeType
tree = generateTreeWithSeed seed 10

maxStamina :: Int
maxStamina = 50

initialState :: GameState
initialState = ((Hole, tree), Idle, maxStamina, [], 0, [])

repl :: IO ()
repl = do
  putStrLn "Welcome to Binary Tree World.\n"

  go initialState (mkStdGen seed) where
    go :: (RandomGen g) => GameState -> g -> IO ()
    go gameState g = do
      let (z, state, stamina, capturePouch, goldPouch, itemPouch) = gameState
      putStrLn ""

      case snd z of
        L node -> do
          putStrLn (displayNode node)
        B node _ _  -> do
          putStrLn (displayNode node)

      let level = getTreeLevel (snd z)

      case state of
        Idle -> do
          putStr ("(" ++ show stamina ++  ") " ++ show state ++ "> ")

          hFlush stdout
          line <- getLine

          case parseInput parseIdleCmd line of
              Nothing -> do
                putStrLn "I'm sorry, I do not understand."
                go gameState g

              Just GoLeft ->
                case z of
                  (c, B node t1 t2) ->
                    go (updateBinZip gameState (B0 c (reveal t2) node, reveal t1)) g
                  (c, L _) -> do
                    putStrLn "You cannot climb any further."
                    go gameState g

              Just GoRight -> do
                case z of
                  (c, B node t1 t2) ->
                    go (updateBinZip gameState (B1 (reveal t1) c node, reveal t2)) g
                  (c, L _) -> do
                    putStrLn "You cannot climb any further."
                    go gameState g

              Just GoDown ->
                case z of
                  (B0 c t2 node, t) -> go (updateBinZip gameState (c, B node t t2)) g
                  (B1 t1 c node, t) -> go (updateBinZip gameState (c, B node t1 t))g
                  (Hole,t) -> do
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
                        go (updateStamina (updateBinZip gameState newz) maxStamina) g''
                      Nothing -> do
                        putStrLn "You woke up feeling refreshed."
                        putStrLn "You did not find any bird nearby."
                        go (setStamina gameState maxStamina) g''
                  else do
                    putStrLn "You woke up feeling refreshed."
                    go (setStamina gameState maxStamina) g'

              Just ShowCapturePouch -> do
                putStrLn "You have captured the following birds:"
                mapM_ (\(index, (name, _, _, rarity)) -> putStrLn (show index ++ ": " ++ show name ++ " (" ++ show rarity ++ ")")) (zip [1..] capturePouch)
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
          putStr (show state ++ "> ")

          let node = getBinZipCurrentNode z

          case node of
            NodeType (Bird bird) _ -> do
              hFlush stdout
              line <- getLine

              let (name, description, chance, rarity) = bird
              case parseInput parseActionCmd line of
                Nothing -> do
                  putStrLn "I'm sorry, I do not understand."
                  go gameState g

                Just BirdCapture -> do
                  putStrLn "Nice! You captured the bird."
                  putStrLn ("[ " ++ show node ++ " ] was added to your capture pouch.")
                  let newTree = replaceBinZipCurrentNode z (NodeType Empty True)
                  go (updateBinZip (addBirdToCapturePouch gameState bird) newTree) g

                Just BirdFeed -> do
                  putStrLn "You fed the bird."
                  go gameState g

                Just BirdDisplay -> do
                  putStrLn ("Bird name: " ++ show name)
                  putStrLn ("Rarity: " ++ show rarity)
                  putStrLn ("Description: " ++ description)
                  putStrLn ("Chance to find one: " ++ show (chance * 100) ++ "%")
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
                      mapM_ (\(index, (name, _, _, rarity)) -> putStrLn (show index ++ ": " ++ show name ++ " (" ++ show rarity ++ ")")) (zip [1..] capturePouch)
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

            NodeType _ _ -> do
              putStrLn "The node you are standing on is not interactable. Back to Idle mode."
              go (togglePlayerState gameState) g

main = repl
