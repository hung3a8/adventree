{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
import Adventree.Bin
import Adventree.Types
import Adventree.Parser
import Adventree.Tree
import Adventree.Game

import System.IO
import Control.Concurrent (threadDelay)
import Data.Tree (drawTree)
import System.Posix.Internals (puts)
import Control.Monad.Accum (MonadAccum(add))

tree :: Bin NodeType
tree = generateTreeWithSeed 75 10

maxStamina :: Int
maxStamina = 50

initialState :: GameState
initialState = ((Hole, tree), Idle, maxStamina, [], 100000, [])

repl :: IO ()
repl = do
  putStrLn "Welcome to Binary Tree World.\n"

  go initialState where
    go :: GameState -> IO ()
    go gameState = do
      let (z, state, stamina, capturePouch, goldPouch, itemPouch) = gameState
      putStrLn ""

      case snd z of
        L node -> do
          putStrLn (displayNode node)
        B node _ _  -> do
          putStrLn (displayNode node)


      case state of
        Idle -> do
          putStr ("(" ++ show stamina ++  ") " ++ show state ++ "> ")

          hFlush stdout
          line <- getLine

          case parseInput parseIdleCmd line of
              Nothing -> do
                putStrLn "I'm sorry, I do not understand."
                go gameState

              Just GoLeft ->
                case z of
                  (c, B node t1 t2) ->
                    go (updateBinZip gameState (B0 c (reveal t2) node, reveal t1))
                  (c, L _) -> do
                    putStrLn "You cannot climb any further."
                    go gameState

              Just GoRight -> do
                case z of
                  (c, B node t1 t2) ->
                    go (updateBinZip gameState (B1 (reveal t1) c node, reveal t2))
                  (c, L _) -> do
                    putStrLn "You cannot climb any further."
                    go gameState

              Just GoDown ->
                case z of
                  (B0 c t2 node, t) -> go (updateBinZip gameState (c, B node t t2))
                  (B1 t1 c node, t) -> go (updateBinZip gameState (c, B node t1 t))
                  (Hole,t) -> do
                    putStrLn "You are already at the root."
                    putStrLn "You cannot climb down any further."
                    go gameState

              Just Display -> do
                let (s, hasParents, hasMoreBranches) = drawCurrentSubTreeBinZip z
                if hasMoreBranches
                  then putStrLn "There are more branches, but your eyes cannot see that far."
                  else putStrLn ""
                putStrLn $ reverseTree s
                if hasParents
                  then putStrLn "\n You can climb down, but looking down is too scary for you."
                  else putStrLn "\nYou are at the root. Lets climb up."
                go gameState

              Just IntoAction -> do
                putStrLn "You prepare for some actions."
                go $ togglePlayerState gameState

              Just Sleep -> do
                putStrLn "You decided to sleep for a while."
                putStrLn "You fell asleep..."
                threadDelay 60000000
                putStrLn "You woke up. You feel refreshed."
                go $ setStamina gameState maxStamina

              Just ShowCapturePouch -> do
                putStrLn "You have captured the following birds:"
                mapM_ (\(index, (name, _, _, rarity)) -> putStrLn (show index ++ ": " ++ show name ++ " (" ++ show rarity ++ ")")) (zip [1..] capturePouch)
                go gameState

              Just ShowGoldPouch -> do
                putStrLn ("You have " ++ show goldPouch ++ " gold.")
                go gameState

              Just ShowItemPouch -> do
                putStrLn "You have the following items:"
                mapM_ (\(index, (name, quantity)) -> putStrLn (show index ++ ": " ++ show name ++ " (" ++ show quantity ++ ")")) (zip [1..] itemPouch)
                go gameState

              Just DisplayCheat -> do
                -- reveal z tree
                let (s, _, _) = drawCurrentSubTreeBinZip (fst z, revealAll (snd z))
                putStrLn $ reverseTree s
                go gameState

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
                  go gameState

                Just BirdCapture -> do
                  putStrLn "Nice! You captured the bird."
                  putStrLn ("[ " ++ show node ++ " ] was added to your capture pouch.")
                  let newTree = replaceBinZipCurrentNode z (NodeType Empty True)
                  -- go (newTree, state, capturePouch ++ [(name, description, chance, rarity)], goldPouch, itemPouch)
                  go (updateBinZip (addBirdToCapturePouch gameState bird) newTree)

                Just BirdFeed -> do
                  putStrLn "You fed the bird."
                  go gameState

                Just BirdDisplay -> do
                  putStrLn ("Bird name: " ++ show name)
                  putStrLn ("Rarity: " ++ show rarity)
                  putStrLn ("Description: " ++ description)
                  putStrLn ("Chance to find one: " ++ show (chance * 100) ++ "%")
                  go gameState

                Just TreeDisplay -> do
                  let (s, hasParents, hasMoreBranches) = drawCurrentSubTreeBinZip z
                  if hasMoreBranches
                    then putStrLn "There are more branches, but your eyes cannot see that far."
                    else putStrLn ""
                  putStrLn $ reverseTree s
                  if hasParents
                    then putStrLn "\n You can climb down, but looking down is too scary for you."
                    else putStrLn "\nYou are at the root. Lets climb up."
                  go gameState

                Just QuitAction -> do
                  putStrLn "You ended the action."
                  go $ togglePlayerState gameState

                _ -> do
                  putStrLn "I'm sorry, I do not understand."
                  go gameState

            NodeType (Store (name, description, items)) _ -> do
              hFlush stdout
              line <- getLine

              case parseInput parseActionCmd line of
                Nothing -> do
                  putStrLn "I'm sorry, I do not understand."
                  go gameState

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
                      go gameState
                    else do
                      let (itemName, price) = items !! (index - 1)
                      if goldPouch < price
                        then do
                          putStrLn "You do not have enough gold."
                          go gameState
                        else do
                          go (addItemToItemPouch (addGoldToGoldPouch gameState (-price)) itemName 1)

                Just StoreSell -> do
                  putStrLn "You sold an item."
                  go gameState

                Just StoreDisplay -> do
                  putStrLn ("Store name: " ++ show name)
                  putStrLn ("Description: " ++ description)
                  putStrLn "Items:"
                  -- map item and price with index and show them
                  mapM_ (\(index, (itemName, price)) -> putStrLn (show index ++ ": " ++ show itemName ++ " (" ++ show price ++ " gold)")) (zip [1..] items)
                  go gameState

                Just TreeDisplay -> do
                  let (s, hasParents, hasMoreBranches) = drawCurrentSubTreeBinZip z
                  if hasMoreBranches
                    then putStrLn "There are more branches, but your eyes cannot see that far."
                    else putStrLn ""
                  putStrLn $ reverseTree s
                  if hasParents
                    then putStrLn "\n You can climb down, but looking down is too scary for you."
                    else putStrLn "\nYou are at the root. Lets climb up."
                  go gameState

                Just QuitAction -> do
                  putStrLn "You ended the action."
                  go $ togglePlayerState gameState

                _ -> do
                  putStrLn "I'm sorry, I do not understand."
                  go gameState

            NodeType _ _ -> do
              putStrLn "The node you are standing on is not interactable. Back to Idle mode."
              go $ togglePlayerState gameState

main = repl
