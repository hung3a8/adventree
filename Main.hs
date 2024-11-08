import Adventree.Bin
import Adventree.Types
import Adventree.Parser
import Adventree.Tree

import System.IO
import Control.Concurrent (threadDelay)
import Data.Tree (drawTree)
import System.Posix.Internals (puts)

tree :: Bin NodeType
tree = generateTreeWithSeed 42 10

initialState :: GameState
initialState = ((Hole, tree), Idle, [], 0)

repl :: IO ()
repl = do
  putStrLn "Welcome to Binary Tree World.\n"

  go initialState where
    go :: GameState -> IO ()
    go (z, state, capturePouch, goldPouch) = do
      putStrLn ""

      case snd z of
        L node -> do
          putStrLn "You are standing on a leaf."
          putStrLn (displayNode node)
        B node _ _  -> do
          putStrLn "You are standing on a branch."
          putStrLn (displayNode node)


      case state of
        Idle -> do
          putStr (show state ++ "> ")

          hFlush stdout
          line <- getLine

          case parseInput parseIdleCmd line of
              Nothing -> do
                putStrLn "I'm sorry, I do not understand."
                go (z, state, capturePouch, goldPouch)

              Just GoLeft ->
                case z of
                  (c, B node t1 t2) -> go ((B0 c (reveal t2) node, reveal t1), state, capturePouch, goldPouch)
                  (c, L _) -> do
                    putStrLn "You cannot climb any further."
                    go (z, state, capturePouch, goldPouch)

              Just GoRight -> do
                case z of
                  (c, B node t1 t2) -> go ((B1 (reveal t1) c node, reveal t2), state, capturePouch, goldPouch)
                  (c, L _) -> do
                    putStrLn "You cannot climb any further."
                    go (z, state, capturePouch, goldPouch)

              Just GoDown ->
                case z of
                  (B0 c t2 node, t) -> go ((c, B node t t2), state, capturePouch, goldPouch)
                  (B1 t1 c node, t) -> go ((c, B node t1 t), state, capturePouch, goldPouch)
                  (Hole,t) -> do
                    putStrLn "You are already at the root."
                    putStrLn "You cannot climb down any further."
                    go (z, state, capturePouch, goldPouch)

              Just Display -> do
                let (s, hasParents, hasMoreBranches) = drawCurrentSubTreeBinZip z
                if hasMoreBranches
                  then putStrLn "There are more branches, but your eyes cannot see that far."
                  else putStrLn ""
                putStrLn $ reverseTree s
                if hasParents
                  then putStrLn "\n You can climb down, but looking down is too scary for you."
                  else putStrLn "\nYou are at the root. Lets climb up."
                go (z, state, capturePouch, goldPouch)

              Just IntoAction -> do
                putStrLn "You prepare for some actions."
                go (z, InAction, capturePouch, goldPouch)

              Just ShowCapturePouch -> do
                putStrLn "You have captured the following birds:"
                mapM_ (\(index, (name, _, _, rarity)) -> putStrLn (show index ++ ": " ++ show name ++ " (" ++ show rarity ++ ")")) (zip [1..] capturePouch)
                go (z, state, capturePouch, goldPouch)

              Just ShowGoldPouch -> do
                putStrLn ("You have " ++ show goldPouch ++ " gold.")
                go (z, state, capturePouch, goldPouch)

              Just DisplayCheat -> do
                let (s, _, _) = drawCurrentSubTreeBinZip (Hole, revealAll tree)
                putStrLn $ reverseTree s
                go (z, state, capturePouch, goldPouch)

              Just Quit -> do
                putStrLn "Okay."
                putStrLn "You ended the game over here:\n"
                let (s, _, _) = drawCurrentSubTreeBinZip z
                putStrLn $ reverseTree s
                putStrLn "Goodbye."
                return ()

        InAction -> do
          putStr (show state ++ "> ")

          hFlush stdout
          line <- getLine

          let node = getBinZipCurrentNode z

          case node of
            NodeType (Bird (name, description, chance, rarity)) _ -> do
              putStrLn ("You are in action. You are in front of a " ++ show node ++ ".")
              putStrLn "What do you want to do?"
              case parseInput parseActionCmd line of
                Nothing -> do
                  putStrLn "I'm sorry, I do not understand."
                  go (z, state, capturePouch, goldPouch)

                Just BirdCapture -> do
                  putStrLn "Nice! You captured the bird."
                  putStrLn ("[ " ++ show node ++ " ] was added to your capture pouch.")
                  let newTree = replaceBinZipCurrentNode z (NodeType Empty True)
                  go (newTree, state, capturePouch ++ [(name, description, chance, rarity)], goldPouch)

                Just BirdFlee -> do
                  putStrLn "You fled from the bird."
                  go (z, state, capturePouch, goldPouch)

                Just BirdFeed -> do
                  putStrLn "You fed the bird."
                  go (z, state, capturePouch, goldPouch)

                Just BirdDisplay -> do
                  putStrLn ("Bird name: " ++ show name)
                  putStrLn ("Rarity: " ++ show rarity)
                  putStrLn ("Description: " ++ description)
                  putStrLn ("Chance to find one: " ++ show (chance * 100) ++ "%")
                  go (z, state, capturePouch, goldPouch)

                Just TreeDisplay -> do
                  let (s, hasParents, hasMoreBranches) = drawCurrentSubTreeBinZip z
                  if hasMoreBranches
                    then putStrLn "There are more branches, but your eyes cannot see that far."
                    else putStrLn ""
                  putStrLn $ reverseTree s
                  if hasParents
                    then putStrLn "\n You can climb down, but looking down is too scary for you."
                    else putStrLn "\nYou are at the root. Lets climb up."
                  go (z, state, capturePouch, goldPouch)

                Just QuitAction -> do
                  putStrLn "You ended the action."
                  go (z, Idle, capturePouch, goldPouch)

            NodeType _ _ -> do
              case parseInput parseActionCmd line of
                Nothing -> do
                  putStrLn "I'm sorry, I do not understand."
                  go (z, state, capturePouch, goldPouch)

                Just QuitAction -> do
                  putStrLn "You ended the action."
                  go (z, Idle, capturePouch, goldPouch)

main = repl
