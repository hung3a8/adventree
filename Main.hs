import Adventree.Bin
import Adventree.Types
import Adventree.Parser
import Adventree.Tree

import System.IO
import Control.Concurrent (threadDelay)
import Data.Tree (drawTree)
import System.Posix.Internals (puts)

tree :: Bin NodeType
tree = generateTreeWithSeed 42 0

initialState :: GameState
initialState = ((Hole, tree), Idle)

repl :: IO ()
repl = do
  putStrLn "Welcome to Binary Tree World.\n"

  go initialState where
    go :: GameState -> IO ()
    go (z, state) = do
      case snd z of
        L node -> do
          putStrLn "You are standing on a leaf."
          putStrLn (displayNode node)
        B node _ _  -> do
          putStrLn "You are standing on a branch."
          putStrLn (displayNode node)

      putStr "> "

      hFlush stdout
      line <- getLine
      let lvl = getTreeLevel (snd z)

      case state of
        Idle -> do
          case parseInput parseCmd line of
              Nothing -> do
                putStrLn "I'm sorry, I do not understand."
                go (z, state)

              Just GoLeft ->
                case z of
                  (c, B node t1 t2) -> go ((B0 c (reveal t2) node, reveal t1), state)
                  (c, L _) -> do
                    putStrLn "You cannot climb any further."
                    go (z, state)

              Just GoRight -> do
                case z of
                  (c, B node t1 t2) -> go ((B1 (reveal t1) c node, reveal t2), state)
                  (c, L _) -> do
                    putStrLn "You cannot climb any further."
                    go (z, state)

              Just GoDown ->
                case z of
                  (B0 c t2 node, t) -> go ((c, B node t t2), state)
                  (B1 t1 c node, t) -> go ((c, B node t1 t), state)
                  (Hole,t) -> do
                    putStrLn "You are already at the root."
                    putStrLn "You cannot climb down any further."
                    go (z, state)

              Just Display -> do
                let (s, hasParents, hasMoreBranches) = drawCurrentSubTreeBinZip z
                if hasMoreBranches
                  then putStrLn "There are more branches, but your eyes cannot see that far."
                  else putStrLn ""
                putStrLn $ reverseTree s
                if hasParents
                  then putStrLn "\n You can climb down, but looking down is too scary for you."
                  else putStrLn "\nYou are at the root. Lets climb up."
                go (z, state)

              Just DisplayCheat -> do
                let (s, _, _) = drawCurrentSubTreeBinZip (Hole, revealAll tree)
                putStrLn $ reverseTree s
                go (z, state)

              Just Quit -> do
                putStrLn "Okay."
                putStrLn "You ended the game over here:\n"
                let (s, _, _) = drawCurrentSubTreeBinZip z
                putStrLn $ reverseTree s
                putStrLn "Goodbye."
                return ()

        InAction -> do
          putStrLn "You are in action."
          go (z, Idle)

main = repl
