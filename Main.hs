import Adventree.Bin
import Adventree.Types
import Adventree.Parser
import Adventree.Tree

import System.IO
import Control.Concurrent (threadDelay)

tree :: Bin Int
tree = generateTreeWithSeed 10

repl :: IO ()
repl = do
  putStrLn "Welcome to Binary Tree World.\n"
  putStrLn "You are at the root of an ancient binary tree."

  go (Hole, tree) where
    go :: BinZip Int -> IO ()
    go z = do
      case z of
        (_,L x) ->
          putStrLn "You see a leaf." >>
          putStrLn ("It has the number " ++ show x ++ " etched into it.")
        (_,B _ _) -> putStrLn "You see a binary node."

      putStr "> "

      hFlush stdout
      line <- getLine
      case parseInput parseCmd line of
          Nothing -> do
            putStrLn "I'm sorry, I do not understand."
            go z

          Just GoLeft ->
            case z of
              (c, B t1 t2) -> go (B0 c t2, t1)
              (c, L _) -> do
                putStrLn "You cannot climb any further."
                go z

          Just GoRight ->
            case z of
              (c,B t1 t2) -> go (B1 t1 c,t2)
              (c,L _) -> do
                putStrLn "You cannot climb any further."
                go z

          Just GoDown ->
            case z of
              (B0 c t2,t) -> go (c,B t t2)
              (B1 t1 c,t) -> go (c,B t1 t)
              (Hole,t) -> do
                putStrLn "You are already at the root."
                putStrLn "You cannot climb down any further."
                go z

          Just Display -> do
            putStrLn (drawBinZip z)
            go z

          Just Quit -> do
            putStrLn "Okay."
            putStrLn "You ended the game over here:\n"
            putStrLn (drawBinZip z)
            putStrLn "Goodbye."
            return ()

main = repl
