{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Adventree.Parser (runParser, parseIdleCmd, parseActionCmd, parseInput) where

import Adventree.Types
import Data.Maybe
import Data.Char
import Control.Applicative

newtype Parser tok a = Parser { runParser :: [tok] -> Maybe (a,[tok]) }

instance Monad (Parser tok) where
  -- return :: a -> Parser tok a
  return x = Parser (\ts -> Just (x,ts))

  -- (>>=) :: Parser a -> (a -> Parser tok b) -> Parser tok b
  p >>= f  = Parser (\ts -> case runParser p ts of
    Nothing -> Nothing
    Just (x,ts') -> runParser (f x) ts')

-- We add some boilerplate code to derive Functor and Applicative
-- instances from the Monad instance
instance Functor (Parser tok) where
  fmap f p = p >>= \x -> return (f x)

instance Applicative (Parser tok) where
  pure = return
  pf <*> p = pf >>= \f -> p >>= \x -> return (f x)

-- Note that the type Parser tok a is isomorphic to StateT [tok] Maybe a,
-- and we could have defined it that way to automatically derive all
-- these type class instances. But we prefer to do it for ourselves.

-- We also define an Alternative instance, which makes it convenient
-- to write backtracking parsers.
instance Alternative (Parser tok) where
  -- empty :: Parser tok a
  empty = Parser (const Nothing)

  -- (<|>) :: Parser tok a -> Parser tok a -> Parser tok a
  (<|>) :: Parser tok a -> Parser tok a -> Parser tok a
  p1 <|> p2 = Parser (\ts -> case runParser p1 ts of
    Just (x,ts') -> Just (x,ts')
    Nothing -> runParser p2 ts
    )

-- The idea is that "empty" is a parser that always fails, while
-- p1 <|> p2 is a parser that first tries to parse a string of tokens using p1,
-- and if that fails tries parsing the same string using p2.

-- Now we define parsers for various kinds of basic stuff.

-- The "token" parser just reads one token of the input and returns it.
-- Note there must be at least one token for item to succeed.
token :: Parser tok tok
token = Parser $ \ts -> case ts of
                          []     -> Nothing
                          (t:ts') -> Just (t,ts')

-- The "sat p" parser matches a token satisfying the predicate p.
sat :: (tok -> Bool) -> Parser tok tok
sat p = do
  t <- token
  if p t then return t else empty

-- Now we move onto our actual example of interest.

-- Our parsers will assume that the input string has already been split
-- up into a space-separated list of words, and thus use `String` as the
-- basic token type from now on.

-- It will be useful to have a parser that consumes a token matching a
-- specific string and ignoring case. This is achieved by "match s".
match :: String -> Parser String String
match s = sat (\s' -> map toLower s == map toLower s')

-- We parse English number words as numbers (restricted to numbers
-- between one and nine).
number :: Parser String Int
number =
  do match "one" >> return 1
    <|> (match "two" >> return 2)
    <|> (match "three" >> return 3)
    <|> (match "four" >> return 4)
    <|> (match "five" >> return 5)
    <|> (match "six" >> return 6)
    <|> (match "seven" >> return 7)
    <|> (match "eight" >> return 8)
    <|> (match "nine" >> return 9)

-- Group of IdleCmd parsers
parseIdleCmd :: Parser String IdleCmd
parseIdleCmd = parseClimb <|> parseIntoAction <|> parseSleep <|> parseShowState <|> parseDisplay <|> parseDisplayCheat <|> parseQuit

-- Parse a climbing command.
parseClimb :: Parser String IdleCmd
parseClimb = do
  match "climb" <|> match "go" <|> match "move" <|> match "g"
  (match "down" >> return GoDown) <|>
    (match "left" >> return GoLeft) <|>
    (match "right" >> return GoRight)

parseIntoAction :: Parser String IdleCmd
parseIntoAction = do
  match "action" <|> match "a"
  return IntoAction

parseSleep :: Parser String IdleCmd
parseSleep = do
  match "sleep" <|> match "s"
  return Sleep

parseShowState :: Parser String IdleCmd
parseShowState = do
  match "display" <|> match "d"
  (match "capture" >> return ShowCapturePouch) <|>
    (match "gold" >> return ShowGoldPouch) <|>
    (match "item" >> return ShowItemPouch)

parseDisplay :: Parser String IdleCmd
parseDisplay = do
  match "display" <|> match "d"
  return Display

parseDisplayCheat :: Parser String IdleCmd
parseDisplayCheat = do
  match "display_cheat" <|> match "show_cheat" <|> match "dc"
  return DisplayCheat

-- Parse a quit command
parseQuit :: Parser String IdleCmd
parseQuit = do
  match "quit" <|> match "q"
  return Quit

-- Group of ActionCmd parsers
parseActionCmd :: Parser String ActionCmd
parseActionCmd = parseBirdCapture
  <|> parseStoreBuy
  <|> parseStoreSell
  <|> parseUseItem
  <|> parseActionDisplay
  <|> parseJumpPortal
  <|> parseQuitAction

parseBirdCapture :: Parser String ActionCmd
parseBirdCapture = do
  match "capture" <|> match "c"
  return BirdCapture

parseStoreBuy :: Parser String ActionCmd
parseStoreBuy = do
  match "buy" <|> match "b"
  return StoreBuy

parseStoreSell :: Parser String ActionCmd
parseStoreSell = do
  match "sell" <|> match "s"
  return StoreSell

parseUseItem :: Parser String ActionCmd
parseUseItem = do
  match "use" <|> match "u"
  return UseItem

parseActionDisplay :: Parser String ActionCmd
parseActionDisplay = do
  match "display" <|> match "d"
  (match "bird" >> return BirdDisplay) <|>
    (match "store" >> return StoreDisplay) <|>
    (match "tree" >> return TreeDisplay)

parseJumpPortal :: Parser String ActionCmd
parseJumpPortal = do
  match "jump" <|> match "j"
  return JumpPortal

parseQuitAction :: Parser String ActionCmd
parseQuitAction = do
  match "quit" <|> match "q"
  return QuitAction

-- Finally, we export a function that runs a parser on the entire input string, broken up into words.
-- This function runs in any MonadFail monad, to deal with the possiblity of failure.
parseInput :: MonadFail m => Parser String a -> String -> m a
parseInput p s = case runParser p (words s) of
  Just (x,ts') -> if null ts' then return x else fail "parseInput: some tokens left"
  Nothing -> fail "parseInput: failed to parse"
