{-# LANGUAGE InstanceSigs #-}
module Data.GameState(Color(..), GameState(..), WordDiff(..), GameStatus(..),  IncorrectStatus(..)) where

data Color = Green | Yellow | Red deriving (Ord, Show, Eq)

newtype WordDiff = WordDiff { diffList :: [(Color, Char)] } deriving (Show, Eq)

data GameStatus = InProgress | Win | Lose deriving Eq

instance Show GameStatus where
  show :: GameStatus -> String
  show InProgress = "In progress"
  show Win = "Win"
  show Lose = "Lose"

data IncorrectStatus = Cancelled

instance Show IncorrectStatus where
  show :: IncorrectStatus -> String
  show Cancelled = "Cancelled"

data GameState = CorrectState GameStatus [WordDiff] String | IncorrectState IncorrectStatus deriving Show