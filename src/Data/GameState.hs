module Data.GameState(Color(..), GameState(..), WordDiff(..)) where

data Color = Green | Yellow | Red deriving (Show, Eq)

newtype WordDiff = WordDiff [(Color, Char)] deriving Show

data GameState = InProgress [WordDiff] String | Win [WordDiff] String | Lose [WordDiff] String | Cancelled deriving Show