module Data.GameState(Color(..), GameState(..), WordDiff(..)) where

data Color = Green | Yellow | Red

newtype WordDiff = WordDiff [(Color, Char)]

newtype GameState = GameState [WordDiff]