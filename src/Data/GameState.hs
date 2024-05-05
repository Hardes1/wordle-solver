module Data.GameState(Color(..), GameState(..)) where

data Color = Green | Yellow | Red

newtype GameState = GameState [(Color, Char)]