module Data.ComputeMenuCommand(Command(..)) where

data Command = NewWord String | Compute | Reset | Status | Help | Back deriving (Eq, Show)