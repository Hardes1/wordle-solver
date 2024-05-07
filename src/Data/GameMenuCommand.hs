module Data.GameMenuCommand(Command(..)) where

data Command = Word String | Guess | Help | Back deriving (Eq, Show)