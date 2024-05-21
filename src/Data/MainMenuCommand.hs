module Data.MainMenuCommand(Command(..)) where

data Command = Game | Search | Compute | Help | Quit deriving (Eq, Show)