module Data.SearchMenuCommand(Command(..)) where
import Data.GameState (WordDiff)


data Command = NewWord WordDiff | Search | Reset | Status | Help | Back deriving (Eq, Show)