module Parser.MainMenuCommandParser (parse) where

import Data.MainMenuCommand (MainMenuCommand (..))

parse :: String -> Maybe MainMenuCommand
parse ":game" = Just Game
parse ":search" = Just Search
parse ":compute" = Just Compute
parse ":help" = Just Help
parse ":quit" = Just Quit
parse _ = Nothing
