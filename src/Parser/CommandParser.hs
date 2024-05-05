module Parser.CommandParser (parseMainMenuCommand) where

import Data.Command (MainMenuCommand (..))

parseMainMenuCommand :: String -> Maybe MainMenuCommand
parseMainMenuCommand ":game" = Just Game
parseMainMenuCommand ":search" = Just Search
parseMainMenuCommand ":compute" = Just Compute
parseMainMenuCommand ":help" = Just Help
parseMainMenuCommand ":quit" = Just Quit
parseMainMenuCommand _ = Nothing