module Parser.MainMenuCommandParser (parse) where

import Data.MainMenuCommand (Command (..))

parse :: String -> Maybe Command
parse ":game" = Just Game
parse ":search" = Just Search
parse ":compute" = Just Compute
parse ":help" = Just Help
parse ":quit" = Just Quit
parse _ = Nothing
