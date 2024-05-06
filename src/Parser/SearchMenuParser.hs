module Parser.SearchMenuParser(parse) where
import Data.SearchMenuCommand (Command(..))



parse :: String -> Command
parse ":search" = Search
parse ":quit" = Quit
parse ":help" = Help
parse ":reset" = Reset
parse ":status" = Status
parse _ = NewWord
