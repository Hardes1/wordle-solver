module Unit.Parser.SearchMenuCommandParserTest(testSearchMenuCommandParser) where
import Test.Tasty (testGroup, TestTree)
import Parser.SearchMenuCommandParser (parse, ParseError(..))
import Data.SearchMenuCommand (Command(..))
import Test.Tasty.HUnit (testCase, (@?=))
import Data.GameState (WordDiff(..), Color(..))
import Parser.SearchMenuCommandParser (ParseError(..))
import qualified Data.WordError as WordError(ParseError(..))
import qualified Data.ColorError as ColorError(ParseError(..))
testSearchMenuCommandParser :: TestTree
testSearchMenuCommandParser = testGroup "Search menu command parser test" [
    testCase ":back -> Back" $ parse ":back" @?= Right Back,
    testCase ":help -> Help" $ parse ":help" @?= Right Help,
    testCase ":search -> Search" $ parse ":search" @?= Right Search,
    testCase ":status -> Status" $ parse ":status" @?= Right Status,
    testCase ":reset -> Reset" $ parse ":reset" @?= Right Reset,
    testCase ":foo -> UnknownCommand" $ parse ":foo" @?= Left (UnknownCommand ":foo"),
    testCase "fres ggggg -> InvalidWordLength" $ parse "fres ggggg" @?= Left (WordParseError WordError.InvalidWordLength "fres"),
    testCase "fresh gggg -> InvalidWordLength" $ parse "fresh gggg" @?= Left (ColorParseError ColorError.InvalidColorLength "gggg"),
    testCase "w:ord ggggg -> BadWordCharacter" $ parse "w:ord ggggg" @?= Left (WordParseError WordError.BadWordCharacter "w:ord"),
    testCase "world rgbrb -> BadColorCharacter" $ parse "world rgbrb" @?= Left (ColorParseError ColorError.BadColorCharacter "rgbrb"),
    testCase "foo bar baz -> InvalidNumberOfWords" $ parse "foo bar baz" @?= Left (InvalidNumberOfWords 3),
    testCase "foo -> InvalidNumberOfWords" $ parse "foo" @?= Left (InvalidNumberOfWords 1),
    testCase "fresh rgyrg" $ parse "fresh rgyrg" @?= Right (NewWord (WordDiff [(Red, 'f'), (Green, 'r'), (Yellow, 'e'), (Red, 's'), (Green, 'h')])),
    testCase "FrEsH RgYRg" $ parse "fresh rgyrg" @?= Right (NewWord (WordDiff [(Red, 'f'), (Green, 'r'), (Yellow, 'e'), (Red, 's'), (Green, 'h')]))
    ]