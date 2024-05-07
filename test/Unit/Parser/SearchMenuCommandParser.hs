module Unit.Parser.SearchMenuCommandParser(testSearchMenuCommandParser) where
import Test.Tasty (testGroup, TestTree)
import Parser.SearchMenuCommandParser (parse, ParseError(..), ErrorType(..))
import Data.SearchMenuCommand (Command(..))
import Test.Tasty.HUnit (testCase, (@?=))
import Data.GameState (WordDiff(..), Color(..))

testSearchMenuCommandParser :: TestTree
testSearchMenuCommandParser = testGroup "Search menu command parser test" [
    testCase ":back -> Back" $ parse ":back" @?= Right Back,
    testCase ":help -> Help" $ parse ":help" @?= Right Help,
    testCase ":search -> Search" $ parse ":search" @?= Right Search,
    testCase ":status -> Status" $ parse ":status" @?= Right Status,
    testCase ":reset -> Reset" $ parse ":reset" @?= Right Reset,
    testCase ":foo -> UnknownCommand" $ parse ":foo" @?= Left (ParseError UnknownCommand ":foo"),
    testCase "fres ggggg -> InvalidWordLength" $ parse "fres ggggg" @?= Left (ParseError InvalidWordLength "fres"),
    testCase "fresh gggg -> InvalidWordLength" $ parse "fresh gggg" @?= Left (ParseError InvalidColorLength "gggg"),
    testCase "w:ord ggggg -> BadWordCharacter" $ parse "w:ord ggggg" @?= Left (ParseError BadWordCharacter "w:ord"),
    testCase "world rgbrb -> BadColorCharacter" $ parse "world rgbrb" @?= Left (ParseError BadColorCharacter "rgbrb"),
    testCase "foo bar baz -> InvalidNumberOfWords" $ parse "foo bar baz" @?= Left (InvalidNumberOfWords 3),
    testCase "foo -> InvalidNumberOfWords" $ parse "foo" @?= Left (InvalidNumberOfWords 1),
    testCase "fresh rgyrg" $ parse "fresh rgyrg" @?= Right (NewWord (WordDiff [(Red, 'f'), (Green, 'r'), (Yellow, 'e'), (Red, 's'), (Green, 'h')])),
    testCase "FrEsH RgYRg" $ parse "fresh rgyrg" @?= Right (NewWord (WordDiff [(Red, 'f'), (Green, 'r'), (Yellow, 'e'), (Red, 's'), (Green, 'h')]))
    ]