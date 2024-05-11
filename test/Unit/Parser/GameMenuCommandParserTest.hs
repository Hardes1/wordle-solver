module Unit.Parser.GameMenuCommandParserTest(testGameMenuCommandParser) where
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Parser.GameMenuCommandParser(parse, ParseError(..))
import qualified Data.WordError as WordError(ParseError(..))
import Data.GameMenuCommand (Command(..))

testGameMenuCommandParser :: TestTree
testGameMenuCommandParser = testGroup "Game menu command parser test" [
    testCase ":back -> Back" $ parse ":back" @?= Right Back,
    testCase ":help -> Help" $ parse ":help" @?= Right Help,
    testCase ":guess -> Guess" $ parse ":guess" @?= Right Guess,
    testCase ":foo -> UnknownCommand" $ parse ":foo" @?= Left (UnknownCommand ":foo"),
    testCase "world -> Word world" $ parse "world" @?= Right (Word "world"),
    testCase "wordle -> InvalidLength" $ parse "wordle" @?= Left (WordParseError WordError.InvalidWordLength "wordle"),
    testCase "word -> InvalidLength" $ parse "word" @?= Left (WordParseError WordError.InvalidWordLength "word"),
    testCase "1word -> BadCharacter" $ parse "1word" @?= Left (WordParseError WordError.BadWordCharacter "1word"),
    testCase "word@ -> BadCharacter" $ parse "word@" @?= Left (WordParseError WordError.BadWordCharacter "word@"),
    testCase "wo?rd -> BadCharacter" $ parse "wo?rd" @?= Left (WordParseError WordError.BadWordCharacter "wo?rd")
    ]