module Unit.Parser.ComputeMenuCommandParserText(testComputeMenuCommandParser) where
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Parser.ComputeMenuCommandParser (parse, ParseError (..))
import Data.ComputeMenuCommand (Command(..))
import qualified Data.WordError as WordError(ParseError(..))

testComputeMenuCommandParser :: TestTree
testComputeMenuCommandParser = testGroup "Game menu command parser test" [
    testCase ":back -> Back" $ parse ":back" @?= Right Back,
    testCase ":help -> Help" $ parse ":help" @?= Right Help,
    testCase ":compute -> Compute" $ parse ":compute" @?= Right Compute,
    testCase ":foo -> UnknownCommand" $ parse ":foo" @?= Left (UnknownCommand ":foo"),
    testCase ":foo -> UnknownCommand" $ parse ":foo" @?= Left (UnknownCommand ":foo"),
    testCase "world -> Word world" $ parse "world" @?= Right (NewWord "world"),
    testCase "wordle -> InvalidLength" $ parse "wordle" @?= Left (WordParseError WordError.InvalidWordLength "wordle"),
    testCase "word -> InvalidLength" $ parse "word" @?= Left (WordParseError WordError.InvalidWordLength "word"),
    testCase "1word -> BadCharacter" $ parse "1word" @?= Left (WordParseError WordError.BadWordCharacter "1word"),
    testCase "word@ -> BadCharacter" $ parse "word@" @?= Left (WordParseError WordError.BadWordCharacter "word@"),
    testCase "wo?rd -> BadCharacter" $ parse "wo?rd" @?= Left (WordParseError WordError.BadWordCharacter "wo?rd")
    ]