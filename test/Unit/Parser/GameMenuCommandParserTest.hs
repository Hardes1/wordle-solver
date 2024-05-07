module Unit.Parser.GameMenuCommandParserTest(testGameMenuCommandParser) where
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Parser.GameMenuCommandParser (parse, ParseError(..), ErrorType(..))
import Data.GameMenuCommand (Command(..))

testGameMenuCommandParser :: TestTree
testGameMenuCommandParser = testGroup "Game menu command parser test" [
    testCase ":back -> Back" $ parse ":back" @?= Right Back,
    testCase ":help -> Help" $ parse ":help" @?= Right Help,
    testCase ":guess -> Guess" $ parse ":guess" @?= Right Guess,
    testCase ":foo -> UnknownCommand" $ parse ":foo" @?= Left (ParseError UnknownCommand ":foo"),
    testCase "world -> Word world" $ parse "world" @?= Right (Word "world"),
    testCase "wordle -> InvalidLength" $ parse "wordle" @?= Left (ParseError InvalidLength "wordle"),
    testCase "word -> InvalidLength" $ parse "word" @?= Left (ParseError InvalidLength "word"),
    testCase "1word -> BadCharacter" $ parse "1word" @?= Left (ParseError BadCharacter "1word"),
    testCase "word@ -> BadCharacter" $ parse "word@" @?= Left (ParseError BadCharacter "word@"),
    testCase "wo?rd -> BadCharacter" $ parse "wo?rd" @?= Left (ParseError BadCharacter "wo?rd")
    ]