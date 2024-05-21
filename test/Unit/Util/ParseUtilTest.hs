module Unit.Util.ParseUtilTest(testParseUtil) where
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Util.ParseUtil (trim, isCorrectWord, isCorrectColorList)
import qualified Data.WordError as WordError(ParseError(..))
import qualified Data.ColorError as ColorError(ParseError(..))

testParseUtil :: TestTree
testParseUtil = testGroup "Parse util Test" [
    trimTest,
    correctWordTest,
    correctColorListTest
    ]

trimTest :: TestTree 
trimTest = testGroup "Trim test" [
    testCase "'   foo' -> 'foo'" $ trim "   foo" @?= "foo",
    testCase "'foo   ' -> 'foo'" $ trim "foo   " @?= "foo",
    testCase "'   foo   ' -> 'foo'" $ trim "   foo   " @?= "foo",
    testCase "'foo bar' -> 'foo bar'" $ trim "foo bar" @?= "foo bar",
    testCase "'   foo bar' -> 'foo bar'" $ trim "   foo bar" @?= "foo bar",
    testCase "'foo bar    ' -> 'foo bar'" $ trim "foo bar    " @?= "foo bar",
    testCase "'    foo bar    ' -> 'foo bar'" $ trim "    foo bar    " @?= "foo bar"
    ]

correctWordTest :: TestTree
correctWordTest = testGroup "Correct word test" [
    testCase "'fresh' -> correct" $ isCorrectWord "fresh" @?= Right (),
    testCase "'looong' -> InvalidWordLength" $ isCorrectWord "looong" @?= Left WordError.InvalidWordLength,
    testCase "'shrt' -> InvalidWordLength" $ isCorrectWord "shrt" @?= Left WordError.InvalidWordLength,
    testCase "'s1mp@' -> BadWordCharacter" $ isCorrectWord "s1mp@" @?= Left WordError.BadWordCharacter
    ]

correctColorListTest :: TestTree
correctColorListTest = testGroup "Correct colour list test" [
    testCase "'rrgyy' -> correct" $ isCorrectColorList "rrgyy" @?= Right (),
    testCase "'rggggg' -> InvalidColorLength" $ isCorrectColorList "rggggg" @?= Left ColorError.InvalidColorLength,
    testCase "'rggg' -> InvalidColorLength" $ isCorrectColorList "rggg" @?= Left ColorError.InvalidColorLength,
    testCase "'ggbyy' -> BBadColorCharacteradColorCharacter" $ isCorrectColorList "ggbyy" @?= Left ColorError.BadColorCharacter,
    testCase "'gg@!y' -> BBadColorCharacteradColorCharacter" $ isCorrectColorList "ggbyy" @?= Left ColorError.BadColorCharacter
    ]