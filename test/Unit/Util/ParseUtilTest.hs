module Unit.Util.ParseUtilTest(parseUtilTest) where
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Util.ParseUtil (trim)


parseUtilTest :: TestTree
parseUtilTest = testGroup "Parse util Test" [
    trimTest
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