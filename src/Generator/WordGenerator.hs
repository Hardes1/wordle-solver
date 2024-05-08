module Generator.WordGenerator(genGuessWord, getLaWordList, getTaWordList) where
import System.Random (randomRIO)

genGuessWord :: IO String
genGuessWord = do
    wordList <- getLaWordList
    index <- randomRIO (0, length wordList - 1)
    return $ wordList !! index

getLaWordList :: IO [String]
getLaWordList = getWordList "res/la-dict.txt"


getTaWordList :: IO [String]
getTaWordList = getWordList "res/ta-dict.txt"

getWordList :: FilePath -> IO [String]
getWordList filepath = do
    content <- readFile filepath
    return (lines content)