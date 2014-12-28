import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Control.Applicative
import Control.Monad(join)

-- example: runhaskell main.hs data/*
main :: IO ()
main = do
    filesPassed <- (not . null) <$> getArgs
    fnames <- if filesPassed then getArgs else error "No filepaths passed as arg!"
    rawContents <- mapM rawContent fnames
    let lineCount = join $ map countWords rawContents
    let rawWords = map textWords rawContents
    let allWords = sum rawWords
    print $ "lineCount: " ++ show lineCount
    print $ "rawWord count: " ++ show rawWords
    print $ "totalWords: " ++ show allWords

rawContent :: String -> IO String
rawContent fname = do
    exists <- doesFileExist fname
    if exists then 
      readFile fname 
    else error "Filepath does not exist!"

countWords :: String -> [Int]
countWords input = map (length . words) (lines input)

textWords :: String -> Int
textWords input = sum (countWords input)



