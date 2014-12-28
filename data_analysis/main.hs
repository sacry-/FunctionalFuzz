import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Control.Applicative


main :: IO ()
main = do
    filePassed <- (not . null) <$> getArgs
    fname <- if filePassed then getPath else error "No filepath passed as arg!"
    exists <- doesFileExist fname
    raw <- if exists then readFile fname else error "Filepath does not exist!"
    print $ countWords raw
    print $ textWords raw

getPath :: IO String
getPath = head <$> getArgs 

countWords :: String -> [Int]
countWords input = map (length . words) (lines input)

textWords :: String -> Int
textWords input = sum (countWords input)