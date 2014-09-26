import Network (listenOn, accept, PortID(..), Socket)
import System.Environment (getArgs)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle, hClose)
import Control.Concurrent (forkIO)

main :: IO ()
main = do
    args <- getArgs
    let port = fromIntegral (read $ head args :: Int)
    sock <- listenOn $ PortNumber port
    putStrLn $ "Listening on " ++ (head args)
    sockHandler sock

sockHandler :: Socket -> IO ()
sockHandler sock = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    forkIO $ commandProcessor handle
    sockHandler sock

commandProcessor :: Handle -> IO ()
commandProcessor handle = do
    line <- hGetLine handle
    let cmd = words line
    case (head cmd) of
        ("echo") -> echoCommand handle cmd
        ("add") -> addCommand handle cmd
        ("madd") -> mAddCommand handle cmd
        ("reverse") -> reverseCommand handle cmd
        ("close") -> hClose handle
        _ -> do hPutStrLn handle "Unknown command"
    commandProcessor handle

echoCommand :: Handle -> [String] -> IO ()
echoCommand handle cmd = do
    hPutStrLn handle (unwords $ tail cmd)

addCommand :: Handle -> [String] -> IO ()
addCommand handle cmd = do
    hPutStrLn handle $ show $ (read $ cmd !! 1) + (read $ cmd !! 2)

mAddCommand :: Handle -> [String] -> IO ()
mAddCommand handle cmd = do
    hPutStrLn handle $ show $ foldl (\acc x -> acc + read x) 0 $ drop 1 cmd

reverseCommand :: Handle -> [String] -> IO ()
reverseCommand handle cmd = do
    hPutStrLn handle $ unwords $ map reverse $ drop 1 cmd



