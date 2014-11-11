import System.IO
import System.Environment
import Data.List
import Data.List.Split
import Data.Bits
import Data.Function
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Time
import System.Posix.Files
import System.Posix.Directory
import System.Locale

main = do
    args <- getArgs
    let f = getFlags $ filter isFlag args 
    let path = "."
    printAllFiles path f

isFlag :: String -> Bool
isFlag ('-':a:xs) = a /= '-'
isFlag _ = False

getFlags :: [String] -> Int
getFlags [] = 0
getFlags a = foldl (.|.) 0 $ map getFlagValue $ concat a

getFlagValue :: Char -> Int
getFlagValue f
    | f == 'a' = 1
    | f == 'l' = 2
    | f == 'R' = 4
    | otherwise = 0

printAllFiles :: FilePath -> Int -> IO()
printAllFiles p f = do
    if (f .&. 4 /= 0)
        then putStrLn (p ++ ":")
        else return ()
    strm <- openDirStream p
    files <- listAllFiles strm f
    if (f .&. 2 /= 0)
        then printDetailList p (sort files)
        else printList files 80
    closeDirStream strm
    if (f .&. 4 /= 0)
        then do
            recursivePrint p files f
        else return()    

recursivePrint :: FilePath -> [String] -> Int -> IO ()
recursivePrint p [] f = return ()
recursivePrint p (x:xs) f = do
    let path = (p ++ "/" ++ x)
    status <- getFileStatus path
    if (isDirectory status && x /= "." && x /= "..")
        then do
            putStrLn ""
            printAllFiles path f
            recursivePrint p xs f
        else do recursivePrint p xs f

printDetailList :: FilePath -> [String] -> IO ()
printDetailList _ [] = return()
printDetailList p (x:xs) = do
    let path = (p ++ "/" ++ x)
    status <- getFileStatus path
    let mode = fileMode status
    putChar $ getFileType status
    putChar (if ownerReadMode .&. mode == 0 then 'r' else '-') 
    putChar (if ownerWriteMode .&. mode == 0 then 'w' else '-') 
    putChar (if ownerExecuteMode .&. mode == 0 then 'x' else '-') 
    putChar (if groupReadMode .&. mode == 0 then 'r' else '-') 
    putChar (if groupWriteMode .&. mode == 0 then 'w' else '-') 
    putChar (if groupExecuteMode .&. mode == 0 then 'x' else '-') 
    putChar (if otherReadMode .&. mode == 0 then 'r' else '-') 
    putChar (if otherWriteMode .&. mode == 0 then 'w' else '-') 
    putChar (if otherExecuteMode .&. mode == 0 then 'x' else '-') 
    putChar ' '
    putStr $ padString 2 $ show $ linkCount status
    putChar ' '
    putStr $ padString 8 $ show $ fileOwner status
    putChar ' '
    putStr $ padString 8 $ show $ fileGroup status
    putChar ' '
    putStr $ padString 10 $ show $ fileSize status
    putChar ' '
    let time = posixSecondsToUTCTime.fromIntegral.fromEnum $ modificationTime status
    putStr $ formatTime defaultTimeLocale "%h %d %H:%M" time
    putChar ' '
    putStrLn x
    printDetailList p xs

printList :: [String] -> Int -> IO ()
printList f w = do
    let max = (+) 1 $ maximum $ map length f
    let cols = div w max
    let files = map (padString max) $ sort f
    mapM_ putStrLn $ map concat (chunksOf cols files)

getFileType :: FileStatus -> Char
getFileType s
    | isBlockDevice s = 'b'
    | isCharacterDevice s = 'c'
    | isNamedPipe s = 'p'
    | isRegularFile s = '-'
    | isDirectory s = 'd'
    | isSymbolicLink s = 'l'
    | isSocket s = 's'
    | otherwise = '?'
    
padString :: Int -> String -> String
padString w s =
    s ++ (replicate x ' ')
    where
        x = w-length s

listAllFiles :: DirStream -> Int -> IO [String]
listAllFiles s f = do
    file <- readDirStream s
    if (file == "") 
        then return []
        else do
            list <- listAllFiles s f
            if (file !! 0 /= '.' || f .&. 1 /= 0)
                then return $ file:list
                else return list
