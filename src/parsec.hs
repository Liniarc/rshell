import Text.ParserCombinators.Parsec
import System.IO
import System.Posix.User
import System.Posix.IO
import System.Posix.Files
import System.Posix.Signals
import System.Posix.Types
import System.Environment
import System.Directory
import Network.BSD
import Control.Applicative ((<*))
import Control.Monad
import Data.Maybe
import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import GHC.IO.Handle

import Debug.Trace

foreign import ccall "unistd.h fork"
    c_fork :: IO CPid
foreign import ccall "unistd.h execv"
    c_execv :: CString -> Ptr CString -> IO CInt
foreign import ccall "sys/wait.h wait"
    c_wait :: Ptr Int -> IO CInt
foreign import ccall "stdlib.h exit"
    c_exit :: CInt -> IO ()

paths = endBy pathList eol
pathList = sepBy path (char ':')
path = many (noneOf ":\0")

commandLine = endBy line eol
line = many cmd
cmd = do
    exec <- spaces >> word
    args <- spaces >> sepBy (optionMaybe word) (many1 (char ' '))
    conn <- optionMaybe connect
    return (exec, catMaybes args, fromMaybe "" conn)

word = quotes <|> many1 (noneOf " \0\n\r|&;#<>")

quotes =
    do char '"'
       content <- many (noneOf "\"")
       char '"' <?> "endquote"
       return content

connect =   try (string "<<<") <* spaces
        <|> try (string "||") <* spaces
        <|> try (string "&&") <* spaces
        <|> try (string ">>") <* spaces
        <|> string "|" <* spaces
        <|> string "<" <* spaces
        <|> string ">" <* spaces
        <|> string ";" <* spaces
        <?> "connector"

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <|> string "\0"
    <|> string "#" <* skipMany anyChar
    <?> "end of line"

main = do
    installHandler keyboardSignal (Catch (do return())) Nothing
    printPrompt
    input <- getLine
    parseInput input
    main

printPrompt :: IO ()
printPrompt = do 
    printExtra <- hIsTerminalDevice stdin
    if printExtra
        then do --getLoginName >>= putStr
                putStr "liniarc@thomas-VirtualBox"
                --getHostName >>= putStr
        else return ()
    putStr ":"
    putStr =<< getCurrentDirectory
    --putStr =<< getEnv "PWD"
    putStr "$ "
    hFlush stdout

parseInput :: String -> IO ()
parseInput input  = do
    case parse commandLine "stdin" (" " ++ input ++ "\0") of
        Left e -> do
            hPutStrLn stderr "Error Parsing Input: "
            hPutStrLn stderr $ lines (show e) !! 1
        Right cmd -> do
            prepCmd cmd stdin stdout

prepCmd :: [[(String,[String],String)]] -> Handle -> Handle -> IO ()
prepCmd [] _ _ = do hPrint stderr "Unknown Parse Error"
prepCmd [[]] _ _= return ()
prepCmd [("exit",_,_):_] _ _ = c_exit 0 
prepCmd [(e,a,"<"):[]] _ _ = hPrint stderr "Bad Input"
prepCmd [(e,a,"<"):(e2,_,c2):xs] _ o = do
    fd <- fdToHandle =<< openFd e2 ReadOnly Nothing defaultFileFlags
    prepCmd [(e,a,c2):xs] fd o
prepCmd [(e,a,"<<<"):[]] _ _ = hPrint stderr "Bad Input"
prepCmd [(e,a,"<<<"):(e2,_,c2):xs] _ o = do
    (fdi, fdo) <- createPipe
    hi <- fdToHandle fdi
    ho <- fdToHandle fdo
    hPutStrLn ho e2
    closeFd =<< handleToFd ho
    prepCmd [(e,a,c2):xs] hi o
prepCmd [(e,a,">>"):[]] _ _ = hPrint stderr "Bad Output"
prepCmd [(e,a,">>"):(e2,_,c2):xs] i _ = do
    let mode = Just $ unionFileModes ownerReadMode ownerWriteMode
    let flags = OpenFileFlags True False False False False
    fd <- fdToHandle =<< openFd e2 WriteOnly mode flags
    prepCmd [(e,a,c2):xs] i fd
prepCmd [(e,a,">"):[]] _ _ = hPrint stderr "Bad Output"
prepCmd [(e,a,">"):(e2,_,c2):xs] i _ = do
    let mode = Just $ unionFileModes ownerReadMode ownerWriteMode
    let flags = OpenFileFlags False False False False True
    fd <- fdToHandle =<< openFd e2 WriteOnly mode flags
    prepCmd [(e,a,c2):xs] i fd
prepCmd [(e,a,"|"):[]] _ _ = hPrint stderr "Bad Pipe" 
prepCmd [(e,a,"|"):xs] i _ = do
    (fdi, fdo) <- createPipe
    hi <- fdToHandle fdi
    ho <- fdToHandle fdo
    execCmd e a i ho
    if (i /= stdin)
        then closeFd =<< handleToFd i
        else return ()
    if (ho /= stdout)
        then closeFd =<< handleToFd ho
        else return ()
    prepCmd [xs] hi stdout
prepCmd [(e,a,c):xs] i o = do
    stat <- execCmd e a i o
    if (i /= stdin)
        then closeFd =<< handleToFd i
        else return ()
    if (o /= stdout)
        then closeFd =<< handleToFd o
        else return ()
    nextCmd [xs] c stat

execCmd :: String -> [String] -> Handle -> Handle -> IO Int
execCmd "cd" (a:_) _ _ = do 
    status <- doesDirectoryExist a
    if (status)
        then setCurrentDirectory a
        else hPrint stderr "Directory not found"
    return 0
execCmd "cd" _ _ _ = do
    setCurrentDirectory =<< getHomeDirectory
    return 0
execCmd e a i o = do
    pid <- c_fork
    checkFailure pid "fork error"
    installHandler keyboardSignal (Catch (do signalProcess softwareTermination pid)) Nothing
    case pid of
        0 -> do
            if (i /= stdin)
                then hDuplicateTo i stdin
                else return ()
            if (o /= stdout)
                then hDuplicateTo o stdout
                else return ()
            c_e <- newCString e
            c_a <- newArray0 nullPtr =<< (sequence $ map newCString (e:a))
            path <- getPaths
            executePaths e path a
            return 0
        _ -> do
            status <- new 0
            err <- c_wait status
            checkFailure err "wait error"
            peek status 

nextCmd :: [[(String, [String], String)]] -> String -> Int -> IO ()
nextCmd c ";" _ = prepCmd c stdin stdout
nextCmd c "&&" 0 = prepCmd c stdin stdout
nextCmd c "||" 0 = return ()
nextCmd c "||" _ = prepCmd c stdin stdout
nextCmd c _ _ = return ()

checkFailure :: (Integral a ) => a -> String -> IO ()
checkFailure (-1) s = do
    hPrint stderr s
    c_exit 1
checkFailure _ _ = return ()

executePaths :: String -> [String] -> [String] -> IO ()
executePaths exec [] args = checkFailure (-1) ("execv error: " ++ exec)
executePaths exec (path:xs) args = do
    c_e <- newCString (path ++ "/" ++  exec)
    c_a <- newArray0 nullPtr =<< (sequence $ map newCString (exec:args))
    err <- c_execv c_e c_a
    executePaths exec xs args

getPaths :: IO [String]
getPaths = do
    p <- getEnv "PATH"
    case parse paths "path" (p ++ ":.\0") of
        Left e -> do
            hPutStrLn stderr "Error Parsing Input: "
            hPutStrLn stderr $ lines (show e) !! 1
            return []
        Right cmd -> do
            return $ concat cmd
