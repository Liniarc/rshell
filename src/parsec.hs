import Text.ParserCombinators.Parsec
import System.IO
import System.Posix.User
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

import Debug.Trace

foreign import ccall "unistd.h fork"
    c_fork :: IO CInt
foreign import ccall "unistd.h execvp"
    c_execvp :: CString -> Ptr CString -> IO CInt
foreign import ccall "sys/wait.h wait"
    c_wait :: Ptr Int -> IO CInt
foreign import ccall "stdlib.h exit"
    c_exit :: CInt -> IO ()

commandLine = endBy line eol
line = many cmd
cmd = do
    exec <- spaces >> word
    args <- spaces >> sepBy (optionMaybe word) (many1 (char ' '))
    conn <- optionMaybe connect
    return (exec, catMaybes args, fromMaybe "" conn)

word = quotes <|> many1 (noneOf " \0\n\r|&;#")

quotes =
    do char '"'
       content <- many (noneOf "\"")
       char '"' <?> "endquote"
       return content



connect =   try (string "||") <* spaces
        <|> try (string "&&") <* spaces
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
    printPrompt
    input <- getLine
    parseInput input
    main

printPrompt :: IO ()
printPrompt = do 
    printExtra <- hIsTerminalDevice stdin
    if printExtra
        then do getLoginName >>= putStr
                putStr "@"
                getHostName >>= putStr
        else return ()
    putStr "$ "
    hFlush stdout

parseInput :: String -> IO ()
parseInput input  = do
    case parse commandLine "stdin" (input ++ "\0") of
        Left e -> do
            hPutStrLn stderr "Error Parsing Input: "
            hPrint stderr e
        Right cmd -> do
            --hPrint stderr cmd
            prepCmd cmd

prepCmd :: [[(String,[String],String)]] -> IO ()
prepCmd [] = do hPrint stderr "unexpected connector"
prepCmd [[]] = return ()
prepCmd [("exit",_,_):_] = do c_exit 0 
prepCmd [(e,a,c):xs] = do
    pid <- c_fork
    case pid of 
        -1 -> do
            hPrint stderr "fork error"
            c_exit 1
        0 -> do
            c_e <- newCString e
            c_a <- newArray0 nullPtr  =<< (sequence $ map newCString (e:a))
            err <- c_execvp c_e c_a
            hPrint stderr $ "execvp error: " ++ e 
            c_exit 1 
            --exitWith ( ExitFailure (-1) )
        _ -> do
            status <- new 0
            err <- c_wait status
            if err == -1 
                then do 
                    hPrint stderr "wait error"
                    c_exit 1
                else do prepCmd [xs]
