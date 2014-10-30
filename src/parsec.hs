import Text.ParserCombinators.Parsec
import System.IO
import System.Exit
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

foreign import ccall "unistd.h fork"
    c_fork :: IO CInt
foreign import ccall "unistd.h execvp"
    c_execvp :: CString -> Ptr CString -> IO CInt
foreign import ccall "sys/wait.h wait"
    c_wait :: Ptr Int -> IO CInt


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
    putStr "$ "
    hFlush stdout
    input <- getLine
    parseInput input
    main

parseInput :: String -> IO ()
parseInput input  = do
    case parse commandLine "stdin" (input ++ "\0") of
        Left e -> do
            hPutStrLn stderr "Error Parsing Input: "
            hPrint stderr e
        Right cmd -> do
            --hPrint stderr cmd
            exec cmd

exec :: [[(String,[String],String)]] -> IO ()
exec [] = do hPrint stderr "unexpected connector"
exec [[]] = return ()
exec [(e,a,c):xs] = do
    pid <- c_fork
    if pid == 0
        then do
            c_e <- newCString e
            c_a <- newArray =<< (sequence $ map newCString (e:a))
            err <- c_execvp c_e c_a
            hPrint stderr "execvp error"
            exitFailure 
        else do
            err <- c_wait =<< new 0
            if err == 0
                then do hPrint stderr "wait error"
                else do exec [xs]
       
