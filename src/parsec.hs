import Text.ParserCombinators.Parsec
import System.IO
import Control.Applicative ((<*))
import Control.Monad
import Data.Maybe
import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array

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



connect =   try (string "||")
        <|> try (string "&&")
        <|> string ";"
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
    pid <- c_fork
    if pid == 0
        then case parse commandLine "(stdin)" (input ++ "\0") of
                  Left e -> do hPutStrLn stderr "Error Parsing input:"
                               hPrint stderr e
                  Right cmd -> do 
                                --c_e <- newCString e
                                --c_a <- map newCString (e:a)
                                --print c_e
                                --print c_a
                                --c_execvp c_e c_a
                                mapM_ print cmd
        else main--c_wait 
    
