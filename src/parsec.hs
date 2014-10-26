import Text.ParserCombinators.Parsec
import System.IO
import Control.Applicative ((<*>), (<$>))
import Control.Monad
import Data.Maybe


commandLine = endBy line eol
line = sepBy cmd connect
cmd = do
    exec <- spaces >> many1 (noneOf " \0\n\r|&;")
    args <- spaces >> sepBy (optionMaybe (many1 (noneOf " \0\n\r|&;"))) (many1 (char ' '))
    return (exec, catMaybes args)

--exec = (noneOf " \0\n\r|&;")

--arg = many (noneOf " \0\n\r|&;")

connect =   try (string "||")
        <|> try (string "&&")
        <|> string ";"
        <?> "connector"

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <|> string "\0"
    <|> string "#"
    <?> "end of line"

--parseCmd :: String -> Either ParseError [[String]]
--parseCmd input = parse commandLine "Error" input

main = do
    putStr "$ "
    hFlush stdout
    input <- getLine
    case parse commandLine "(stdin)" (input ++ "\0") of
        Left e -> do putStrLn "Error Parsing input:"
                     print e
        Right cmd -> mapM_ print cmd
    main
