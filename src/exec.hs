import Control.Monad
import System.IO
import Data.List.Split
import Data.List
import System.Exit
import System.Posix.User
import Network.BSD
import System.Process
import System.Posix.Process

main = do
    printExtra <- hIsTerminalDevice stdin
    if printExtra
        then do getLoginName >>= putStr
                putStr "@"
                getHostName >>= putStr
        else return()
    putStr "$ "
    hFlush stdout
    cmd <- getLine
    readCmd cmd
    main

--Gets the first command from a line with (potentially) multiple commands
getCmd :: String -> (String, String)
getCmd [] = ([],[])
getCmd [x] = ([x],[])
getCmd (x:y:xs)
        | x == ';' || x == '#'  =   ([x],y:xs)
        | x == '|' && y == '|'  =   (x:[y],xs)
        | x == '&' && y == '&'  =   (x:[y],xs)
        | otherwise             =   (x:ys,zs)
                                    where (ys,zs) = getCmd (y:xs)

--Takes a cmd and splits it into a executable, argumentList, and connector
splitCmd :: String -> (String, [String], String)
splitCmd [] = ([],[[]],[])
splitCmd cmd = 
        let exec = takeWhile (`notElem` " ;|&#") $ dropWhile (==' ') cmd
            args = tail' $ split (dropBlanks $ dropDelims $ oneOf " ;|&#") cmd
            conn = dropWhile (`notElem` ";|&#") cmd
        in  (exec, args, conn)

--Performs the tasks for reading the cmd
readCmd cmd = do
    let (cmd1, rest) = getCmd cmd
    let (exec,args,conn) = splitCmd cmd1
    if (exec == "exit")
        then exitSuccess
        else return()
    (code, out, err) <- readProcessWithExitCode exec args "" 
    putStr out
    hPutStr stderr err
    if (rest /= [] && conn /= "#" && checkNext code conn )
        then readCmd rest 
        else return()

checkNext :: ExitCode -> String -> Bool 
checkNext code conn
    | conn == ";"                         = True
    | conn == "||" && code /= ExitSuccess = True
    | conn == "&&" && code == ExitSuccess = True
    | otherwise                           = False

--tail that returns empty lists safely
tail' :: [a] -> [a]
tail' [] = []
tail' (x:xs) = xs
