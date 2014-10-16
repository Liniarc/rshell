import Control.Monad
import System.IO
import Data.List.Split
import Data.List
import System.Cmd

main = do
    putStr "$ "
    hFlush stdout
    cmd <- getLine
    let (e,a,c) = splitCmd cmd in rawSystem e a
    print $ splitCmd cmd
    print "TEST"
    
    main

--Gets the first command from a line with (potentially) multiple commands
getCmd :: String -> (String, String)
getCmd [] = ([],[])
getCmd [x] = ([x],[])
getCmd (x:y:xs)
        | x == ';'              =   ([x],y:xs)
        | x == '|' && y == '|'  =   (x:[y],xs)
        | x == '&' && y == '&'  =   (x:[y],xs)
        | otherwise             =   (x:ys,zs)
                                    where (ys,zs) = getCmd (y:xs)

--Takes a cmd and splits it into a executable, argumentList, and connector
splitCmd :: String -> (String, [String], String)
splitCmd [] = ([],[[]],[])
splitCmd cmd = 
        let exec = takeWhile (`notElem` [' ',';','|','&']) cmd
            args = tail $ split (dropBlanks $ dropDelims $ oneOf " ;|&") cmd
            conn = dropWhile (`notElem` [';','|','&']) cmd
        in  (exec, args, conn)
