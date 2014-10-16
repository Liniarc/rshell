import Control.Monad
import System.IO
import Data.List.Split

main = do
    putStr "$ "
    hFlush stdout
    cmd <- getLine
    print $ getCmd cmd
    main

getCmd :: String -> (String, String)
getCmd [] = ([],[])
getCmd [x] = ([x],[])
getCmd (x:y:xs)
        | x == ';'              =   ([x],y:xs)
        | x == '|' && y == '|'  =   (x:[y],xs)
        | x == '&' && y == '&'  =   (x:[y],xs)
        | otherwise             =   (x:ys,zs)
                                    where (ys,zs) = getCmd (y:xs)
