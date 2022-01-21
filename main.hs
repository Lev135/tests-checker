module Main where
import Control.Monad.Trans.Except (runExcept)
import TestAlign (readDescr)

main = do
    putStr "Type input file name: "
    fileName <- getLine
    descr <- readFile ("descr/" ++ fileName ++ ".ttempl")
    putStrLn "Input file code:"
    putStr descr
    putStrLn "Processed data:"
    let outp = readDescr descr
    case runExcept outp of
        Left  e      -> putStrLn e
        Right descrs -> putStrLn $ concatMap ((++"\n") . show) descrs
    
