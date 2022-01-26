module Main where
import Control.Monad.Trans.Except (runExcept)
import TestAlign (readDescr)
import TestParser (makeParser)
import Utils (parseAll)

main :: IO ()
main = do
    putStr "Type description file name: "
    descrFileName <- getLine
    descr <- readFile ("descr/" ++ descrFileName ++ ".ttempl")
    putStrLn "Input file code:"
    putStr descr
    putStrLn "Processed data:"
    let descr' = readDescr descr
    case runExcept descr' of
        Left  e      -> putStrLn e
        Right descrs -> do
            putStrLn $ concatMap ((++"\n") . show) descrs
            putStr "Test file name: "
            testFileName <- getLine
            test <- readFile ("test/" ++ testFileName ++ ".txt")
            putStrLn "Test file code:"
            putStr test
            putStrLn "Processed test file:"
            let p = makeParser descrs
            case parseAll p test of
                Left  e     -> print e
                Right tmp   -> mapM_ h tmp
    where
        h ((name, ixs), v) = putStrLn $ name 
                            ++ concatMap (\i -> "[" ++ show i ++ "]") ixs
                            ++ " = " ++ show v