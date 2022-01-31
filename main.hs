module Main where
import Control.Monad.Trans.Except (runExcept)
import Layout (readLayout)
import TestParser (makeParser)
import Utils (parseAll)
import Descr (descrP)

main :: IO ()
main = do
    putStr "Type description file name: "
    descrFileName <- getLine
    descr <- readFile ("descr/" ++ descrFileName ++ ".ttempl")
    putStrLn "Input file code:"
    putStr descr
    putStrLn "Processed data:"
    let descr' = parseAll descrP descr
    case descr' of
        Left  e      -> print e
        Right descrs -> do
            print descrs
            putStr "Test file name: "
            testFileName <- getLine
            test <- readFile ("test/" ++ testFileName ++ ".txt")
            putStrLn "Test file code:"
            putStr test
            putStrLn "Processed test file:"
            let p = makeParser descrs
            case parseAll p test of
                Left  e     -> print e
                Right vals  -> mapM_ h vals
    where
        h ((name, ixs), v) = putStrLn $ name
                            ++ concatMap (\i -> "[" ++ show i ++ "]") ixs
                            ++ " = " ++ show v