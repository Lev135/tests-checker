module TestParser where

import Layout
    ( LayoutSpaces(ANewLine, ASpace, ANoSpace),
      LayoutDescr(LSpaces, LBlock, LString, LLoop, LVar),
      Loop(lLastI, lFirstI, lIdxVar, lBlock, lSepSp),
      readLayout,
      LayoutDescrPos )
import Utils (Parser, Pos (pVal, Pos), concatM, parseAll, numberP, Wrap)
import Text.Parsec.Char (string, char, newline)
import Control.Monad.Trans.Except (runExcept, Except, ExceptT (ExceptT))
import Ariphm (Var (Var), VarValues, evalAriphm, EvalError)
import Text.Parsec.Prim (Parsec, parserFail, (<?>))
import Control.Monad (forM, when)
import Descr (DescrPos, Descr (dLayout, dConds))
import Conditions (Cond, CondPos, evalCond)
import qualified Data.Map as M


makeParser :: DescrPos -> Parser VarValues
makeParser (Pos p descr)= do
    vals <- makeParserImpl' (dLayout descr) M.empty
    checkConds (dConds descr) vals
    return vals

checkConds :: [CondPos] -> VarValues -> Parser ()
checkConds conds vals = () <$ forM conds (\cond -> do
    case runExcept $ evalCond vals cond of
        Left  e     -> parserFail $ show e
        Right True  -> return ()
        Right False -> parserFail $ "conditionFailed: " ++ show cond
        )

makeParserImpl' :: [LayoutDescrPos] -> VarValues -> Parser VarValues
makeParserImpl' pes vals = foldl h (return M.empty) (makeParserImpl <$> pes)
    where
        h :: Parser VarValues -> (VarValues -> Parser VarValues) -> Parser VarValues
        h a f = do
            a' <- a
            b  <- f (M.unionWith (<>) a' vals)
            return $ M.unionWith (<>) a' b

makeParserImpl :: LayoutDescrPos -> VarValues -> Parser VarValues
makeParserImpl pe vals = case pVal pe of
    LVar (Var s idxs) -> do
        case runExcept $ mapM (evalAriphm vals) idxs of
            Left  e     -> parserFail $ show e
            Right idxs' -> M.singleton s . M.singleton idxs' <$> (
                    numberP <?> s <> concatMap (\s -> "[" ++ show s ++ "]") idxs'
                )
    LLoop l -> do
        let eis = do
            fI <- evalAriphm vals $ lFirstI l
            lI <- evalAriphm vals $ lLastI l
            -- TODO : check for correct bounds
            return ([fI .. lI], lI)
        case runExcept eis of
            Left  e         -> parserFail $ show e
            Right (is, lI)  -> M.unionsWith (<>) <$> forM is (\i -> do
                    let vals' = M.insert (lIdxVar l) (M.singleton [] i) vals
                    makeParserImpl (lBlock l) vals'
                        <* when (i < lI) (spacesParser (lSepSp l))
                )
    LString s   -> M.empty <$ string s
    LBlock poss -> makeParserImpl' poss vals
    LSpaces sp  -> M.empty  <$ spacesParser sp

spacesParser :: LayoutSpaces -> Parser ()
spacesParser ANoSpace   = return ()
spacesParser ASpace     = () <$ char ' '
spacesParser ANewLine   = () <$ newline

helper :: (String, String) -> String -> IO ()
helper (msg, inp) test = do
    putStrLn msg
    putStrLn "input data"
    mapM_ (\(i, s) -> putStrLn $ show i ++ ".\t" ++ s) $ zip [1..] $ lines inp
    putStrLn "output data"
    let outp = readLayout inp
    case runExcept outp of
        Left  e      -> putStrLn e
        Right descrs -> do
            putStrLn $ concatMap show descrs
            let p = makeParserImpl' descrs M.empty
            putStrLn "Test to check"
            putStrLn test
            case parseAll p test of
                Left  e     -> print e
                Right vals  -> mapM_ print vals
    putStrLn "-------------------\n"

main :: IO ()
main = mapM_ h $ zip [1..] [
        (
            "n\nc[1] a[1][1] .. a[1][c[1]]\n..\nc[n] a[n][1] .. a[n][c[n]]\n",
            "3\n3 1 2 3\n5 10 11 12 13 14\n0 \n"
        ), (
            "n\na[1][1] .. a[1][n]\n..\na[n][1] .. a[n][n]\n",
            "3\n2 3 1\n1 2 3\n2 3 4\n"
        )
    ]
    where h (i, (inp, test)) = do
            putStrLn $ "\t\t(" ++ show i ++ ")"
            helper ("", inp) test
