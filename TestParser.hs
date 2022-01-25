module TestParser where

import TestAlign
    ( AlignSpaces(ANewLine, ASpace, ANoSpace),
      AlignDescr'(ASpaces, ABlock, AString, ALoop, ADVar),
      Loop(lLastI, lFirstI, lIdxVar, lBlock, lSepSp),
      readDescr,
      AlignDescrPos )
import Utils (Parser, Pos (pVal), concatM, parseAll, numberP)
import Text.Parsec.Char (string, char, newline)
import Control.Monad.Trans.Except (runExcept, Except, ExceptT (ExceptT))
import TestAlignAriphm (Var (Var), Tmp, eval, EvalAriphmError)
import Text.Parsec.Prim (Parsec, parserFail, (<?>))
import Control.Monad (forM, when)


makeParser :: [AlignDescrPos] -> Parser Tmp
makeParser = flip makeParserImpl' []

makeParserImpl' :: [AlignDescrPos] -> Tmp -> Parser Tmp
makeParserImpl' pes = concatM (makeParserImpl <$> pes)
-- a -> m (p a) -> 
makeParserImpl :: AlignDescrPos -> Tmp -> Parser Tmp
makeParserImpl pe tmp = case pVal pe of
    ADVar (Var s idxs) -> do
        case runExcept $ mapM (eval tmp) idxs of
            Left  e     -> parserFail $ show e
            Right idxs' -> (\n -> ((s, idxs'), n) : tmp) <$> (numberP <?> s <> concatMap (\s -> "[" ++ show s ++ "]") idxs')
    ALoop l -> do
        let eis = do
            fI <- eval tmp $ lFirstI l
            lI <- eval tmp $ lLastI l
            -- TODO : check for correct bounds
            return ([fI .. lI], lI)
        case runExcept eis of
            Left  e         -> parserFail $ show e
            Right (is, lI)  -> concat <$> forM is (\i -> do
                    let tmp' = ((lIdxVar l, []), i) : tmp
                    makeParserImpl (lBlock l) tmp' 
                        <* when (i < lI) (spacesParser (lSepSp l))
                )
    AString s -> tmp <$ string s
    ABlock poss -> makeParserImpl' poss tmp
    ASpaces sp -> tmp <$ spacesParser sp

spacesParser :: AlignSpaces -> Parser ()
spacesParser ANoSpace   = return ()
spacesParser ASpace     = () <$ char ' '
spacesParser ANewLine   = () <$ newline

helper :: (String, String) -> String -> IO ()
helper (msg, inp) test = do
    putStrLn msg
    putStrLn "input data"
    mapM_ (\(i, s) -> putStrLn $ show i ++ ".\t" ++ s) $ zip [1..] $ lines inp
    putStrLn "output data"
    let outp = readDescr inp
    case runExcept outp of
        Left  e      -> putStrLn e
        Right descrs -> do
            putStrLn $ concatMap show descrs
            let p = makeParser descrs
            putStrLn "Test to check"
            putStrLn test
            case parseAll p test of
                Left  e     -> print e
                Right tmp   -> mapM_ print tmp
    putStrLn "-------------------\n"

main :: IO ()
main = helper ("", "n\nc[1] a[1][1] .. a[1][c[1]]\n..\nc[n] a[n][1] .. a[n][c[n]]\n") "3\n3 1 2 3\n1 1\n0 \n"