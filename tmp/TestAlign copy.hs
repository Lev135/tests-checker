module TestAlign (

)   where

import TestAlignAriphm ( TAlignAriphmExpr (TAVar, TAConst, TABinOp), ariphmExprP )
import Utils

import Control.Applicative ((<|>))
import Text.Parsec
    ( letter, satisfy, string, eof,
      many, many1,
      try, (<?>),
      parse, ParseError )

import Control.Monad (guard)
import Data.List (group)
import Data.Maybe (catMaybes)

data AlignSpaces = ANoSpace | ASpace | ANewLine
    deriving Eq

data TAlignDescrLex
    = TALVar        String [TAlignAriphmExpr] AlignSpaces
    | TALLoop       TAlignDescrLex TAlignDescrLex
    | TALString     String AlignSpaces
    | TALBlock      [TAlignDescrLex] AlignSpaces
    deriving Eq

alignP :: Parser [TAlignDescrLex]
alignP = many (try vLoopP <|> try vTermP)

vTermP :: Parser TAlignDescrLex
vTermP = TALBlock <$> lineP <*> vSpacesP
--    <|> TALBlock <$> betweenCh '{' '}' alignP <*> vSpacesP

vSpacesP :: Parser AlignSpaces
vSpacesP = ANewLine <$ vSpaces1

vLoopP :: Parser TAlignDescrLex
vLoopP = TALLoop
    <$> vTermP
    <*  string ".." <* vSpaces1
    <*> vTermP
    <?> "vertical loop"

lineP :: Parser [TAlignDescrLex]
lineP = many (try hLoopP <|> try termP)
    <?> "line"

termP :: Parser TAlignDescrLex
termP
    = TALVar <$> many1 letter <*> many indexes <*> lineSpacesP
    <|> TALString <$> betweenCh '\"' '\"' (many $ satisfy (/= '\"')) <*> lineSpacesP
    <|> TALString <$> betweenCh '\'' '\'' (many $ satisfy (/= '\'')) <*> lineSpacesP
    <|> TALBlock  <$> betweenCh '{' '}' lineP <*> lineSpacesP
    <?> "term"
    where
        indexes = betweenCh '[' ']' (lineSpaces *> ariphmExprP)

lineSpacesP :: Parser AlignSpaces
lineSpacesP = ASpace <$ lineSpaces1 <|> pure ANoSpace

hLoopP :: Parser TAlignDescrLex
hLoopP = TALLoop
    <$> termP
    <*  string ".." <* lineSpaces
    <*> termP
    <?> "horizontal loop"

-- Processing

varByLvl :: Int -> String
varByLvl = (map ('$':) ["i", "j", "k", "l"] !!)

data Loop = Loop {
        lBlock   :: TAlignDescr,
        lIdxVar  :: String,
        lFirstI  :: TAlignAriphmExpr,
        lLastI   :: TAlignAriphmExpr,
        lSepSp   :: AlignSpaces
    }

data TAlignDescr
    = TADVar        String [TAlignAriphmExpr]
    | TALoop        Loop
    | TAString      String
    | TABlock       [TAlignDescrSp]

data TAlignDescrSp = TAlignDescrSp TAlignDescr AlignSpaces

makeDescr :: TAlignDescrLex -> Maybe TAlignDescrSp
makeDescr = makeDescrImpl 0

makeDescrImpl :: Int -> TAlignDescrLex -> Maybe TAlignDescrSp
makeDescrImpl lvl (TALVar name idxs sp) = Just $ TAlignDescrSp (TADVar name idxs) sp
makeDescrImpl lvl (TALString str sp)    = Just $ TAlignDescrSp (TAString str) sp
makeDescrImpl lvl (TALBlock es sp)      = flip TAlignDescrSp sp. TABlock <$> mapM (makeDescrImpl lvl) es
makeDescrImpl lvl (TALLoop b1 b2)       = do
                    TAlignDescrSp b1' sepSp <- makeDescrImpl lvl' b1
                    TAlignDescrSp b2' eSp   <- makeDescrImpl lvl' b2
                    (block, mIs) <- compareParts idxVar b1' b2'
                    (fI, lI) <- mIs
                    let loop = TALoop $ Loop block idxVar fI lI sepSp
                    return $ TAlignDescrSp  loop  eSp
                where
                    lvl' = lvl + 1
                    idxVar = varByLvl lvl

comparePartsSp :: String -> TAlignDescrSp -> TAlignDescrSp
    -> Maybe (TAlignDescrSp, Maybe (TAlignAriphmExpr, TAlignAriphmExpr))
comparePartsSp idxVar (TAlignDescrSp e1 sp1) (TAlignDescrSp e2 sp2)
     = do
        guard $ sp1 == sp2
        compPart <- compareParts idxVar e1 e2
        let comp = snd compPart
            part = fst compPart
        return (TAlignDescrSp part sp1, comp)

compareAriphm :: String -> TAlignAriphmExpr -> TAlignAriphmExpr
    -> Maybe (TAlignAriphmExpr, Maybe (TAlignAriphmExpr, TAlignAriphmExpr))
compareAriphm idxVar v1@(TAVar s1) v2@(TAVar s2)
    | s1 == s2  = Just (v1,             Nothing)
    | otherwise = Just (TAVar idxVar,   Just (v1, v2))
compareAriphm idxVar c1@(TAConst n1) c2@(TAConst n2)
    | n1 == n2  = Just (c1,             Nothing)
    | otherwise = Just (TAVar idxVar,   Just (c1, c2))
compareAriphm idxVar e1''@(TABinOp op1 e1 e1') e2''@(TABinOp op2 e2 e2')
    = do
        guard $ op1 == op2
        compE  <- compareAriphm idxVar e1  e2
        compE' <- compareAriphm idxVar e1' e2'
        comp <- allEq [snd compE, snd compE']
        return (TABinOp op1 (fst compE) (fst compE'), comp)
    <|> do
        return (TAVar idxVar, Just (e1'', e2'')) 
compareAriphm idxVar v1@(TAVar _) c2@(TAConst _)
    = Just (TAVar idxVar, Just (v1, c2))
compareAriphm idxVar c1@(TAConst _) v2@(TAVar _)
    = Just (TAVar idxVar, Just (c1, v2))
compareAriphm idxVar v1@(TAVar _) e2@TABinOp{}
    = Just (TAVar idxVar, Just (v1, e2))
compareAriphm idxVar e1@TABinOp{} v2@(TAVar _)
    = Just (TAVar idxVar, Just (e1, v2))
compareAriphm idxVar c1@(TAConst _) e2@TABinOp{}
    = Just (TAVar idxVar, Just (c1, e2))
compareAriphm idxVar e1@TABinOp{} c2@(TAConst _)
    = Just (TAVar idxVar, Just (e1, c2))

compareParts :: String -> TAlignDescr -> TAlignDescr
    -> Maybe (TAlignDescr, Maybe (TAlignAriphmExpr, TAlignAriphmExpr))
compareParts idxVar (TADVar name1 idxs1) (TADVar name2 idxs2)
     = do
        guard $ name1 == name2
        compMaybes <- map2 (compareAriphm idxVar) idxs1 idxs2
        comps <- sequence compMaybes
        comp <- allEq $ map snd comps
        let var = TADVar name1 $ map fst comps
        return (var, comp)
compareParts idxVar (TALoop l1) (TALoop l2)
     = do
        guard $ lSepSp l1  == lSepSp l2
        guard $ lIdxVar l1 == lIdxVar l2 -- should be always true
        compBlock <- compareParts  idxVar (lBlock  l1) (lBlock  l2)
        compFIdx  <- compareAriphm idxVar (lFirstI l1) (lFirstI l2)
        compLIdx  <- compareAriphm idxVar (lLastI  l1) (lLastI  l2)
        comp <- allEq [snd compBlock, snd compFIdx, snd compLIdx]
        let block  = fst compBlock
            idxVar = lIdxVar l1
            fIdx   = fst compFIdx
            lIdx   = fst compLIdx
            sepSp  = lSepSp l1
        let loop = TALoop $ Loop block idxVar fIdx lIdx sepSp
        return (loop, comp)
compareParts idxVar (TAString s1) (TAString s2)
    = do
        guard $ s1 == s2
        return (TAString s1, Nothing)
compareParts idxVar (TABlock b1) (TABlock b2)
    = do
        compMaybes <- map2 (comparePartsSp idxVar) b1 b2
        comps <- sequence compMaybes
        comp <- allEq $ map snd comps
        return (TABlock $ map fst comps, comp)
compareParts _ _ _ = Nothing

-- SHOW

instance Show AlignSpaces where
    show ANoSpace = ""
    show ASpace   = "_"
    show ANewLine = "\\n"

instance Show TAlignDescrLex where
    show (TALVar name idxs sp) = name ++ concatMap h idxs ++ show sp
        where h = \i -> "[" ++ show i ++ "]"
    show (TALLoop e e')        = show e ++ ".." ++ show e'
    show (TALString s sp)      = show s ++ show sp
    show (TALBlock es sp)      = "{" ++ concatMap show es ++ "}" ++ show sp

instance Show Loop where
    show (Loop block idxV fIdx lIdx sepSp)
        = "<" ++ show block ++ show sepSp ++ "|" ++ idxV ++ "=" ++ show fIdx ++ ".." ++ show lIdx ++ ">"

instance Show TAlignDescr where
    show (TADVar name idxs) = name ++ concatMap h idxs
        where h = \i -> "[" ++ show i ++ "]"
    show (TALoop l)         = show l -- show e ++ ".." ++ show e'
    show (TAString s)       = show s
    show (TABlock es )      = "{" ++ concatMap show es ++ "}"

instance Show TAlignDescrSp where
    show (TAlignDescrSp e sp) = show e ++ show sp


--- TESTS
pp :: Parser a -> String -> Either ParseError a
pp parser = parse (parser <* eof) ""

readDescr :: String -> Maybe [TAlignDescrSp]
readDescr s = do
    lex <- case parse (alignP <* eof) "" s of
      Left  _ -> Nothing
      Right lex -> Just lex
    mapM makeDescr lex

main :: IO()
main = mapM_ helper
    [ ("Половина матрицы (нижний левый треугольник). Нули нужны, чтобы в первом цикле догадаться по какому индексу итерироваться"
     , "n\na[1][1] .. a[1][0+1]\n..\na[n][1] .. a[n][0+n]\n")
    , ("Обычная прямоугольная матрица"
     , "n m\na[1][1] .. a[1][m]\n..\na[n][1] .. a[n][m]\n")
    , ("Задача на отрезки массива"
     , "N\na[1] .. a[N]\nQ\nl[1] r[1]\n..\nl[Q] r[Q]\n")
    , ("Очень странный пример. Не знаю, баг это или фича :)"
     , "{a[1] .. a[n] b[1] .. b[m]} .. {a[k] .. a[n] b[k] .. b[m]}\n")
    , ("Более логичный вариант"
     , "{a[1][1] .. a[n][1] b[1][1] .. b[m][1]} .. {a[k][k] .. a[n][k] b[k][k] .. b[m][k]}\n")
    , ("Как же не похвастаться арифметикой. Трапеция"
     , "a[1][1]..a[1][N-1+1]\n..\na[K][K]..a[K][N-K+1]\n")
    , ("Выражение и сначала и в конце"
     , "a[n + 1]..a[m + 2]\n")
    , ("И даже такую лесенку можно сделать, только приходится хитрить. Но зачем это нужно?"
     , "a[1][1-1+1] .. a[1][1-1+n]\n..\na[n][n-1+1] .. a[n][n-1+n]\n")
    ]
    where
        helper (msg, inp)= do
            putStrLn msg
            let outp = readDescr inp
            putStrLn "input data"
            putStrLn inp
            putStrLn "output data"
            case outp of
              Nothing     -> putStrLn "Process error!!!"
              Just descrs -> putStrLn $ concatMap ((++"\n") . show) descrs
            putStrLn "-------------------\n"
