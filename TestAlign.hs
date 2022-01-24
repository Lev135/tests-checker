module TestAlign (
    AlignSpaces, 
    AlignDescrLex' (..), AlignDescrLex,
    -- Loop (..), AlignDescr' (..), AlignDescr, AlignDescrSp,
    -- readDescr
)   where

import TestAlignAriphm
    ( ariphmExprP,
      AlignAriphmExpr,
      AlignAriphmExpr'(ABinOp, AConst, AVar) )
import Utils
    ( PosSegm,
      withPosP,
      maybeToExcept,
      map2,
      lineSpaces1,
      lineSpaces,
      guardE,
      betweenCh,
      allEq,
      Pos(..),
      Parser,
      vSpace,
      Wrap,
      Box,
      ($$) )


import Control.Applicative ((<|>))
import Text.Parsec
    ( letter, satisfy, string, eof,
      many, many1,
      try, (<?>),
      parse, ParseError, sourceLine, sourceColumn )

import Control.Monad (guard)
import Data.List (group, nub, sort)
import Data.Maybe (catMaybes)
import Control.Monad.Trans.Except (Except, except, throwE, catchE, runExcept)
import Control.Monad.Trans.Maybe (maybeToExceptT)

data AlignSpaces = ANoSpace | ASpace | ANewLine
    deriving Eq

data AlignDescrLex'
    = ALVar        String [AlignAriphmExpr]
    | ALLoop       AlignDescrLex (Pos AlignSpaces) (Pos AlignSpaces) AlignDescrLex
    | ALString     String
    | ALBlock      [AlignDescrLex]
    | ALSpaces     AlignSpaces
    deriving Eq

type AlignDescrLex = Pos AlignDescrLex'

alignP :: Parser [AlignDescrLex]
alignP = concat <$> many (((:[]) <$> try vLoopP <|> try lineP) <> ((:[]) <$> newLineP))
    where newLineP  = withPosP (ALSpaces <$> vSpacesP)

vSpacesP :: Parser AlignSpaces
vSpacesP = ANewLine <$ vSpace

vLoopP :: Parser AlignDescrLex
vLoopP = withPosP (ALLoop
        <$> withPosP (ALBlock <$> lineP)
        <*> withPosP vSpacesP <* string ".." <*> withPosP vSpacesP
        <*> withPosP (ALBlock <$> lineP)
    ) <?> "vertical loop"

lineP :: Parser [AlignDescrLex]
lineP = many (try hLoopP <|> try termP <|> spP) 
    <?> "line"
    where 
        spP = withPosP $ ALSpaces <$> (ASpace <$ lineSpaces1)
        

termP :: Parser AlignDescrLex
termP = withPosP (
        ALVar <$> many1 letter <*> many indexes
    <|> ALString <$> betweenCh '\"' '\"' (many $ satisfy (/= '\"'))
    <|> ALString <$> betweenCh '\'' '\'' (many $ satisfy (/= '\''))
    <|> ALBlock  <$> betweenCh '{' '}' lineP -- lineP
        ) <?> "term"
    where
        indexes = betweenCh '[' ']' (lineSpaces *> ariphmExprP)

hLoopP :: Parser AlignDescrLex
hLoopP = withPosP (ALLoop
    <$> termP
    <*> spP <*  string ".." <*> spP
    <*> termP
    ) <?> "horizontal loop"
    where
        spP = withPosP $ ASpace <$ lineSpaces1 <|> pure ANoSpace

pp :: Parser a -> String -> Either ParseError a
pp p = parse (p <* eof) ""

-- Processing

varByLvl :: Int -> String
varByLvl = (map ('$':) ["i", "j", "k", "l"] !!)

data Loop p = Loop {
        lBlock   :: Wrap p AlignDescr',
        lIdxVar  :: String,
        lFirstI  :: AlignAriphmExpr,
        lLastI   :: AlignAriphmExpr,
        lSepSp   :: AlignSpaces
    }

data AlignDescr' p
    = ADVar        String [AlignAriphmExpr]
    | ALoop        (Loop p)
    | AString      String
    | ABlock       [Wrap p AlignDescr']
    | ASpaces      AlignSpaces

type AlignDescrPos = Wrap Pos AlignDescr'

makeDescr :: AlignDescrLex -> Except AlignDescrProcessError AlignDescrPos
makeDescr = makeDescrImpl 0

makeDescrImpl :: Int -> AlignDescrLex -> Except AlignDescrProcessError AlignDescrPos
makeDescrImpl lvl (Pos p (ALVar name idxs)) = return $ Pos p $ ADVar name idxs
makeDescrImpl lvl (Pos p (ALString str))    = return $ Pos p $ AString str
makeDescrImpl lvl (Pos p (ALBlock es))      = Pos p . ABlock <$> mapM (makeDescrImpl lvl) es
makeDescrImpl lvl (Pos p (ALSpaces sp))     = return $ Pos p $ ASpaces sp
makeDescrImpl lvl (Pos p (ALLoop b1 (Pos p1 sepSp1) (Pos p2 sepSp2) b2))    = do
                    guardE (ADE ADE_UnequalSpaces p1 p2) $ sepSp1 == sepSp2 
                    b1' <- makeDescrImpl lvl' b1
                    b2' <- makeDescrImpl lvl' b2
                    (block, mIs) <- compareParts idxVar b1' b2'
                    (fI, lI) <- maybeToExcept (ADE ADE_IdenticalBeginEnd (pPos b1') (pPos b2')) mIs
                    let loop = Pos p . ALoop $ Loop block idxVar fI lI sepSp1
                    return loop
                where
                    lvl' = lvl + 1
                    idxVar = varByLvl lvl

compareAriphm :: String -> AlignAriphmExpr -> AlignAriphmExpr
    -> Except AlignDescrProcessError (AlignAriphmExpr, Maybe (AlignAriphmExpr, AlignAriphmExpr))
compareAriphm idxVarName pe1@(Pos p1 e1) pe2@(Pos p2 e2) = case (e1, e2) of
    (AVar s1, AVar s2) ->
        if s1 == s2 then equal
                    else foundIdx
    (AConst n1, AConst n2) ->
        if n1 == n2 then equal
                    else foundIdx
    (ABinOp op1 a1 b1, ABinOp op2 a2 b2) ->
        do
            guardE (ADE ADE_UnequalOperations p1 p2) $ op1 == op2
            compA <- compareAriphm idxVarName (Pos p1 a1) (Pos p2 a2)
            compB <- compareAriphm idxVarName (Pos p1 b1) (Pos p2 b2)
            comp  <- makeUnifyError p1 p2 $ allEq [snd compA, snd compB]
            return (Pos p $ ABinOp op1 (pVal $ fst compA) (pVal $ fst compB), comp)
        `catchE`
            const foundIdx
    (_, _) ->  foundIdx
    where
        p = p1 <> p2
        idxVar = Pos p $ AVar idxVarName
        equal    = return (Pos p e1, Nothing)
        foundIdx = return (idxVar, Just(pe1, pe2))

compareParts :: String -> AlignDescrPos -> AlignDescrPos
    -> Except AlignDescrProcessError (AlignDescrPos, Maybe (AlignAriphmExpr, AlignAriphmExpr))
compareParts idxVar pb1@(Pos p1 b1) pb2@(Pos p2 b2) = case (b1, b2) of
    (ADVar name1 idxs1, ADVar name2 idxs2) -> do
        guardE (ADE ADE_UnequalNames p1 p2) $ name1 == name2
        compMaybes <- maybeToExcept (ADE ADE_UnequalLength p1 p2) $ map2 (compareAriphm idxVar) idxs1 idxs2
        comps <- sequence compMaybes
        comp <- makeUnifyError p1 p2 $ allEq $ map snd comps
        let var = Pos p $ ADVar name1 $ map fst comps
        return (var, comp)
    (ALoop l1, ALoop l2) -> do
        guardE (ADE ADE_UnequalSpaces p1 p2) $ lSepSp l1  == lSepSp l2
        guardE (ADE ADE_UnknownError  p1 p2) $ lIdxVar l1 == lIdxVar l2 -- should be always true
        compBlock <- compareParts  idxVar (lBlock  l1) (lBlock  l2)
        compFIdx  <- compareAriphm idxVar (lFirstI l1) (lFirstI l2)
        compLIdx  <- compareAriphm idxVar (lLastI  l1) (lLastI  l2)
        comp <- makeUnifyError p1 p2 $ allEq [snd compBlock, snd compFIdx, snd compLIdx]
        let block  = fst compBlock
            idxVar'= lIdxVar l1
            fIdx   = fst compFIdx
            lIdx   = fst compLIdx
            sepSp  = lSepSp l1
        let loop = Pos p $ ALoop $ Loop block idxVar' fIdx lIdx sepSp
        return (loop, comp)
    (AString s1, AString s2) -> do
        guardE (ADE ADE_UnequalStrings p1 p2) $ s1 == s2
        return (Pos p $ AString s1, Nothing)
    (ABlock b1, ABlock b2) -> do
        compMaybes <- maybeToExcept (ADE ADE_UnequalLength p1 p2) $ map2 (compareParts idxVar) b1 b2
        comps <- sequence compMaybes
        comp <- makeUnifyError p1 p2 $ allEq $ map snd comps
        return (Pos p $ ABlock $ map fst comps, comp)
    (ASpaces sp1, ASpaces sp2) -> do
        guardE (ADE ADE_UnequalSpaces p1 p2) $ sp1 == sp2
        return (Pos p $ ASpaces sp1, Nothing)
    (_, _) ->  throwE (ADE ADE_UnequalBlockTypes p1 p2)
    where
        p = p1 <> p2


-- SHOW

instance Show AlignSpaces where
    show ANoSpace = ""
    show ASpace   = "_"
    show ANewLine = "\\n"

instance Show AlignDescrLex' where
    show (ALVar name idxs) = name ++ concatMap h idxs
        where h = \i -> "[" ++ show i ++ "]"
    show (ALLoop e sp1 sp2 e')  = show e ++ show sp1 ++ ".." ++ show sp2 ++ show e'
    show (ALString s)           = show s
    show (ALBlock es)           = "{" ++ concatMap show es ++ "}"
    show (ALSpaces sp)          = show sp

instance Box p => Show (Loop p) where
    show (Loop block idxV fIdx lIdx sepSp)
        = "<" ++ show $$ block ++ show sepSp ++ "|" ++ idxV ++ "=" ++ show fIdx ++ ".." ++ show lIdx ++ ">"

instance Box p => Show (AlignDescr' p) where
    show (ADVar name idxs) = name ++ concatMap h idxs
        where h = \i -> "[" ++ show i ++ "]"
    show (ALoop l)         = show l -- show e ++ ".." ++ show e'
    show (AString s)       = show s
    show (ABlock es )      = "{" ++ concatMap (show $$) es ++ "}"
    show (ASpaces sp)      = show sp

readDescr :: String -> Except String [AlignDescrPos]
readDescr s = do
    lex <- case parse (alignP <* eof) "" s of
      Left  e ->    throwE $ "ParseError: " ++ show e
      Right lex ->  return lex
    mapM makeDescr lex `catchE` (throwE . makeProcessErrorMsg (lines s))

makeProcessErrorMsg :: [String] -> AlignDescrProcessError -> String
makeProcessErrorMsg sourceLines (ADE errT segms1 segms2)
    = ((show errT ++ "\n") ++ ) $
        flip concatMap lines $ \n ->
                                show (n + 1) ++ ".\t"
                            ++  sourceLines !! n ++ "\n"
                            ++  "\t"
                            ++  makeMask2 (lineLen n) (lineSegms n) ++ "\n"
    where
        segms       = segms1 <> segms2
        lines       = nub . sort  . concatMap segmLines $ segms

        segmLines (p1, p2) = [sourceLine p1 - 1 .. (sourceLine p2 - 1) `min` (length sourceLines - 1)]
        makeMask1 = makeMask '^' '-'
        makeMask2 = makeMask '^' '~'
        lineSegms n = catMaybes $ segmInLine n (lineLen n) <$> segms
        lineLen   n = length $ sourceLines !! n

segmInLine :: Int -> Int -> PosSegm -> Maybe (Int, Int)
segmInLine n len (p1, p2)
    | sourceLine p1 <= n' && n'  <= sourceLine p2
                = let l = if sourceLine p1 < n' then 1   else sourceColumn p1
                      r = if sourceLine p2 > n' then len else sourceColumn p2
                  in Just (l, r)
    | otherwise = Nothing
            where n' = n + 1


makeMask :: Char -> Char -> Int -> [(Int, Int)] -> [Char]
makeMask borderCh innerCh len ixs = flip map [1..len] $ \i ->
        if   any (begEnd i) ixs
        then borderCh
        else if any (containes i) ixs
             then innerCh
             else ' '
    where
        containes i (l, r) = l <= i && i < r
        begEnd    i (l, r) = l == i || i == r - 1

-- ERRORS

data AlignDescrProcessErrorType
    = ADE_UnequalBlockTypes
    | ADE_UnequalLength
    | ADE_UnequalNames
    | ADE_UnequalSpaces        
    | ADE_UnequalStrings
    | ADE_UnequalOperations
    | ADE_UnifyError          [(AlignAriphmExpr, AlignAriphmExpr)]
    | ADE_IdenticalBeginEnd    
    | ADE_UnknownError

makeUnifyError :: [PosSegm] -> [PosSegm] -> Either [(AlignAriphmExpr, AlignAriphmExpr)] a -> Except AlignDescrProcessError a
makeUnifyError _ _   (Right val)  = return val
makeUnifyError p1 p2 (Left descr) = throwE $ ADE (ADE_UnifyError descr) p1 p2

data AlignDescrProcessError = ADE AlignDescrProcessErrorType [PosSegm] [PosSegm]
    deriving Show

instance Show AlignDescrProcessErrorType where
    show ADE_UnequalBlockTypes{}       = "Block types are unequal"
    show ADE_UnequalLength{}           = "Block length/index count are unequal"
    show ADE_UnequalNames{}            = "Variable names are unequal"
    show ADE_UnequalSpaces{}           = "Spaces are unequal"
    show ADE_UnequalStrings{}          = "String constants are unequal"
    show ADE_UnequalOperations{}       = "Binary operations are unequal"
    show (ADE_UnifyError exprs)        = "Unable to unify these changes:"
                                            ++ concatMap ((" " ++) . show) exprs 
    show ADE_IdenticalBeginEnd{}       = "First and last blocks of loop are identical"
    show ADE_UnknownError{}            = "UNKNOWN_ERROR this msg shouldn't be shown"



--- TESTS

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
    , ("Не получается унифицировать блоки"
     , "{l[1] r[1]}..{l[M] r[N]}\n")
    , ("Не получается унифицировать циклы"
     , "{a[1][1] .. a[1][K]}..{a[N][1] .. a[M][K]}\n")
    , ("Блоки разных типов (слева цикл, справа переменная)"
     , "{a[1][1] .. a[1][K]}..{a[N][1]}\n")
    , ("Пробелы"
     , "{a[1][1]..a[1][K]}..{a[N][1] .. a[N][K]}\n")
    , ("Одинаковые начало и конец цикла"
     , "a[1]..a[1]\n")
    , ("Разные пробелы перед и после .."
     , "a[1] ..a[n]\n"
     )
    ]
    where
        helper (msg, inp)= do
            putStrLn msg
            putStrLn "input data"
            mapM_ (\(i, s) -> putStrLn $ show i ++ ".\t" ++ s) $ zip [1..] $ lines inp
            putStrLn "output data"
            let outp = readDescr inp
            case runExcept outp of
              Left  e      -> putStrLn e
              Right descrs -> putStrLn $ concatMap ((++"\n") . show) descrs
            putStrLn "-------------------\n"
