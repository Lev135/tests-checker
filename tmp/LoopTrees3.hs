{-# LANGUAGE UndecidableInstances #-}
module LoopTrees3 (

) where

import Text.Parsec
    ( Parsec,
      char, letter, string, eof,
      many1, sepBy, (<|>),
      parse, try, ParseError, getPosition, SourcePos, errorPos, sourceLine, sourceColumn )
import Data.Maybe (catMaybes)
import Control.Monad ( forM_ ) 
import Data.List (group)
import Text.Parsec.Error (errorMessages, messageString, showErrorMessages)

concatMapSep :: (a -> [b]) -> [b] -> [a] -> [b]
concatMapSep f sep [] = []
concatMapSep f sep (x : xs) = f x <> concatMap ((sep <>) . f) xs

type Position = (SourcePos, SourcePos)

data PosT t = PosT [Position] t

instance Functor PosT where
    f `fmap` (PosT p t) = PosT p (f t)

instance Applicative PosT where
    pure a = PosT [] a
    (PosT fp ft) <*> (PosT ap at) = PosT (fp <> ap) (ft at)

instance Monad PosT where
    (PosT p t) >>= f = PosT (p <> p') t'
        where PosT p' t' = f t 

getPos :: PosT t -> [Position]
getPos (PosT p _) = p

instance Show t => Show (PosT t) where
    show (PosT _ t) = show t

withPosP :: Parser t -> Parser (PosT t)
withPosP p = do
    fPos <- getPosition
    x <- p
    lPos <- getPosition
    return $ PosT [(fPos, lPos)] x

-- Input structure

data TreeLex
    = TLVar String
    | TLBlock [PosT TreeLex]
    | TLLoop (PosT TreeLex) (PosT TreeLex)

-- PARSER
type Parser t = Parsec String () t

treeP :: Parser (PosT TreeLex)
treeP = try loopP <|> try termP

termP :: Parser (PosT TreeLex)
termP = withPosP
     $  TLVar <$> many1 letter
    <|> TLBlock <$> (char '(' *> treeP `sepBy` char ' ' <* char ')')

loopP :: Parser (PosT TreeLex)
loopP = withPosP $ TLLoop <$> termP <* string ".." <*> termP

parseTree :: String -> Either ParseError (PosT TreeLex)
parseTree = parse (treeP <* eof) ""

-- Processed structure

data Tree
    = TVar String
    | TBlock [PosT Tree]
    | TLoop {
        lBlock  :: PosT Tree,
        lFirstI :: String,
        lLastI  :: String,
        lIdxVar :: String
    }


-- Procesing

data ProcessError = ProcessError [Position] String

errorMsg :: ProcessError -> String
errorMsg (ProcessError _ s) = s

instance Show ProcessError where
    show (ProcessError [] msg) = "Error: " ++ msg
    show (ProcessError ps msg) = "Error " ++ concatMap show ps ++ ": " ++ msg

processTree :: PosT TreeLex -> Either ProcessError (PosT Tree)
processTree = processTreeImpl 0

varByLvl :: Int -> String
varByLvl = (map ('$':) ["i", "j", "k", "l"] !!)

-- | lvl - уровень вложенности циклов. В зависимости от него выберается переменная для индекса
processTreeImpl :: Int -> PosT TreeLex -> Either ProcessError (PosT Tree)
processTreeImpl lvl (PosT pos (TLVar str))     = PosT pos <$> Right (TVar str)
processTreeImpl lvl (PosT pos (TLBlock xs))    = PosT pos . TBlock <$> mapM (processTreeImpl lvl) xs
processTreeImpl lvl (PosT pos (TLLoop b1 b2))  = PosT pos <$> do
            b1' <- processTreeImpl (lvl + 1) b1
            b2' <- processTreeImpl (lvl + 1) b2
            (block, mIs) <- compParts idxVar b1' b2'
            (lI, rI) <- maybeToEither (err (getPos b1) (getPos b2)) mIs
            return $ TLoop block lI rI idxVar
        where
            idxVar = varByLvl lvl
            err p1 p2 = ProcessError (p1 <> p2) "First & last blocks are equal"

-- | Just a map, if sizes of lists are equal
map2 :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
map2 _ [] []             = Just []
map2 g (a : as) (b : bs) = (g a b :) <$> map2 g as bs
map2 _ _ _               = Nothing

-- | @ 
-- allEq [Just 1, Nothing, Just 1] == Just Just 1
-- allEq [Nothing, Nothing] == Just Nothing
-- allEq [Just 1, Just 2] == Nothing @
allEq :: Eq a => [Maybe a] -> Maybe (Maybe a)
allEq = h . group . catMaybes
    where
        h []  = Just Nothing
        h [x] = Just (Just $ head x)
        h _   = Nothing

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither _ (Just v)  = Right v
maybeToEither msg Nothing = Left msg

-- | Сравнивает две (предположительно соответствующие друг другу) части синтаксического дерева
-- если части абсолютно идентичны, возвращает исходную часть и Nothing
-- если различны, "обобщает" их, и возвращает обобщённую часть, а также индексы в первой и последней итерации
compParts :: String -> PosT Tree -> PosT Tree -> Either ProcessError (PosT Tree, Maybe (String, String))
compParts idxVar (PosT p1 (TVar s1)) (PosT p2 (TVar s2))
    | s1 == s2   = Right (PosT p $ TVar s1,     Nothing)
    | otherwise  = Right (PosT p $ TVar idxVar, Just (s1, s2))
        where p = p1 <> p2
compParts idxVar (PosT p1 (TBlock xs)) (PosT p2 (TBlock ys)) = do
                compMaybes <- maybeToEither (ProcessError (p1 <> p2) "Block length are unequal") $ map2 (compParts idxVar) xs ys
                comps <- sequence compMaybes
                comp <- maybeToEither (ProcessError (p1 <> p2) "Unable to uniform blocks") $ allEq $ map snd comps
                return (PosT (p1 <> p2) . TBlock $ map fst comps, comp)
compParts idxVar (PosT p1 (TLoop block1 fI1 lI1 idxVar1)) (PosT p2 (TLoop block2 fI2 lI2 _) ) = do
                compBlocks <- compParts idxVar block1 block2
                compFirstI <- compParts idxVar (PosT p1 (TVar fI1)) (PosT p2 (TVar fI2))
                compLastI  <- compParts idxVar (PosT p1 (TVar lI1)) (PosT p2 (TVar lI2))
                comp <- maybeToEither unifLoopsErr $ allEq $ map snd [compBlocks, compFirstI, compLastI]
                -- Мы точно знаем, что внутри переменные, так как их сами туда положили
                let PosT _ (TVar fI') = fst compFirstI
                    PosT _ (TVar lI') = fst compLastI
                    block = fst compBlocks
                return (PosT (p1 <> p2) $ TLoop block fI' lI' idxVar1, comp)
            where unifLoopsErr = ProcessError (p1 <> p2) "Unable to uniform loops"

compParts _ (PosT p1 _) (PosT p2 _) = Left (ProcessError (p1 <> p2) "Type of parts are unequal")

-- Test and show functions

instance Show TreeLex where
    show (TLVar str ) = str
    show (TLBlock xs) = "(" ++ concatMapSep show " " xs ++ ")"
    show (TLLoop x y) = show x ++ ".." ++ show y


instance Show Tree where
    show (TVar str)                    = str
    show (TBlock xs)                   = "(" ++ concatMapSep show " " xs ++ ")"
    show (TLoop block fI lI idxVar)    = "{" ++ show block ++ " | " ++ idxVar ++ "=" ++ fI ++ ".." ++ lI ++ "}"

toStrError :: Show e => Either e a -> Either String a
toStrError (Left  e) = Left $ show e
toStrError (Right a) = Right a

toProcessError :: ParseError -> ProcessError
toProcessError e = ProcessError [(errorPos e, errorPos e)]
     (showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of" $ errorMessages e)

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = either (Left . f) Right

readTree :: String -> Either ProcessError (PosT Tree)
readTree s = do
    lex <- mapLeft toProcessError $ parseTree s
    processTree lex

tests :: [(String, Either ProcessError (PosT Tree))]
tests = (\s -> (s, readTree s)) <$> [
        -- OK
        "((o o)..(o m))..((n o)..(n m))",
        "(((o o o)..(o o M))..((o N o)..(o N M)))..(((L o o)..(L o M))..((L N o)..(L N M)))",
        "(((a o)..(a a))..((b o)..(b b)))",
        "(o..M)..(N..M)",
        -- Unable to uniform
        "(o o)..(P Q)",         -- blocks
        "(o..P)..(N..Q)",       -- loops
        "((a o)..(a N))..(b o)",
        "(a b (o o)..(o o))",
        "(((o o o)..(o o M))..((o N o)..(o N M)))..(((L o o)..(L o K))..((L N o)..(L N K)))",
        "(3"
    ]

testMain :: IO ()
testMain = do
    forM_ (zip [1..] tests) $ \(i, s) -> print i >> printTest s
    where
        printTest (s, Right t)                 = print s >> print t
        printTest (s, Left (ProcessError p m)) = print s >> underlinePos (length s) p >> putStrLn ("Error: " ++ m)

        underlinePos l e = putStrLn $ " " ++ makeMask l (makeSegment <$> e)

        makeSegment (p, q)  = (sourceColumn p, sourceColumn q)

makeMask :: Int -> [(Int, Int)] -> [Char]
makeMask l ixs = flip map [1..l] $ \i ->
        if   any (begEnd i) ixs
        then '^'
        else if any (containes i) ixs
             then '~'
             else ' '
    where
        containes i (f, l) = f <= i && i < l
        begEnd    i (f, l) = f == i || i == l - 1
