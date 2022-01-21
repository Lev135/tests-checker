{-# LANGUAGE UndecidableInstances #-}
module LoopTrees2 (

) where

import Text.Parsec
    ( Parsec,
      char, letter, string, eof,
      many1, sepBy, (<|>),
      parse, try, ParseError, getPosition, SourcePos, errorPos, sourceLine, sourceColumn )
import Data.Maybe (isJust, catMaybes)
import Control.Monad (join, (>=>), forM_, forM)
import Data.List (group, sortOn)
import Text.Parsec.Error (errorMessages, messageString, showErrorMessages)

concatMapSep :: (a -> [b]) -> [b] -> [a] -> [b]
concatMapSep f sep [] = []
concatMapSep f sep (x : xs) = f x <> concatMap ((sep <>) . f) xs

data Position = Pos SourcePos | Segm SourcePos SourcePos
instance Show Position where
    show (Pos   p)      = show p
    show (Segm  f l)    = "[" ++ show f ++ ", " ++ show l ++ "]"

data PosT t = PosT (t (PosT t)) [Position]

getPos :: PosT t -> [Position]
getPos (PosT _ p) = p

instance Show (t (PosT t)) => Show (PosT t) where
    show (PosT t p) = show t

withPosP :: Parser (t (PosT t)) -> Parser (PosT t)
withPosP p = do
    pos <- getPosition
    x <- p
    return $ PosT x [Pos pos]

withSegmP :: Parser (t (PosT t)) -> Parser (PosT t)
withSegmP p = do
    fPos <- getPosition 
    x <- p
    lPos <- getPosition 
    return $ PosT x [Segm fPos lPos] 

-- Input structure

data TreeLex' lex
    = TLVar String
    | TLBlock [lex]
    | TLLoop lex lex

type TreeLex = PosT TreeLex'

-- PARSER
type Parser t = Parsec String () t

treeP :: Parser TreeLex
treeP = try loopP <|> try termP

termP :: Parser TreeLex
termP = withPosP  (TLVar <$> many1 letter)
    <|> withSegmP (TLBlock <$> (char '(' *> treeP `sepBy` char ' ' <* char ')'))

loopP :: Parser TreeLex
loopP = withSegmP $ TLLoop <$> termP <* string ".." <*> termP

parseTree :: String -> Either ParseError TreeLex
parseTree = parse (treeP <* eof) ""

-- Processed structure

data Tree' tree
    = TVar String
    | TBlock [tree]
    | TLoop {
        lBlock  :: tree,
        lFirstI :: String,
        lLastI  :: String,
        lIdxVar :: String
    }

type Tree = PosT Tree'

-- Procesing

data ProcessError = ProcessError [Position] String

errorMsg :: ProcessError -> String
errorMsg (ProcessError _ s) = s

instance Show ProcessError where
    show (ProcessError [] msg) = "Error: " ++ msg
    show (ProcessError ps msg) = "Error " ++ concatMap show ps ++ ": " ++ msg

processTree :: TreeLex -> Either ProcessError Tree
processTree = processTreeImpl 0

varByLvl :: Int -> String
varByLvl = (map ('$':) ["i", "j", "k", "l"] !!)

-- | lvl - уровень вложенности циклов. В зависимости от него выберается переменная для индекса
processTreeImpl :: Int -> TreeLex -> Either ProcessError Tree
processTreeImpl lvl (PosT (TLVar str) pos)     = flip PosT pos <$> Right (TVar str)
processTreeImpl lvl (PosT (TLBlock xs) pos)    = flip PosT pos . TBlock <$> mapM (processTreeImpl lvl) xs
processTreeImpl lvl (PosT (TLLoop b1 b2) pos)  = flip PosT pos <$> do
            b1' <- processTreeImpl (lvl + 1) b1
            b2' <- processTreeImpl (lvl + 1) b2
            (block, mIs) <- compParts idxVar b1' b2'
            (lI, rI) <- maybeToEither (err (getPos b1) (getPos b2)) mIs
            return $ TLoop block lI rI idxVar
        where
            idxVar = varByLvl lvl
            err p1 p2 = ProcessError (p1 <> p2) "Error while processing loop: first & last blocks are equal"

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
compParts :: String -> Tree -> Tree -> Either ProcessError (Tree, Maybe (String, String))
compParts idxVar (PosT (TVar s1) p1) (PosT (TVar s2) p2)
    | s1 == s2   = Right (PosT (TVar s1)     $ p1 <> p2, Nothing)
    | otherwise  = Right (PosT (TVar idxVar) $ p1 <> p2, Just (s1, s2))
compParts idxVar (PosT (TBlock xs) p1) (PosT (TBlock ys) p2) = do
                compMaybes <- maybeToEither (ProcessError (p1 <> p2) "Block length are unequal") $ map2 (compParts idxVar) xs ys
                comps <- sequence compMaybes
                comp <- maybeToEither (ProcessError (p1 <> p2) "Unable to uniform blocks") $ allEq $ map snd comps
                return (flip PosT (p1 <> p2) . TBlock $ map fst comps, comp)
compParts idxVar (PosT (TLoop block1 fI1 lI1 idxVar1) p1) (PosT (TLoop block2 fI2 lI2 _) p2) = do
                compBlocks <- compParts idxVar block1 block2
                compFirstI <- compParts idxVar (PosT (TVar fI1) p1) (PosT (TVar fI2) p2)
                compLastI  <- compParts idxVar (PosT (TVar lI1) p1) (PosT (TVar lI2) p2)
                comp <- maybeToEither (ProcessError (p1 <> p2) "Unable to uniform loops") $ allEq $ map snd [compBlocks, compFirstI, compLastI]
                -- Мы точно знаем, что внутри переменные, так как их сами туда положили
                let PosT (TVar fI') _ = fst compFirstI
                    PosT (TVar lI') _ = fst compLastI
                    block = fst compBlocks
                return (flip PosT (p1 <> p2) $ TLoop block fI' lI' idxVar1, comp)

compParts _ (PosT _ p1) (PosT _ p2) = Left (ProcessError (p1 <> p2) "Type of parts are unequal")

-- Test and show functions

instance Show lex => Show (TreeLex' lex) where
    show (TLVar str     ) = str
    show (TLBlock xs    ) = "(" ++ concatMapSep show " " xs ++ ")"
    show (TLLoop x y    ) = show x ++ ".." ++ show y


instance Show tree => Show (Tree' tree) where
    show (TVar str)                    = str
    show (TBlock xs)                   = "(" ++ concatMapSep show " " xs ++ ")"
    show (TLoop block fI lI idxVar)    = "{" ++ show block ++ " | " ++ idxVar ++ "=" ++ fI ++ ".." ++ lI ++ "}"

toStrError :: Show e => Either e a -> Either String a
toStrError (Left  e) = Left $ show e
toStrError (Right a) = Right a

toProcessError :: ParseError -> ProcessError
toProcessError e = ProcessError [Pos $ errorPos e]
     (showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of" $ errorMessages e)

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = either (Left . f) Right

readTree :: String -> Either ProcessError Tree
readTree s = do
    lex <- mapLeft toProcessError $ parseTree s
    processTree lex

tests :: [(String, Either ProcessError Tree)]
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
        "(a b (o o)..(o o))"
    ]

testMain :: IO ()
testMain = do
    forM_ (zip [1..] tests) $ \(i, s) -> print i >> printTest s
    where
        printTest :: (String, Either ProcessError Tree) -> IO ()
        printTest (s, Right t)                 = print s >> print t
        printTest (s, Left (ProcessError p m)) = print s >> underlinePos (length s) p >> putStrLn m

        underlinePos l e = putStrLn $ " " ++ makeMask l (makeSegment <$> e)

        makeSegment (Pos p)     = (sourceColumn p, sourceColumn p + 1)
        makeSegment (Segm p q)  = (sourceColumn p, sourceColumn q)

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
