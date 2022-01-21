module LoopTrees (
    Tree (..), readTree
) where

import Text.Parsec
    ( Parsec,
      char, letter, string, eof,
      many1, sepBy, (<|>),
      parse, try, ParseError )
import Data.Maybe (isJust, catMaybes)
import Control.Monad (join, (>=>), forM_)
import Data.List (group)
import Distribution.Parsec.Common (Position)

concatMapSep :: (a -> [b]) -> [b] -> [a] -> [b]
concatMapSep f sep [] = []
concatMapSep f sep (x : xs) = f x <> concatMap ((sep <>) . f) xs

-- Input structure

data TreeLex
    = TLVar String
    | TLBlock [TreeLex]
    | TLLoop TreeLex TreeLex
 
-- PARSER

type Parser t = Parsec String () t

treeP :: Parser TreeLex
treeP = try loopP <|> try termP

termP :: Parser TreeLex
termP = TLVar <$> many1 letter
    <|> TLBlock <$> (char '(' *> treeP `sepBy` char ' ' <* char ')')

loopP :: Parser TreeLex
loopP = TLLoop <$> termP <* string ".." <*> termP

parseTree :: String -> Either ParseError TreeLex
parseTree = parse (treeP <* eof) ""

-- Processed structure

data Tree
    = TVar String
    | TBlock [Tree]
    | TLoop {
        lBlock  :: Tree,
        lFirstI :: String,
        lLastI  :: String,
        lIdxVar :: String
    }

-- Procesing

processTree :: TreeLex -> Either String Tree
processTree = processTreeImpl 0

varByLvl :: Int -> String
varByLvl = (map ('$':) ["i", "j", "k", "l"] !!)

-- | lvl - уровень вложенности циклов. В зависимости от него выберается переменная для индекса
processTreeImpl :: Int -> TreeLex -> Either String Tree
processTreeImpl lvl (TLVar str) = Right $ TVar str
processTreeImpl lvl (TLBlock xs)   = TBlock <$> mapM (processTreeImpl lvl) xs
processTreeImpl lvl (TLLoop b1 b2)   = do
            b1' <- processTreeImpl (lvl + 1) b1
            b2' <- processTreeImpl (lvl + 1) b2
            (block, mIs) <- compParts idxVar b1' b2'
            (lI, rI) <- maybeToEither "Error while processing loop: first & last blocks are equal" mIs
            return $ TLoop block lI rI idxVar
        where idxVar = varByLvl lvl

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
compParts :: String -> Tree -> Tree -> Either String (Tree, Maybe (String, String))
compParts idxVar (TVar s1) (TVar s2)
    | s1 == s2   = Right (TVar s1, Nothing)
    | otherwise  = Right (TVar idxVar, Just (s1, s2))
compParts idxVar (TBlock xs) (TBlock ys) = do
                compMaybes <- maybeToEither "Block length are unequal" $ map2 (compParts idxVar) xs ys
                comps <- sequence compMaybes
                comp <- maybeToEither "Error while comparing blocks" $ allEq $ map snd comps
                return (TBlock $ map fst comps, comp)
compParts idxVar (TLoop block1 fI1 lI1 idxVar1) (TLoop block2 fI2 lI2 _) = do
                compBlocks <- compParts idxVar block1 block2
                compFirstI <- compParts idxVar (TVar fI1) (TVar fI2)
                compLastI  <- compParts idxVar (TVar lI1) (TVar lI2)
                comp <- maybeToEither "Error while comparing loops" $ allEq $ map snd [compBlocks, compFirstI, compLastI]
                -- Мы точно знаем, что внутри переменные, так как их сами туда положили
                let TVar fI' = fst compFirstI
                    TVar lI' = fst compLastI
                    block = fst compBlocks
                return (TLoop block fI' lI' idxVar1, comp)

compParts _ _ _ = Left "Error while comparing parts: type of parts are unequal"

-- Test and show functions

instance Show TreeLex where
    show (TLVar str  ) = str
    show (TLBlock xs    ) = "(" ++ concatMapSep show " " xs ++ ")"
    show (TLLoop x y    ) = show x ++ ".." ++ show y

instance Show Tree where
    show (TVar str)  = str
    show (TBlock xs) = "(" ++ concatMapSep show " " xs ++ ")"
    show (TLoop block fI lI idxVar)   = "{" ++ show block ++ " | " ++ idxVar ++ "=" ++ fI ++ ".." ++ lI ++ "}"

readTree :: String -> Either String Tree
readTree str = processTree
            =<< case parseTree str of
                    Left e -> Left $ show e
                    Right v -> Right v

tests :: [(String, Either String Tree)]
tests = (\s -> (s, readTree s)) <$> [
        "((o o)..(o m))..((n o)..(n m))",
        "(((o o o)..(o o M))..((o N o)..(o N M)))..(((L o o)..(L o M))..((L N o)..(L N M)))",
        "(((a o)..(a a))..((b o)..(b b)))",
        "((a o)..(a N))..(b o)",
        "(o o)..(N M)"
    ]

testMain :: IO ()
testMain = do
    forM_ tests $ \s -> printTest s >> putStrLn ""
    where
        printTest :: (String, Either String Tree) -> IO ()
        printTest (s, Right t) = print s >> print t
        printTest (s, Left  e) = print s >> putStrLn ("Error: " ++ show e)
