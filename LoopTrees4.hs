module LoopTrees4 (

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
import Data.Function (on)


class Box p where
    unBox :: p a -> a

($$) :: Box p => (a -> b) -> p a -> b
f $$ pa = f $ unBox pa

(=<.<) :: (Functor f, Functor p, Box p) => (a -> f b) -> p a -> f (p b)
f =<.< a = (<$ a) <$> unBox (f <$> a)

type Wrap p t = p (t p)

-- Pos

type Position = (SourcePos, SourcePos)

data Pos t = Pos {pPos :: [Position], pVal :: t}

instance Eq t => Eq (Pos t) where
    (==) = (==) `on` pVal

instance Box Pos where
    unBox = pVal

instance Functor Pos where
    f `fmap` (Pos p a) = Pos p (f a)

instance Show t => Show (Pos t) where
    show = show . pVal

withPosP :: Parser t -> Parser (Pos t)
withPosP p = do
    fPos <- getPosition
    x <- p
    lPos <- getPosition
    return $ Pos [(fPos, lPos)] x

-- Input structure

data TreeLex' p
    = TLVar String
    | TLBlock [Wrap p TreeLex']
    | TLLoop (Wrap p TreeLex') (Wrap p TreeLex')

type TreeLexPos = Wrap Pos TreeLex'

-- PARSER
type Parser t = Parsec String () t

treeP :: Parser TreeLexPos
treeP = try loopP <|> try termP

termP :: Parser TreeLexPos
termP = withPosP
     $  TLVar <$> many1 letter
    <|> TLBlock <$> (char '(' *> treeP `sepBy` char ' ' <* char ')')

loopP :: Parser TreeLexPos
loopP = withPosP $ TLLoop <$> termP <* string ".." <*> termP

parseTree :: String -> Either ParseError TreeLexPos
parseTree = parse (treeP <* eof) ""

-- Processed structure

data Tree' p
    = TVar String
    | TBlock [Wrap p Tree']
    | TLoop {
        lBlock  :: Wrap p Tree',
        lFirstI :: p String,
        lLastI  :: p String,
        lIdxVar :: String
    }

type TreePos = Wrap Pos Tree'

-- Procesing

data ProcessError = ProcessError [Position] String

errorMsg :: ProcessError -> String
errorMsg (ProcessError _ s) = s

instance Show ProcessError where
    show (ProcessError [] msg) = "Error: " ++ msg
    show (ProcessError ps msg) = "Error " ++ concatMap show ps ++ ": " ++ msg

processTree :: TreeLexPos -> Either ProcessError TreePos
processTree = processTreeImpl 0

varByLvl :: Int -> String
varByLvl = (map ('$':) ["i", "j", "k", "l"] !!)


-- | lvl - уровень вложенности циклов. В зависимости от него выберается переменная для индекса
processTreeImpl :: Int -> TreeLexPos -> Either ProcessError TreePos
processTreeImpl lvl pt = f =<.< pt
        where 
            f :: TreeLex' Pos -> Either ProcessError (Tree' Pos)
            f (TLVar   s)  = Right $ TVar s
            f (TLBlock xs) = TBlock <$> mapM (processTreeImpl lvl) xs
            f (TLLoop b1 b2) = do
                    b1' <- processTreeImpl (lvl + 1) b1
                    b2' <- processTreeImpl (lvl + 1) b2
                    (block, mIs) <- compParts idxVar b1' b2'
                    (lI, rI) <- maybeToEither (err (pPos b1) (pPos b2)) mIs
                    return $ TLoop block lI rI idxVar
            idxVar = varByLvl lvl
            err p1 p2 = ProcessError (p1 <> p2) "First & last blocks are equal"

-- processTreeImpl lvl (Pos pos (TLVar str))     = Pos pos <$> Right (TVar str)
-- processTreeImpl lvl (Pos pos (TLBlock xs))    = Pos pos . TBlock <$> mapM (processTreeImpl lvl) xs
-- processTreeImpl lvl (Pos pos (TLLoop b1 b2))  = Pos pos <$> do

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
compParts :: String -> TreePos -> TreePos -> Either ProcessError (TreePos, Maybe (Pos String, Pos String))
compParts idxVar pt1 pt2 
    = case (pVal pt1, pVal pt2) of
        (TVar s1, TVar s2) -> if s1 == s2
            then return (Pos p $ TVar s1,       Nothing)  
            else return (Pos p $ TVar idxVar,   Just (Pos p1 s1, Pos p2 s2))
        (TBlock xs, TBlock ys) -> do
            compMaybes <- maybeToEither (ProcessError p "Block length are unequal") $ map2 (compParts idxVar) xs ys
            comps <- sequence compMaybes
            comp <- maybeToEither (ProcessError p "Unable to uniform blocks") $ allEq $ map snd comps
            return (Pos p . TBlock $ map fst comps, comp)
        (TLoop block1 fI1 lI1 idxVar1, TLoop block2 fI2 lI2 idxVar2) -> do
            compBlocks <- compParts idxVar block1 block2
            compFirstI <- compParts idxVar (Pos p1 (TVar $ pVal fI1)) (Pos p2 (TVar $ pVal fI2))
            compLastI  <- compParts idxVar (Pos p1 (TVar $ pVal lI1)) (Pos p2 (TVar $ pVal lI2))
            comp <- maybeToEither unifLoopsErr $ allEq $ map snd [compBlocks, compFirstI, compLastI]
            -- Мы точно знаем, что внутри переменные, так как их сами туда положили
            let TVar fI' = pVal $ fst compFirstI
                TVar lI' = pVal $ fst compLastI
                block = fst compBlocks
            return (Pos p $ TLoop block (Pos p1 fI') (Pos p2 lI') idxVar1, comp)
         
        (_, _) -> Left (ProcessError p "Type of parts are unequal")
    where
        p1 = pPos pt1
        p2 = pPos pt2 
        p  = p1 <> p2
        unifLoopsErr = ProcessError (p1 <> p2) "Unable to uniform loops"
    
-- Test and show functions

concatMapSep :: (a -> [b]) -> [b] -> [a] -> [b]
concatMapSep f sep [] = []
concatMapSep f sep (x : xs) = f x <> concatMap ((sep <>) . f) xs

instance Box p => Show (TreeLex' p) where
    show (TLVar str ) = str
    show (TLBlock xs) = "(" ++ concatMapSep (show $$) " " xs ++ ")"
    show (TLLoop x y) = show $$ x ++ ".." ++ show $$ y


instance Box p => Show (Tree' p) where
    show (TVar str)                    = str
    show (TBlock xs)                   = "(" ++ concatMapSep (show $$) " " xs ++ ")"
    show (TLoop block fI lI idxVar)    = "{" ++ show $$ block ++ " | " ++ idxVar ++ "=" ++ unBox fI ++ ".." ++ unBox lI ++ "}"

toStrError :: Show e => Either e a -> Either String a
toStrError (Left  e) = Left $ show e
toStrError (Right a) = Right a

toProcessError :: ParseError -> ProcessError
toProcessError e = ProcessError [(errorPos e, errorPos e)]
     (showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of" $ errorMessages e)

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = either (Left . f) Right

readTree :: String -> Either ProcessError TreePos
readTree s = do
    lex <- mapLeft toProcessError $ parseTree s
    processTree lex

tests :: [(String, Either ProcessError TreePos)]
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
