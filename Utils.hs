module Utils (
    Parser, betweenCh, lineSpaces, lineSpaces1, vSpace, parseAll, numberP,
    allEq, map2, concatM,
    Box (..), ($$), (=<.<), Wrap,
    Pos (..), PosSegm, makePos, withPosP,
    maybeToExcept, guardE
) where

import Text.Parsec
    ( char, endOfLine, oneOf, between, many1, many, Parsec, SourcePos, getPosition, ParseError, parse, eof, digit )
import Data.List (group, nub)
import Data.Maybe (catMaybes)
import Control.Monad.Trans.Except (Except, throwE)
import Control.Monad ((>=>))

type Parser = Parsec String ()

betweenCh :: Char -> Char -> Parser a -> Parser a
betweenCh a b = between (char a) (char b)

lineSpaces :: Parser [Char]
lineSpaces = many $ oneOf " \t"

lineSpaces1 :: Parser [Char]
lineSpaces1 = many1 $ oneOf " \t"

vSpace :: Parser Char
vSpace = endOfLine

numberP :: Parser Int
numberP = read <$> many1 digit 

parseAll :: Parser a -> String -> Either ParseError  a
parseAll p = parse (p <* eof) ""


-- |Compose a list of monadic actions into one action.  Composes using
-- ('>=>') - that is, the output of each action is fed to the input of
-- the one after it in the list.
concatM :: (Monad m) => [a -> m a] -> (a -> m a)
concatM = foldr (>=>) return

-- | @ 
-- allEq [Just 1, Nothing, Just 1] == Just Just 1
-- allEq [Nothing, Nothing] == Just Nothing
-- allEq [Just 1, Just 2] == Nothing @
allEq :: Eq a => [Maybe a] -> Either [a] (Maybe a)
allEq = h . nub . catMaybes
    where
        h []  = Right Nothing
        h [x] = Right $ Just x
        h xs  = Left xs

-- | Just a map, if sizes of lists are equal
map2 :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
map2 _ [] []             = Just []
map2 g (a : as) (b : bs) = (g a b :) <$> map2 g as bs
map2 _ _ _               = Nothing


class Box p where
    unBox :: p a -> a

($$) :: Box p => (a -> b) -> p a -> b
f $$ pa = f $ unBox pa

(=<.<) :: (Functor f, Functor p, Box p) => (a -> f b) -> p a -> f (p b)
f =<.< a = (<$ a) <$> unBox (f <$> a)

type Wrap p t = p (t p)

type PosSegm = (SourcePos, SourcePos)

makePos :: SourcePos -> PosSegm
makePos p = (p, p)

data Pos t = Pos {pPos :: [PosSegm], pVal :: t}

instance Box Pos where
    unBox = pVal

instance Functor Pos where
    fmap f (Pos p a) = Pos p $ f a

instance Eq t => Eq (Pos t) where
    p == q = pVal p == pVal q

instance Show t => Show (Pos t) where
    show = show . pVal

withPosP :: Parser t -> Parser (Pos t)
withPosP p = do
    fPos <- getPosition
    x <- p
    lPos <- getPosition
    return $ Pos [(fPos, lPos)] x

maybeToExcept :: e -> Maybe a -> Except e a
maybeToExcept _ (Just a) = return a
maybeToExcept e Nothing  = throwE e

guardE :: e -> Bool -> Except e ()
guardE _ True  = return ()
guardE e False = throwE e
