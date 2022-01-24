module Utils (
    Parser, betweenCh, lineSpaces, lineSpaces1, vSpace,
    allEq, map2,
    Pos (..), PosSegm, makePos, withPosP,
    maybeToExcept, guardE
) where

import Text.Parsec
    ( char, endOfLine, oneOf, between, many1, many, Parsec, SourcePos, getPosition )
import Data.List (group, nub)
import Data.Maybe (catMaybes)
import Control.Monad.Trans.Except (Except, throwE)

type Parser t = Parsec String () t

betweenCh :: Char -> Char -> Parser a -> Parser a
betweenCh a b = between (char a) (char b)

lineSpaces :: Parser [Char]
lineSpaces = many $ oneOf " \t"

lineSpaces1 :: Parser [Char]
lineSpaces1 = many1 $ oneOf " \t"

vSpace :: Parser Char
vSpace = endOfLine

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

type PosSegm = (SourcePos, SourcePos)

makePos :: SourcePos -> PosSegm
makePos p = (p, p)

data Pos t = Pos {pPos :: [PosSegm], pVal :: t}

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
