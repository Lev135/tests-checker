module Ariphm (
    AriphmOp (..), VarName (..), Var (..), AriphmExprPos (..), AriphmExpr (..),
    ariphmExprP, ariphmTermP, varP,
    VarValues (..), EvalError (..),
    evalAriphm
) where

import Control.Applicative ((<|>), Alternative (many))
import Text.Parsec ( char, between, digit, letter, many1, Parsec, (<?>))
import Text.Parsec.Char ( spaces )
import Control.Monad.Combinators.Expr ( makeExprParser, Operator(InfixL, InfixR) )
import Utils (lineSpaces, Pos (pVal, Pos, pPos), withPosP, Wrap, Box (unBox), ($$), betweenCh, maybeToExcept, numberP, Parser, VarName, VarValues, EvalError (EE_UninitializedVar, EE_UninitializedIndex))
import Data.Function (on)
import Control.Monad.Trans.Except (Except)
import qualified Data.Map as M

data AriphmOp
    = ASum | ADiff | ADiv | AProd | APow
    deriving Eq

data Var p = Var VarName [Wrap p AriphmExpr]

instance (Box p) => Eq (Var p) where
    (Var s1 ixs1) == (Var s2 ixs2) = s1 == s2 && and (zipWith ((==) `on` unBox) ixs1 ixs2)

data AriphmExpr p
    = AVar     (Var p)
    | AConst   Int
    | ABinOp   AriphmOp (Wrap p AriphmExpr) (Wrap p AriphmExpr)

instance (Box p) => Eq (AriphmExpr p) where
    (AVar v1) == (AVar v2) = v1 == v2
    (AConst c1) == (AConst c2) = c1 == c2
    (ABinOp op1 e1 e1') == (ABinOp op2 e2 e2') = op1 == op2 && unBox e1 == unBox e2 && unBox e1' == unBox e2'
    _ == _ = False

type AriphmExprPos = Wrap Pos AriphmExpr

instance Show AriphmOp where
    show ASum  = "+"
    show ADiff = "-"
    show AProd = "*"
    show ADiv  = "/"
    show APow  = "^"

instance Box p => Show (Var p) where
    show (Var s idxs) = s ++ concatMap h idxs
        where h = \i -> "[" ++ show $$ i ++ "]"

instance Box p => Show (AriphmExpr p) where
    show (AVar v)           = show v
    show (AConst n)         = show n
    show (ABinOp op e e')   = "(" ++ show $$ e ++ " " ++ show op ++ " " ++ show $$ e' ++ ")"

ariphmExprP :: Parser AriphmExprPos
ariphmExprP = makeExprParser (ariphmTermP <* lineSpaces) operators <* lineSpaces
        <?> "expression"

ariphmTermP :: Parser AriphmExprPos
ariphmTermP = between (char '(') (char ')') ariphmExprP
    <|> withPosP (AConst <$> numberP)
    <|> withPosP (AVar <$> varP)
    <?> "term"

varP :: Parser (Var Pos)
varP = Var <$> many1 letter <*> many indexes
    where indexes = betweenCh '[' ']' (lineSpaces *> ariphmExprP)

operators :: [[Operator Parser AriphmExprPos]]
operators = [
        [ InfixR $ tmp (ABinOp APow <$ char '^' <* lineSpaces)
        ],
        [ InfixL $ tmp (ABinOp AProd <$ char '*' <* lineSpaces)
        , InfixL $ tmp (ABinOp ADiv  <$ char '/' <* lineSpaces)
        ],
        [ InfixL $ tmp (ABinOp ASum  <$ char '+' <* lineSpaces)
        , InfixL $ tmp (ABinOp ADiff <$ char '-' <* lineSpaces)
        ]
    ]
    where
        tmp pg = do
            g <- withPosP pg
            let g' a b = Pos (pPos g) $ pVal g a b
            return g'

evalAriphm :: VarValues -> AriphmExprPos -> Except EvalError Int
evalAriphm vals pe = case pVal pe of
    AVar (Var s idxs) -> do
        idxs' <- mapM (evalAriphm vals) idxs
        findVar vals s idxs'
    AConst n ->
        return n
    ABinOp op pe1 pe2 -> do
        n1 <- evalAriphm vals pe1
        n2 <- evalAriphm vals pe2
        let op' = case op of
                        ASum -> (+)
                        ADiff -> (-)
                        ADiv -> div
                        AProd -> (*)
                        APow -> (^)
        return $ n1 `op'` n2


findVar :: VarValues -> String -> [Int] -> Except EvalError Int
findVar vals varName idxs = do
    x <- maybeToExcept (EE_UninitializedVar varName) $ M.lookup varName vals
    maybeToExcept (EE_UninitializedIndex varName idxs) $ M.lookup idxs x
