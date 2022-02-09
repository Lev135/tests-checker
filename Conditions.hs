module Conditions (
    CondRel (..), Cond (..), CondPos (..), condP, evalCond
)   where

import Ariphm
import Utils
import Control.Applicative ( Alternative(empty, (<|>)) )
import Text.ParserCombinators.Parsec.Char (string)
import Text.ParserCombinators.Parsec.Prim (try, (<?>))
import Control.Monad.Combinators.Expr (Operator(InfixL, Prefix), makeExprParser)
import Control.Monad.Trans.Except (Except)

data CondRel = CLess | CLessEq | CGreater | CGreaterEq | CEq | CNeq

data Cond p
    = Cond (Wrap p AriphmExpr) CondRel (Wrap p AriphmExpr)
    | CAnd (Wrap p Cond) (Wrap p Cond)
    | COr  (Wrap p Cond) (Wrap p Cond)
    | CNot (Wrap p Cond)

instance Show CondRel where
    show CLess      = " < "
    show CLessEq    = " <= "
    show CGreater   = " > "
    show CGreaterEq = " >= "
    show CEq        = " = "
    show CNeq       = " <> "

instance Box p => Show (Cond p) where
    show (Cond e r e') = "(" ++ show $$ e ++ show r ++ show $$ e' ++ ")"
    show (CAnd c c')   = "(" ++ show $$ c ++ " && " ++ show $$ c' ++ ")"
    show (COr  c c')   = "(" ++ show $$ c ++ " || " ++ show $$ c' ++ ")"
    show (CNot c)      = "(~" ++ show $$ c ++ ")"

type CondPos = Wrap Pos Cond

condP :: Parser CondPos
condP = makeExprParser (condTermP <* lineSpaces) operators <* lineSpaces
    <?> "Cond expression"

condTermP :: Parser CondPos
condTermP = betweenCh '(' ')' condP
    <|> withPosP (Cond <$> ariphmExprP <*> condRelP <*> ariphmExprP)
    <?> "Cond term"

condRelP :: Parser CondRel
condRelP = foldr ((<|>) . hMakeP) empty [
        (CLessEq,    "<="),
        (CGreaterEq, ">="),
        (CNeq,       "<>"),
        (CLess,      "<"),
        (CGreater,   ">"),
        (CEq,        "=")
    ]
    where
        hMakeP :: (b, String) -> Parser b
        hMakeP (constr, str) = try $ constr <$ string str <* lineSpaces

operators :: [[Operator Parser CondPos]]
operators = [
        [Prefix $ tmp1 (CNot <$ string "~"  <* lineSpaces)],
        [InfixL $ tmp2 (CAnd <$ string "&&" <* lineSpaces)],
        [InfixL $ tmp2 (COr  <$ string "||" <* lineSpaces)]
    ]
    where
        tmp1 pg = do
            g <- withPosP pg
            let g' a = Pos (pPos g) $ pVal g a
            return g'
        tmp2 pg = do
            g <- withPosP pg
            let g' a b = Pos (pPos g) $ pVal g a b
            return g'

evalCond :: VarValues -> CondPos -> Except EvalError Bool
evalCond vals pe = case pVal pe of
    Cond e rel e' -> do
        n   <- evA e
        n'  <- evA e'
        let rel' = case rel of
                        CLess       -> (<)
                        CLessEq     -> (<=)
                        CGreater    -> (>)
                        CGreaterEq  -> (>=)
                        CEq         -> (==)
                        CNeq        -> (/=)
        return $ n `rel'` n'
    CAnd c c' -> (&&) <$> evC c <*> evC c'
    COr  c c' -> (||) <$> evC c <*> evC c'
    CNot c    -> not  <$> evC c
    where 
        evC = evalCond vals
        evA = evalAriphm vals

evalTest :: VarValues -> String -> IO ()
evalTest vals str = do
    case parseAll condP str of
        Left  e -> print e
        Right c -> do
            print $ evalCond vals c
