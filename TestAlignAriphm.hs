module TestAlignAriphm (
    AlignAriphmOp (..), Var (..), AlignAriphmExprPos (..), AlignAriphmExpr' (..),
    ariphmExprP, ariphmTermP, varP,
    VarName (..), Indexes (..), Tmp (..), EvalAriphmError (..),
    eval
) where

import Control.Applicative ((<|>), Alternative (many))
import Text.Parsec
    ( char, between, digit, letter, many1, Parsec, (<?>))
import Text.Parsec.Char ( spaces )
import Control.Monad.Combinators.Expr
    ( makeExprParser, Operator(InfixL, InfixR) )
import Text.Parsec.Token (GenTokenParser(parens, integer))
import Utils (lineSpaces, Pos (pVal, Pos, pPos), withPosP, Wrap, Box (unBox), ($$), betweenCh, maybeToExcept, numberP)
import Data.Function (on)
import Control.Monad.Trans.Except (Except)

data AlignAriphmOp
    = ASum | ADiff | ADiv | AProd | APow
    deriving Eq

data Var p = Var String [Wrap p AlignAriphmExpr']

instance (Box p) => Eq (Var p) where
    (Var s1 ixs1) == (Var s2 ixs2) = s1 == s2 && and (zipWith ((==) `on` unBox) ixs1 ixs2)

data AlignAriphmExpr' p
    = AVar     (Var p)
    | AConst   Int
    | ABinOp   AlignAriphmOp (Wrap p AlignAriphmExpr') (Wrap p AlignAriphmExpr')

instance (Box p) => Eq (AlignAriphmExpr' p) where
    (AVar v1) == (AVar v2) = v1 == v2
    (AConst c1) == (AConst c2) = c1 == c2
    (ABinOp op1 e1 e1') == (ABinOp op2 e2 e2') = op1 == op2 && unBox e1 == unBox e2 && unBox e1' == unBox e2'
    _ == _ = False

type AlignAriphmExprPos = Wrap Pos AlignAriphmExpr'

instance Show AlignAriphmOp where
    show ASum  = "+"
    show ADiff = "-"
    show AProd = "*"
    show ADiv  = "/"
    show APow  = "^"

instance Box p => Show (Var p) where
    show (Var s idxs) = s ++ concatMap h idxs
        where h = \i -> "[" ++ show $$ i ++ "]"

instance Box p => Show (AlignAriphmExpr' p) where
    show (AVar v)           = show v
    show (AConst n)         = show n
    show (ABinOp op e e')   = "(" ++ show $$ e ++ " " ++ show op ++ " " ++ show $$ e' ++ ")"

type Parser t = (Parsec String () t)

ariphmExprP :: Parser AlignAriphmExprPos
ariphmExprP = makeExprParser (ariphmTermP <* lineSpaces) operators <* lineSpaces
        <?> "expression"

ariphmTermP :: Parser AlignAriphmExprPos
ariphmTermP = between (char '(') (char ')') ariphmExprP
    <|> withPosP (AConst <$> numberP)
    <|> withPosP (AVar <$> varP)
    <?> "term"

varP :: Parser (Var Pos)
varP = Var <$> many1 letter <*> many indexes
    where indexes = betweenCh '[' ']' (lineSpaces *> ariphmExprP)

operators :: [[Operator (Parsec String ()) AlignAriphmExprPos]]
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


tmp :: Parser (AlignAriphmExprPos -> AlignAriphmExprPos -> AlignAriphmExpr' Pos)
     -> Parser (AlignAriphmExprPos -> AlignAriphmExprPos -> AlignAriphmExprPos)
tmp pg = do
    g <- withPosP pg
    let g' a b = Pos (pPos g) $ pVal g a b
    return g'

type VarName = String
type Indexes = [Int]

type Tmp = [((VarName, Indexes), Int)]

data EvalAriphmError = EAE_UninitializedVar VarName Indexes
    deriving Show

eval :: Tmp -> AlignAriphmExprPos -> Except EvalAriphmError Int
eval tmp pe = case pVal pe of
    AVar (Var s idxs) -> do
        idxs' <- mapM (eval tmp) idxs
        maybeToExcept (EAE_UninitializedVar s idxs') $ lookup (s, idxs') tmp
    AConst n ->
        return n
    ABinOp op pe1 pe2 -> do
        n1 <- eval tmp pe1
        n2 <- eval tmp pe2
        let op' = case op of
                        ASum -> (+)
                        ADiff -> (-)
                        ADiv -> div
                        AProd -> (*)
                        APow -> (^)
        return $ n1 `op'` n2
