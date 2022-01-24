module TestAlignAriphm (
    AlignAriphmOp (..), AlignAriphmExprPos (..), AlignAriphmExpr' (..), ariphmExprP, ariphmTermP
) where

import Control.Applicative ((<|>))
import Text.Parsec
    ( char, between, digit, letter, many1, Parsec, (<?>))
import Text.Parsec.Char ( spaces )
import Control.Monad.Combinators.Expr
    ( makeExprParser, Operator(InfixL, InfixR) )
import Text.Parsec.Token (GenTokenParser(parens, integer))
import Utils (lineSpaces, Pos (pVal, Pos, pPos), withPosP, Wrap, Box (unBox), ($$))

data AlignAriphmOp
    = ASum | ADiff | ADiv | AProd | APow
    deriving Eq

data AlignAriphmExpr' p
    = AVar     String
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

instance Box p => Show (AlignAriphmExpr' p) where
    show (AVar str) = str
    show (AConst n) = show n
    show (ABinOp op e e') = "(" ++ show $$ e ++ " " ++ show op ++ " " ++ show $$ e' ++ ")"

type Parser t = (Parsec String () t)

ariphmExprP :: Parser AlignAriphmExprPos
ariphmExprP = makeExprParser (ariphmTermP <* lineSpaces) operators <* lineSpaces
        <?> "expression"

ariphmTermP :: Parser AlignAriphmExprPos
ariphmTermP = between (char '(') (char ')') ariphmExprP
    <|> withPosP (AConst . read <$> many1 digit)
    <|> withPosP (AVar <$> many1 letter)
    <?> "term"

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