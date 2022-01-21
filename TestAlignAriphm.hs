module TestAlignAriphm (
    AlignAriphmOp (..), AlignAriphmExpr (..), AlignAriphmExpr' (..), ariphmExprP, ariphmTermP
) where

import Control.Applicative ((<|>))
import Text.Parsec
    ( char, between, digit, letter, many1, Parsec, (<?>))
import Text.Parsec.Char ( spaces )
import Control.Monad.Combinators.Expr
    ( makeExprParser, Operator(InfixL, InfixR) ) 
import Text.Parsec.Token (GenTokenParser(parens, integer))
import Utils (lineSpaces, Pos, withPosP)

data AlignAriphmOp
    = ASum | ADiff | ADiv | AProd | APow
    deriving Eq

data AlignAriphmExpr'
    = AVar     String
    | AConst   Int
    | ABinOp   AlignAriphmOp AlignAriphmExpr' AlignAriphmExpr'
    deriving Eq

type AlignAriphmExpr = Pos AlignAriphmExpr'

instance Show AlignAriphmOp where
    show ASum  = "+"
    show ADiff = "-"
    show AProd = "*"
    show ADiv  = "/"
    show APow  = "^"

instance Show AlignAriphmExpr' where
    show (AVar str) = str
    show (AConst n) = show n
    show (ABinOp op e e') = "(" ++ show e ++ " " ++ show op ++ " " ++ show e' ++ ")"

type Parser t = (Parsec String () t)

ariphmExprP' :: Parser AlignAriphmExpr'
ariphmExprP' = makeExprParser (ariphmTermP' <* lineSpaces) operators <* lineSpaces 
    <?> "expression"

ariphmExprP :: Parser AlignAriphmExpr
ariphmExprP = withPosP ariphmExprP'

ariphmTermP' :: Parser AlignAriphmExpr'
ariphmTermP' = between (char '(') (char ')') ariphmExprP' 
    <|> AConst . read <$> many1 digit 
    <|> AVar <$> many1 letter 
    <?> "term"

ariphmTermP :: Parser AlignAriphmExpr
ariphmTermP = withPosP ariphmTermP'

operators :: [[Operator (Parsec String ()) AlignAriphmExpr']]
operators = [
        [ InfixR (ABinOp APow <$ char '^' <* lineSpaces)
        ],
        [ InfixL (ABinOp AProd <$ char '*' <* lineSpaces)
        , InfixL (ABinOp ADiv  <$ char '/' <* lineSpaces)
        ],
        [ InfixL (ABinOp ASum  <$ char '+' <* lineSpaces)
        , InfixL (ABinOp ADiff <$ char '-' <* lineSpaces)
        ]
    ]
