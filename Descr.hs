module Descr where

import Layout
import Conditions
import Utils
import Control.Applicative (Alternative(many, (<|>)))
import Text.ParserCombinators.Parsec.Char (string, newline, satisfy, spaces)
import Text.ParserCombinators.Parsec (eof, sepBy, sepEndBy, endBy)
import Text.Parsec ( (<?>) ) 
import Data.Char (isSpace)

data Descr p = Descr {
        dLayout :: [Wrap p LayoutDescr],
        dConds  :: [Wrap p Cond]
    }

instance Box p => Show (Descr p) where
    show (Descr playout pconds) 
                    =  unlines ((show $$) <$> playout)
                    ++ "===\n"
                    ++ unlines ((show $$) <$> pconds)
type DescrPos = Wrap Pos Descr

descrP :: Parser DescrPos
descrP = withPosP (Descr
        <$> layoutP
        <*  string "===" <* newline  
        <*> condP `endBy` (newline <* lineSpaces)
        <*  eof
    ) <?> "Test description"
    