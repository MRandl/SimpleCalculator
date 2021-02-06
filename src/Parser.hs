module Parser ( inputParser, Token(..) ) where

import Control.Applicative ( Alternative((<|>)) )
import Text.ParserCombinators.ReadP ( ReadP, eof, manyTill, munch1, satisfy )
import Operation ( Operation, Bracket, opFromChar, brackFromChar )

data Token = Op Operation | Br Bracket | In Integer deriving (Eq, Show)

numberParser :: ReadP Token
numberParser = do
    i <- munch1 (`elem` "0123456789")
    return $ In $ read i

operatorParser :: ReadP Token
operatorParser = do
    i <- satisfy (`elem` "+-*/^")
    return $ Op $ opFromChar i

bracketParser :: ReadP Token
bracketParser = do
    i <- satisfy (`elem` "()")
    return $ Br $ brackFromChar i

inputParser :: ReadP [Token]
inputParser = manyTill (numberParser <|> operatorParser <|> bracketParser) eof
