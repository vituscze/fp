module Parse
    ( parse
    , parseUnsafe
    ) where

import Data.Functor
import Text.Parsec qualified as Parsec
import Text.Parsec hiding (parse)
import Text.Parsec.String

import Expr

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

symbol :: String -> Parser ()
symbol s = lexeme $ string s $> ()

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser Name
identifier = lexeme $ many1 lower

variable :: Parser Expr
variable = Var <$> identifier

abstraction :: Parser Expr
abstraction = flip (foldr (:->)) <$> (symbol "\\" *> many1 identifier <* symbol ".") <*> expr

expr :: Parser Expr
expr = foldl1 (:.) <$> many1 (parens expr <|> variable <|> abstraction)

parse :: String -> Maybe Expr
parse s = case Parsec.parse (spaces *> expr <* eof) "" s of
    Right e -> Just e
    _       -> Nothing

parseUnsafe :: String -> Expr
parseUnsafe s = case parse s of
    Just e  -> e
    Nothing -> error "parseUnsafe: no parse"
