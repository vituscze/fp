-- | This module provides lambda expression parsers.
module Parse
    ( expr
    , parse
    , parseUnsafe
    ) where

import Data.Functor
import Text.Parsec qualified as Parsec
import Text.Parsec hiding (parse)
import Text.Parsec.String

import Expr

-- | Utility for lexical token parsers.
--
-- Runs the given parser and then skips following whitespace, if any.
lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

-- | Parses a single symbol.
symbol :: String -> Parser ()
symbol s = lexeme $ string s $> ()

-- | Parses a specified value enclosed in parentheses.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parses an identifier.
--
-- Identifier is a sequence of lower-case letters.
identifier :: Parser Name
identifier = lexeme $ many1 lower

-- | Parses a variable.
--
-- See 'identifier'.
variable :: Parser Expr
variable = Var <$> identifier

-- | Parses a lambda abstraction.
--
-- The abstraction can also be telescoped.
abstraction :: Parser Expr
abstraction = flip (foldr (:->)) <$> (symbol "\\" *> many1 identifier <* symbol ".") <*> expr

-- | Parses a lambda expression.
expr :: Parser Expr
expr = foldl1 (:.) <$> many1 (parens expr <|> variable <|> abstraction)

-- | Attempts to read a string as a lambda expression.
--
-- > parse "\\x y. x" == Just ("x" :-> "y" :-> Var "x")
parse :: String -> Maybe Expr
parse s = case Parsec.parse (spaces *> expr <* eof) "" s of
    Right e -> Just e
    _       -> Nothing

-- | Same as 'parse' except that the caller must ensure the
-- string is a valid lambda expression.
parseUnsafe :: String -> Expr
parseUnsafe s = case parse s of
    Just e  -> e
    Nothing -> error "parseUnsafe: no parse"
