-- Add your names and student numbers to the following lines. Do not change anything else on these lines, since they are parsed.
-- Student 1: Ivan Mandev (s2989190)
-- Student 2: Ivan Gyunderov (s3138585)
-- Student 3: Darius Luca (s3131777)


module BasicParsers where

import Control.Applicative
import Data.Char
import Test.QuickCheck
import PComb

--FP2.1 by Ivan Mandev
--Parses any single alphabetic letter
letter :: Parser Char
letter = Parser $ \inp -> 
    case inp of
        Stream (c:cs) | isAlpha c -> [(c, Stream cs)]
        _ -> []

--FP2.1 by Ivan Mandev
--Parses any single digit character
dig :: Parser Char
dig = Parser $ \inp -> 
    case inp of
        Stream (c:cs) | isDigit c -> [(c, Stream cs)]
        _ -> []


--FP2.2 by Ivan Mandev
--Parses a single whitespace character (space, tab, newline)
space :: Parser Char
space = Parser $ \inp ->
    case inp of
        Stream (c:cs) | isSpace c -> [(c, Stream cs)]
        _ -> []

--FP2.2 by Ivan Mandev
--Parser that consumes zero or more whitespace characters
skipSpaces :: Parser [Char]
skipSpaces = many space

--FP2.2 by Ivan Mandev
--Runs the parsers in sequence
between :: Parser a -> Parser b -> Parser c -> Parser b
between p1 p2 p3 = p1 *> p2 <* p3

--FP2.2 by Ivan Mandev
-- Wraps parser p by skipping whitespace before and after
whitespace :: Parser a -> Parser a
whitespace p = skipSpaces *> p <* skipSpaces

--FP2.3 by Ivan Mandev
--sep1 p s parses one or more occurrences of p, separated by s
sep1 :: Parser a -> Parser b -> Parser [a]
sep1 p s = (:) <$> p <*> many (s *> p)

--FP2.3 by Ivan Mandev
-- sep p s parses zero or more occurrences of p, separated by s
sep :: Parser a -> Parser b -> Parser [a]
sep p s = sep1 p s <|> pure []

--FP2.3 by Ivan Mandev
-- option x p tries to run p; if p fails, returns x without consuming input
option :: a -> Parser a -> Parser a
option x p = p <|> pure x

--FP2.4 by Ivan Mandev
string :: String -> Parser String
string [] = pure []
string (c:cs) = (:) <$> char c <*> fmap (c:) (string cs)

--FP2.4 by Ivan Mandev
-- Parses an identifier surrounded by optional whitespace
identifier :: Parser String
identifier =
    whitespace ((:) <$> letter <*> many (letter <|> dig))

--FP2.4 by Ivan Mandev
integerBare :: Parser Integer
integerBare =
  fmap read $ (++) <$> option "" (string "-") <*> some dig 

--FP2.4 by Ivan Mandev
integer :: Parser Integer
integer = whitespace integerBare

--FP2.4 by Ivan Mandev
symbol :: String -> Parser String
symbol lit = whitespace (string lit)

--FP2.4 by Ivan Mandev
parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"

--FP2.4 by Ivan Mandev
braces :: Parser a -> Parser a
braces p = symbol "{" *> p <* symbol "}"