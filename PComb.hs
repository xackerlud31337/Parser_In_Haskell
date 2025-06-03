-- Add your names and student numbers to the following lines. Do not change anything else on these lines, since they are parsed.
-- Student 1: Ivan Mandev (s2989190)
-- Student 2: Ivan Gyunderov (s3138585)
-- Student 3: Darius Luca (s3131777)


module PComb where
import Control.Applicative
import Data.Char
import Test.QuickCheck

-- Stream of Chars - can be extended with the location for error handling
data Stream = Stream [Char] deriving (Eq, Show)

--FP1.1 by <Ivan Mandev>
newtype Parser a = Parser { runParser :: Stream -> [(a,Stream)]}

-- FP1.2 by Ivan Mandev
instance Functor Parser where
  fmap f (Parser pa) = Parser $ \inp ->
    [ (f x, rest) | (x, rest) <- pa inp ]

--FP1.3 by Ivan Mandev
char :: Char -> Parser Char
char c = Parser $ \inp@(Stream xs) -> char' xs
    where
        char' [] = []
        char' (d:ds)
            | d == c = [(c, Stream ds)]
            | otherwise = []

--FP1.4 by Ivan Mandev
failure :: Parser a
failure = Parser $ const []

--FP1.5 by Ivan Mandev
instance Applicative Parser where
    pure x = Parser $ \inp -> [(x, inp)]
    (Parser pf) <*> (Parser pa) = Parser $ \inp -> [(f x, rest2) | (f, rest1) <- pf inp, (x, rest2) <- pa rest1]

--FP1.6 by Ivan Mandev
instance Alternative Parser where
    empty = failure
    (Parser p1) <|> (Parser p2) = Parser $ \inp -> 
        let r1 = p1 inp
        in if null r1 then p2 inp else r1