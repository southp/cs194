{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- exercise 1
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

maybeFirst :: (a -> b) -> Maybe (a, c) -> Maybe (b, c)
maybeFirst _ Nothing = Nothing
maybeFirst f (Just (a, c)) = Just (f a, c)

maybeSecond :: (b -> c) -> Maybe (a, b) -> Maybe (a, c)
maybeSecond _ Nothing = Nothing
maybeSecond f (Just (a, b)) = Just (a, f b)

instance Functor Parser where
    fmap f pa = Parser (maybeFirst f . runParser pa)

-- exercise 2
instance Applicative Parser where
    pure a = Parser f
        where f x = Just (a, x)
    pf <*> pa = Parser f
        where
            f x = case runParser pf x of
                    Nothing -> Nothing
                    Just(f, s) -> case runParser pa s of
                                    Nothing -> Nothing
                                    Just(a, ss) -> Just(f a, ss)
-- exercise 3
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\i _ j -> [i, j]) <$> posInt <*> char ' ' <*> posInt

-- exercise 4
instance Alternative Parser where
    empty = Parser (const Nothing)
    p1 <|> p2 = Parser f
        where
            f x = runParser p1 x <|> runParser p2 x

intOrUppercase :: Parser ()
intOrUppercase = (nil `fmap` posInt) <|> (nil `fmap` satisfy isUpper)
    where nil = const ()


----------------------------
-- Something for test
----------------------------
type Name = String
data Employee = Emp {
                    name :: Name,
                    phone :: String
                }
                deriving Show

parseName :: Parser Name
parseName = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (ns, rest)
      where (ns, rest) = span isLetter xs

parsePhone :: Parser String
parsePhone = show `fmap` posInt

