import Data.List

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- fibs2 :: [Integer]
-- fibs2 =

data Stream a = Cons a (Stream a)
                -- deriving Show

streamToList :: Stream a -> [a]
streamToList (Cons s ss) = s : streamToList ss

instance Show a => Show (Stream a) where
    show s = show $ take 20 (streamToList s)

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

-- Functor?
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons s ss) = Cons (f s) (streamMap f ss)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

maxPowerOf2 :: Integer -> Integer
maxPowerOf2 0 = 0
maxPowerOf2 x = toInteger (length $ takeWhile (== 0) (mod2List x))

mod2List :: Integer -> [Integer]
mod2List x = map (x `mod`) (iterate (*2) 2)

ruler :: Stream Integer
ruler = streamMap maxPowerOf2 (streamFromSeed (+1) 1)

