{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

import Data.List

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- courtesy of https://www.haskell.org/tutorial/patterns.html
-- I didn't come up with a viable solution myself :(
fibs2 :: [Integer]
fibs2@(0:tfib) = 0 : 1 : zipWith (+) fibs2 tfib

-- courtesy of Haskell wiki http://www.haskell.org/haskellwiki/The_Fibonacci_sequence
-- I didn't know unfoldr when I did this homework. Cite it here for remembering.
-- Using tuple here was just so brilliant. Map + iterate + tuple impl. was also creative.
fibs2' :: [Integer]
fibs2' = unfoldr (\(a,b) -> Just (a, (b, a+b))) (0,1)

data Stream a = Cons a (Stream a)

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

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

intMul :: Integer -> Stream Integer -> Stream Integer
a `intMul` (Cons b bs) = Cons (a*b) (a `intMul` bs)

instance Num (Stream Integer) where
    fromInteger n = Cons n (streamRepeat 0)

    (Cons a as) + (Cons b bs) = Cons (a + b) (as + bs)

    (Cons a as) * (Cons b bs) = (fromInteger (a*b)) + (b `intMul` as*x) + (a `intMul` bs*x) + (as*bs*x*x)

