-- Exercise 1.

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' x = foldr (*) 1 (map (subtract 2) (filter even x))

fun2' :: Integer -> Integer
fun2' n = sum $ filter even (takeWhile (/= 1) (iterate (\x -> if even x then x `div` 2 else 3*x + 1) n))

-- Exercise 2.
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)


-- Exercise 3
xor :: [Bool] -> Bool
xor xs = foldr (xor') False xs
    where
        xor' a b | a == b    = False
                 | otherwise = True

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

-- Exercise 4
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

sievePairs :: Integer -> [(Integer, Integer)]
sievePairs n = filter (\(i, j) -> i + j + 2*i*j <= n) (filter (uncurry (<=)) (cartProd [1..n] [1..n]))

sieveNum :: Integer -> [Integer]
sieveNum n = map (\(i, j) -> i + j + 2*i*j) (sievePairs n)

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (+1) . map (*2) $ foldr (\x y -> filter (/= x) y) [1..n] (sieveNum n)

