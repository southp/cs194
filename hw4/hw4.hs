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
fun1' x = product (map (subtract 2) (filter even x))

fun2' :: Integer -> Integer
fun2' n = sum $ filter even (takeWhile (/= 1) (iterate (\x -> if even x then x `div` 2 else 3*x + 1) n))

-- Exercise 2.
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

getHeight :: Tree a -> Integer
getHeight Leaf           = -1
getHeight (Node h _ _ _) = h

baInsert :: a -> Tree a -> Tree a
baInsert x Leaf = Node 0 Leaf x Leaf
baInsert x (Node h tl a tr)
    | hl <  hr = Node h        tn a tr
    | hl >  hr = Node h        tl a (baInsert x tr)
    | hr == hl = Node (hn + 1) tn a tr
    where hl = getHeight tl
          hr = getHeight tr
          tn = baInsert x tl
          hn = getHeight tn    -- thanks for stackoverflow.
                               -- I didn't come up this hn when I wrote this and struggled

foldTree :: [a] -> Tree a
foldTree = foldr baInsert Leaf

-- Exercise 3
xor :: [Bool] -> Bool
xor = foldr xor' False
    where
        xor' a b | a == b    = False
                 | otherwise = True

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f z xs = foldr (flip f) z (reverse xs)

-- from Haskell wiki. Much more cleaner
foldl2 :: (a -> b -> a) -> a -> [b] -> a
foldl2 f a bs =
   foldr (\b g x -> g (f x b)) id bs a

-- Exercise 4
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

sievePairs :: Integer -> [(Integer, Integer)]
sievePairs n = filter (\(i, j) -> i + j + 2*i*j <= n) (filter (uncurry (<=)) (cartProd [1..n] [1..n]))

sieveNum :: Integer -> [Integer]
sieveNum n = map (\(i, j) -> i + j + 2*i*j) (sievePairs n)

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1) $ foldr (\x y -> filter (/= x) y) [1..n] (sieveNum n)

