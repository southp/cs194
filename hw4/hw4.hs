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


-- toNode :: a -> Tree a
-- toNode x = Node 0 Leaf x Leaf
--
-- merge :: Tree a -> Tree a -> Tree a
-- merge t Leaf = t
-- merge Leaf t = t
-- merge tl@(Node hl tll al tlr) tr@(Node hr trl ar trr)
--     | hl == hr = Node (hl + 1) (merge tll tr) al tlr
--     | hl <  hr = Node hr (merge tl trl) ar trr
--
-- foldTree :: [a] -> Tree a
-- foldTree x = foldr merge Leaf (map toNode x)

-- Exercise 3
xor :: [Bool] -> Bool
xor = foldr xor' False
    where
        xor' a b | a == b    = False
                 | otherwise = True

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f z xs = foldr (\x y -> f y x) z (reverse xs)

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

