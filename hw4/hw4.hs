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

-- fun2' :: Integer -> Integer

-- Exercise 2.
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

balanceInsert :: a -> Tree a -> Tree a
balanceInsert x Leaf = Node 0 Leaf x Leaf
balanceInsert x (Node h tl   t Leaf) = Node h tl                     t (balanceInsert x Leaf)
balanceInsert x (Node h Leaf t tr  ) = Node h (balanceInsert x Leaf) t tr
balanceInsert x (Node h tl@(Node hl _ _ _) t tr@(Node hr _ _ _))
                     | hl >= hr  = Node (h + 1) tl                   t (balanceInsert x tr)
                     | otherwise = Node (h + 1) (balanceInsert x tl) t tr

foldTree :: [a] -> Tree a
foldTree xs = foldr balanceInsert Leaf xs
