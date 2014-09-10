module Golf where

pick_n x n = do_pick x n
    where
        do_pick [] _      = []
        do_pick (x:xs) i
            | i == 1        = x : do_pick xs n
            | otherwise     = do_pick xs (i-1)

skips :: [a] -> [[a]]
skips x = do_skips 1
    where
        do_skips n
            | n > length x = []
            | otherwise    = pick_n x n : do_skips (n+1)

localMaxima :: [Integer] -> [Integer]
localMaxima (x1:x2:x3:xs)
    | d1 > 0 && d2 > 0    = x2 : localMaxima (x2:x3:xs)
    | otherwise           = localMaxima (x2:x3:xs)
    where
        d1 = x2 - x1
        d2 = x2 - x3
localMaxima _             = []

count_9 x = [(count 0), (count 1), (count 2), (count 3), (count 4), (count 5), (count 6), (count 7), (count 8), (count 9)]
    where
        count n = length (filter (\x' -> x' == n) x)

histo_n x n = do_histo_n (count_9 x)
    where
        do_histo_n []     = "\n"
        do_histo_n (c:cs)
            | c >= n      = '*' : do_histo_n cs
            | otherwise   = ' ' : do_histo_n cs

histogram :: [Integer] -> String
histogram x = histo_all (maximum counts) ++ "==========\n0123456789\n"
    where
        counts     = count_9 x
        histo_all n
            | n == 0    = ""
            | otherwise = histo_n x n ++ histo_all (n-1)
