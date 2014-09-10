module Golf where

pickN x n = do_pick x n
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
            | otherwise    = pickN x n : do_skips (n+1)

localMaxima :: [Integer] -> [Integer]
localMaxima (x1:x2:x3:xs)
    | d1 > 0 && d2 > 0    = x2 : localMaxima (x2:x3:xs)
    | otherwise           = localMaxima (x2:x3:xs)
    where
        d1 = x2 - x1
        d2 = x2 - x3
localMaxima _             = []

count9 x = [(count i) | i <- [0..9]]
    where
        count n = length (filter (== n) x)

histoN x n = do_histoN (count9 x)
    where
        do_histoN []     = "\n"
        do_histoN (c:cs)
            | c >= n      = '*' : do_histoN cs
            | otherwise   = ' ' : do_histoN cs

histogram :: [Integer] -> String
histogram x = histo_all (maximum counts) ++ "==========\n0123456789\n"
    where
        counts          = count9 x
        histo_all n
            | n == 0    = ""
            | otherwise = histoN x n ++ histo_all (n-1)
