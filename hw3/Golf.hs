module Golf where

pick_n x n = reverse $ do_pick x n []
    where
        do_pick [] _ r      = r
        do_pick (x:xs) i r
            | i == 1        = do_pick xs n (x:r)
            | otherwise     = do_pick xs (i-1) r

skips :: [a] -> [[a]]
skips x = reverse $ do_skips (length x) []
    where
        do_skips n r
            | n == 0    = r
            | otherwise = pick_n x n : do_skips (n-1) r

localMaxima :: [Integer] -> [Integer]
localMaxima []            = []
localMaxima [x]           = []
localMaxima [x1,x2]       = []
localMaxima (x1:x2:x3:xs)
    | d1 > 0 && d2 > 0    = x2 : localMaxima (x2:x3:xs)
    | otherwise           = localMaxima (x2:x3:xs)
    where
        d1 = x2 - x1
        d2 = x2 - x3

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
