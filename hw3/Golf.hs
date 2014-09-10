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

-- histogram :: [Integer] -> String
