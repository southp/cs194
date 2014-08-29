lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

toInverseDigits :: Integer -> [Integer]
toInverseDigits n
    | n <= 0    = []
    | otherwise = lastDigit n : toInverseDigits(dropLastDigit n)

toDigits :: Integer -> [Integer]
toDigits = reverse . toInverseDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther dl = reverse(double_worker rl)
    where
        rl = reverse dl
        double_worker [] = []
        double_worker [x] = [x]
        double_worker (r1:r2:rs) = r1:2*r2:double_worker(rs)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (d:ds) = sum(toDigits d) + sumDigits ds

validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigits $ n) `mod` 10 == 0
