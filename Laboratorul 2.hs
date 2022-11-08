eeny :: Integer -> String
eeny x = if even x 
            then "eeny"
        else "meeny"

fizzbuzz :: Integer -> String
fizzbuzz x = if (mod x 3 == 0)
                then if (mod x 5 == 0)
                        then "FizzBuzz"
                     else "Fizz"
            else if (mod x 5 == 0)
                    then "Buzz"
                 else "" 

fizzbuzz2 :: Integer -> String
fizzbuzz2 x 
    | mod x 15 ==0   = "FizzBuzz"
    | mod x 3  ==0   = "Fizz"
    | mod x 5  ==0   = "Buzz"
    | otherwise      = ""


tribonacciCaz :: Integer -> Integer
tribonacciCaz n
    | n < 3     = 1
    | n == 3    = 2
    | otherwise = tribonacciCaz (n-1) + tribonacciCaz (n-2) + tribonacciCaz (n-3)

tribonacciEc :: Integer -> Integer
tribonacciEc 1 = 1
tribonacciEc 2 = 1
tribonacciEc 3 = 2
tribonacciEc n = tribonacciEc (n-1) + tribonacciEc (n-2) + tribonacciEc (n-3)


binomial :: Integer -> Integer -> Integer
binomial n 0 = 1
binomial 0 k = 0
binomial n k = binomial (n-1) k + binomial (n-1) (k-1)

verifL :: [Int] -> Bool
verifL a = even (length a)

takefinal :: [a] -> Int -> [a]
takefinal a n 
    | n > length a = a
    | otherwise    = drop (length a - n) a 

remove :: [a] -> Int -> [a]
remove a n
    | n > length a = a
    | otherwise    = take (n-1) a ++ drop n a

myreplicate :: Int -> a -> [a]
myreplicate n v
    | n == 0 = []
    | otherwise = (v:t)
    where t = myreplicate (n-1) v

sumImp :: [Int] -> Int
sumImp [] = 0
sumImp (h:t) 
    | t == [] && odd h = h
    | t == [] && even h = 0
    | even h = sumImp t
    | otherwise = h + sumImp t

totalLen :: [String] -> Int
totalLen [] = 0
totalLen (h:t)
    | t == [] && h1 == 'A' = length h
    | t == [] && h1 /= 'A' = 0
    | h1 == 'A' = length h + totalLen t
    | otherwise = totalLen t
    where (h1:t1) = h