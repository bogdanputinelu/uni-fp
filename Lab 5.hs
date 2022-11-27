-- Lab 5 

-- 1)
sumaPatrate :: [Int] -> Int
sumaPatrate l = foldr (+) 0 (map (\x -> x*x) (filter odd l))

-- 2)

elemTrue :: [Bool] -> Bool
elemTrue l = foldr (&&) True l

-- 3)

allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies f l = elemTrue (map f l)

-- 4)

anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies f l = foldr (||) False (map f l)

-- 5)

mapFoldr :: (a->b) -> [a] -> [b]
mapFoldr _ [] = []
mapFoldr f l = foldr (\x r -> (f x):r) [] l

filterFoldr :: (a->Bool) -> [a] -> [a]
filterFoldr _ [] = []
filterFoldr f l = foldr (\x r -> if f x then x:r else r) [] l

-- 6)

listToInt :: [Integer] -> Integer
listToInt l = foldl (\x r -> 10*x+r) 0  l

-- 7)

-- a)

rmChar :: Char -> String -> String
rmChar c s = filter (\x -> x /= c) s

-- b)

rmCharsRec :: String -> String -> String
rmCharsRec _ [] = []
rmCharsRec c (h:t)
    | h `elem` c = rmCharsRec c t
    | otherwise = h : rmCharsRec c t

-- c)

rmCharsFold :: String -> String -> String
rmCharsFold c s = foldr (\x r -> if x `elem` c then  r else x : r) "" s 
