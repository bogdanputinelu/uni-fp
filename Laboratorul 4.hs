-- Lab 4

myzip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
myzip3 [] _ _ = []
myzip3 _ [] _ = []
myzip3 _ _ [] = []
myzip3 (x:xs) (y:ys) (z:zs) = (x,y,z) : myzip3 xs ys zs


-- 5)
firstEl :: [(a,b)] -> [a]
firstEl l =  map fst l


-- 6)
sumList :: [[Int]] -> [Int]
sumList l = map sum l

-- 7)
prel2 :: [Int] -> [Int]
prel2 l = map (\x -> if even x then x `div` 2 else x*2) l

-- 8)
contine :: Char -> [[Char]] ->[[Char]]
contine c s = filter (c `elem`) s

-- 9)
patrate :: [Int] -> [Int]
patrate l = filter odd (map (\x -> x*x) l)

-- 10)
patratePoz :: [Int] -> [Int]
patratePoz l = map (\x -> fst x) (filter (\(x,y) -> odd y) ((map (\x -> x*x) l) `zip` [0..]))

-- 11)
numaiVocale :: [[Char]] -> [[Char]]
numaiVocale l = map (\s -> filter (\c -> c `elem` "aeiouAEIOU") s) l


-- 12)

mymap :: (a->b) -> [a] -> [b]
mymap _ [] = []
mymap f (h:t) = f h : mymap f t

myfilter :: (a->Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter f (h:t)
    | f h = h : myfilter f t
    | otherwise = myfilter f t