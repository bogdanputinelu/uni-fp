-- Lab 8
data Punct = Pt [Int]

data Arb = Vid | F Int | N Arb Arb
    deriving Show

class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a
    

-- 1)

-- a)

instance Show Punct where
    show (Pt []) = "()"
    show (Pt (x:l)) = "(" ++ show x ++ foldr(\s r -> "," ++ show s ++ r) "" l ++ ")"

-- b)

punctToList :: Punct -> [Int]
punctToList (Pt x) = x

instance ToFromArb Punct where
    toArb (Pt []) = Vid
    toArb (Pt [x]) = F x
    toArb (Pt (h:t)) = (N (F h) (toArb (Pt t)))

    fromArb Vid = Pt []
    fromArb (F x) = Pt [x]
    fromArb (N a b) = Pt (punctToList (fromArb a) ++ punctToList (fromArb b))

-- 2)
data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

class GeoOps g where
    perimeter :: (Floating a) => g a -> a
    area :: (Floating a) => g a -> a

-- a)

instance GeoOps Geo  where
    perimeter (Square a) = 4*a
    perimeter (Rectangle a b) = 2*a + 2*b
    perimeter (Circle a) = 2*pi*a

    area (Square a) = a*a
    area (Rectangle a b) = a*b
    area (Circle a) = pi*a*a


-- b)

instance (Eq a, Floating a) => Eq (Geo a) where
    x == y = perimeter x == perimeter y