-- lab 13

-- 1)
pos :: Int -> Bool
pos x = if (x>=0) then True else False

fct :: Maybe Int -> Maybe Bool
fct mx = mx >>= (\x -> Just (pos x))

fct' :: Maybe Int -> Maybe Bool
fct' mx = do
    x <- mx
    return (pos(x))


-- 2)

addM :: Maybe Int -> Maybe Int -> Maybe Int
addM Nothing _ = Nothing
addM _ Nothing = Nothing
addM (Just x) (Just y) = Just (x+y)

addM' :: Maybe Int -> Maybe Int -> Maybe Int
addM' mx my = mx >>= (\x -> my >>= (\y -> Just (x+y)))

addM'' :: Maybe Int -> Maybe Int -> Maybe Int
addM'' mx my = do
    x <- mx
    y <- my
    return (x+y)

-- 3)

cartesian_product xs ys = xs >>= ( \x -> (ys >>= \y-> return (x,y)))

cartesian_product' xs ys = do
    x <- xs
    y <- ys
    return (x,y)

prod f xs ys = [f x y | x <- xs, y<-ys]

prod' f xs ys = do
    x <- xs
    y <- ys
    return (f x y)

myGetLine :: IO String
myGetLine = getChar >>= \x ->
    if x == '\n' then
        return []
    else
        myGetLine >>= \xs -> return (x:xs)

myGetLine' :: IO String
myGetLine' = do
    x <- getChar
    if x == '\n' then
        return [] 
    else
        do
            xs <- myGetLine
            return (x:xs)

-- 4)

prelNo noin = sqrt noin
ioNumber = do
    noin <- readLn :: IO Float
    putStrLn $ "Intrare\n" ++ (show noin)
    let noout = prelNo noin
    putStrLn $ "Iesire"
    print noout

ioNumber' = readLn >>= (\noin -> (putStrLn $ "Intrare\n" ++ (show noin)) >> let noout = prelNo noin in (putStrLn $ "Iesire") >> print noout)

-- 5)
--- Monada Writer

-- newtype WriterS a = Writer { runWriter :: (a, String) } 


-- instance  Monad WriterS where
--   return va = Writer (va, "")
--   ma >>= k = let (va, log1) = runWriter ma
--                  (vb, log2) = runWriter (k va)
--              in  Writer (vb, log1 ++ log2)


-- instance  Applicative WriterS where
--   pure = return
--   mf <*> ma = do
--     f <- mf
--     a <- ma
--     return (f a)       

-- instance  Functor WriterS where              
--   fmap f ma = pure f <*> ma     

-- tell :: String -> WriterS () 
-- tell log = Writer ((), log)

-- logIncrement :: Int -> WriterS Int
-- logIncrement x = do 
--     tell ("increment: " ++ show x ++ "\n")
--     return (x+1)


-- 5.2)
newtype WriterS a = Writer { runWriter :: (a, [String]) } 

instance  Monad WriterS where
  return va = Writer (va, [])
  ma >>= k = let (va, log1) = runWriter ma
                 (vb, log2) = runWriter (k va)
             in  Writer (vb, log1 ++ log2)


instance  Applicative WriterS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterS where              
  fmap f ma = pure f <*> ma     


tell :: String -> WriterS () 
tell log = Writer ((), [log])


  
logIncrement :: Int -> WriterS Int
logIncrement x = do 
    tell ("increment: " ++ show x)
    return (x+1)

logIncrement2 :: Int -> WriterS Int
logIncrement2 x = do
    y <- logIncrement x
    logIncrement y


logIncrementN :: Int -> Int -> WriterS Int
logIncrementN x n = 
    if n > 1 then
        do
        y <- logIncrement x
        logIncrementN y (n-1)
    else
        logIncrement x



-- 6)

data Person = Person { name :: String, age :: Int }
showPersonN :: Person -> String
showPersonN (Person name _) = "NAME: " ++ name
showPersonA :: Person -> String
showPersonA (Person _ age) = "AGE: " ++ show age 
  

showPerson :: Person -> String
showPerson person = "(" ++ showPersonN person ++ "," ++ showPersonA person ++ ")"


-- 6.3)
newtype Reader env a = Reader { runReader :: env -> a }

ask :: Reader env env
ask = Reader id

instance Monad (Reader env) where
  return x = Reader (\_ -> x)
  ma >>= k = Reader f
    where f env = let a = runReader ma env
                  in  runReader (k a) env



instance Applicative (Reader env) where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance Functor (Reader env) where              
  fmap f ma = pure f <*> ma    

mshowPersonN ::  Reader Person String
mshowPersonN = do
    env <- ask
    return ("NAME: " ++ (name env))
mshowPersonA ::  Reader Person String
mshowPersonA = do
    env <- ask
    return ("AGE: " ++ show (age env))
mshowPerson ::  Reader Person String
mshowPerson = do
    env <- ask
    return ("(NAME: " ++ (name env) ++ ",AGE: " ++ show (age env) ++ ")" )