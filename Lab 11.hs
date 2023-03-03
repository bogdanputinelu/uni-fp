-- lab 11


-- ex 1

data List a = Nil
            | Cons a (List a)
    deriving (Eq, Show)


instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x l) = (Cons (f x)) (fmap f l)

concatenate :: List a -> List a -> List a
concatenate Nil l = l
concatenate l Nil = l
concatenate (Cons a l) l2 = Cons a (concatenate l l2)

instance Applicative List where
    pure x = Cons x Nil
    _ <*> Nil = Nil
    Nil <*> _ = Nil
    (Cons f ft) <*> (Cons a l) = (Cons (f a) (concatenate (fmap f l) (ft <*> Cons a l))) 


-- ex 2

data Cow = Cow {
        name :: String
        , age :: Int
        , weight :: Int
        } deriving (Eq, Show)


noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty s = Just s

noNegative :: Int -> Maybe Int
noNegative x
    | x > 0 = Just x
    | otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name age weight 
    | noEmpty name == Nothing || noNegative age == Nothing || noNegative weight == Nothing = Nothing
    | otherwise = Just (Cow {name=name, age=age, weight=weight})

cowFromString2 :: String -> Int -> Int -> Maybe Cow
cowFromString2 name age weight = Cow <$> noEmpty name <*> noNegative age <*> noNegative weight


-- ex 3 

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)
data Person = Person Name Address
    deriving (Eq, Show)

validateLength :: Int -> String -> Maybe String
validateLength x s 
    | length s < x = Just s
    | otherwise = Nothing

mkName :: String -> Maybe Name
mkName name 
    | validateLength 26 name == Nothing = Nothing
    | otherwise = Just (Name name)

mkAddress :: String -> Maybe Address
mkAddress address
    | validateLength 101 address == Nothing = Nothing
    | otherwise = Just (Address address)

mkPerson :: String -> String -> Maybe Person
mkPerson name address
    | mkName name == Nothing || mkAddress address == Nothing = Nothing
    | otherwise = Just (Person (Name name) (Address address))



mkName2 :: String -> Maybe Name
mkName2 name = Name <$> (validateLength 26 $ name)

mkAddress2 :: String -> Maybe Address
mkAddress2 address = Address <$> (validateLength 101 $ address)

mkPerson2 :: String -> String -> Maybe Person
mkPerson2 name address = Person <$> mkName2 name <*> mkAddress2 address