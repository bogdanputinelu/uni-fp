-- Lab 7
data Expr = Const Int -- integer constant
    | Expr :+: Expr -- addition
    | Expr :*: Expr -- multiplication
    deriving Eq

data Operation = Add | Mult deriving (Eq, Show)

data Tree = Lf Int -- leaf
    | Node Operation Tree Tree -- branch
    deriving (Eq, Show)


class Collection c where
    empty :: c key value
    singleton :: key -> value -> c key value
    insert
        :: Ord key
        => key -> value -> c key value -> c key value
    clookup :: Ord key => key -> c key value -> Maybe value
    
    delete :: Ord key => key -> c key value -> c key value -- 2.1)
    keys :: c key value -> [key]
    keys x = [key | (key,_) <- toList x]
    values :: c key value -> [value]
    values x = [value | (_,value) <- toList x]
    toList :: c key value -> [(key, value)]
    fromList :: Ord key => [(key,value)] -> c key value
    fromList [] = empty 
    fromList (h:t) = insert (fst h) (snd h) (fromList t)



-- 1.1)

instance Show Expr where
    show (Const x) = "Const " ++ show x
    show (e1 :+: e2) = show e1 ++ " :+: " ++ show e2
    show (e1 :*: e2) = show e1 ++ " :*: " ++ show e2

-- 1.2)

evalExp :: Expr -> Int
evalExp (Const x) = x
evalExp (x :+: y) = evalExp x + evalExp y
evalExp (x :*: y) = evalExp x * evalExp y

-- 1.3)

evalArb :: Tree -> Int
evalArb (Lf x) = x
evalArb (Node Add x y) = evalArb x + evalArb y
evalArb (Node Mult x y) = evalArb x * evalArb y

-- 1.4)

expToArb :: Expr -> Tree
expToArb (Const x) = (Lf x)
expToArb (x :+: y) = (Node Add (expToArb x) (expToArb y))
expToArb (x :*: y) = (Node Mult (expToArb x) (expToArb y))

-- 2.2)
newtype PairList k v
    = PairList { getPairList :: [(k, v)] }

instance Collection PairList where
    empty = PairList []
    singleton k v = PairList [(k,v)]
    insert k v l = PairList $ (k,v) :(getPairList(delete k l))
    clookup k l = if length [value | (key,value)<- toList l] == 0 then Nothing 
                    else Just ([value | (key,value)<- toList l] !! 0)
    delete k l = fromList [(key,value) | (key,value) <- toList l, key /= k]
    toList = getPairList

-- 2.3)
data SearchTree key value
    = Empty
    | BNode
    (SearchTree key value) -- elemente cu cheia mai mica
    key -- cheia elementului
    (Maybe value) -- valoarea elementului
    (SearchTree key value) -- elemente cu cheia mai mare

instance Collection SearchTree where
    empty = Empty
    singleton k v = BNode Empty k (Just v) Empty
    insert k v (BNode l key value r) 
        | k == key = (BNode l key (Just v) r)
        | k > key = insert k v r
        | otherwise = insert k v l
    clookup key Empty = Nothing
    clookup key (BNode l k value r)
        | key == k = value
        | key > k = clookup key l
        | otherwise = clookup key r
    delete key (BNode l k value r)
        | key == k = (BNode l k Nothing r)
        | key > k = delete key r
        | otherwise = delete key l
    toList Empty = []
    toList (BNode l key Nothing r) = toList l ++ toList r
    toList (BNode l key (Just value) r) = toList l ++ [(key,value)] ++ toList r