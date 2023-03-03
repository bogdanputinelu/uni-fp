-- lab 12

import Data.Monoid

elem1 :: (Foldable t, Eq a) => a -> t a -> Bool
elem1 x l = foldr (\y r -> x==y || r) False l

elem1' :: (Foldable t, Eq a) => a -> t a -> Bool
elem1' x l = getAny $ foldMap (\y -> Any (y==x)) l

null1 :: (Foldable t) => t a -> Bool
null1 l = foldr (\x r -> 1) 0 l == 0

null1' :: (Foldable t) => t a -> Bool
null1' l = (getSum $ foldMap (\x -> Sum 1) l) == 0

length1 :: (Foldable t) => t a -> Int
length1 l = foldr (\x r -> r + 1) 0 l 

length1' :: (Foldable t) => t a -> Int
length1' l = getSum $ foldMap (\x -> Sum 1) l

toList1 :: (Foldable t) => t a -> [a]
toList1 l = foldMap (:[]) l

fold1 :: (Foldable t, Monoid m) => t m -> m
fold1 = foldMap id

data Constant a b = Constant b
data Two a b = Two a b
data Three a b c = Three a b c
data Three' a b = Three' a b b
data Four' a b = Four' a b b b
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Foldable (Constant a) where
    foldMap f (Constant b) = f b

instance Foldable (Two a) where
    foldMap f (Two a b) = f b

instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c

instance Foldable (Three' a) where
    foldMap f (Three' a b1 b2) = f b1 <> f b2

instance Foldable (Four' a) where
    foldMap f (Four' a b1 b2 b3) = f b1 <> f b2 <> f b3

instance Foldable GoatLord where
    foldMap f (NoGoat) = mempty
    foldMap f (OneGoat a) = f a
    foldMap f (MoreGoats a b c) = foldMap f a <> foldMap f b <> foldMap f c