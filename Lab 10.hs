-- lab 10

newtype Identity a = Identity a
data Pair a = Pair a a
data Constant a b = Constant b
data Two a b = Two a b
data Three a b c = Three a b c
data Three' a b = Three' a b b
data Four a b c d = Four a b c d
data Four'' a b = Four'' a a a b
data Quant a b = Finance | Desk a | Bloor b

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Functor Pair where
    fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance Functor (Constant a)where
    fmap f (Constant b) = Constant (f b)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance Functor (Three' a) where
    fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance Functor (Four'' a) where
    fmap f (Four'' a1 a2 a3 b) = Four'' a1 a2 a3 (f b)

instance Functor (Quant a) where
    fmap f (Finance) = Finance
    fmap f (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)


data LiftItOut f a = LiftItOut (f a)
data Parappa f g a = DaWrappa (f a) (g a)
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
data Notorious g o a t = Notorious (g o) (g a) (g t)
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) = LiftItOut (fmap f fa) 

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

instance Functor g => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

instance Functor GoatLord where
    fmap f (NoGoat) = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats a1 a2 a3) = MoreGoats (fmap f a1) (fmap f a2) (fmap f a3)

instance Functor TalkToMe where
    fmap f (Halt) = Halt
    fmap f (Print s a) = (Print s (f a))
    fmap f (Read g) = Read (f . g)