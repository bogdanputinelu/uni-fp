import Data.Maybe
import Data.List

type Nume = String
data Prop
    = Var Nume
    | F
    | T
    | Not Prop
    | Prop :|: Prop
    | Prop :&: Prop
    | Prop :->: Prop
    | Prop :<->: Prop
    deriving Eq
infixr 2 :|:
infixr 3 :&:


-- ex 1

-- 1. (P ∨ Q) ∧ (P ∧ Q)

p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

-- 2. (P ∨ Q) ∧ (¬P ∧ ¬Q)

p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not (Var "Q"))
-- 3. (P ∧ (Q ∨ R)) ∧ ((¬P ∨ ¬Q) ∧ (¬P ∨ ¬R))

p3 :: Prop
p3 = (Var "P" :&: (Var "Q" :|: Var "R")) :&: ((Not (Var "P") :|: Not (Var "Q")) :&: (Not (Var "P") :&: Not (Var "R")))


-- ex 2

instance Show Prop where
    show (Var x) = x
    show F = "False"
    show T = "True"
    show (Not x) = "(~" ++ show x ++ ")"
    show (x :|: y) = "(" ++ show x ++ "|" ++ show y ++ ")"
    show (x :&: y) = "(" ++ show x ++ "&" ++ show y ++ ")"
    show (x :->: y) = "(" ++ show x ++ "->" ++ show y ++ ")"
    show (x :<->: y) = "(" ++ show x ++ "<->" ++ show y ++ ")"


-- ex 3

type Env = [(Nume, Bool)]

impureLookup :: Eq a => a -> [(a,b)] -> b
impureLookup a = fromJust . lookup a

eval :: Prop -> Env -> Bool
eval T _ = True
eval F _ = False
eval (Var x) l = impureLookup x l
eval (Not x) l = not (eval x l)
eval (x :|: y) l = (eval x l) || (eval y l)
eval (x :&: y) l = (eval x l) && (eval y l)
eval (x :->: y) l = not (eval x l) || (eval y l)
eval (x :<->: y) l = eval (x :->: y) l && eval (y :->: x) l

-- ex 4

variabile :: Prop -> [Nume]
variabile F = []
variabile T = []
variabile (Var x) = [x]
variabile (Not x) = variabile x
variabile (x :|: y) = nub (variabile x ++ variabile y)
variabile (x :&: y) = nub (variabile x ++ variabile y)
variabile (x :->: y) = nub (variabile x ++ variabile y)
variabile (x :<->: y) = nub (variabile x ++ variabile y)

-- ex 5

envs :: [Nume] -> [Env]
envs n = [zip n val | val <- sequence (take (length n) (repeat [False, True]))]

-- ex 6

satisfiabila :: Prop -> Bool
satisfiabila p = or [eval p l | l <- envs (variabile p)]

-- ex 7

valida :: Prop -> Bool
valida p = and [eval p l | l <- envs (variabile p)]

-- ex 10

echivalenta :: Prop -> Prop -> Bool
echivalenta p1 p2 = valida (p1 :<->: p2) 