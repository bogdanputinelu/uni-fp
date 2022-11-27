-- Lab 6

data Fruct
  = Mar String Bool
  | Portocala String Int

ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10
listaFructe = [Mar "Ionatan" False,
                Portocala "Sanguinello" 10,
                Portocala "Valencia" 22,
                Mar "Golden Delicious" True,
                Portocala "Sanguinello" 15,
                Portocala "Moro" 12,
                Portocala "Tarocco" 3,
                Portocala "Moro" 12,
                Portocala "Valencia" 2,
                Mar "Golden Delicious" False,
                Mar "Golden" False,
                Mar "Golden" True]

-- 1)

-- a)

ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Mar _ _) = False
ePortocalaDeSicilia (Portocala s _)
    | s `elem` ["Tarocco","Moro","Sanguinello"] = True
    | otherwise = False

-- b)


comp3Portocala :: Fruct -> Int
comp3Portocala (Portocala _ x) = x

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia [] = 0
nrFeliiSicilia (h:t)
    | ePortocalaDeSicilia h = comp3Portocala h + nrFeliiSicilia t
    | otherwise = nrFeliiSicilia t

-- c)

comp3Mar :: Fruct -> Bool
comp3Mar (Mar _ x) = x

eMar :: Fruct -> Bool
eMar (Portocala _ _) = False
eMar (Mar _ _) = True

nrMereViermi :: [Fruct] -> Int
nrMereViermi [] = 0
nrMereViermi (h:t)
    | eMar h && comp3Mar h= 1 + nrMereViermi t
    | otherwise = nrMereViermi t


-- 2)

-- a)

type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

vorbeste :: Animal -> String
vorbeste (Pisica _) = "Meow!"
vorbeste (Caine _ _) = "Woof!"

-- b)

rasa :: Animal -> Maybe String
rasa (Pisica _) = Nothing
rasa (Caine _ s) = Just s

-- 3)

data Linie = L [Int]
    deriving Show
data Matrice = M [Linie]
    deriving Show

-- a)

verifica :: Matrice -> Int -> Bool
verifica (M []) n = True
verifica (M ((L h): t)) n = ((foldr (+) 0 h) == n) && (verifica (M t) n)

-- b)

doarPozN :: Matrice -> Int -> Bool
doarPozN (M []) n = True
doarPozN (M ((L h): t)) n 
    | length h == n = (foldr (&&) True (map (\x -> if x>0 then True else False) h)) && (doarPozN (M t) n)
    | otherwise = doarPozN (M t) n

-- c)

corect :: Matrice -> Bool

corect (M l) = length (filter (\x -> x==(head (map (\(L x) -> length x)  l))) (map (\(L x) -> length x)  l)) == length (map (\(L x) -> length x)  l)
--corect (l1 : l2 : ls)
