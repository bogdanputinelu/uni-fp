import Data.Char

voc :: String -> Int
voc "" = 0
voc (h:t)
    | h `elem` "aeiouAEIOU" = 1 + voc t
    | otherwise = voc t

nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale (h:t)
    | h == reverse h = voc h + nrVocale t
    | otherwise = nrVocale t

f :: Int -> [Int] -> [Int]
f n [] = []
f n (h:t)
    | even h = [h]++[n] ++ f n t
    | otherwise = [h] ++ f n t

nrVoc :: [String] -> Int
nrVoc l = sum[vocale x | x<-l , x== reverse x]
vocale :: String ->Int
vocale s = sum[1 | c<-s, c `elem` "aeiouAEIOU"]

divizori :: Int -> [Int]
divizori n = [x | x<-[1..n], mod n x == 0]

listadiv :: [Int] -> [[Int]]
listadiv l = [divizori x | x<-l]

inInterval :: Int -> Int -> [Int] -> [Int]
inInterval a b l = [x | x<-l, a<=x && x<=b]

pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (h:t)
    | h>0 = 1+pozitiveRec t
    | otherwise = pozitiveRec t

pozitiveComp :: [Int] -> Int
pozitiveComp l = sum[1 | x<-l, x>0]

pozImpRec :: Int -> [Int] -> [Int]
pozImpRec n [] = []
pozImpRec n (h:t)
    | odd h = [n] ++ pozImpRec (n+1) t
    | otherwise = pozImpRec (n+1) t

pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec [] = []
pozitiiImpareRec l = pozImpRec 0 l

pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp l = [ i | (i,x)<- [0..] `zip` l, odd x ]

multDigitsRec :: String -> Int
multDigitsRec "" = 1
multDigitsRec (h:t)
    | isDigit h = digitToInt h * multDigitsRec t
    | otherwise = multDigitsRec t

multDigitsComp :: String -> Int
multDigitsComp s = product[digitToInt c | c<-s, isDigit c ]


