import Data.List

myInt = 5555555555555555555555555555555555555555555555555555555555555555555555555555555555555

double :: Integer -> Integer
double x = x+x


--maxim :: Integer -> Integer -> Integer
maxim x y = if (x > y)
               then x
          else y

max3 x y z = let
             u = maxim x y
             in (maxim  u z)

patrat :: Integer ->Integer ->Integer
patrat x y = x*x+y*y

par :: Integer -> String
par x = if (mod x 2==0) 
            then "par" 
        else "impar"

factorial :: Integer -> Integer
factorial x = if(x>1) 
                then x*factorial (x-1) 
            else 1

dublu :: Integer->Integer->Bool
dublu x y = if(x>y*2) 
                then True
            else False

