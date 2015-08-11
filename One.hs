module One where

import Data.List

add a 0 = a
add a b
  | b > 0 = add (succ a) (pred b)
  | b < 0 = add (pred a) (succ b)

ambil n [] = []
ambil 0 xs = []
ambil n (x:xs) = x: (ambil (n - 1) xs)

cat xs [] = xs
cat [] xs = xs
cat (x:xs) ls = x : (cat xs ls)

buntut (x:xs) = xs

balik [] = []
balik (x:[]) = [x]
balik (x:xs) = cat (balik xs) [x]

head' (x:xs) = x

tail' (x:xs) = xs

null' [] = True
null' _ = False

elem' e [] = False
elem' e (x:xs)
  | e == x = True
  | otherwise = elem' e xs

notElem' e [] = True
notElem' e (x:xs)
  | e == x = False
  | otherwise = notElem' e xs

length' [] = 0
length' (x:xs) = 1 + length' xs

sum' [] = 0
sum' (x:xs) = x + sum' xs

product' [] = 1
product' [x] = x
product' (x:xs) = x * product' xs

last' [x] = x
last' (x:xs) = last' xs

take' 0 _ = []
take' a [] = []
take' a (x:xs) = x : take' (pred a) xs

drop' 0 a = a
drop' a [] = []
drop' a (x:xs) = drop' (pred a) xs

max' a b
  | a >= b = b

min' a b
  | a >= b = b

maximum' [x] = x
maximum' (x:xs) = max' x (maximum' xs)

minimum' [x] = x
minimum' (x:xs) = min' x (minimum' xs)

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

concat' [] = []
concat' (x:xs) = x ++ concat' xs

fst' (a,b) = a

snd' (a,b) = b

and' [] = True
and' (x:xs) = x && and' xs

repeat' (x:xs) = x : repeat' xs

zip' _ [] = []
zip' [] (x:xs) = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

--- zipWith' (+) [1,2] [3,4] = [4,6]

--- unzip' [(1,2)] --> ([1], [2])

map' _ [] = []
map' f (x:xs) = f x : map' f xs

--- map' (+2) [1,2,3] = [3,4,5]
--- map' (replicate)

filter'_ [] = []
filter' f (x:xs)
  | f x = x : filter' f xs
  | otherwise = filter f xs

--- filter' (<=5) [2,3,5,6,7] = [2,3,5]
--- filter' (odd) [1..10] = [1,3,5,7,9]

or' [] = False
or' (x:xs)
  | x == True = True
  | otherwise = or' xs

zip3' [] _ _ = []
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3' xs ys zs
