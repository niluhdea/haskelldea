module One where

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
  | otherwise = elem' xs

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
last' (x:xs) = x : last' xs

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
maximum' (x:xs) = max' (maximum' xs)

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
