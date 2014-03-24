import Data.List
import System.Random
import Data.Ord
import Debug.Trace

-- 99 Haskell Problems
-- Skipped so far: 11, 13, 46, 47, 48
-- Current: 49 (Gray Codes)

-- Lists
-- 1
last' :: [a] -> Maybe a
last' [] = Nothing
last' (x:[]) = Just(x)
last' (x:xs) = last' xs

-- 2
lastButOne :: [a] -> Maybe a
lastButOne [] = Nothing
lastButOne (z:x:[]) = Just(z)
lastButOne (x:xs) = lastButOne xs

-- 3
kthElem :: Int -> [a] -> Maybe a
kthElem _ [] = Nothing
kthElem x ys = Just(head $ drop (x-1) ys)

-- 4
len :: [a] -> Int
len [] = 0
len xs = len' 0 xs
	where
		len' n [] = n
		len' n (y:ys) = len' (n+1) ys

len' :: [a] -> Int
len' xs = foldl (+) 0 [ 1 | _ <- xs]

-- 5
rev :: [a] -> [a]
rev = foldl (\ys y -> y:ys) []

revList :: [a] -> [a]
revList [] = []
revList (xs) = (last xs) : (revList (init xs))

revAccu :: [a] -> [a]
revAccu xs = revAccu' xs []
	where
		revAccu' [] ys = ys
		revAccu' (x:xs) ys = revAccu' xs (x:ys)

-- 6
isPali :: (Eq a) => [a] -> Bool
isPali xs
		| even xslength = (take xslength2 xs) == revAccu(drop xslength2 xs)
		| otherwise = (take xslength2 xs) == revAccu(drop (xslength2+1) xs)
		where
			xslength = len xs
			xslength2 = xslength `div` 2

isPali' :: (Eq a) => [a] -> Bool
isPali' xs = xs == reverse xs

-- 7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x ) = [x]
flatten (List xs) =  foldr (++) [] $ map flatten xs

-- 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress xs = compress' (tail xs) [head xs]
	where
		compress' [] ys = ys
		compress' (x:xs) ys = compress' xs (add x ys)
		add y xs = if y == (last xs) then xs else xs++[y]

-- 9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack xs = reverse $ pack' (tail xs) [[head xs]]
	where
		pack' [] ys = ys
		pack' (x:xs) ys = pack' xs $ add x ys (head ys)
		add x ys hys | x == head hys = (x:hys) : (tail ys)
				 	 | otherwise = [x] : ys

-- 10
encode :: (Ord a, Eq a) => [a] -> [(a, Int)]
encode [] = []
encode xs = [(head x, length x) | x <- (pack $ sort xs)]

encode' :: (Eq a) => [a] -> [(a, Int)]
encode' [] = []
encode' xs = map (\x -> (head x, length x)) (group xs)

-- 11
-- Evers...

 -- 12
decode xs = foldl (\acc (x, y) -> acc ++ take y (repeat x)) [] xs

-- 13
-- Not really possible, same as 11

-- 14
duplicate_end :: [a] -> [a]
duplicate_end [] = []
duplicate_end xs = dupli xs []
	where
		dupli [] ys = ys
		dupli (x:xs) ys = dupli xs (ys ++ take 2 (repeat x))

dup xs = foldl (\acc x -> acc ++ take 2 (repeat x)) [] xs

-- 15
dupNth n xs = foldl (\acc x -> acc ++ take n (repeat x)) [] xs

-- 16
dropNth :: Int -> [a] -> [a]
dropNth 0 xs = xs
dropNth n [] = []
dropNth n xs = take (n-1) xs ++ dropNth n (drop n xs)

-- 17
splitAt' :: Int -> [a] -> ([a],[a])
splitAt' 0 xs = ([],xs)
splitAt' n [] = ([], [])
splitAt' n xs = (take n xs, drop n xs)

-- 18
slice :: Int -> Int -> [a] -> [a]
slice i k [] = []
slice i k xs = take k $ drop i xs

-- 19
rotate :: Int -> [a] -> [a]
rotate i [] = []
rotate i xs = drop modI xs ++ take modI xs
	where
		modI = i `mod` (length xs)

-- 20
rmKth :: Int -> [a] -> [a]
rmKth i [] = []
rmKth 0 xs = xs
rmKth i xs = rmKth' (i-1) 0 xs
	where
		rmKth' i n (x:xs) 
			| i == n = xs
			| otherwise = x : rmKth' i (n+1) xs 

rm :: Int -> [a] -> [a]
rm 1 (x:xs) = xs
rm n [] = []
rm n (x:xs) = x : rm (n-1) xs

-- 21
insertKth :: Int -> a -> [a] -> [a]
insertKth i y [] = y : []
insertKth 0 y xs = y : xs
insertKth i y xs = (take i xs) ++ (y : (drop i xs)) 

-- 22
range :: Int -> Int -> [Int]
range i k
	| k < i = [i,(i-1)..(k)]
	| otherwise = [i..k]

-- 23
-- take n random elements of list xs
rdm_select :: [a] -> Int -> IO [a]
rdm_select xs n = rdm_select' xs n []
	where
 		rdm_select' _ 0 ys = return ys
 		rdm_select' xs n ys = 
 			do 
 				r <- randomRIO(0, (length xs)-1)
 				rest <- rdm_select' xs (n-1) ((xs!!r):ys)
 				return rest

-- 24
-- get a random permutation of the given list 
-- 6 46 => [_,_,_,_,_,_]
diff_select :: Int -> Int -> IO [Int]
diff_select n to = diff_select' n [1..to]
 
diff_select' 0 _  = return []
diff_select' _ [] = error "too few elements to choose from"
diff_select' n xs = do r <- randomRIO (0, (length xs)-1)
                       rest <- diff_select' (n-1) (take r xs ++ drop (r+1) xs)
                       return ((xs!!r) : rest)

-- 25
-- get a random permutation of the given list
rdm_permu :: [a] -> IO [a]
rdm_permu xs = rdm_permu' xs []
	where
 		rdm_permu' [] ys = return ys
 		rdm_permu' xs ys = 
 			do 
 				r <- randomRIO(0, (length xs)-1)
 				rest <- rdm_permu' (take r xs ++ drop (r+1) xs) ((xs!!r):ys)
 				return rest

-- 26
combinations :: Int -> [a] -> [[a]]
combinations _ [] = [[]]
combinations 0 _  = [[]]
combinations k (x:xs) = x_start ++ others
    where x_start = [ x : rest | rest <- combinations (k-1) xs ]
          others  = if k <= length xs then combinations k xs else []

-- 27
-- find all combinations of a group divided in 3 distinct groups so that all members are used
-- [0, 1, 2] [1,2,3] => [[[1,2],[],[3]],[[1,3],[],[2]],[[2,3],[],[1]]]

-- 28
-- positive sorting on sublists length
lsort :: (Ord a) => [[a]] -> [[a]]
lsort xs = sortBy (comparing length) xs

-- negative sorting on sublists length
lrsort :: (Ord a) => [[a]] -> [[a]]
lrsort = sortBy (\xs ys -> compare (-length xs) (-length ys))

-- counting all lengths of sublist and the ones with highest count appear first
lfsort :: (Ord a) => [[a]] -> [[a]]
lfsort xs = concat $ lrsort $ groupBy eq (lsort xs)
	where
		eq y x = length y == length x

-- Arithmetic
-- 31
is_prime :: Int -> Bool
is_prime x | x < 4 = x /= 1
		   | even x = False
		   | otherwise = not (any (\y -> (x `mod` y) == 0) [5,7..(x-1)])

-- map (fst) $ concat $ tail $ groupBy (\x y -> (snd x) == (snd y)) $ sortBy (comparing snd) $ is_primeL [0..19] --> all primes in raw numbers
is_primeL :: [Int] -> [(Int,Bool)]
is_primeL = map (\x -> (x, is_prime x))

-- all primes in raw numbers
is_primeL' :: [Int] -> [Int]
is_primeL' = filter (is_prime)

-- 32
-- euclids greatest common divisor
mgcd :: Int -> Int -> Int
mgcd 0 b = abs b
mgcd a b = mgcd (b `mod` a) a

-- 33
-- coprimes -> two numbers are only divisible by 1
coprime :: Int -> Int -> Bool
coprime a b = (mgcd a b) == 1

-- 34
-- coprimes up to a (Eulers Totient)
totientL :: Int -> [Int]
totientL a = filter (\x -> coprime x a) [0..a]

totient :: Int -> Int
totient 1 = 1
totient a = length $ totientL a

-- 35
factor :: Int -> [Int]
factor 1 = []
factor n = let prime = head $ dropWhile ((/= 0) . mod n) [2 .. n]
           in (prime :) $ factor $ div n prime

-- 36
factor_mult :: Int -> [(Int,Int)]
factor_mult a = map (\x -> (head x, length x)) $ groups
	where groups = group $ sort $ factor a

-- 37
-- Efficient solution to Eulers Totient Function
totient' m = foldl (*) 1 primy
	where 
		primy = [calc p c | (p, c) <- factor_mult m]
		calc p c = (p - 1) * p ^ (c - 1)

-- 38
-- :load 99haskell.hs
-- :set +s +t
-- totient' 1090000000000000000 => (0.00 secs, 1025448 bytes) -> 37
-- totient 1090000 => (6.90 secs, 1652620000 bytes) -> 34

-- 39
primesR :: Int -> Int -> [Int]
primesR a b | even a = filter is_prime [a+1,a+3..b]
            | otherwise = filter is_prime [a,a+2..b]

-- 40
-- goldbach conjecture -> every even number above 2 is the sum of two prime numbers
goldbach :: Int -> (Int,Int)
goldbach n = head [(x,y) | x <- pr, y <- pr, x+y==n]
    where pr = primesR 2 (n-2)

-- 41
goldbachList :: Int -> Int -> [(Int,Int)]
goldbachList a b = map goldbach $ filter (even) [a..b]

goldbachList' :: Int -> Int -> Int -> [(Int,Int)]
goldbachList' a b c = filter (\(x,y) -> (y > c) && (x > c)) $ goldbachList a b 

-- Logic and Codes
-- 46
not' :: Bool -> Bool
not' False = True
not' _ = False

and', or', nor', xor', impl', equ' :: Bool -> Bool -> Bool

and' True True = True
and' _ _ = False

or' False False = False
or' _ _ = True

nor' False False = True
nor' _ _ = False

xor' False True = True
xor' True False = True
xor' _ _ = False

impl' True False = False
impl' _ _ = True

equ' True True = True
equ' False False = True
equ' _ _ = False

table :: (Bool -> Bool -> Bool) -> String
table f = printBinary f [True, False]
 
printBinary :: (Show a, Show b) => (a -> a -> b) -> [a] -> String
printBinary f domain = foldr (++) "" [printBinaryInstance f x y | x <- domain, y <- domain]
 
printBinaryInstance :: (Show a, Show b) => (a -> a -> b) -> a -> a -> String
printBinaryInstance f x y = show x ++ " " ++ show y ++ " " ++ show (f x y)

-- 47, 48 
-- ToDo


-- 49
-- Gray Codes -> gray 1 = ["1","0"]
gray :: (Show a, Integral a) => a -> [String]
gray 0 = [""]
gray n = foldr (\s acc -> ("0" ++ s):("1" ++ s):acc) [] $ gray (n-1)
	where
		tra s acc n = trace ("s=" ++ show s ++ " acc=" ++ show acc ++ " n=" ++ show n)

--54A
data Tree a = Empty | Branch a (Tree a) (Tree a)
	deriving (Show, Eq)

leaf x = Branch x Empty Empty


-- Other stuff
ins :: Int -> a -> [a] -> [a]
ins 0 y xs = y:xs
ins n y xs = ins' n y xs 0
	where
		ins' n y (x:xs) i
			| n == i = y:xs
			| otherwise = x : ins' n y xs (i+1)

insi :: Int -> a -> [a] -> [a]
insi 0 a xs = a:xs
insi n a (x:xs) = x : insi (n-1) a xs 

dropi :: Int -> [a] -> [a]
dropi 0 xs = xs
dropi n (x:xs) = dropi (n-1) xs

takei :: Int -> [a] -> [a]
takei 0 xs = []
takei n (x:xs) = x : takei (n-1) xs

prep :: [a] -> [(Int, a)]
prep xs = zip [0..] xs

takef :: Int -> [a] -> [a]
takef n xs = map (snd) $ filter (\(a, b) -> a <= n) (prep xs)

takef' :: Int -> [(Int, a)] -> [a]
takef' n xs = foldr (\(a, b) acc -> if a < n then b:acc else acc) [] xs

leni :: [a] -> Int
leni [] = 0
leni (x:xs) = 1 + leni xs







