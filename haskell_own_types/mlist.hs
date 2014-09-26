
data MList a = Empty | Cons a (MList a) deriving (Ord)

infixr 0 #
(#) :: a -> MList a -> MList a
(#) a ml = Cons a ml

infixr 0 ##
(##) :: MList a -> MList a -> MList a
(##) b Empty = b
(##) Empty c = c
(##) (Cons hd tl) c = hd # (tl ## c)

instance Eq a => Eq (MList a) where
		Empty 			 == Empty 			 = True
		(Cons a b) 	 == (Cons c d)   = a == c && b == d
		_            == _            = False

instance (Show a) => Show (MList a) where
		show Empty = "Empty"
		show ml    = "(" ++ (lshow ml) ++ ")"

lshow :: (Show a) => MList a -> String
lshow Empty = "Empty"
lshow (Cons hd tl) = (show hd) ++ " # " ++ (lshow tl)

{-
Clojure Style Parenthesis
instance (Eq a, Show a) => Show (MList a) where
		show Empty = "Empty"
		show (Cons a b) = (show a) ++ " # " ++ (if b == Empty then (show b) else "(" ++ (show b) ++ ")")
-}

mempty :: Eq a => MList a -> Bool
mempty ml = ml == Empty

mhead :: MList a -> a
mhead Empty = error "Head of an Empty List"
mhead (Cons a _) = a

mlast :: MList a -> a
mlast =
		\ml ->
			case ml of
				Empty -> error "Last of an Empty List"
				(Cons hd tl) ->
					case tl of
						Empty -> hd
						(Cons hd tl) -> mlast tl

mtail :: MList a -> MList a
mtail Empty = error "Tail of an Empty List"
mtail (Cons hd tl) = tl

mdrop :: Int -> MList a -> MList a
mdrop =
		\n -> \ml ->
			case ml of
				Empty -> Empty
				(Cons hd tl) ->
					if (n - 1) == 0
							then tl
							else mdrop (n - 1) tl

mtake :: Int -> MList a -> MList a
mtake _ Empty = Empty
mtake 0 _ = Empty
mtake n (Cons hd tl) = hd # (mtake (n - 1) tl)

mmap :: (a -> b) -> MList a -> MList b
mmap _ Empty = Empty
mmap f (Cons hd tl) = (f hd) # (mmap f tl)

mfoldr :: (a -> b -> b) -> b -> MList a -> b
mfoldr _ b Empty = b
mfoldr f b (Cons hd tl) = mfoldr f (f hd b) tl 

mfoldl :: (b -> a -> b) -> b -> MList a -> b
mfoldl _ b Empty = b
mfoldl f b (Cons hd tl) = mfoldl f (f b hd) tl 

mlength :: MList a -> Int
mlength ml = mfoldr (\x acc -> acc + 1) 0 ml
msize = mlength

mfilter :: (a -> Bool) -> MList a -> MList a
mfilter _ Empty = Empty
mfilter p (Cons hd tl)
		| p hd = hd # (mfilter p tl)
		| otherwise = mfilter p tl

minsert :: a -> Int -> MList a -> MList a
minsert b _ Empty = b # Empty
minsert b i (Cons hd tl) 
		| i == 0 = b # (hd # tl)
		| otherwise = hd # (minsert b (i - 1) tl)

madd :: a -> MList a -> MList a
madd a Empty = a # Empty
madd a (Cons hd tl) = hd # (madd a tl)

mreverse :: MList a -> MList a
mreverse Empty = Empty
mreverse (Cons hd tl) = madd hd (mreverse tl)

mquicksort :: (Ord a) => MList a -> MList a
mquicksort Empty = Empty
mquicksort (Cons hd tl) = lower ## (hd # Empty) ## greater
			where
				lower = mquicksort (mfilter (<=hd) tl)
				greater = mquicksort (mfilter (>hd) tl)

msort :: (Ord a) => MList a -> MList a
msort =
			\ml ->
				case ml of
					Empty -> Empty
					(Cons hd tl) ->
						let part p = msort (mfilter p tl)
						in part (<=hd) ## hd # Empty ## part (>hd)

melem :: (Eq a) => a -> MList a -> Bool
melem =
		\a -> \ ml ->
			case ml of
				Empty -> False
				(Cons hd tl) ->
					if hd == a 
						then True 
						else melem a tl

range :: Int -> Int -> MList Int
range x y
		| x == y = Cons x Empty
		| otherwise = x # (range (x + 1) y)


{-

1. Construction

> Empty
Empty

> 1 # Empty
(1 # Empty)

> 1 # 2 # Empty
(1 # 2 # Empty)

> (1 # 2 # 3 # Empty) ## (4 # 5 # 6 # Empty)
(1 # 2 # 3 # 4 # 5 # 6 # Empty)

> 1 # 2 # 3 # Empty ## 4 # 5 # 6 # Empty
(1 # 2 # 3 # 4 # 5 # 6 # Empty)

> Cons 4 (Cons 5 (Cons 6 Empty)) ## 1 # 2 # 3 # Empty
(4 # 5 # 6 # 1 # 2 # 3 # Empty)

> range 1 5
(1 # 2 # 3 # 4 # 5 # Empty)


2. Usage

> mquicksort $ 5 # 4 # 3 # 2 # 1 # Empty
(1 # 2 # 3 # 4 # 5 # Empty)

> msort $ 5 # 4 # 3 # 2 # 1 # Empty
(1 # 2 # 3 # 4 # 5 # Empty)

> 1 `melem` ml4
True

> mreverse $ 1 # 2 # 3 # 4 # 5 # Empty
(5 # 4 # 3 # 2 # 1 # Empty)

> mfoldr (+) 0 $ range 1 5
15

> mmap (+1) $ 1 # 2 # 3 # 4 # 5 # Empty
(2 # 3 # 4 # 5 # 6 # Empty)

> (1 # 2 # 3 # Empty ## 4 # 5 # 6 # Empty) == (range 1 6)
True

> let f = mtake 3 . mmap (+1) . mfilter (>3)
> f (1 # 2 # 3 # 4 # 5 # 6 # 7 # 8 # Empty) 
(5 # 6 # 7 # Empty)

-}

ml1 = Cons 1 (Cons 2 (Cons 3 Empty))
ml2 = Cons 4 (Cons 5 (Cons 6 Empty))
ml3 = ml2 ## ml1
ml4 = Cons 4 (Cons 5 (Cons 6 Empty)) ## 1 # 2 # 3 # Empty