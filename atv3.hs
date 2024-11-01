
{- 1. Mostre que pode-se denfinir uma função isort :: Ord a ⇒ [a] → [a] para ordenar uma
lista pelo método de inserção usando foldr e insert. -}
insert :: Ord a => a -> [a] -> [a]
insert n [] = [n]
insert n (x:xs) = if n <= x then n : x : xs else x : insert n xs


isort :: Ord a => [a] -> [a] 
isort [] = []
isort xs = foldr (insert) [] xs

-- [3,1,4,2] -> [1,3,4,2] -> [1,3,4,2] -> [1,3,2,4] -> [1,2,3,4]

{- 2. A função add pode ser definida em termos das funções:
	succ i = i + 1
	pred i = i - 1
pelas equações:
	add i 0 = i
	add i j = succ (add i (pred j)) -}
{- (a) Dê uma definição semelhante para mult que use apenas add e pred.
   Dê uma definição para exp que use apenas mult e pred.
   Qual sera a próxima função nesta sequência? -}

succ' :: Num a => a -> a
succ' i = i + 1

pred' :: Num a => a -> a
pred' i = i - 1

add :: (Eq a, Num a) => a -> a -> a
add i 0 = i
add i j = succ' (add i (pred' j))

mult :: (Eq a, Num a) => a -> a -> a
mult i 0 = 0
mult i j = add i $ mult i (pred' j)

--		   succ' (add i (pred' $ mult i (pred' j))) 
-- mult 2 2 = add 2 $ mult 2 (pred 2)
--			= add 2 $ mult 2 1
--			= add 2 $ (add 2 $ mult 2 (pred 1))
--			= add 2 $ (add 2 $ mult 2 0)
--			= add 2 $ (add 2 0)
--			= add 2 2
--			= succ (add 2 (pred 2))
--			= succ (add 2 1)
--			= succ (succ (add 2 (pred 1)))
--			= succ (succ (add 2 0))
--			= succ (succ 2)
--			= succ 3
--			= 4

-- 3 ^ 3  = 27
-- 3 ^ 2  = 9
-- 3 ^ 1  = 3
-- 3 ^ 0  = 1
-- 3 ^ -1 = 1/3

exp' :: (Eq a, Num a) => a -> a -> a
exp' i 0 = 1
exp' i j = mult i $ exp' i (pred' j)

-- exp' 3 2 = mult 3 $ exp' 3 (pred' 2)
--			= mult 3 $ exp' 3 1
--			= mult 3 $ (mult 3 $ exp' 3 (pred' 1))
--			= mult 3 $ (mult 3 $ exp' 3 0)
--			= mult 3 $ (mult 3 1)
--			= mult 3 3
--			= 9

tetra :: (Eq a, Num a) => a -> a -> a
tetra i 0 = 1
tetra i j = exp' i $ tetra i (pred' j)

-- tetra 3 2 = exp' 3 $ tetra 3 (pred' 2)
--			= exp' 3 $ tetra 3 1
--			= exp' 3 $ (exp' 3 $ tetra 3 (pred' 1))
--			= exp' 3 $ (exp' 3 $ tetra 3 0)
--			= exp' 3 $ (exp' 3 1)
--			= exp' 3 3
--			= 27

-- add 3 2  = 5

-- mult 3 2 = 3 `add` 3  = 6
-- mult 3 1 = 3 `sub` 3
-- mult 3 0 = 0

-- exp 3 2  = 3 `mult` 3 = 9
-- exp 3 1  = 3 `div` 3
-- exp 3 0  = 1

-- func 3 2 = 3 `exp` 3 = 27
-- func 3 1 = 3 `log` 3
-- func 3 0 = 1

{- (b) A função foldi sobre inteiros pode ser definida da seguinte forma:
	foldi :: (a -> a) -> a -> Integer -> a
	foldi f q 0 = q
	foldi f q i = f (foldi f q (pred i))
Defina as funções add, mult e exp em termos foldi. -}
foldi :: (a -> a) -> a -> Integer -> a
foldi f q 0 = q
foldi f q i = f (foldi f q (pred i))

addF :: (Num a) => a -> Integer -> a
addF i j = foldi succ' i j
-- foldi f q 2 = f (foldi f q 1)
--			   = f (f (foldi f q 0))
--			   = f (f (q))
--	3 + 1	 4 = succ (3)
--	3 + 2	 5 = succ (succ (3))

multF :: (Num a) => a -> Integer -> a
multF i 0 = 0
multF i j = foldi (+i) i (j-1)
--	3 * 1	 3 = (3)
--	3 * 2	 6 = (addF 3) (3)

expF :: (Num a) => a -> Integer -> a
expF i 0 = 1
expF i j = foldi (*i) i (j-1)
--  3 ^ 1    3 = (3)
--  3 ^ 2    9 = (multF 3) (3)

{- (c) Defina as funções fat (fatorial) e fib (Fibonacci) 
	utilizando a função foldi. -}

fat :: Integer -> Integer
fat 0 = 1
fat n = fst $ foldi (f) (n,n-1) (n-1)
		where f (x,y) = (x*y, y-1)

-- fat 0	1	= 
-- fat 1	1	= 1
-- fat 2	2	= f 2
-- fat 3	6	= f $ f 3
-- fat 4	24	= fst $ f $ f $ f (4,3)
--				= fst $ f $ f (12,2)
--				= fst $ f (24,1)
--				= fst $ (24,0)
--				= 24

fib :: Integer -> Integer
fib 1 = 1
fib 2 = 1
fib n = fst $ foldi (f) (1,1) (n-2)
		where f (x,y) = (x+y,x)
-- fib 1	1	= f 1
-- fib 2	1	= f $ f i
-- fib 3	2	= fst $ f (1,1)
--				= fst (2,1)
--				= 2
-- fib 4	3	= fst $ f $ f (1,1)
--				= fst $ f (2,1)
--				= fst (3,2)
--				= 3
-- fib 5	5	= fst $ f $ f $ f (1,1)
--				= fst $ f $ f (2,1)
--				= fst $ f (3,2)
--				= fst (5,3)
--				= 5


{- A função do prelúdio scanl é uma variante do foldl que produz a lista
   com os valores acumulados:
		scanl f z [x1, x2, . . .] = [z, f z x1, f (f z x1) x2, . . .]
   Por exemplo:
		scanl (+) 0 [1, 2, 3] = [0, 0 + 1, 0 + 1 + 2, 0 + 1 + 2 + 3] = [0, 1, 3, 6]
   Em particular, para listas finitas xs temos que last (scanl f z xs) = foldl f z xs.
   Escreva uma definição recursiva de scanl; deve-se usar outro nome para evitar
   colidir com a definição do prelúdio. -}
scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' f z [] = [z]
scanl' f z (x:xs) = z : scanl' f (f z x) xs
-- foldl f z (x:xs) = foldl f (f z x) xs

-- scanl' (+) 0 [1,2,3] = 0 : scanl' (+) ((+) 0 1) [2,3]
--						= 0 : scanl' (+) (1) [2,3]
--						= 0 : (1 : scanl' (+) ((+) 1 2) [3])
--						= 0 : (1 : scanl' (+) 3 [3])
--						= 0 : (1 : (3 : scanl' (+) ((+) 3 3) []))
--						= 0 : (1 : (3 : scanl' (+) 6 []))
--						= 0 : (1 : (3 : [6]))
--						= [0, 1, 3, 6]

