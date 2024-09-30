-- 1)
anyZero :: (Integer -> Integer) -> Integer -> Bool
anyZero f n = or [f i == 0 | i<-[0..n]]

qualquerUma :: Integer -> Integer
qualquerUma n = (n + 5) `mod` 10

-- 2)
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub [x' | x' <- xs, x' /= x]

-- 3)
-- (a)
insert :: Ord a => a -> [a] -> [a]
insert n [] = [n]
insert n (x:xs) = if n <= x then n : x : xs else x : insert n xs

-- (b)
isort :: Ord a => [a] -> [a] 
isort [] = []
isort (x:xs) = insert x (isort xs)

-- 4)
-- (a)
minimum' :: Ord a => [a] -> a
minimum' [n] = n
minimum' (x:xs) = min x (minimum' xs)

-- (b)
delete' :: Eq a => a -> [a] -> [a]
delete' n [] = []
delete' n (x:xs) = if x == n then xs else x : delete' n xs

-- (c)
ssort :: Ord a => [a] -> [a]


main = do
	nome <- getLine
	putStrLn "Olá, "
	putStrLn nome




