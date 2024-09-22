---------- Definições de funções simples ----------

------ 1)
-- inc (quadrado 5) => inc (25) => 26
-- quadrado (inc 5) => quadrado (6) => 36
-- media (dobro 3)(inc 5) => media (6)(6) => 6


------ 2)
triangulo :: (Ord a, Num a) => a -> a -> a -> Bool
triangulo a b c = (a + b) > c && (a + c) > b && (b + c) > a 


------ 3)
areaT :: Floating a => a -> a -> a -> a
areaT a b c = sqrt(s * (s-a) * (s-b) * (s-c))
    where s = (a + b + c)/2


------ 4)
metades :: [a] -> ([a], [a])
metades l = (take m l , drop m l)
    where m = (length l) `div` 2


------ 5)
-- (a)
last1 :: [a] -> a
last1 l = l !! ((length l) - 1)

last2 :: [a] -> a
last2 l = head (reverse l)
-- (b)
init1 :: [a] -> [a]
init1 l = reverse (tail (reverse l))

init2 :: [a] -> [a]
init2 l = take ((length l) - 1) l


------ 6)


dobro x = x + x
quadruplo x = dobro (dobro x)

tresiguais :: Int -> Int -> Int -> Bool
tresiguais x y z = 
    if x == y then x == z
    else False

max3 x y z = max (max x y) z
min3 x y z = min (min x y) z


--xor p q = if p then (not q) else q
--xor p q = not (p == q)

xor True q = not q
xor False q = q

safetail [] = []
safetail l = drop 1 l

curta1 l = if (length l) < 3 then True else False

curta2 [] = True
curta2 [a] = True
curta2 [a,b] = True
curta2 _ = False

curta3 l | length l > 2 = False
         | otherwise = True


