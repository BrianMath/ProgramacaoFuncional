
{- 1. Escreva uma definição da função intersperse :: a → [a] → [a] do módulo Data.List que
intercala um valor entre os elementos duma lista. Exemplo: intersperse '-' "banana" =
"b-a-n-a-n-a" -}
intersperse' :: Eq a => a -> [a] -> [a]
intersperse' c [] = []
intersperse' c (x:xs) | xs == [] = [x] 
					  | otherwise = x : c : (intersperse' c xs)


{- 2. Escreva uma função permutations :: [a] → [[a]] para obter a lista com todas as permutações
dos elementos duma lista (a ordem das permutações não é importante). Assim, se xs tem comprimento n, 
então permutations xs tem comprimento n!. Exemplo:
- permutations [1, 2, 3] = [[1, 2, 3], [2, 1, 3], [2, 3, 1], [1, 3, 2], [3, 1, 2], [3, 2, 1]] 
- permutations [1, 2] = [[1, 2], [2, 1]] -}
-- Não consegui 😢

{- 3. Ordenação de listas pelo método merge sort. -}
{- (a) Defina recursivamente a função merge :: Ord a ⇒ [a] → [a] → [a] para juntar duas
listas ordenadas numa so mantendo a ordenação.
Exemplo: merge [3, 5, 7] [1, 2, 4, 6] = [1, 2, 3, 4, 5, 6, 7] -}
merge :: Ord a => [a] -> [a] -> [a]
merge [] (y:ys) = (y:ys)
merge (x:xs) [] = (x:xs)
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
					| otherwise = y : merge (x:xs) ys

{- (b) Usando a função merge, escreva uma definição recursiva da função
msort :: Ord a ⇒ [a] → [a] que implementa o metodo merge sort: -}
msort :: Ord a => [a] -> [a]
msort [] = []
msort (x:xs) | xs == [] = [x]
			 | otherwise = merge (msort left) (msort right)
						where
							metade = length (x:xs) `div` 2
							left = take metade (x:xs)
							right = drop metade (x:xs)

