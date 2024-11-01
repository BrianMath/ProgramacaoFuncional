----- 1)
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] ys = []
zipWith' f xs [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys


----- 2)
-- (a)
(+++) :: [a] -> [a] -> [a]
(+++) xs ys = foldr (:) ys xs 

-- (b)
concat' :: [[a]] -> [a]
concat' xs = foldr (++) [] xs

-- (c)
reverseR :: [a] -> [a]
reverseR xs = foldr (\x -> \y -> y ++ [x]) [] xs

-- (d)
reverseL :: [a] -> [a]
reverseL ys = foldl (\x -> \y -> y : x) [] ys


----- 3)
mdc :: Integral a => a -> a -> a
mdc a b = until (condicao) (funcao) arg 
-- mdc 4 3 -> if 3 == 0 then 4 else mdc 3 (4 `mod` 3)
-- mdc 3 1 -> if 1 == 0 then 3 else mdc 1 (3 `mod` 1)
-- mdc 1 0 -> 1


