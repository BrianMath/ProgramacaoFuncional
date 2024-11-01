----- 1)
dec2int :: [Int] -> Int
dec2int xs = foldl (\x -> \y -> x*10+y) 0 xs

----- 2)
-- (a)
maximum' :: Ord a => [a] -> a
maximum' xs = foldl1 (\x -> \y -> max x y) xs

minimum' :: Ord a => [a] -> a
minimum' xs = foldr1 (\x -> \y -> min x y) xs

----- 3)
-- (a)
(+++) :: [a] -> [a] -> [a]
(+++) xs ys = foldr (:) ys xs

-- (c)
reverseR :: [a] -> [a]
reverseR xs = foldr () [] xs 

