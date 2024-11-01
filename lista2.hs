
----- 6)
-- (a)
shift :: [a] -> [a]
shift (x:xs) = xs ++ [x]

-- (b)
rotate :: [a] -> [[a]]
rotate xs = xs : [[]]
-- rotate [1,2,3] = [[1,2,3], [2,3,1], [3,1,2]]



