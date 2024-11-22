----- 1)
seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (m:ms) = m >> seqn ms

falaElefante :: Int -> IO()
falaElefante i = putStr ("Se " ++ show (i-1) ++ " elefantes incomodam muita gente,\n" ++ show i ++ " elefantes incomodam muito mais!\n")

elefantes :: Int -> IO()
elefantes n = seqn [ falaElefante i | i<-[3..n]]

alfa :: [Char]
alfa = "abcdefghijklmnopqrstuvwxyz"

pos :: Eq a => a -> [a] -> Int
pos n [] = -1
pos n (x:xs)
	| n==x      = 0
	| otherwise = 1 + pos n xs



