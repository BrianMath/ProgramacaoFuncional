----- 1)
quadrados :: Integer
quadrados = sum [x*x | x<-[1..100]]
quadrados2 = map (\x -> x*x) [1..100]

somaLista [] = 0
somaLista (x:xs) = x + somaLista xs


----- 2)
-- (a)
seriePi :: (Fractional a, Integral b) => b -> [a]
seriePi n = [((-1)^x)/(2 * fromIntegral x+1) | x<-[0..n]]

aprox :: (Fractional a, Integral b) => b -> a
aprox n = (sum (seriePi n)) * 4

-- (b)
seriePi2 :: (Fractional a, Integral b) => b -> [a]
seriePi2 n = [((-1)^x)/((fromIntegral x + 1)^2) | x<-[0..n]]

aprox2 :: (Floating a, Integral b) => b -> a
aprox2 n = sqrt ((sum (seriePi2 n)) * 12)


----- 3)
divprop :: Int -> [Int]
divprop n = [x | x<-[1..n-1], n`mod`x == 0]


----- 4)
perfeitos :: Int -> [Int]
perfeitos n = [x | x<-[1..n], sum (divprop x) == x]


----- 5)
dobroLuhn :: Int -> Int
luhn :: Int -> Bool





