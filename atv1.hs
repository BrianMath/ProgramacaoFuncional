import Data.Char

{- 1) Usando uma funcão 'binom' que calcula o coeficiente binomial, escreva uma definição da
função 'pascal' :: Int → [[Int]] que calcula as primeiras linhas do triângulo de Pascal. O
triângulo de Pascal é constituido pelos valores (n k) das combinações de n em k em que
n é a linha e k é a coluna -}
binom :: Int -> Int -> Int
binom n 0 = 1
binom n k = if n == k then 1 else (binom (n-1)(k-1)) + (binom (n-1)(k))
{-
0,0				1
1,0			1		1			1,1
2,0		1		2		1		2,2
			   2,1
-}

pascalRuim :: Int -> [[Int]]
pascalRuim linha = [[binom i j] | i<-[0..linha], j<-[0..i]]

pascal :: Int -> [[Int]]
pascal linha = [[binom i j | j<-[0..i]] | i<-[0..linha]]

{- 2) Defina uma função	'forte' :: String → Bool para vericar se uma senha dada por uma
cadeia de carateres é "forte", ou seja: tem 8 carateres ou mais e pelo menos uma letra
maiúscula, uma letra minúscula e um algarismo.
Sugestão: use a função 'or' :: [Bool] → Bool e listas em compreensão -}
forte :: String -> Bool
forte senha = if (tam && min && mai && alg) then True else False
				where 
					tam = length senha >= 8
					min = or [True | c<-senha, isLower c]
					mai = or [True | c<-senha, isUpper c]
					alg = or [True | c<-senha, isDigit c]


{- 3) Defina extensões das funções 'encode' e 'crack' para a Cifra de César que também tratem
letras maiúsculas -}
-- Função que mapeia uma letra em um número de 0 a 25:
let2int :: Char -> Int
let2int c | isLower c = ord c - ord 'a'
		  | isUpper c = ord c - ord 'A'

-- E suas funções inversas para minúsculas e maiúsculas:
int2letMin :: Int -> Char
int2letMin n = chr (ord 'a' + n)

int2letMai :: Int -> Char
int2letMai n = chr (ord 'A' + n)

-- Aplicação de deslocamento (módulo 26) para letras:
shift :: Int -> Char -> Char
shift n c | isLower c = int2letMin ((let2int c + n) `mod` 26)
		  | isUpper c = int2letMai ((let2int c + n) `mod` 26)
		  | otherwise = c

-- Finalmente definimos a função de codificação utilizando a função
-- shift e uma lista por compreensão:
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]


-- Encriptações como a Cifra de César são suscetíveis a criptoanálise baseada na frequência de letras.
-- Frequência de cada letra para textos em português:
tabela :: [Float]
tabela = [ 14.6, 1.0, 3.9, 5.0, 12.6, 1.0, 1.3, 1.3, 6.2, 0.4, 0.02, 2.8, 4.7,
		  5.1, 10.7, 2.52, 1.2, 6.5, 7.8, 4.3, 4.6, 1.7, 0.01, 0.2, 0.01, 0.5 ]

-- Vamos definir um ataque baseado nessas frequências utilizando as seguintes funções:
letras :: String -> Int
letras cs = length [c | c<-cs, isAlpha c]

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x==(toLower x')]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

indices :: Eq a => a -> [a] -> [Int]
indices x ys = [i | (i,y)<-zip [0..n] ys, x==y]
				where n = length ys - 1

-- Primeiro definimos uma função que calcule a frequência de letras em uma string qualquer:
freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
			where n = letras xs

-- O método do chi-quadrado para a comparação entre frequências
-- observadas (os) e frequências esperadas (es)
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e)^2)/e | (o,e) <- zip os es]

-- Função que rotaciona uma lista em n posições:
rot :: Int -> [a] -> [a]
rot n xs = drop n xs ++ take n xs

-- Assim, podemos definir uma função para obter a mensagem original:
crack :: String -> String
crack xs = encode (-fator) xs
			where
				fator = head (indices (minimum chitab) chitab)
				chitab = [chisqr (rot n tabela') tabela | n <- [0..25]]
				tabela' = freqs xs

