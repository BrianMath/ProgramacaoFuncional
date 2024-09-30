![haskell-logo](https://github.com/user-attachments/assets/07ef08c1-3000-45ae-8656-a0389c97b548)

# Programação funcional

Repositório com os exercícios feitos na disciplina de Programação Funcional com a linguagem [Haskell](https://www.haskell.org/)

```haskell
-- EXEMPLO DE FUNÇÕES

-- Calcula o coeficiente binomial (combinação) entre 'n' e 'k'
binom :: Int -> Int -> Int
binom n 0 = 1
binom n k = if n == k then 1 else (binom (n-1)(k-1)) + (binom (n-1)(k))

-- Calcula as n+1 primeiras linhas do triângulo de Pascal usando a função de coeficientes
pascal :: Int -> [[Int]]
pascal n = [[binom i j | j<-[0..i]] | i<-[0..n]]

{- Usando o interpretador GHCI
ghci> pascal 3
[[1],[1,1],[1,2,1],[1,3,3,1]]
-}
```
