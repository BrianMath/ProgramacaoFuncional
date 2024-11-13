data Arv a = No a (Arv a) (Arv a)
		   | Vazia
		   deriving Show

a :: Arv Int
a = No 11 (No 3 (No 2 Vazia Vazia) (No 5 Vazia Vazia)) (No 19 (No 13 Vazia Vazia) (No 23 Vazia Vazia))

listar :: Arv a -> [a]
listar Vazia = []
listar (No x (esq) (dir)) = listar esq ++ [x] ++ listar dir

ordenada :: Ord a => Arv a -> Bool
ordenada arv = crescente (listar arv)
		where crescente xs = and (zipWith (<=) xs (tail xs))

pertence :: Ord a => a -> Arv a -> Bool
pertence x Vazia = False
pertence x (No y (esq) (dir))
	| x==y = True
	| x<y  = pertence x esq
	| x>y  = pertence x dir

inserir :: Ord a => a -> Arv a -> Arv a
inserir x Vazia = No x Vazia Vazia
inserir x (No y (esq) (dir))
	| x==y = No y (esq) (dir)
	| x<y  = No y (inserir x esq) (dir)
	| x>y  = No y (esq) (inserir x dir)

construir :: Ord a => [a] -> Arv a
construir [] = Vazia
construir xs = No x (construir xs') (construir xs'')
	where 
		  n = length xs`div`2
		  xs' = take n xs		-- lado esquerdo
		  x:xs'' = drop n xs	-- meio e lado direito

mais_esq :: Arv a -> a
mais_esq (No x Vazia _) = x
mais_esq (No _ esq _) = mais_esq esq

mais_dir :: Arv a -> a
mais_dir (No x _ Vazia) = x
mais_dir (No _ _ dir) = mais_dir dir

remover :: Ord a => a -> Arv a -> Arv a
remover x (No y Vazia dir) | x==y = dir		-- somente filho na direita
remover x (No y esq Vazia) | x==y = esq		-- somente filho na esquerda
remover x (No y (esq) (dir))				-- dois filhos
	| x<y  = No y (remover x esq) (dir)
	| x>y  = No y (esq) (remover x dir)
	| x==y = let z = mais_esq dir in No z (esq) (remover z dir)

remover' :: Ord a => a -> Arv a -> Arv a
remover' x (No y Vazia dir) | x==y = dir		-- somente filho na direita
remover' x (No y esq Vazia) | x==y = esq		-- somente filho na esquerda
remover' x (No y (esq) (dir))					-- dois filhos
	| x<y  = No y (remover x esq) (dir)
	| x>y  = No y (esq) (remover x dir)
	| x==y = let z = mais_dir esq in No z (remover z esq) dir

altura :: Arv a -> Int
altura Vazia = 0
altura (No _ (esq) (dir)) = 1 + max (altura esq) (altura dir)

equilibrada :: Arv a -> Bool
equilibrada Vazia = True
equilibrada (No _ (esq) (dir)) = 
	abs (altura esq - altura dir) <= 1 &&
	equilibrada esq && equilibrada dir

