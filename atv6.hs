data Arv a = No a alt (Arv a) (Arv a)
		   | Vazia
		   deriving Show

a :: Arv Int
a = No 11 1 (No 3 2 (No 2 3 Vazia Vazia) (No 5 3 Vazia Vazia)) (No 19 2 (No 13 3 Vazia Vazia) (No 23 3 Vazia Vazia))

a_dir :: Arv Int
a_dir = No 10 1 (No 1 2 Vazia Vazia) (No 15 2 (No 14 3 Vazia Vazia) (No 16 3 Vazia Vazia))

-- altura :: Arv a -> Int
-- altura Vazia = 0
-- altura (No _ esq dir) = 1 + max (altura esq) (altura dir)

-- desvio :: Arv a -> Int
-- desvio Vazia = 0
desvio (No _ esq dir) = altura esq - altura dir

pesquisaAVL :: Ord a => a -> Arv a -> Bool
pesquisaAVL x Vazia = False
pesquisaAVL x (No y (esq) (dir))
	| x==y = True
	| x<y  = pesquisaAVL x esq
	| x>y  = pesquisaAVL x dir

rodar_dir :: Arv a -> Arv a
rodar_dir (No x (No y t1 t2) t3) = No y t1 (No x t2 t3)
rodar_dir t = t

rodar_esq :: Arv a -> Arv a
rodar_esq (No x t1 (No y t2 t3)) = No y (No x t1 t2) t3
rodar_esq t = t

corrige_dir :: Arv a -> Arv a
corrige_dir (No x t1 t2)
	| desvio t1 == (-1) = rodar_dir (No x (rodar_esq t1) t2)
	| otherwise	        = rodar_dir (No x t1 t2)
corrige_dir t = t

corrige_esq :: Arv a -> Arv a
corrige_esq (No x t1 t2)
	| desvio t2 == 1 = rodar_esq (No x t1 (rodar_dir t2))
	| otherwise      = rodar_esq (No x t1 t2)
corrige_esq t = t

rebalancear :: Arv a -> Arv a
rebalancear t
	| d == 2    = corrige_dir t
	| d == (-2) = corrige_esq t
	| otherwise = t
	where d = desvio t

inserirAVL :: Ord a => a -> Arv a -> Arv a
inserirAVL x Vazia = No x Vazia Vazia
inserirAVL x (No y esq dir)
	| x == y = No y esq dir
	| x < y  = rebalancear (No y (inserirAVL x esq) dir)
	| x > y  = rebalancear (No y esq (inserirAVL x dir))

mais_esq :: Arv a -> a
mais_esq (No x Vazia _) = x
mais_esq (No _ esq _) = mais_esq esq

removerAVL :: Ord a => a -> Arv a -> Arv a
removerAVL x (No y Vazia dir)
	| x == y = rebalancear dir
removerAVL x (No y esq Vazia)
	| x == y = rebalancear esq
removerAVL x (No y esq dir)
	| x < y  = rebalancear (No y (removerAVL x esq) dir)
	| x > y  = rebalancear (No y esq (removerAVL x dir))
	| x == y = let z = mais_esq dir in rebalancear (No z esq (removerAVL z dir))


