----- 1)
data Arv a = Vazia
		   | No a (Arv a) (Arv a)

--- (a)
profund :: Arv a -> Int
profund Vazia = 0
profund (No a (esq) (dir)) = 1 + max (profund esq) (profund dir)

--- (b)
tamanho :: Arv a -> Int
tamanho Vazia = 0
tamanho (No a (esq) (dir)) = 1 + tamanho esq + tamanho dir

--- (c)
eq_struct :: Arv a -> Arv b -> Bool
eq_struct Vazia Vazia = True
eq_struct Vazia (No a (esq) (dir)) = False
eq_struct (No a (esq) (dir)) Vazia = False
eq_struct (No a (esq1) (dir1)) (No b (esq2) (dir2)) = 
	(eq_struct esq1 esq2) && (eq_struct dir1 dir2)


----- 2)
balanceada :: Arv a -> Bool
balanceada arv1 arv2 = tamanho arv1 == tamanho arv2




