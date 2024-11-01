import Data.List

----- 1) e 2)

data Prop = Const Bool
		  | Var Char
		  | Neg Prop
		  | Conj Prop Prop
		  | Disj Prop Prop
		  | Impl Prop Prop
		  | Equi Prop Prop
			deriving (Eq, Show)

-- a => ((¬a) => F)
-- Impl (Var 'a') (Impl (Neg (Var 'a')) (Const False))

type Assoc ch v = [(ch, v)]

find' :: Eq ch => ch -> Assoc ch v -> v
find' ch assocs = head [v | (ch',v)<-assocs, ch==ch']

type Atrib = Assoc Char Bool

valor :: Atrib -> Prop -> Bool
valor s (Const b)	= b
valor s (Var x)		= find' x s
valor s (Neg p)		= not (valor s p)
valor s (Conj p q)	= valor s p && valor s q
valor s (Disj p q)	= valor s p || valor s q
valor s (Impl p q)	= not (valor s p) || valor s q
valor s (Equi p q)	= (not (valor s p) || valor s q) && (not (valor s q) || valor s p)


bits :: Int -> [[Bool]]
bits 0 = [[]]
bits n = [b:bs | bs<-bits (n-1), b<-[False, True]]
-- bits 1 = [b:bs | bs<-bits 0, b<-[False, True]]
-- [False:[], True:[]] = [[False], [True]]
--
-- bits 2 = [b:bs | bs<-bits 1, b<-[False, True]]
-- [False:[False], True:[False], False:[True], True:[True]] = 
--					[[False, False], [True, False], [False, True], [True, True]]

vars :: Prop -> [Char]
vars (Const _)	= []
vars (Var x)	= [x]
vars (Neg p)	= vars p
vars (Conj p q)	= vars p ++ vars q
vars (Disj p q)	= vars p ++ vars q
vars (Impl p q)	= vars p ++ vars q
vars (Equi p q) = vars p ++ vars q

atribs :: Prop -> [Atrib]
atribs p = map (zip vs) (bits (length vs))
			where vs = nub (vars p)

tautologia :: Prop -> Bool
tautologia p = and [valor s p | s<-atribs p]

tornaFalso p = [s | s<-atribs p, (valor s p)==False]


----- 3)

data Expr = Val Int
		  | Soma Expr Expr
		  | Mult Expr Expr

data Op = AVAL_S Expr
	    | AVAL_M Expr
	    | SOMA Int
	    | MULT Int

type Cont = [Op]

aval :: Expr -> Cont -> Int
aval (Val n) c = exec c n
aval (Soma x y) c = aval x (AVAL_S y : c)
aval (Mult x y) c = aval x (AVAL_M y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (AVAL_S y : c) n = aval y (SOMA n : c)
exec (AVAL_M y : c) n = aval y (MULT n : c)
exec (SOMA n : c) m = exec c (n+m)
exec (MULT n : c) m = exec c (n*m)

valor' :: Expr -> Int
valor' e = aval e []

