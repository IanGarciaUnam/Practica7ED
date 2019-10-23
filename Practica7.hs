data Nat = Cero | Suc Nat deriving (Show)

suma :: Nat -> Nat -> Nat
suma Cero n = n
suma (Suc x) n = Suc (suma x n)

multiplica :: Nat -> Nat -> Nat
multiplica Cero _ = Cero
multiplica (Suc x) n = suma (multiplica x n) n

fibonacci :: Nat -> Nat
fibonacci Cero = Cero
fibonacci (Suc Cero)= Suc Cero
fibonacci (Suc (Suc x)) = suma (fibonacci (Suc x))(fibonacci x)

sumaLista :: [Int] -> Int
sumaLista [] = 0
sumaLista (x:xs) = x + (sumaLista xs)

resta :: Nat -> Nat -> Nat
resta n Cero = n
resta Cero _ = error "Ne pas"
resta (Suc n) (Suc m) = resta n m

toNat :: Int -> Nat
toNat 0 = Cero
toNat 1 = Suc (Cero)
toNat x = Suc (toNat (x-1))

factorial :: Nat -> Nat
factorial (Cero)=Cero
factorial (Suc Cero) = Suc(Cero)
factorial x = multiplica x (factorial (resta x (Suc Cero)))

potencia :: Nat -> Nat -> Nat
potencia x Cero = Suc Cero
potencia x  y = multiplica x (potencia x (resta y (Suc Cero)))
