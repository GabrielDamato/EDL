data Arvore = Folha | Galho Int Arvore Arvore
    deriving Show

a1 = Galho 1 (Galho 2 Folha Folha) (Galho 3 Folha Folha)
a2 = Galho 1 (Galho 2 (Galho 3 Folha Folha) Folha) Folha
-- a3 respeita as propriedades de arvore binaria de busca
a3 = Galho 2 (Galho 1 Folha Folha) (Galho 4 (Galho 3 Folha Folha) (Galho 5 Folha Folha))
 
folhas :: Arvore -> Int
folhas Folha = 1
folhas (Galho v l r) = (folhas l) + (folhas r)

altura :: Arvore -> Int
altura Folha = 0
altura (Galho v l r) = if (altura l) > (altura r) then 1 + (altura l) else 1 + (altura r)  

espelho :: Arvore -> Arvore
espelho Folha = Folha
espelho (Galho v l r) = Galho v (espelho r) (espelho l)

soma :: Arvore -> Int
soma Folha = 0
soma (Galho v l r) = (soma l) + (soma r) + v

dobra :: Arvore -> Arvore
dobra Folha = Folha
dobra (Galho v l r) = Galho (v*2) (dobra l) (dobra r)

possui :: Arvore -> Int -> Bool
possui Folha n = False
possui (Galho v l r) n = if v==n then True 
                        else if (possui l n) == True then True
                        else (possui r n)

possui_busca :: Arvore -> Int -> Bool
possui_busca Folha n = False
possui_busca (Galho v l r) n = if n == v then True
                              else if (n < v) then (possui l n)
                              else (possui r n)

-- Outra sintaxe
--possui_busca (Galho v l r) n | n == v = True
                            -- | n < v = (possui_busca l n)
                            -- | n > v = (possui_busca r n)

maximo :: Arvore -> Int
maximo (Galho v l Folha) = v
maximo (Galho v l r) = maximo r

insere :: Arvore -> Int -> Arvore
insere (Galho v Folha Folha) n = if n >= v then Galho v Folha (Galho n Folha Folha)
                                else Galho v (Galho n Folha Folha) Folha
insere (Galho v l Folha) n = Galho v l (Galho n Folha Folha) 
insere (Galho v Folha r) n = Galho v (Galho n Folha Folha) r
insere (Galho v l r) n = if n >= v then Galho v l (insere r n) 
                        else Galho v (insere l n) r

main = do putStrLn ("\nFolhas:\n")
          print(folhas a3)
          putStrLn ("\nAltura:\n")
          print(altura a3)
          putStrLn ("\nespelho:\n")
          print(espelho a3)
          putStrLn ("\nSoma:\n")
          print(soma a1)
          putStrLn ("\nDobra:\n")
          print(dobra a1)
          putStrLn ("\nPossui:\n")
          print(possui a3 2)
          putStrLn ("\nPossui Arvore Binaria de Busca:\n")
          print(possui_busca a3 2)
          putStrLn ("\nMaximo:\n")
          print(maximo a1)
          putStrLn ("\nInsere:\n")
          print(insere a3 0)