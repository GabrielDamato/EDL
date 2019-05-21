data Arvore = Folha | Galho Arvore Arvore

a1 = Galho (Galho Folha Folha) (Galho Folha Folha)
a2 = Galho (Galho (Galho Folha Folha) Folha) Folha
a3 = Galho Folha (Galho (Galho Folha Folha) (Galho Folha Folha))
 
folhas :: Arvore -> Int
folhas Folha = 1
folhas (Galho l r) = (folhas l) + (folhas r)

altura :: Arvore -> Int
altura Folha = 0
altura (Galho l r) = if (altura l) > (altura r) then 1 + (altura l) else 1 + (altura r)  

espelho :: Arvore -> Arvore
espelho Folha = Folha
espelho (Galho l r) = Galho (espelho r) (espelho l)


main = do print(folhas a3)
          print(altura a3)
          print(espelho a3)