type Autores = [String]
type Musica  = (String, Int, Int)


bandas :: [Autores]
bandas = [ ["Gilberto Gil"],
           ["Victor","Leo"],
           ["Gonzagao"],
           ["Claudinho","Bochecha"] ]

musicas :: [Musica]
musicas = [ ("Aquele Abraco", 1, 100),
            ("Esperando na Janela", 1, 150),
            ("Borboletas", 2, 120),
            ("Asa Branca", 3, 120),
            ("Assum Preto", 3, 140),
            ("Vem Morena", 3, 200),
            ("Nosso Sonho", 4, 150),
            ("Quero te Encontrar", 4, 100) ]

nome (nm,_,_) = nm
musica_dois (nm, v1, v2) = v2 >= 120
musica_maior (nm, v1, v2) maior = if v2 > maior then v2 else maior

pretty (nm, v1, v2) = (nm, bandas!!(v1-1), v2)
formata_autores a saida = a ++ " " ++ saida 
formata (nm, a, v2) saida = "Nome: " ++ nm ++ "\nAutores: " ++ foldr formata_autores "" a ++ "\nDuracao: " ++ show v2 ++ "\n\n" ++ saida
pretty_print = (foldr formata "" (map pretty musicas))

main = do print (map nome musicas)
          putStrLn ("\n")  
          print (filter musica_dois musicas)
          putStrLn ("\n")
          print (foldr musica_maior 0 musicas)
          putStrLn ("\n") 
          print (map nome (filter musica_dois musicas))
          putStrLn ("\n")
          putStrLn (pretty_print)