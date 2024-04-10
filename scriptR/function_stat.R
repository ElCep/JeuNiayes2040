
# Définir une fonction 'strat_culture' qui prend 'mylist', une liste de tableaux 
# 3D (ou une liste de matrices), comme argument.
strat_culture <- function(mylist){
  # Extraire les 8e, 9e et 12e colonne du premier élément de 'mylist' et les
  # stocker dans une nouvelle liste 'a'.
  a <- list(mylist[[1]][,,c(8,9,12)])
  
  # Utiliser 'apply' sur un array simplifié de 'a' pour sommer les valeurs 
  # correspondantes dans chaque cellule des matrices et stocker le résultat dans 'SumListA'.
  SumListA <- apply(simplify2array(a), c(1,2), sum)
  
  # Extraire les 8e à 13e colonnes du premier élément de 'mylist' et les 
  # stocker dans une nouvelle liste 'b'.
  b <- list(mylist[[1]][,,c(8:13)])
  
  # Utiliser 'apply' sur un array simplifié de 'b' pour sommer les valeurs 
  # correspondantes dans chaque cellule des matrices de 8e à 13e, et stocker le 
  # résultat dans 'SumListB'.
  SumListB <- apply(simplify2array(b), c(1,2), sum)
  
  # Calculer le rapport de 'SumListA' sur 'SumListB' pour chaque cellule correspondante et retourner ce résultat.
  return(SumListA / SumListB)
}

