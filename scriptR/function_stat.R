## TODO Verifier les index du keyMap et les appel dans les fonction lignes 18, 26, 43, 52, 62, 67
## TODO dans la fonction strat_priorite_irrigation choisir ce qu'on renvois, actuellement le plus frequant


## Le keymap du model renvoie les valeurs dans cette ordre : 
## List|of(
## [1]  profnappe, p.capital,   p.prelevtot,              p.tot_vente_parcelles, p.tot_achat_parcelles,
## [6]  p.profmax, p.totoignon, p.totchou,                p.totaubergine,        p.totpiment, 
## [11] p.totpdt,  p.totcarotte,p.nbparcelle_noncultivee, p.totlance,            p.totgag,
## [16] p.totseau, p.rangpartie1,p.rangpartie2,           p.rangpartie3)

# Définir une fonction 'strat_culture' 
# part de chou/oignon ou pomme de terre sur total
# qui prend 'mylist', une liste de tableaux 
# 3D (ou une liste de matrices), comme argument.
strat_culture <- function(mylist){
  # Extraire les 8e, 9e et 12e colonne du premier élément de 'mylist'
  # c.a.d chou/oignon ou pomme de terre, et les
  # stocker dans une nouvelle liste 'a'.
  a <- list(mylist[[1]][,,c(7,8,11)])
  
  # Utiliser 'apply' sur un array simplifié de 'a' pour sommer les valeurs 
  # correspondantes dans chaque cellule des matrices et stocker le résultat dans 'SumListA'.
  SumListA <- apply(simplify2array(a), c(1,2), sum)
  
  # Extraire les 8e à 13e colonnes du premier élément de 'mylist' et les 
  # stocker dans une nouvelle liste 'b'.
  b <- list(mylist[[1]][,,c(7:12)])
  
  # Utiliser 'apply' sur un array simplifié de 'b' pour sommer les valeurs 
  # correspondantes dans chaque cellule des matrices de 8e à 13e, et stocker le 
  # résultat dans 'SumListB'.
  SumListB <- apply(simplify2array(b), c(1,2), sum)
  
  # Calculer le rapport de 'SumListA' sur 'SumListB' pour chaque cellule correspondante et retourner ce résultat.
  return(SumListA / SumListB)
}


# Définir une fonction 'strat_parcelle' 
# nombre parcelles cultivées sur les trois tours (seuil : 6 ou 12)
# Sum(p.totoignon, p.totchou, p.totaubergine,p.totpiment,p.totpdt,p.totcarotte)
strat_parcelle <- function(mylist){
  a <- list(mylist[[1]][,,c(7:12)])
  SumListA <- apply(simplify2array(a), c(1,2), sum)
  return(SumListA)
}

# Définir une fonction 'strat_techno' goute a goute
#  prend en argument 'mylist', une liste de tableaux 3D (ou une liste de matrices).
strat_gg <- function(mylist){
  # Extraire la 15e colonne du premier élément de 'mylist' et la stocker dans une nouvelle liste 'a'.
  a <- mylist[[1]][,,15]
  # Retourner la liste 'a'.
  return(unlist(simplify2array(a)))
}

# Définir une fonction 'strat_lance' qui prend en argument 'mylist', une liste de tableaux 3D (ou une liste de matrices).
strat_lance <- function(mylist){
  # Extraire la 14e collone du premier élément de 'mylist' et la stocker dans une nouvelle liste 'a'.
  a <- mylist[[1]][,,14]
  # Retourner la liste 'a'.
  return(simplify2array(a))
}

# Définir une fonction 'strat_seau' qui prend en argument 'mylist', une liste de tableaux 3D (ou une liste de matrices).
strat_seau <- function(mylist){
  # Extraire la 16e colonne du premier élément de 'mylist' et la stocker dans une nouvelle liste 'a'.
  a <- mylist[[1]][,,16]
  # Retourner la liste 'a'.
  return(simplify2array(a))
}

# Définir une fonction 'strat_priorite_irrigation' qui prend en argument 'mylist', une liste de tableaux 3D (ou une liste de matrices).
strat_priorite_irrigation <- function(mylist){
  # Extraire les colonnes 17 à 19 du premier élément de 'mylist' et les stocker dans une nouvelle liste 'a'.
  a <- list(mylist[[1]][,,c(17:19)])
  
  # Utiliser 'apply' sur un array simplifié de 'a' pour calculer la médiane des valeurs correspondantes dans chaque cellule des matrices 17 à 19, et stocker le résultat dans 'medListA'.
  medListA <- apply(simplify2array(a), c(1,2), median)
  
  # Utiliser 'apply' sur un array simplifié de 'a' pour calculer la moyenne des valeurs correspondantes dans chaque cellule des matrices 17 à 19, et stocker le résultat dans 'averageA'.
  averageA <- apply(simplify2array(a), c(1,2), mean)
  
  # Définir une fonction 'mostFreq' qui trouve et retourne la valeur la plus fréquente (le mode) dans un vecteur 'x'.
  mostFreq <- function(x){sort(table(x),decreasing=T)[1]}
  
  # Utiliser 'apply' sur un array simplifié de 'a' pour appliquer 'mostFreq' à chaque cellule à travers les matrices 17 à 19, et stocker le résultat dans 'freqListA'.
  freqListA <- apply(simplify2array(a), c(1,2), mostFreq)
  return(freqListA)
}


# Définir une fonction capital qui prend en argument 'mylist', une liste de tableaux 3D (ou une liste de matrices).
capital <- function(mylist){
  # Extraire la 16e colonne du premier élément de 'mylist' et la stocker dans une nouvelle liste 'a'.
  a <- mylist[[1]][,,2]
  # Retourner la liste 'a'.
  return(simplify2array(a))
}

# Définir une fonction "profnappe" qui prend en argument 'mylist', une liste de tableaux 3D (ou une liste de matrices).
profnappe <- function(mylist){
  # Extraire la 16e colonne du premier élément de 'mylist' et la stocker dans une nouvelle liste 'a'.
  a <- mylist[[1]][,,1]
  # Retourner la liste 'a'.
  return(simplify2array(a))
}


# Définir une fonction "prelevtot" qui prend en argument 'mylist', une liste de tableaux 3D (ou une liste de matrices).
prelevement <- function(mylist){
  # Extraire la 16e colonne du premier élément de 'mylist' et la stocker dans une nouvelle liste 'a'.
  a <- mylist[[1]][,,3]
  # Retourner la liste 'a'.
  return(simplify2array(a))
}

# Définir une fonction "profmax" qui prend en argument 'mylist', une liste de tableaux 3D (ou une liste de matrices).
puits <- function(mylist){
  # Extraire la 16e colonne du premier élément de 'mylist' et la stocker dans une nouvelle liste 'a'.
  a <- mylist[[1]][,,6]
  # Retourner la liste 'a'.
  return(simplify2array(a))
}

# Définir une fonction "agrandissement" qui prend en argument 'mylist', une liste de tableaux 3D (ou une liste de matrices).
prelevtot <- function(mylist){
  # Extraire la 16e colonne du premier élément de 'mylist' et la stocker dans une nouvelle liste 'a'.
  a <- mylist[[1]][,,c(4:5)]
  
  # A FAIRE : COMMENT ON CODE UNE SOUSTRACTION ENTRE LES VARIABLES 4 ET 5?
  # Retourner la liste 'a'.
  return(simplify2array(a))
}

