## TODO Verifier les index du keyMap et les appel dans les fonction lignes 18, 26, 43, 52, 62, 67
## TODO dans la fonction strat_priorite_irrigation choisir ce qu'on renvois, actuellement le plus frequant


## Le keymap du model renvoie les valeurs dans cette ordre : 

# 1 capitalini,
#2 parcellesini,
#3profnappe, 
#4capital, 
#5prelevtot, 
#6tot_vente_parcelles,
#7tot_achat_parcelles,
#8profmax,
#9totoignon,
#10totchou,
#11totaubergine
#12totpiment,
#13totpdt
#14totcarotte
#15nbparcelle_noncultivee,
#16totlance,
#17totgag,
#18totseau
#19rangpartie1,
#20rangpartie2,
#21rangpartie3
#22parce_empeche1
#23parce_empeche2
#24parce_empeche3

# Définir une fonction 'strat_culture' 
# part de chou/oignon ou pomme de terre sur total
# qui prend 'mylist', une liste de tableaux 
# 3D (ou une liste de matrices), comme argument.
strat_culture <- function(mylist){
  # Extraire les 9e, 10e et 13e colonne du premier élément de 'mylist'
  # c.a.d chou/oignon ou pomme de terre, et les
  # stocker dans une nouvelle liste 'a'.
  a <- list(mylist[[1]][,,c(9,10,13)])
  
  # Utiliser 'apply' sur un array simplifié de 'a' pour sommer les valeurs 
  # correspondantes dans chaque cellule des matrices et stocker le résultat dans 'SumListA'.
  SumListA <- apply(simplify2array(a), c(1,2), sum)
  
  # Extraire les 9e à 14e colonnes du premier élément de 'mylist' et les 
  # stocker dans une nouvelle liste 'b'.
  b <- list(mylist[[1]][,,c(9,10,11,12,13,14)])
  
  # Utiliser 'apply' sur un array simplifié de 'b' pour sommer les valeurs 
  # correspondantes dans chaque cellule des matrices de 9e à 14e, et stocker le 
  # résultat dans 'SumListB'.
  SumListB <- apply(simplify2array(b), c(1,2), sum)
  
  # Calculer le rapport de 'SumListA' sur 'SumListB' pour chaque cellule correspondante et retourner ce résultat.
  return(SumListA / SumListB)
}


# Définir une fonction 'strat_parcelle' 
# nombre parcelles cultivées sur les trois tours (seuil : 6 ou 12)
# Sum(p.totoignon, p.totchou, p.totaubergine,p.totpiment,p.totpdt,p.totcarotte)
strat_parcelle <- function(mylist){
  a <- list(mylist[[1]][,,c(9:14)])
  SumListA <- apply(simplify2array(a), c(1,2), sum)
  return(SumListA)
}

## Une analyse de flux de parcelles

## parcelle achetée - parcelles vendue
strat_dynFoncier <- function(mylist){
  a <- list(mylist[[1]][,,6])
  b <- list(mylist[[1]][,,7])
  m <- simplify2array(b) - simplify2array(a)
  # s <- rowSums(abs(simplify2array(m)))
  return(as.data.frame(m))
}

# Définir une fonction 'strat_techno' goute a goute
#  prend en argument 'mylist', une liste de tableaux 3D (ou une liste de matrices).
strat_gg <- function(mylist){
  # Extraire la 17e colonne du premier élément de 'mylist' et la stocker dans une nouvelle liste 'a'.
  a <- mylist[[1]][,,17]
  # Retourner la liste 'a'.
  return(unlist(simplify2array(a)))
}

# Définir une fonction 'strat_lance' qui prend en argument 'mylist', une liste de tableaux 3D (ou une liste de matrices).
strat_lance <- function(mylist){
  # Extraire la 16e collone du premier élément de 'mylist' et la stocker dans une nouvelle liste 'a'.
  a <- mylist[[1]][,,16]
  # Retourner la liste 'a'.
  return(simplify2array(a))
}

# Définir une fonction 'strat_seau' qui prend en argument 'mylist', une liste de tableaux 3D (ou une liste de matrices).
strat_seau <- function(mylist){
  # Extraire la 16e colonne du premier élément de 'mylist' et la stocker dans une nouvelle liste 'a'.
  a <- mylist[[1]][,,18]
  # Retourner la liste 'a'.
  return(simplify2array(a))
}

# Définir une fonction 'strat_priorite_irrigation' qui prend en argument 'mylist', 
# une liste de tableaux 3D (ou une liste de matrices).
strat_priorite_irrigation <- function(mylist){
  # Extraire les colonnes 19 à 21 du premier élément de 'mylist' et les stocker dans une nouvelle liste 'a'.
  a <- list(mylist[[1]][,,c(19:21)])
  
  # Utiliser 'apply' sur un array simplifié de 'a' pour calculer la médiane des valeurs correspondantes dans chaque cellule des matrices 19 à 21, et stocker le résultat dans 'medListA'.
  medListA <- apply(simplify2array(a), c(1,2), median)
  
  # Utiliser 'apply' sur un array simplifié de 'a' pour calculer la moyenne des valeurs correspondantes dans chaque cellule des matrices 19 à 21, et stocker le résultat dans 'averageA'.
  averageA <- apply(simplify2array(a), c(1,2), mean)
  
  # Définir une fonction 'mostFreq' qui trouve et retourne la valeur la plus fréquente (le mode) dans un vecteur 'x'.
  mostFreq <- function(x){sort(table(x),decreasing=T)[1]}
  
  # Utiliser 'apply' sur un array simplifié de 'a' pour appliquer 'mostFreq' à chaque cellule à travers les matrices 19 à 21, et stocker le résultat dans 'freqListA'.
  freqListA <- apply(simplify2array(a), c(1,2), mostFreq)
  return(averageA)
}


# Définir une fonction capital qui prend en argument 'mylist', une liste de tableaux 3D (ou une liste de matrices).
capital <- function(mylist){
  # Extraire la 4e colonne du premier élément de 'mylist' et la stocker dans une nouvelle liste 'a'.
  a <- mylist[[1]][,,4]
  # Retourner la liste 'a'.
  return(simplify2array(a))
}

# Définir une fonction "profnappe" qui prend en argument 'mylist', une liste de tableaux 3D (ou une liste de matrices).
profnappe <- function(mylist){
  # Extraire la 16e colonne du premier élément de 'mylist' et la stocker dans une nouvelle liste 'a'.
  a <- mylist[[1]][,,3]
  # Retourner la liste 'a'.
  return(simplify2array(a))
}


# Définir une fonction "prelevtot" qui prend en argument 'mylist', une liste de tableaux 3D (ou une liste de matrices).
prelevement <- function(mylist){
  # Extraire la 5e colonne du premier élément de 'mylist' et la stocker dans une nouvelle liste 'a'.
  a <- mylist[[1]][,,5]
  # Retourner la liste 'a'.
  return(simplify2array(a))
}

# Définir une fonction "profmax" qui prend en argument 'mylist', une liste de tableaux 3D (ou une liste de matrices).
puits <- function(mylist){
  # Extraire la 8e colonne du premier élément de 'mylist' et la stocker dans une nouvelle liste 'a'.
  a <- mylist[[1]][,,8]
  # Retourner la liste 'a'.
  return(simplify2array(a))
}


# Compter le nb de fois où le joueur a été empêché de puiser (2 saisons par parcelle max)
# sum de 

strat_empechement <- function(mylist){
  a <- list(mylist[[1]][,,c(22:24)])
  SumListA <- apply(simplify2array(a), c(1,2), sum)
  return(SumListA)
}

