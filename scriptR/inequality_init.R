###############################################################################
#                    Travail sur des indicateur d'inequité
# On veux ici crée un dataframe qui servira d'entré à OpenMole pour générer
# toutes les conditions de capital et de parcelles qui soit compatible avec
# les conditions d'inequité que l'on a observer dans les parties du jeu f'eau diem
###############################################################################


rm(list = ls())
###############################################################################
# Installation et chargement du package ineq (si non déjà installé)
###############################################################################
# install.packages("ineq")
library(ineq)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- read.csv('../data/inti_parties.csv')


ineq(data[1,2:5], type = "Gini")
# On applique la fonction ineq(...) à chaque ligne, uniquement sur les colonnes 2:5
data$gini_capital <- apply(data[, 2:5], 1, function(x) ineq(x, type = "Gini"))
data$gini_parcelles <- apply(data[, 6:9], 1, function(x) ineq(x, type = "Gini"))
data$sum_parcelles <- apply(data[, 6:9], 1, function(x) sum(x))
data$sum_capital <- apply(data[, 2:5], 1, function(x) sum(x))

range(data$gini_capital) #[1] 0.1171875 0.1800000
range(data$gini_parcelles) #[1] 0.10 0.25
range(data$sum_capital) #[1] 50 64
unique(c(unique(data$cap1),unique(data$cap2),unique(data$cap3),unique(data$cap4)))

###############################################################################
#                             PARCELLES
# Génération de l'espace de recherche
# Ici, on considère tous les quadruplets d'entiers entre 0 et 10
###############################################################################
all_combos <- expand.grid(rep(list(1:3), 4))
# 'all_combos' est un data frame dont chaque ligne est un quadruplet (x1, x2, x3, x4)

# On calcule les sommes de chaque ligne
somme_par_ligne <- rowSums(all_combos)

# On construit la condition : 7 <= somme <= 10
sel <- (somme_par_ligne >= 7) & (somme_par_ligne <= 10)

# On sélectionne les lignes vérifiant cette condition
all_combos <- all_combos[sel, ]

###############################################################################
# Calcul du Gini pour chaque quadruplet
###############################################################################
# apply(...) sur les lignes : on transforme chaque ligne en vecteur et on calcule le Gini
all_combos$gini_vals <- apply(all_combos, 1, function(x) ineq(x, type = "Gini"))

###############################################################################
# Recherche des solutions dans les bornes idenitifié dans les données Réels
###############################################################################

# Indices des lignes qui satisfont la condition
sel <- all_combos$gini_vals >= 0.10 & all_combos$gini_vals <= 0.25

# On récupère les quadruplets correspondants
combinaisons_parcelles <- all_combos[sel, , drop = FALSE]
combinaisons_parcelles <- combinaisons_parcelles[,1:4]
colnames(combinaisons_parcelles)[1:4] <- c('surf1','surf2','surf3','surf4')
rownames(combinaisons_parcelles) <- NULL

###############################################################################
#                             CAPiTAL
# Génération de l'espace de recherche
# Ici, on considère tous les quadruplets d'entiers entre 0 et 10
###############################################################################
# différentes valeurs qu'on pris le capital dans les parties irl
vec <- c(20,11,8,18,15,10) # 

# Générer toutes les combinaisons (au sens "cartésien")
all_combos <- expand.grid(rep(list(vec), 4))
# 'all_combos' est un data frame dont chaque ligne est un quadruplet (x1, x2, x3, x4)

# On calcule les sommes de chaque ligne
somme_par_ligne <- rowSums(all_combos)

# On construit la condition : 7 <= somme <= 10
sel <- (somme_par_ligne >= 50) & (somme_par_ligne <= 64)

# On sélectionne les lignes vérifiant cette condition
all_combos <- all_combos[sel, ]

###############################################################################
# Calcul du Gini pour chaque quadruplet
###############################################################################
# apply(...) sur les lignes : on transforme chaque ligne en vecteur et on calcule le Gini
all_combos$gini_vals <- apply(all_combos, 1, function(x) ineq(x, type = "Gini"))

###############################################################################
# Recherche des solutions dans les bornes idenitifié dans les données Réels
###############################################################################

# Indices des lignes qui satisfont la condition
sel <- all_combos$gini_vals >= 0.11 & all_combos$gini_vals <= 0.18

# On récupère les quadruplets correspondants
combinaisons_capital <- all_combos[sel, , drop = FALSE]
combinaisons_capital <- combinaisons_capital[,1:4]
colnames(combinaisons_capital)[1:4] <- c("cap1","cap2","cap3","cap4")
rownames(combinaisons_capital) <- NULL


###############################################################################
#                             ASSEMBLAGE
# Génération de l'espace de recherche
# ICI on veux crée un dataframe avec toutes les combinaisons possible de 
# capital et de surface
###############################################################################

# 1) Générer toutes les paires d'indices de lignes
grid_rows <- expand.grid(
  row_parcelles   = seq_len(nrow(combinaisons_parcelles)),# 1..56
  row_capital = seq_len(nrow(combinaisons_capital))   # 1..492
)

# 2) Pour chaque paire d'indices (i, j),
#    on concatène la ligne i de df1 avec la ligne j de df2
combinaisons_totales <- cbind(
  combinaisons_parcelles[grid_rows$row_parcelles, ],
  combinaisons_capital[grid_rows$row_capital,]
)
sum(is.na(combinaisons_totales))

write.csv(combinaisons_totales,file = "../data/init_partie_combinatoire_totale.csv", row.names = F)

