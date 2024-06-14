# analyse des stratégies a partir des parties simulées

rm(list = ls())
library(ineq)
library(Hmisc)
library(corrplot)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source(file = "createTablePartie.R")

#tabparties<-tabparties_irl

### ANALYSES  #############
#♥Matrice de corrélation
mcor <- cor(tabparties[,2:5], use="complete.obs")
png(filename = "../results/partie/sim/fichier_correlation.png", width = 800, height = 600)
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)
dev.off()

# GRAPHIQUE GINI PRELEV- GINI CAPITAL
png(filename = "../results/partie/sim/ginicap_giniprelev.png", width = 800, height = 600)
plot(x = tabparties$giniprelev, y = tabparties$ginicap, 
     xlab = "GINI des prélèvements", ylab = "GINI du capital", 
     main = "Relation entre le GINI des prélèvements et le GINI du capital",
     col = "blue", pch = 16)
dev.off()


# GRAPHIQUE GINI CAPITAL - tot capital partir
png(filename = "../results/partie/sim/ginicap_VA.png", width = 800, height = 600)
plot(x = tabparties$ginicap, y = tabparties$sum_cap, 
     xlab = "GINI du capital", ylab = "valeur ajoutée tot", 
     main = "Relation entre le GINI du capital valeur ajoutée tot",
     col = "blue", pch = 16)
dev.off()


# GRAPHIQUE CAPITAL tot - prelevement
png(filename = "../results/partie/sim/VA_profnappe.png", width = 800, height = 600)
plot(x = tabparties$sum_cap, y = tabparties$profnappe, 
     xlab = "sumcap", ylab = "profnappe", 
     main = "Relation entre entre valeur ajoutée tot et la profondeur de la nappe",
     col = "blue", pch = 16)
dev.off()

# GRAPHIQUE GINI prelev - prelevement
png(filename = "../results/partie/sim/giniprelev_profnappe.png", width = 800, height = 600)
plot(x = tabparties$giniprelev, y = tabparties$profnappe, 
     xlab = "giniprelev", ylab = "profnappe", 
     main = "Relation entre le GINI des prélèvements etla profondeur de la nappe ",
     col = "blue", pch = 16)
dev.off()

# GRAPHIQUE GINI prelev - sumcap

png(filename = "../results/partie/sim/giniprelev_VA_giniVA.png", width = 800, height = 600)

color_palette <- colorRampPalette(c("blue", "red"))

colors <- color_palette(100)[cut(as.numeric(tabparties$ginicap), breaks = 100)]

plot(x = tabparties$giniprelev, y = tabparties$sum_cap, 
     xlab = "giniprelev", ylab = "sum_cap", 
     main = "Relation entre le GINI des prélèvements et sum_cap ",
      col = colors, pch = 16)

par(xpd = TRUE) # Permet d'afficher la légende en dehors de la zone du graphique
legend("topright", legend = seq(0, 1, length.out = 3), fill = color_palette(3), 
       title = "GINI du capital", cex = 0.8, bty = "n", inset = c(0, -0.1))

dev.off()

# GRAPHIQUE GINI CAPITAL - tot capital partir
# Créer une palette de couleurs graduelle de bleu à rouge

png(filename = "../results/partie/sim/giniVA_profnappe_VA.png", width = 800, height = 600)

color_palette <- colorRampPalette(c("blue", "red"))
# Convertir les valeurs de "ginicap" en couleurs selon la palette graduelle
colors <- color_palette(100)[cut(as.numeric(tabparties$sum_cap), breaks = 100)]
mincap<-min(tabparties$sum_cap)
maxcap<-max(tabparties$sum_cap)
plot(x = tabparties$ginicap, y = tabparties$profnappe, 
     xlab = "GINI du capital", ylab = "profnappe", 
     main = "Relation entre le GINI du capital et profnappe",
     col = colors, pch = 16)

par(xpd = TRUE) # Permet d'afficher la légende en dehors de la zone du graphique
legend("topright", legend = seq(mincap, maxcap, length.out = 5), fill = color_palette(5), 
       title = "sum_capital", cex = 0.8, bty = "n", inset = c(0, -0.1))
dev.off()


# GRAPHIQUE GINI CAPITAL - tot capital partir
# Créer une palette de couleurs graduelle de bleu à rouge

png(filename = "../results/partie/sim/VA_profnappe_Gini VA.png", width = 800, height = 600)
#png(filename = "../results/partie/VA_profnappe_Gini VA.png", width = 800, height = 600)

color_palette <- colorRampPalette(c("blue", "red"))

# Convertir les valeurs de "ginicap" en couleurs selon la palette graduelle
colors <- color_palette(100)[cut(as.numeric(tabparties$ginicap), breaks = 100)]

mincap<-min(tabparties$ginicap, na.rm = TRUE)
maxcap<-max(tabparties$ginicap, na.rm = TRUE)
plot(x = tabparties$sum_cap, y = tabparties$profnappe, 
     xlab = "sum du capital", ylab = "profnappe", 
     main = " VA/profnappe / Gini VA",
     col = colors, pch = 16)

par(xpd = TRUE) # Permet d'afficher la légende en dehors de la zone du graphique
legend("topright", legend = seq(mincap, maxcap, length.out = 5), fill = color_palette(5), 
       title = "gini_capital", cex = 0.8, bty = "n", inset = c(0, -0.1))
dev.off()



### ANALYSE  acp#############
library(dplyr)
library(FactoMineR)
library(factoextra)
dev.off()
res.pca <- PCA(tabparties[,c(-1)], scale.unit=TRUE, ncp=2, graph=T)
#res.pca <- PCA(tabparties_irl[,c(-1)], scale.unit=TRUE, ncp=2, graph=T)

png(filename = "../results/partie/sim/acp_var.png", width = 800, height = 600)

fviz_pca_var(res.pca, col.var = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE )
dev.off()

png(filename = "../results/partie/sim/acp_indiv.png", width = 800, height = 600)

fviz_pca_ind(res.pca, axes = c(1,2), geom.ind = "point", 
             addEllipses = TRUE, legend.title = "idividu")
dev.off()


png(filename = "../results/partie/sim/acp_indiv_var.png", width = 800, height = 600)
fviz_pca_biplot(res.pca, label ="var", col.ind="cos2") +
       theme_minimal()
dev.off()









###########################Essai irl

# Charger les bibliothèques nécessaires
library(reshape2)
library(ggplot2)



# Créer un graphique à points avec ggplot2
ggplot() +
  geom_point(data = tabparties, aes(x = sum_cap, y = profnappe), colour = "grey") + # Ajouter les points pour les données du joueur simulé
  geom_point(data = tabparties_irl, aes(x = sum_cap, y = profnappe), colour = "black", size = 3) + # Ajouter les points pour les données du joueur réel
  #xlim(c(0, 200)) + # Définir les limites de l'axe des x
  theme_bw() + # Appliquer un thème de type "black and white"
  labs(x = "capital", y = "prof nappe", title = "Joueurs réel et joueurs virtuels") # Ajouter des étiquettes et un titre 



# Créer un graphique à points avec ggplot2
ggplot() +
  geom_point(data = tabparties, aes(x = sum_cap, y = prelevements), colour = "grey") + # Ajouter les points pour les données du joueur simulé
  geom_point(data = tabparties_irl, aes(x = sum_cap, y = prelevements), colour = "black", size = 3) + # Ajouter les points pour les données du joueur réel
  #xlim(c(0, 200)) + # Définir les limites de l'axe des x
  theme_bw() + # Appliquer un thème de type "black and white"
  labs(x = "capital", y = "prelevements", title = "Joueurs réel et joueurs virtuels") # Ajouter des étiquettes et un titre 


# Créer un graphique à points avec ggplot2
ggplot() +
  geom_point(data = tabparties, aes(x = sum_cap, y = ginicap), colour = "grey") + # Ajouter les points pour les données du joueur simulé
  geom_point(data = tabparties_irl, aes(x = sum_cap, y = ginicap), colour = "black", size = 3) + # Ajouter les points pour les données du joueur réel
  #xlim(c(0, 200)) + # Définir les limites de l'axe des x
  theme_bw() + # Appliquer un thème de type "black and white"
  labs(x = "capital", y = "gini_cap", title = "Joueurs réel et joueurs virtuels") # Ajouter des étiquettes et un titre 




# Créer un graphique à points avec ggplot2
ggplot() +
  geom_point(data = tabparties, aes(x = giniprelev, y = ginicap), colour = "grey") + # Ajouter les points pour les données du joueur simulé
  geom_point(data = tabparties_irl, aes(x = giniprelev, y = ginicap), colour = "black", size = 3) + # Ajouter les points pour les données du joueur réel
  #xlim(c(0, 200)) + # Définir les limites de l'axe des x
  theme_bw() + # Appliquer un thème de type "black and white"
  labs(x = "giniprelev", y = "gini_cap", title = "Joueurs réel et joueurs virtuels") # Ajouter des étiquettes et un titre 



# Créer un graphique à points avec ggplot2
ggplot() +
  geom_point(data = tabparties, aes(x = giniprelev, y = prelevements), colour = "grey") + # Ajouter les points pour les données du joueur simulé
  geom_point(data = tabparties_irl, aes(x = giniprelev, y = prelevements), colour = "black", size = 3) + # Ajouter les points pour les données du joueur réel
  #xlim(c(0, 200)) + # Définir les limites de l'axe des x
  theme_bw() + # Appliquer un thème de type "black and white"
  labs(x = "giniprelev", y = "prelevements", title = "Joueurs réel et joueurs virtuels") # Ajouter des étiquettes et un titre 






### ANALYSE  ACP simul #############
library(dplyr)
library(FactoMineR)
library(factoextra)

res.pca <- PCA(tabparties[,c(-1)], scale.unit=TRUE, ncp=2, graph=T)


png(filename = "../results/partie/irl/acp_var.png", width = 800, height = 600)

fviz_pca_var(res.pca, col.var = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE )
dev.off()

png(filename = "../results/partie/irl/acp_indiv.png", width = 800, height = 600)

fviz_pca_ind(res.pca, axes = c(1,2), geom.ind = "point", 
             addEllipses = TRUE, legend.title = "idividu")
dev.off()


png(filename = "../results/partie/irl/acp_indiv_var.png", width = 800, height = 600)
fviz_pca_biplot(res.pca, label ="var", col.ind="cos2") +
  theme_minimal()
dev.off()





## ICI

# Projections des irl dans l'espace des simuls
library(missMDA)
library(dplyr)
library(FactoMineR)
library(factoextra)
library("PerformanceAnalytics")
tabparties1<-tabparties[,c(-1)]
#chart.Correlation(tabparties, histogram=TRUE, pch=19)
tabparties1$name <- "sim"
tabparties_irl1<-tabparties_irl[,c(-1)]
tabparties_irl1$name <- "irl"



data <- rbind(tabparties1, tabparties_irl1)

# Afficher les col de data


# Liste des colonnes à tracer
cols <- c("ginicap", "giniprelev", "profnappe", "sum_cap", 
          "stratculture", "stratParcelle", "stratSeau", 
          "StratGG", "stratLance", "puits", "empechement", 
          "foncier", "prelevements")

# Boucle pour créer un graphique pour chaque colonne
for (col in cols) {
  p <- ggplot(data, aes_string(x = "name", y = col, color = "name")) +
    geom_point() +
    labs(title = paste("Graphique de", col), x = "Name", y = col) +
    scale_color_manual(values = c("irl" = "blue", "sim" = "red")) +
    theme_minimal()
  
  # Afficher le graphique
  print(p)
}



data<-data[,c(-14)]


#res.pca <- PCA(data, ind.sup = length(tabparties[,1])+1:length(tabparties_irl[,1]), graph = T)
#fviz_pca_ind(res.pca, axes = c(1,2), geom.ind = "point", 
#             addEllipses = TRUE, legend.title = "idividu")




# Assurez-vous que les indices des individus supplémentaires sont corrects
ind_sup_indices <- length(tabparties[,1]) + 1:length(tabparties_irl[,1])

# Réalisation de l'ACP
res.pca <- PCA(data, ind.sup = ind_sup_indices, graph = TRUE)

# Visualisation de l'ACP
fviz_pca_ind(res.pca, axes = c(1,2), geom.ind = "point", 
             addEllipses = TRUE, legend.title = "individu",
             geom = c("point"),
             geom.sup = c("point"),
             col.ind.sup = "red", # Couleur des points supplémentaires
             pointsize.ind.sup = 15)  # Taille des points supplémentaires

##
# source :  http://www.sthda.com/english/wiki/wiki.php?id_contents=7851
library("PerformanceAnalytics")
data("decathlon2")
decathlon2.active <- decathlon2[1:23, 1:10]
chart.Correlation(decathlon2.active[, 1:6], histogram=TRUE, pch=19)
