# analyse des stratégies a partir des parties simulées


rm(list = ls())
library(ineq)
library(Hmisc)
library(corrplot)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#simulations
df<- read.csv("../data/applatJoueur_simule_complet.csv")
#df<- read.csv("../data/applatJoueur_simule_SameInit.csv")
# Essai joueurs
real_gameSession <- read.csv("../data/gameSession_config_pluie_26.csv", sep = ";", encoding="latin1")

### PREPARATION DES DONNEES #############
# enlever le capital négatif...

df$capitalmodif <- ifelse(df$capital < 0, 0, df$capital)

agg_data_gini <- aggregate(cbind(capitalmodif, prelevement) ~ partie, data = df, FUN = function(x) {
  GINI = ineq(x)
}, na.action = na.pass)
colnames(agg_data_gini)<-c("partie","ginicap","giniprelev")
# ¨pour les vraies parties
 irl_gini <-aggregate(cbind(Capital_final,Consommation_eau)~ID.partie, data=real_gameSession,FUN = function(x) {
   GINI = ineq(x)
 }, na.action = na.pass)

 colnames(irl_gini)<-c("ID.partie","ginicap","giniprelev")

# prélèvements
 agg_data_prelevement<-aggregate(prelevement ~ partie, data = df, FUN = sum)
 irl_prelevement<- aggregate(Consommation_eau ~ ID.partie, data = real_gameSession, FUN = sum)
# somme capital
agg_data_capital<- aggregate(capital ~ partie, data = df, FUN = sum)
irl_captot <-aggregate(Capital_final ~ID.partie, data=real_gameSession,FUN = sum)

# strat Culture
agg_data_stratculture<- aggregate(stratCulture ~ partie, data = df, FUN = sum)
irl_stratculture<-aggregate(strat_culture ~ID.partie, data=real_gameSession,FUN = sum)


# strat Parcelle
agg_data_stratParcelle<- aggregate(stratParcelle ~ partie, data = df, FUN = sum)
irl_stratParcelle<- aggregate(strat_parcelle ~ ID.partie, data = real_gameSession, FUN = sum)


# stratSeau
agg_data_stratSeau<- aggregate(stratSeau ~ partie, data = df, FUN = sum)
irl_stratSeau <- aggregate(stratSeau ~ ID.partie, data = real_gameSession, FUN = sum)

# "stratGG"    
agg_data_stratGG<- aggregate(stratGG ~ partie, data = df, FUN = sum)
irl_stratGG <- aggregate(strat_gag ~ ID.partie, data = real_gameSession, FUN = sum)


#"stratLance" 
agg_data_stratLance<- aggregate(stratLance ~ partie, data = df, FUN = sum)
irl_stratlance <- aggregate(strat_lance ~ ID.partie, data = real_gameSession, FUN = sum)


#"puits"  

agg_data_puits<- aggregate(puits ~ partie, data = df, FUN = sum)
irl_puits <- aggregate(puits_profmax ~ ID.partie, data = real_gameSession, FUN = sum)


#"agrandissement"
agg_data_foncier<- aggregate(agrandissement ~ partie, data = df, FUN = sum)
irl_foncier <- aggregate(agrandissement ~ ID.partie, data = real_gameSession, FUN = sum)


#"empechement"
agg_data_empechement<- aggregate(empechement ~ partie, data = df, FUN = sum)
irl_empechement <- aggregate(empechement ~ ID.partie, data = real_gameSession, FUN = sum)


# prof nap
agg_data_prof<- aggregate(profnappe ~ partie, data = df, FUN = mean)

irl_conso <- aggregate(Consommation_eau ~ ID.partie, data = real_gameSession, FUN = sum)
irl_pluies2<- aggregate(Pluie_annee2 ~ ID.partie, data = real_gameSession, FUN = mean)
irl_pluies3<- aggregate(Pluie_annee3 ~ ID.partie, data = real_gameSession, FUN = mean)
irl_profnappe <- irl_conso*0.039-irl_pluies2*0.039-irl_pluies3*0.039
colnames(irl_profnappe)<- c("ID.partie","profnappe")
# irl : grouper les prélèvement et pluies
# =E2*0.039-I2*0.039



# Tableau final
tabparties<-cbind(agg_data_gini,agg_data_prof$profnappe,agg_data_capital$capital,
                  agg_data_stratculture$stratCulture,agg_data_stratParcelle$stratParcelle,
                  agg_data_stratSeau$stratSeau,agg_data_stratGG$stratGG,agg_data_stratLance$stratLance,
                  agg_data_puits$puits,agg_data_empechement$empechement,agg_data_foncier$agrandissement,
                  agg_data_prelevement$prelevement
                  )
                  
colnames(tabparties)<- c("partie","ginicap","giniprelev","profnappe","sum_cap","stratculture",
                         "stratParcelle","stratSeau","StratGG","stratLance","puits","empechement","foncier","prelevements")

# Tableau final irl
tabparties_irl<-cbind(irl_gini,irl_profnappe$profnappe,irl_captot$Capital_final, irl_stratculture$strat_culture, 
                      irl_stratParcelle$strat_parcelle,irl_stratSeau$stratSeau, irl_stratGG$strat_gag,
                      irl_stratlance$strat_lance, irl_puits$puits_profmax, 
                      irl_empechement$empechement,irl_foncier$agrandissement,
                      irl_prelevement$Consommation_eau
                       )


colnames(tabparties_irl)<-c("partie","ginicap","giniprelev","profnappe","sum_cap","stratculture",
                            "stratParcelle","stratSeau","StratGG","stratLance","puits","empechement","foncier","prelevements")

#tabparties<-tabparties_irl

### ANALYSES  #############
#♥Matrice de corrélation
mcor <- cor(tabparties[,2:5], use="complete.obs")
png(filename = "../results/partie/irl/fichier_correlation.png", width = 800, height = 600)
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)
dev.off()

# GRAPHIQUE GINI PRELEV- GINI CAPITAL
png(filename = "../results/partie/irl/ginicap_giniprelev.png", width = 800, height = 600)
plot(x = tabparties$giniprelev, y = tabparties$ginicap, 
     xlab = "GINI des prélèvements", ylab = "GINI du capital", 
     main = "Relation entre le GINI des prélèvements et le GINI du capital",
     col = "blue", pch = 16)
dev.off()


# GRAPHIQUE GINI CAPITAL - tot capital partir
png(filename = "../results/partie/irl/ginicap_VA.png", width = 800, height = 600)
plot(x = tabparties$ginicap, y = tabparties$sum_cap, 
     xlab = "GINI du capital", ylab = "valeur ajoutée tot", 
     main = "Relation entre le GINI du capital valeur ajoutée tot",
     col = "blue", pch = 16)
dev.off()


# GRAPHIQUE CAPITAL tot - prelevement
png(filename = "../results/partie/irl/VA_profnappe.png", width = 800, height = 600)
plot(x = tabparties$sum_cap, y = tabparties$profnappe, 
     xlab = "sumcap", ylab = "profnappe", 
     main = "Relation entre entre valeur ajoutée tot et la profondeur de la nappe",
     col = "blue", pch = 16)
dev.off()

# GRAPHIQUE GINI prelev - prelevement
png(filename = "../results/partie/irl/giniprelev_profnappe.png", width = 800, height = 600)
plot(x = tabparties$giniprelev, y = tabparties$profnappe, 
     xlab = "giniprelev", ylab = "profnappe", 
     main = "Relation entre le GINI des prélèvements etla profondeur de la nappe ",
     col = "blue", pch = 16)
dev.off()

# GRAPHIQUE GINI prelev - sumcap

png(filename = "../results/partie/irl/giniprelev_VA_giniVA.png", width = 800, height = 600)

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

png(filename = "../results/partie/irl/giniVA_profnappe_VA.png", width = 800, height = 600)

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

png(filename = "../results/partie/irl/VA_profnappe_Gini VA.png", width = 800, height = 600)
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
res.pca <- PCA(tabparties_irl[,c(-1)], scale.unit=TRUE, ncp=2, graph=T)

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









###########################Essai joueurs

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






### ANALYSE  ACP simul et projection parties irl#############
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


library(missMDA)




# Projections des irl dans l'espace des simuls

tabparties<-tabparties[,c(-1)]
tabparties_irl<-tabparties_irl[,c(-1)]

res.pca <- PCA(as.data.frame(tabparties), ind.sup = as.data.frame(tabparties_irl), graph = FALSE)

fviz_pca_ind(res.pca, habillage = "ind.sup",axes = c(1,2), geom.ind = "point", 
             addEllipses = TRUE, legend.title = "idividu")