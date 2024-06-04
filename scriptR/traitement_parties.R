# analyse des stratégies a partir des parties simulées


rm(list = ls())
library(ineq)
library(Hmisc)
library(corrplot)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#simulations
df<- read.csv("../data/applatJoueur_simule_complet.csv")

# Essai joueurs
real_gameSession <- read.csv("../data/gameSession_config_pluie_26.csv", sep = ";", encoding="latin1")


# enlever le capital négatif...

df$capitalmodif <- ifelse(df$capital < 0, 0, df$capital)

agg_data_gini <- aggregate(cbind(capitalmodif, prelevement) ~ partie, data = df, FUN = function(x) {
  GINI = ineq(x)
}, na.action = na.pass)

# ¨pour les vraies parties
 irl_gini <-aggregate(cbind(Capital_final,Consommation_eau)~ID.partie, data=real_gameSession,FUN = function(x) {
   GINI = ineq(x)
 }, na.action = na.pass)


# prof nap
agg_data_prof<- aggregate(profnappe ~ partie, data = df, FUN = mean)

# ¨pour les joeurs
#irl_prof <-aggregate(


# somme capital
agg_data_capital<- aggregate(capital ~ partie, data = df, FUN = sum)
irl_captot <-aggregate(Capital_final ~ID.partie, data=real_gameSession,FUN = sum)


# Tableau final
tabparties<-cbind(agg_data_gini,agg_data_prof$profnappe,agg_data_capital$capital)
colnames(tabparties)<- c("partie","ginicap","giniprelev","profnappe","sum_cap")

# Tableau final irl
tabparties_irl<-cbind(irl_gini,irl_captot$Capital_final)
colnames(tabparties_irl)<-c("partie.irl","ginicap.irl","giniprelev.irl","sum_cap.irl")



#♥Matrice de corrélation
mcor <- cor(tabparties[,2:5], use="complete.obs")
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)


# GRAPHIQUE GINI PRELEV- GINI CAPITAL
plot(x = tabparties$giniprelev, y = tabparties$ginicap, 
     xlab = "GINI des prélèvements", ylab = "GINI du capital", 
     main = "Relation entre le GINI des prélèvements et le GINI du capital",
     col = "blue", pch = 16)


# GRAPHIQUE GINI CAPITAL - tot capital partir
plot(x = tabparties$ginicap, y = tabparties$sum_cap, 
     xlab = "GINI du capital", ylab = "valeur ajoutée tot", 
     main = "Relation entre le GINI du capital valeur ajoutée tot",
     col = "blue", pch = 16)


# GRAPHIQUE CAPITAL tot - prelevement
plot(x = tabparties$sum_cap, y = tabparties$profnappe, 
     xlab = "sumcap", ylab = "profnappe", 
     main = "Relation entre entre valeur ajoutée tot et la profondeur de la nappe",
     col = "blue", pch = 16)

# GRAPHIQUE GINI prelev - prelevement
plot(x = tabparties$giniprelev, y = tabparties$profnappe, 
     xlab = "giniprelev", ylab = "profnappe", 
     main = "Relation entre le GINI des prélèvements etla profondeur de la nappe ",
     col = "blue", pch = 16)


# GRAPHIQUE GINI prelev - sumcap
color_palette <- colorRampPalette(c("blue", "red"))

colors <- color_palette(100)[cut(as.numeric(tabparties$ginicap), breaks = 100)]

plot(x = tabparties$giniprelev, y = tabparties$sum_cap, 
     xlab = "giniprelev", ylab = "sum_cap", 
     main = "Relation entre le GINI des prélèvements et sum_cap ",
      col = colors, pch = 16)

par(xpd = TRUE) # Permet d'afficher la légende en dehors de la zone du graphique
legend("topright", legend = seq(0, 1, length.out = 3), fill = color_palette(3), 
       title = "GINI du capital", cex = 0.8, bty = "n", inset = c(0, -0.1))



# GRAPHIQUE GINI CAPITAL - tot capital partir
# Créer une palette de couleurs graduelle de bleu à rouge
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




# GRAPHIQUE GINI CAPITAL - tot capital partir
# Créer une palette de couleurs graduelle de bleu à rouge
color_palette <- colorRampPalette(c("blue", "red"))

# Convertir les valeurs de "ginicap" en couleurs selon la palette graduelle
colors <- color_palette(100)[cut(as.numeric(tabparties$ginicap), breaks = 100)]

mincap<-min(tabparties$ginicap)
maxcap<-max(tabparties$ginicap)
plot(x = tabparties$sum_cap, y = tabparties$profnappe, 
     xlab = "sum du capital", ylab = "profnappe", 
     main = "Relation entre le VA et profnappe et répartission richesse",
     col = colors, pch = 16)

par(xpd = TRUE) # Permet d'afficher la légende en dehors de la zone du graphique
legend("topright", legend = seq(mincap, maxcap, length.out = 5), fill = color_palette(5), 
       title = "gini_capital", cex = 0.8, bty = "n", inset = c(0, -0.1))









# Essai joueurs

tabparties_irl<-cbind(irl_gini,irl_captot$Capital_final)
colnames(tabparties_irl)<-c("partie.irl","ginicap.irl","giniprelev.irl","sum_cap.irl")

library(ggplot2)
ggplot()+
  geom_point(data = tabparties, aes( x = ginicap, y = giniprelev), size = 0.5, alpha = 0.4)+
  geom_point(data = tabparties_irl, aes(x =ginicap.irl, y = giniprelev.irl),  size = 6 , alpha = 0.4)+
  # geom_hline(yintercept=0.75, linetype="dashed", color = "grey")+
  # geom_vline(xintercept=0.75, linetype="dashed", color = "grey")+
  theme_bw()+
  labs(x = "final capital", y= "water withdrawal", 
       title = "300 replication with diff. rainfall pattern", 
       subtitle = "By players with a variation on underground water recharge\n")+
  xlim(c(0,150))+
  ylim(c(0,150))



