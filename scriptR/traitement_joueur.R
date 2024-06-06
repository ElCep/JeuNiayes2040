# analyse des stratégies a partir des joueurs simulé
# les données utiliser ici sont produite par le script `applatTableau.R`
# dans le commit fc7a305b065ca124f6ee796ab69a8468e22024a9 pour les données
# et 3529be08d1abfc4f75936293e8489c69d3970e3a pour le script

rm(list = ls())
library(jsonlite)
library(reshape2)
library(ggplot2)
# library(DescTools)
library(dplyr)
library(FactoMineR)
# library(Factoshiny)
library(corrplot)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data.js <- read.csv("../data/applatJoueur_simule_complet.csv")
data.js$stratCulture <- as.numeric(data.js$stratCulture)
summary(is.na(data.js$stratCulture))
data.js <- data.js[!is.na(data.js$stratCulture),]


data.jr <- read.csv("../data/gameSession_config_pluie_26.csv", header = T,sep =";", encoding = "latin1")

names(data.jr)
# [1] "partie"          "player"          "stratCulture"    "stratParcelle"   "stratOrdreIrrig" "stratSeau"       "stratGG"         "stratLance"      "capital"        
# [10] "profnappe"       "prelevement"     "puits"  


## -- CAH

# Distance matrix
d <- dist(data.jr[,4:5])

# Hierarchical clustering
hc <- hclust(d, method = "ward.D")

# Dendrogram
plot(hc)
rect.hclust(hc, k = 3)

data.jr$cluster <- cutree(hc, k=3)

ggplot(data = data.jr, aes(x = Capital_final , y=Consommation_eau, colour = as.factor(cluster)))+
  geom_point()+
  theme_bw()

## -- Simu

# Distance matrix
d <- dist(data.js[,c(9,11)])

# Hierarchical clustering
hc <- hclust(d, method = "ward.D")

# Dendrogram
plot(hc)
rect.hclust(hc, k = 3)

data.js$cluster <- cutree(hc, k=3)

ggplot()+
  geom_point(data = data.js, aes(x = capital , y=prelevement, colour = as.factor(cluster)))+
  geom_point(data = data.jr, aes(x = Capital_final, y = Consommation_eau))+
  theme_bw()+
  labs(x="capital", y = "water consumtion")+
  guides(colour=guide_legend(title="Cluster"))+
  scale_color_brewer(type = "qual", palette = 3)
ggsave("../img/cluster_joueurs.png", height = 7, width = 9)


saveRDS(c(hc,data.jr,data.js), "../data/Cluster.RDS")


## --- 

# Charger les packages nécessaires
library(mclust)

# Charger les données
# Supposons que nous avons un dataframe: players
# players contient les observations des joueurs


players <- data.jr

# Appliquer le modèle de mélange gaussien
gmm_model <- Mclust(players[,4:14])

# Afficher les résultats du modèle
summary(gmm_model)

# Afficher les paramètres du modèle
print(gmm_model$parameters)

# Visualiser les clusters
plot(gmm_model, what = "classification")

# Ajouter les prédictions des clusters aux données
players$cluster <- gmm_model$classification

# Afficher les données avec les clusters prédites
print(players)


#♥Matrice de corrélation
mcor <- cor(data.j[,3:12], use="complete.obs")
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)


## traitement des résultat sur des joueurs strictement identifque

data.j <- read.csv(file = "../data/applatJoueur_simule_SameInit.csv")
names(data.j)
# [1] "partie"          "player"          "stratCulture"    "stratParcelle"   "stratOrdreIrrig" "stratSeau"       "stratGG"        
# [8] "stratLance"      "capital"         "profnappe"       "prelevement"     "puits"
mcor <- cor(data.j[,3:12], use="complete.obs")
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)
