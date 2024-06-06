# install.packages("ggplot2")
# install.packages("FactoMineR")
# install.packages("factoextra")

library(ggplot2)
library(FactoMineR)
library(factoextra)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data.js <- read.csv("../data/applatJoueur_simule_complet.csv")
data.js$stratCulture <- as.numeric(data.js$stratCulture)
summary(is.na(data.js$stratCulture))
data.js <- data.js[!is.na(data.js$stratCulture),]


data.jr <- read.csv("../data/gameSession_config_pluie_26.csv", header = T,sep =";", encoding = "latin1")

set.seed(123)
data1 <- data.frame(matrix(rnorm(100*5), ncol=5))  # 100 échantillons, 5 variables
data2 <- data.frame(matrix(rnorm(100*5, mean=5), ncol=5))  # 100 échantillons, 5 variables avec une moyenne différente

data.js <- subset(data.js, select = c("stratCulture","stratParcelle", "stratSeau","stratGG","stratLance","capital", 
                                      "profnappe", "prelevement","agrandissement","empechement","stratOrdreIrrig"))

data.jr <- subset(data.jr, select = c("strat_culture", "strat_parcelle","stratSeau","strat_gag", "strat_lance", "Capital_final",
                                      "puits_profmax", "Consommation_eau", "agrandissement", "empechement", "enchainement"))

colnames(data.jr) <- c("stratCulture","stratParcelle", "stratSeau","stratGG","stratLance","capital", 
                       "profnappe", "prelevement","agrandissement","empechement","stratOrdreIrrig")

pca1 <- PCA(data.jr, graph = FALSE)
pca2 <- PCA(data.js, graph = FALSE)


coord1 <- data.frame(pca1$ind$coord[,1:2])
coord2 <- data.frame(pca2$ind$coord[,1:2])

coord1$Dataset <- "Dataset 1"
coord2$Dataset <- "Dataset 2"

coord <- rbind(coord1, coord2)
names(coord) <- c("PC1", "PC2", "Dataset")


var1 <- data.frame(pca1$var$coord[,1:2])
var2 <- data.frame(pca2$var$coord[,1:2])

var1$Dataset <- "JS"
var2$Dataset <- "JR"

var <- rbind(var1, var2)
names(var) <- c("PC1", "PC2", "Dataset")

ggplot() +
  geom_point(data = coord, aes(x = PC1, y = PC2, color = Dataset)) +
  geom_segment(data = var, aes(x = 0, y = 0, xend = PC1, yend = PC2, color = Dataset),
               arrow = arrow(length = unit(0.3, "cm")), alpha = 0.75) +
  geom_text(data = var, aes(x = PC1, y = PC2, label = rownames(var), color = Dataset), 
            hjust = 1.2, vjust = 1.2) +
  theme_minimal() +
  labs(title = "Comparaison des deux jeux de données avec ACP",
       x = "Composante principale 1",
       y = "Composante principale 2") +
  scale_color_manual(values = c("blue", "red", "yellow", "green"))
