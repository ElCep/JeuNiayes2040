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

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data.j <- read.csv("../data/applatJoueur_simule.csv")
data.j$stratCulture <- as.numeric(data.j$stratCulture)
summary(is.na(data.j$stratCulture))
data.j <- data.j[!is.na(data.j$stratCulture),]

names(data.j)
# [1] "partie"          "player"          "stratCulture"    "stratParcelle"   "stratOrdreIrrig" "stratSeau"       "stratGG"         "stratLance" 

res.pca <- PCA(data.j[,c(-1,-2,-4,-6,-7,-8)], scale.unit=TRUE, ncp=2, graph=T)

res.pca <- PCA(data.j[,c(-1,-2,-3,-4,-5)], scale.unit=TRUE, ncp=2, graph=T)

