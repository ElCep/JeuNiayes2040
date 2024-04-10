rm(list = ls())
library(jsonlite)
library(reshape2)
library(ggplot2)
library(DescTools)
library(dplyr)
library(FactoMineR)


# Lire le fichier JSON issue du script OpenMole replication.oms

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("function_stat.R") ## read all fonction API for manipulation


df.meltd <- data.frame()
file.l <- c("../data/result_pluies20-6.json", "../data/result_pluies6-20.json", "../data/result_pluies0-26.json")
pluie <- c('20-6','6-20','0-26')
strat_culture.df <- NULL
for(i in 1:length(file.l)){
  data <- fromJSON(file.l[i])
  results <- data$data$variables$result
  strat_culture.df <- rbind(strat_culture.df, cbind(strat_culture(results),pluie[i]))
}

colnames(strat_culture.df) <- c("p1", "p2", "p3", "p4", "pluie")
a <- mutate_all(strat_culture.df[,-5], function(x) as.numeric(x))

# tout mettre en ligne, ajouter une colonne id joueurs, et une id partie
# Regénérer les memes tableaux pour toutes les variables (strategies eau, capital, conso, profondeur puitd, etc)
# faire une jointure par id participant




res <- FactoMineR::PCA(t(a))
