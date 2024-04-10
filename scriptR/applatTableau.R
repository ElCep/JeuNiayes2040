rm(list = ls())
library(jsonlite)
library(reshape2)
library(ggplot2)
# library(DescTools)
library(dplyr)
#library(FactoMineR)


# Lire le fichier JSON issue du script OpenMole replication.oms

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("function_stat.R") ## read all fonction API for manipulation


df.meltd <- data.frame()
file.l <- c("../data/result_pluies20-6.json", "../data/result_pluies6-20.json", "../data/result_pluies0-26.json")
pluie <- c('20-6','6-20','0-26')

s_culture.df <- NULL
s_gg.df <- NULL
s_lance.df <- NULL
s_parcelle.df <- NULL
s_prioIrrig.df <- NULL
s_seau.df <- NULL

for(i in 1:length(file.l)){
  data <- fromJSON(file.l[i])
  results <- data$data$variables$result
  s_culture.df <- rbind(s_culture.df, cbind(strat_culture(results),pluie[i]))
  s_parcelle.df <- rbind(s_parcelle.df, strat_parcelle(results))
  s_prioIrrig.df <- rbind(s_prioIrrig.df, strat_priorite_irrigation(results))
  s_seau.df <- rbind(s_seau.df, strat_seau(results))
  s_gg.df <- rbind(s_gg.df, strat_gg(results))
  s_lance.df <- rbind(s_lance.df, strat_lance(results))

}

# tableau propre pour les stratégie de culture
s_culture.df <- as.data.frame(s_culture.df)
a <- mutate_all(s_culture.df[,-5], function(x) as.numeric(x))
a$pluie <- s_culture.df$V5
s_culture.df <- a
s_culture.df$partie <- seq(from = 1, to = length(s_culture.df[,1]), by = 1)
colnames(s_culture.df) <- c("p1", "p2", "p3", "p4", "pluie", "partie")
s_culture.m <- melt(s_culture.df, id.vars = "partie")
colnames(s_culture.m) <- c("partie","player","stratCulture")

s_parcelle.df <- as.data.frame(s_parcelle.df)
s_parcelle.df$partie <- seq(from = 1, to = length(s_parcelle.df[,1]), by = 1)
colnames(s_parcelle.df) <- c("p1", "p2", "p3", "p4", "partie")
s_parcelle.m <- melt(s_parcelle.df, id.vars = "partie")
colnames(s_parcelle.m) <- c("partie","player","stratParcelle")


s_prioIrrig.df <- as.data.frame(s_prioIrrig.df)
s_prioIrrig.df$partie <- seq(from = 1, to = length(s_prioIrrig.df[,1]), by = 1)
colnames(s_prioIrrig.df) <- c("p1", "p2", "p3", "p4", "partie")
s_prioIrrig.m <- melt(s_prioIrrig.df, id.vars = "partie")
colnames(s_prioIrrig.m) <- c("partie","player","stratOrdreIrrig")

s_seau.df <- as.data.frame(s_seau.df)
s_seau.df$partie <- seq(from = 1, to = length(s_seau.df[,1]), by = 1)
colnames(s_seau.df) <- c("p1", "p2", "p3", "p4", "partie")
s_seau.m <- melt(s_seau.df, id.vars = "partie")
colnames(s_seau.m) <- c("partie","player","stratSeau")


s_gg.df <- as.data.frame(s_gg.df)
s_gg.df$partie <- seq(from = 1, to = length(s_gg.df[,1]), by = 1)
colnames(s_gg.df) <- c("p1", "p2", "p3", "p4", "partie")
s_gg.m <- melt(s_gg.df, id.vars = "partie")
colnames(s_gg.m) <- c("partie","player","stratSeau")

s_lance.df <- as.data.frame(s_lance.df)
s_lance.df$partie <- seq(from = 1, to = length(s_lance.df[,1]), by = 1)
colnames(s_lance.df) <- c("p1", "p2", "p3", "p4", "partie")
s_lance.m <- melt(s_lance.df, id.vars = "partie")
colnames(s_lance.m) <- c("partie","player","stratSeau")

## jointure 

df <- left_join(s_culture.m, s_parcelle.m, by=c('player'='player', 'partie'='partie'))
df <- left_join(df, s_prioIrrig.m, by=c('player'='player', 'partie'='partie'))
df <- left_join(df, s_seau.m, by=c('player'='player', 'partie'='partie'))
df <- left_join(df, s_gg.m, by=c('player'='player', 'partie'='partie'))
df <- left_join(df, s_lance.m, by=c('player'='player', 'partie'='partie'))

write.csv(df, file = "../data/applatJoueur_simule.csv", row.names = F)
