rm(list = ls())
library(jsonlite)
library(reshape2)
library(dplyr)



# Lire le fichier JSON issue du script OpenMole replication.oms

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("function_stat.R") ## read all fonction API for manipulation

data <- fromJSON("../data/result_pluies_sameInit.json")
results <- data$data$variables$result
#  s_culture.df <- rbind(s_culture.df, cbind(strat_culture(results),pluie[i])) # j'ai enlevé la colonne pluie
s_culture.df <- strat_culture(results)
s_parcelle.df <- strat_parcelle(results)
s_prioIrrig.df <- strat_priorite_irrigation(results)
s_seau.df <- strat_seau(results)
s_gg.df <- strat_gg(results)
s_lance.df <- strat_lance(results)
s_capital.df <- capital(results)
s_profnappe.df <- profnappe(results)
s_prelevement.df <- prelevement(results)
s_puits.df <- puits(results)


# tableau propre pour les stratégies de culture
s_culture.df <- as.data.frame(s_culture.df)
a <- mutate_all(s_culture.df[,-5], function(x) as.numeric(x))
#a$pluie <- s_culture.df$V5
s_culture.df <- a
s_culture.df$partie <- seq(from = 1, to = length(s_culture.df[,1]), by = 1)
#colnames(s_culture.df) <- c("p1", "p2", "p3", "p4", "pluie", "partie")
colnames(s_culture.df) <- c("p1", "p2", "p3", "p4", "partie")
s_culture.m <- melt(s_culture.df, id.vars = "partie") # inverse ligne et colonne # pb : il met aussi la pluie en colonne !
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
colnames(s_gg.m) <- c("partie","player","stratGG")

s_lance.df <- as.data.frame(s_lance.df)
s_lance.df$partie <- seq(from = 1, to = length(s_lance.df[,1]), by = 1)
colnames(s_lance.df) <- c("p1", "p2", "p3", "p4", "partie")
s_lance.m <- melt(s_lance.df, id.vars = "partie")
colnames(s_lance.m) <- c("partie","player","stratLance")


s_capital.df <- as.data.frame(s_capital.df)
s_capital.df$partie <- seq(from = 1, to = length(s_capital.df[,1]), by = 1)
colnames(s_capital.df) <- c("p1", "p2", "p3", "p4", "partie")
s_capital.m <- melt(s_capital.df, id.vars = "partie")
colnames(s_capital.m) <- c("partie","player","capital")

s_profnappe.df <- as.data.frame(s_profnappe.df)
s_profnappe.df$partie <- seq(from = 1, to = length(s_profnappe.df[,1]), by = 1)
colnames(s_profnappe.df) <- c("p1", "p2", "p3", "p4", "partie")
s_profnappe.m <- melt(s_profnappe.df, id.vars = "partie")
colnames(s_profnappe.m) <- c("partie","player","profnappe")

s_prelevement.df <- as.data.frame(s_prelevement.df)
s_prelevement.df$partie <- seq(from = 1, to = length(s_prelevement.df[,1]), by = 1)
colnames(s_prelevement.df) <- c("p1", "p2", "p3", "p4", "partie")
s_prelevement.m <- melt(s_prelevement.df, id.vars = "partie")
colnames(s_prelevement.m) <- c("partie","player","prelevement")

s_puits.df <- as.data.frame(s_puits.df)
s_puits.df$partie <- seq(from = 1, to = length(s_puits.df[,1]), by = 1)
colnames(s_puits.df) <- c("p1", "p2", "p3", "p4", "partie")
s_puits.m <- melt(s_puits.df, id.vars = "partie")
colnames(s_puits.m) <- c("partie","player","puits")


## jointure 

df <- left_join(s_culture.m, s_parcelle.m, by=c('player'='player', 'partie'='partie'))
df <- left_join(df, s_prioIrrig.m, by=c('player'='player', 'partie'='partie'))
df <- left_join(df, s_seau.m, by=c('player'='player', 'partie'='partie'))
df <- left_join(df, s_gg.m, by=c('player'='player', 'partie'='partie'))
df <- left_join(df, s_lance.m, by=c('player'='player', 'partie'='partie'))
df <- left_join(df, s_capital.m, by=c('player'='player', 'partie'='partie'))
df <- left_join(df, s_profnappe.m, by=c('player'='player', 'partie'='partie'))
df <- left_join(df, s_prelevement.m, by=c('player'='player', 'partie'='partie'))
df <- left_join(df, s_puits.m, by=c('player'='player', 'partie'='partie'))


write.csv(df, file = "../data/applatJoueur_simule_SameInit.csv", row.names = F)
