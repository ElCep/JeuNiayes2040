rm(list = ls())
library(jsonlite)
library(reshape2)
library(dplyr)



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
s_capital.df <- NULL
s_profnappe.df <- NULL
s_prelevement.df <- NULL
s_puits.df <- NULL
s_agrandissement.df <- NULL
s_empechement.df <- NULL
s_capitalini.df<-NULL
s_parcelleini.df <- NULL
s_accumcap.df<-NULL
for(i in 1:length(file.l)){
  data <- fromJSON(file.l[i])
  results <- data$data$variables$result
#  s_culture.df <- rbind(s_culture.df, cbind(strat_culture(results),pluie[i])) # j'ai enlevé la colonne pluie
  s_culture.df <- rbind(s_culture.df, strat_culture(results))
  s_parcelle.df <- rbind(s_parcelle.df, strat_parcelle(results))
  s_prioIrrig.df <- rbind(s_prioIrrig.df, strat_priorite_irrigation(results))
  s_seau.df <- rbind(s_seau.df, strat_seau(results))
  s_gg.df <- rbind(s_gg.df, strat_gg(results))
  s_lance.df <- rbind(s_lance.df, strat_lance(results))
  s_capital.df <- rbind(s_capital.df, capital(results) )
  s_profnappe.df <- rbind(s_profnappe.df, profnappe(results) )
  s_prelevement.df <- rbind(s_prelevement.df, prelevement(results) )
  s_puits.df <- rbind(s_puits.df, puits(results) )
  s_agrandissement.df <- rbind(s_agrandissement.df, strat_dynFoncier(results) )
  s_empechement.df <- rbind(s_empechement.df, strat_empechement(results))
  s_capitalini.df<-rbind(s_capitalini.df, capitalini(results) )
  s_parcelleini.df <-rbind(s_parcelleini.df, parcelleini(results))
  s_accumcap.df<-rbind(s_accumcap.df,accumcap(results))
# paramètre capital
#  parampluie1 <- data$data$variables$pluie1
#  parampluie2 <- data$data$variables$pluie2
  }


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
colnames(s_parcelle.m) <- c("partie","player","Parcelle")


s_parcelleini.df <- as.data.frame(s_parcelleini.df)
s_parcelleini.df$partie <- seq(from = 1, to = length(s_parcelleini.df[,1]), by = 1)
colnames(s_parcelleini.df) <- c("p1", "p2", "p3", "p4", "partie")
s_parcelleini.m <- melt(s_parcelleini.df, id.vars = "partie")
colnames(s_parcelleini.m) <- c("partie","player","Parcelleini")


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


s_capitalini.df <- as.data.frame(s_capitalini.df)
s_capitalini.df$partie <- seq(from = 1, to = length(s_capitalini.df[,1]), by = 1)
colnames(s_capitalini.df) <- c("p1", "p2", "p3", "p4", "partie")
s_capitalini.m <- melt(s_capitalini.df, id.vars = "partie")
colnames(s_capitalini.m) <- c("partie","player","capitalini")


s_accumcap.df <- as.data.frame(s_accumcap.df)
s_accumcap.df$partie <- seq(from = 1, to = length(s_accumcap.df[,1]), by = 1)
colnames(s_accumcap.df) <- c("p1", "p2", "p3", "p4", "partie")
s_accumcap.m <- melt(s_accumcap.df, id.vars = "partie")
colnames(s_accumcap.m) <- c("partie","player","accumcap")


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


s_agrandissement.df <- as.data.frame(s_agrandissement.df)
s_agrandissement.df$partie <- seq(from = 1, to = length(s_agrandissement.df[,1]), by = 1)
colnames(s_agrandissement.df) <- c("p1", "p2", "p3", "p4", "partie")
s_agrandissement.m<- melt(s_agrandissement.df, id.vars = "partie")
colnames(s_agrandissement.m) <- c("partie","player","agrandissement")



s_empechement.df <- as.data.frame(s_empechement.df)
s_empechement.df$partie <- seq(from = 1, to = length(s_empechement.df[,1]), by = 1)
colnames(s_empechement.df) <- c("p1", "p2", "p3", "p4", "partie")
s_empechement.m<- melt(s_empechement.df, id.vars = "partie")
colnames(s_empechement.m) <- c("partie","player","empechement")



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
df <- left_join(df, s_agrandissement.m, by=c('player'='player', 'partie'='partie'))
df <- left_join(df, s_empechement.m, by=c('player'='player', 'partie'='partie'))
df <- left_join(df, s_capitalini.m, by=c('player'='player', 'partie'='partie'))
df <- left_join(df, s_parcelleini.m, by=c('player'='player', 'partie'='partie'))
df <- left_join(df,s_accumcap.m, by=c('player'='player', 'partie'='partie'))
#A FAIRE : ajouter une colonne id joueur : pour retrouver directement les joueurs en individuel


write.csv(df, file = "../data/applatJoueur_simule_complet.csv", row.names = F)
