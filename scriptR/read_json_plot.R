# 1) graphique croisant Capital moyen (ou min ?) par partie / conso totale eau par partie / gini du capital de la partie

rm(list = ls())
library(jsonlite)
library(reshape2)
library(ggplot2)
library(DescTools)


# Lire le fichier JSON issue du script OpenMole replication.oms

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df.meltd <- data.frame()
file.l <- c("../data/result_pluies20-6.json", "../data/result_pluies6-20.json", "../data/result_pluies0-26.json")
for(i in 1:length(file.l)){
  data <- fromJSON(file.l[i])
  dim(data$data$variables$result[[1]])
  # 100000  4 13  donc 100000 réplication, 4 joueur et 13 variables. Donc une partie est une ligne.
  tps1 <- unlist(data$data$variables$pluie1)
  tps2 <- unlist(data$data$variables$pluie2)
  tps <- as.data.frame(cbind(tps1, tps2))
  tps$sum_recharge <- apply(tps, 1, sum) # Calculer la médiane de chaque ligne
  
  ## organisation des données par PARTIES ####
  # Étape 1 : Extraction des données
  results <- data$data$variables$result
  
  # results[[1]][,,1:13]
  # List|of(profnappe, p.capital, p.prelevtot, p.tot_vente_parcelles,p.tot_achat_parcelles,p.profmax,p.totoignon,p.totchou,p.totaubergine
  #        ,p.totpiment,p.totpdt,p.totcarotte,p.nbparcelle_noncultivee)
  
  p.capital <- as.data.frame(results[[1]][,,2])
  p.capital$pluies <- tps$sum_recharge
  p.capital.melt <- melt(p.capital, id.vars = "pluies")
  colnames(p.capital.melt) <- c('pluie','player','capital')
  
  
  
  # Étape 1 : Extraction des données
  p.prelev <- as.data.frame(results[[1]][,,3])
  
  # Étape 2 : Transformation en format long
  # Crée un data frame 'p.prelev' avec deux colonnes : 'player2' et 'prelevement'
  p.prelev.melt <- melt(p.prelev)
  colnames(p.prelev.melt) <- c('player2','prelevement')
  
  
  
  ## organisation des données par JOUEURS ####
  
  # Renommez les levels
  levels(p.capital.melt$player) <- c("p1", "p2", "p3","p4")
  
  
  # On va normaliser la colonne capitale entre 0 et 1 pour avoir des valeurs comparable
  p.capital.melt$capitalNorm <- apply(p.capital.melt[, "capital", drop = FALSE], 2, function(x) {
    (x - min(x)) / (max(x) - min(x))
  })
  
  names(p.capital.melt) <- c("pluie", "player", "capital", "capitalNorm")
  
  min_val <- min(p.prelev.melt$prelevement)
  max_val <- max(p.prelev.melt$prelevement)
  p.prelev.melt$prelevNorm <- (p.prelev.melt$prelevement - min_val) / (p.prelev.melt$prelevement - min_val)
  
  df.p <- data.frame(p.capital.melt[,1:4], p.prelev.melt[,2:3])
  df.p$partie <- i
  df.meltd <- rbind(df.meltd, df.p)
}

data.p.meltd <- melt(data = subset(df.meltd, 
                                   select = c("pluie", "player", "partie", "capital", "prelevement")
                                   ), 
                     id.vars = c("pluie", "player", "partie") )




## Visualisation ggplot par JOUEURS ####

ggplot(data = df.meltd)+
  geom_point(aes(x = capital, y = prelevement, colour = as.factor(partie)), size = 0.5)+
  # geom_hline(yintercept=0.75, linetype="dashed", color = "grey")+
  # geom_vline(xintercept=0.75, linetype="dashed", color = "grey")+
  theme_bw()+
  labs(x = "final capital", y= "water withdrawal", 
       title = "300 replication with diff. rainfall pattern", 
       subtitle = "By players with a variation on underground water recharge",
       colour = "Rainfall\nPattern")+
  xlim(c(0,150))+
  ylim(c(0,150))+
  scale_colour_manual(values = c('1' = "#a6cee3", '2' = "#1f78b4", '3'= '#b2df8a'),
                      labels = c("20-6", "6-20", "0-26"))
ggsave(filename = "../img/capital_water_players_pluie.png", width = 11)

labels_map <- setNames(c("20-6", "6-20", "0-26"), c("1", "2","3"))
ggplot(data = data.p.meltd)+
  geom_boxplot(aes(x = variable, y = value))+
  facet_wrap(~partie, labeller = as_labeller(labels_map))+
  theme_bw()+
  labs(x = "", y= "", 
       title = "300 replication with diff. rainfall pattern", 
       subtitle = "By players with a variation on underground water recharge")
ggsave(filename = "../img/capital_water_players_pluie_boxplot.png", width = 11)



### Add players ####

real_gameSession <- read.csv("../data/gameSession_config_pluie_26.csv", sep = ";", encoding="latin1")


ggplot()+
  geom_point(data = df.meltd, aes( x = capital, y = prelevement), size = 0.5, alpha = 0.4)+
  geom_point(data = real_gameSession, aes( x = Capital_final, y = Consommation_eau, colour = as.factor(ID.partie)), size = 2)+
  # geom_hline(yintercept=0.75, linetype="dashed", color = "grey")+
  # geom_vline(xintercept=0.75, linetype="dashed", color = "grey")+
  theme_bw()+
  labs(x = "final capital", y= "water withdrawal", 
       title = "300 replication with diff. rainfall pattern", 
       subtitle = "By players with a variation on underground water recharge\n")+
  xlim(c(0,150))+
  ylim(c(0,150))
ggsave(filename = "../img/capital_water_players_pluie_realSession.png", width = 11)
