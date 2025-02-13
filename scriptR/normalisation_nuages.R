library(FactoMineR)
library(factoextra)
library(dplyr)
library(gridExtra)

# Charger les bibliothèques nécessaires
library(ggplot2)
library(RColorBrewer)


rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
####################Préparation des tableaux de données

#data.js <- read.csv("../data/applatJoueur_simule_complet.csv")  # js pour joueur simulé

# Sorties des simuls des 23 différentes configurations
data <- read.csv("../data/sorties_jeu.csv",sep=";")

names(data)<-c("numbergame","id","capitalini","parcellesini","profnappe","pluie1","pluie2","capital","prelevtot",
               "tot_vente_parcelles","tot_achat_parcelles","profmax","totoignon","totchou","totaubergine","totpiment",
               "totpdt","totcarotte","nbparcelle_noncultivee","totlance","totgag","totseau","rangpartie1","rangpartie2",
               "rangpartie3","parce_empeche1","parce_empeche2","parce_empeche3")


data$stratCulture<-as.numeric((data$totoignon+data$totchou+data$totpdt)/(data$totoignon+data$totchou+data$totaubergine+data$totpiment+data$totpdt+data$totcarotte))
data$parcelleculti<-data$totoignon+data$totchou+data$totaubergine+data$totpiment+data$totpdt+data$totcarotte
data$stratSeau<-data$totseau
data$stratGG<-data$totgag
data$stratLance<-data$totlance
data$prelevement<-data$prelevtot
data$agrandissement<-data$tot_achat_parcelles-data$tot_vente_parcelles
data$empechement<-data$parce_empeche1+data$parce_empeche2+data$parce_empeche3


# Création des groupes
data$groupe <- cut(data$numbergame, breaks = seq(1, 69001, by = 3000), include.lowest = TRUE, labels = FALSE)

data$groupe <-as.factor(data$groupe)
#####################Premier graph général

# Fonction pour calculer l'enveloppe convexe d'un groupe
convex_hull <- function(df) {
  df[chull(df$capital, df$prelevement), ]  # chull() retourne les indices du contour convexe
}

# Appliquer la fonction à chaque groupe
hulls <- data %>%
  group_by(groupe) %>%
  group_split() %>%
  lapply(convex_hull) %>%
  bind_rows()

# Générer 23 couleurs distinctes
palette_couleurs <- colorRampPalette(brewer.pal(12, "Set3"))(23)

# Créer le graphique avec contours convexes et points

ggplot() +
  geom_point(data = data, aes(x = capital, y = prelevement, color = groupe), alpha = 1.0, size = 1.5) +  # Points transparents
geom_polygon(data = hulls, aes(x = capital, y = prelevement, color = groupe, group = groupe),
             fill = NA, size = 1) +  # Contours colorés, intérieur transparent
  scale_color_manual(values = palette_couleurs) +  # Appliquer les couleurs distinctes aux contours et aux points
  labs(title = "Contours convexes des nuages de points",
       x = "Capital",
       y = "Prélèvements",
       color = "Groupe") + 
  theme_minimal() +  
  theme(text = element_text(size = 14), legend.position = "right")

# Sauvegarder en image
#ggsave("graphique_convex_hull.png", plot = p, width = 12, height = 8, dpi = 300)





# Création des sous-tableaux sous forme de liste
sous_tableaux <- split(data, data$groupe)


combi<-read.csv("../data/inti_parties.csv", header = TRUE, sep = ",", encoding = "latin1") 
data.jr.complet <- read.csv("../data/Game_irl_2025.csv", header = TRUE, sep = ";", encoding = "latin1", dec = ',')  # jr pour joueur réel
tableau_normal.js<-NULL
tableau_normal.jr<-NULL
for (simul in sous_tableaux){
  
 
  indice<- simul$numbergame[1]
  confi<-simul[simul$numbergame==indice,]
  player1<-confi[confi$id==1,]    
  player2<-confi[confi$id==2,]    
  player3<-confi[confi$id==3,]    
  player4<-confi[confi$id==4,]   
 
   # extraire la config dans simul
  # extraire la ligne correspondant à la configuration
  combicible<-combi[combi$cap1==player1$capitalini & 
                      combi$cap2==player2$capitalini & 
                      combi$cap3==player3$capitalini & 
                      combi$cap4==player4$capitalini & 
                      combi$surf1==player1$parcellesini &
                      combi$surf2==player2$parcellesini&
                      combi$surf3==player3$parcellesini&
                      combi$surf4==player4$parcellesini&
                      combi$pluie1==player1$pluie1 &
                      combi$pluie2==player2$pluie2,]

  # Filtrer les lignes de jr où ID.partie est présent dans combi$Idpartie
  data.jr <- data.jr.complet %>% semi_join(combicible, by = c("ID.partie" = "Idpartie"))
  

# Extraire les variables d'interet

data.js <- subset(simul, select = c("capitalini","parcellesini","stratCulture","parcelleculti", "stratSeau","stratGG","stratLance","capital", 
                                      "prelevement","agrandissement","empechement"))

data.jr <- subset(data.jr, select = c("capital_initial","parcelles_initiales","strat_culture", "strat_parcelle","stratSeau","strat_gag", "strat_lance", "Capital_final",
                                      "Consommation_eau", "agrandissement...achat.vente.", "empechement"))

colnames(data.jr) <- c("capitalini","parcellesini","stratCulture","parcelleculti", "stratSeau","stratGG","stratLance","capital", 
                       "prelevement","agrandissement","empechement")

# Checker la cohérence de chaque varaible dans chaque config

# for(i in 1:length(colnames(data.jr))){
#     boxplot(data.jr[,i], data.js[,i],
#           names = c("data.jr", "data.js"),  # Noms des groupes
#           col = c("blue", "red"),  # Couleurs différentes
#           main = colnames(data.jr)[i],  # Titre
#           ylab = "Valeurs")  # Axe des Y
#   }



# Centrer sur la moyenne & division par variance (ou max, ou plus grand percentil)

moy_cap<-mean(data.js$capital)
moy_conso<-mean(data.js$prelevement)

max_cap<-max(abs(data.js$capital))
max_conso<-max(abs(data.js$prelevement))

var_cap<-var(data.js$capital)
var_conso<-var(data.js$prelevement)

data.js$capital_centre<-data.js$capital-moy_cap
data.js$prelevement_centre<-data.js$prelevement-moy_conso

data.jr$capital_centre<-data.jr$capital-moy_cap
data.jr$prelevement_centre<-data.jr$prelevement-moy_conso


###  Standardiser en divisant par la variance
# 
# data.js$capital_norma<-data.js$capital_centre/var_cap
# data.js$prelevement_norma<-data.js$prelevement_centre/var_conso
# 
# data.jr$capital_norma<-data.jr$capital_centre/var_cap
# data.jr$prelevement_norma<-data.jr$prelevement_centre/var_conso


###  Standardiser en divisant par le max
# 
data.js$capital_norma<-data.js$capital_centre/max_cap
 data.js$prelevement_norma<-data.js$prelevement_centre/max_conso
 
 data.jr$capital_norma<-data.jr$capital_centre/max_cap
 data.jr$prelevement_norma<-data.jr$prelevement_centre/max_conso

 titre<-paste0(player1$capitalini,"_",player2$capitalini,"_",player3$capitalini,"_",player4$capitalini,
        "_",player1$parcellesini,"_",player2$parcellesini,"_",player3$parcellesini,"_",player4$parcellesini,
        "_",player1$pluie1,
          "_",player2$pluie2)

# f<-ggplot() +
#   geom_point(data = data.js, aes(x = capital, y = prelevement), colour = "red") + # Ajouter les points pour les données du joueur simulé
#   geom_point(data = data.jr, aes(x = capital, y =prelevement), colour = "blue", size = 3) + # Ajouter les points pour les données du joueur réel
#   #xlim(c(0, 200)) + # Définir les limites de l'axe des x
#   theme_bw() + # Appliquer un thème de type "black and white"
#   labs(x = "capital", y = "water consumption") # Ajouter des étiquettes et un titre 
# 
# 
# d<-ggplot() +
#   geom_point(data = data.js, aes(x = capital_norma, y = prelevement_norma), colour = "grey") + # Ajouter les points pour les données du joueur simulé
#   geom_point(data = data.jr, aes(x = capital_norma, y =prelevement_norma), colour = "black", size = 3) + # Ajouter les points pour les données du joueur réel
#   #xlim(c(0, 200)) + # Définir les limites de l'axe des x
#   theme_bw() + # Appliquer un thème de type "black and white"
#   labs(x = "cap normalise", y = "prelev norma") # Ajouter des étiquettes et un titre 
# 
# 
# grid.arrange(f, d, ncol = 2,top = textGrob(titre))  # Deux colonnes

tableau_normal.js<-rbind(tableau_normal.js,data.frame(capital_norma = data.js$capital_norma, 
                                                prelevement_norma = data.js$prelevement_norma))


tableau_normal.jr<-rbind(tableau_normal.jr,data.frame(capital_norma = data.jr$capital_norma, 
                                                   prelevement_norma = data.jr$prelevement_norma))

}

f<-ggplot() +
    geom_point(data = tableau_normal.js, aes(x = capital_norma, y = prelevement_norma), colour = "grey") + # Ajouter les points pour les données du joueur simulé
     geom_point(data = tableau_normal.jr, aes(x = capital_norma, y =prelevement_norma), colour = "black", size = 3) + # Ajouter les points pour les données du joueur réel
     #xlim(c(0, 200)) + # Définir les limites de l'axe des x
     theme_bw() + # Appliquer un thème de type "black and white"
     labs(x = "cap normalise", y = "prelev norma",title="données normalisées") # Ajouter des étiquettes et un titre 
   

data.jr.complet1 <- subset(data.jr.complet , select = c("capital_initial","parcelles_initiales","strat_culture", "strat_parcelle","stratSeau","strat_gag", "strat_lance", "Capital_final",
                                      "Consommation_eau", "agrandissement...achat.vente.", "empechement"))

colnames(data.jr.complet1 ) <- c("capitalini","parcellesini","stratCulture","parcelleculti", "stratSeau","stratGG","stratLance","capital", 
                       "prelevement","agrandissement","empechement")

m<-ggplot() +
  geom_point(data = data, aes(x = capital, y = prelevement), colour = "grey") + # Ajouter les points pour les données du joueur simulé
  geom_point(data = data.jr.complet1, aes(x = capital, y =prelevement), colour = "black", size = 3) + # Ajouter les points pour les données du joueur réel
  #xlim(c(0, 200)) + # Définir les limites de l'axe des x
  theme_bw() + # Appliquer un thème de type "black and white"
  labs(x = "cap normalise", y = "prelev norma", title="données brutes") # Ajouter des étiquettes et un titre 


grid.arrange(f, m, ncol = 2)  # Deux colonnes




#############Ancienne version avec ACP

# pca <- PCA(data.js, graph = FALSE)
# 
# ###  On normalise.
# dim1.normalise<-pca$ind$coord[,1]/max(abs(pca$ind$coord[,1]))
# dim2.normalise<-pca$ind$coord[,2]/max(abs(pca$ind$coord[,2]))
# sim_normalise<-data.frame(dim1.normalise,dim2.normalise)
# 
# # Centrage-réduction des nouveaux individus (en utilisant les moyennes et écarts-types de l'ACP)
# new_ind_scaled <- scale(data.jr, center = pca$call$centre, scale = pca$call$ecart.type)
# 
# # Projection sur les axes factoriels
# new_coord <- as.matrix(new_ind_scaled) %*% as.matrix(pca$var$coord)
# dim1.new_coord_norma<- new_coord[,1]/max(abs(pca$ind$coord[,1])) 
# dim2.new_coord_norma<- new_coord[,2]/max(abs(pca$ind$coord[,2])) 
# irl_normalise<-data.frame(dim1.new_coord_norma,dim2.new_coord_norma)
