# Supprimer tous les objets de l'environnement de travail
rm(list = ls())

# Charger les bibliothèques nécessaires
library(reshape2)
library(ggplot2)

# Définir le répertoire de travail comme celui du script en cours
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Charger les données du joueur simulé à partir d'un fichier CSV
data.js <- read.csv("../data/applatJoueur_simule_complet.csv")  # js pour joueur simulé

# Convertir la colonne stratCulture en numérique
data.js$stratCulture <- as.numeric(data.js$stratCulture)

# Afficher les noms des colonnes du jeu de données simulé
names(data.js)

data.jr <- read.csv("../data/gameSession_config_pluie_26.csv", header = TRUE, sep = ";", encoding = "latin1")  # jr pour joueur réel
names(data.jr)

# Créer un graphique à points avec ggplot2
ggplot() +
  geom_point(data = data.js, aes(x = capital, y = prelevement), colour = "grey") + # Ajouter les points pour les données du joueur simulé
  geom_point(data = data.jr, aes(x = Capital_final, y = Consommation_eau), colour = "black", size = 3) + # Ajouter les points pour les données du joueur réel
  #xlim(c(0, 200)) + # Définir les limites de l'axe des x
  theme_bw() + # Appliquer un thème de type "black and white"
  labs(x = "capital", y = "water consumption", title = "Joueurs réel et joueurs virtuels") # Ajouter des étiquettes et un titre 
