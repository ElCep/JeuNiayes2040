# Installer les packages nécessaires si ce n'est pas déjà fait
install.packages("e1071")
install.packages("caret")

# Charger les packages nécessaires
library(e1071)
library(caret)

# Charger les données
# Supposons que nous avons deux dataframes: real_players et simulated_players
# real_players contient les observations réelles et simulated_players contient les observations simulées

# Exemple de données fictives (remplacez cela par vos données réelles)
set.seed(125)
# real_players <- data.frame(
#   score = rnorm(100, mean = 75, sd = 10),
#   experience = rnorm(100, mean = 5, sd = 2),
#   class = sample(c('Beginner', 'Intermediate', 'Advanced'), 100, replace = TRUE)
# )

real_players <- data.jr
names(data.jr)
real_players <- subset(data.jr, select = c("Capital_final", "Consommation_eau", "cluster"))

simulated_players <- subset(data.js, select = c("capital", "prelevement"))
# simulated_players <- data.frame(
#   score = rnorm(50, mean = 70, sd = 15),
#   experience = rnorm(50, mean = 4, sd = 3)
# )

# Diviser les données réelles en ensembles de formation et de test

train_index <- createDataPartition(real_players$cluster, p = 0.8, list = FALSE)
train_data <- real_players[train_index, ]
test_data <- real_players[-train_index, ]

# Entraîner le modèle Naive Bayes
model <- naiveBayes(cluster ~ ., data = train_data)

# Prédire les classes sur l'ensemble de test pour évaluation
pred_test <- predict(model, test_data)

# Évaluer la performance du modèle
confusionMatrix(pred_test, as.factor(test_data$cluster))

# Prédire les classes des joueurs simulés
pred_simulated <- predict(model, simulated_players)

# Ajouter les prédictions aux données simulées
simulated_players$cluster <- pred_simulated

# Afficher les données simulées avec les classes prédites
print(simulated_players)

ggplot(data = simulated_players, aes(x=capital, y = prelevement, colour = cluster))+
  geom_point()+
  theme_bw()
