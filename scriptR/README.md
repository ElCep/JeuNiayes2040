## A quoi servent les différents scripts

├── applatTableau_gini.R Creer un _data frame_ a partir des données simulé et sauvegardé dans ../data/applatJoueurs*
├── applatTableau.R
├── applatTableau_sameInit.R
├── createTablePartie.R creer un tableau agrégé par partie des données simulé et donné des joueurs (avec Gini) --> deux tableau en sortie, un pour les simulations et un pour les joueurs 
├── function_stat_OLD.R la fonction stat est appeller dans les script applatTableau. Ce fichier est une vieille fonction
├── function_stat.R     la fonction stat est appeller par les script applatTableau. C'est le fichier utiliser actuellement
├── irl_vs_sim.R        on part des tableau applat, et on fait un plot entre les données somuler et plot.
├── PCA_test_ggplot.R   
├── read_json_plot.R
├── replication.R                script qui produit des boxplot pour savoir compbien de réplication doivent être pour échapé au l'aléatoire.
├── traitement_joueur.R          script d'analyse qui produit les plots a l'échelle des joueurs
├── traitement_joueur_training.R script qui essaye d'utiliser du deepleaning pour classifier les joueur
└── traitement_parties.R         script qui analyse les parties

