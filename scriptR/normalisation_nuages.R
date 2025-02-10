

library(jsonlite)



# Lire le fichier JSON issue du script OpenMole replication.oms
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data <- fromJSON("../data/result_csv_sampling.json")
results <- data$data$variables$result

combi<-data$data$result
