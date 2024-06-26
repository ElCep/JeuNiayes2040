library("dplyr")
library("ggplot2")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#df <- read.csv("../data/result_replication_100k.csv")
#saveRDS(df, file = "../data/result_replication_100k.Rdata")

df <- readRDS(file = "../data/result_replication_100k.Rdata")
set.seed(123)
# on va tirer 10 fois le même nombre de sample 
# créer des groupes dans un data frame
# et on plotera les boxplot correspondant par groupe

sample.v <- c(10,100,1000,2000,3000) # vecteur du nombre de tirage

df.gp <- data.frame()
for(i in sample.v){
  for(j in 1:10){
    a <- df %>% slice_sample(n = i, replace = T)
    a$gp <- j
    df.gp <- rbind(df.gp, a)
  }
  ggplot(data = df.gp)+
    geom_boxplot(aes(x = as.factor(gp), y = waterConsumption))+
    labs(x  = "sample", title = paste("nombre de réplication:", i))+
    ylab(c(0,250))
  ggsave(paste0("../img/sample/sample",i,".png"))
}

