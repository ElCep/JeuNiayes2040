

# CODE POUR FLORENT
# ne pas oublier d'installer le package future
#install.package("future")
library(purrr)
library(furrr)

combi<- read.csv("D:/Mes Donnees/GitHub2025/JeuNiayes2040/data/inti_parties.csv")
j=1
input_df<-NULL
for (i in 1:nrow(combi)){

input_df_transi <- expand.grid(
  seq(j, j+2999, 1.0),  # entre 0 et 10
  combi[i,2],
  combi[i,3],
  combi[i,4],
  combi[i,5],
  combi[i,6],
  combi[i,7],
  combi[i,8],
  combi[i,9],
  combi[i,10],
  combi[i,11],
  c(25.0),
  c(15.0),
  c(12.0),
  c(9.0),
  c(10.0),
  c(6.0),
  c(3.0)
)
input_df<-rbind(input_df,input_df_transi)
j=j+3000

}
names(input_df) <- c("id" ,"cap1","cap2","cap3","cap4","surf1","surf2","surf3","surf4","pluie1","pluie2","pxpiment","pxaubergine","pxcarotte","pxchou","pxoignon","pxpdt","nbparties")

no_cores <- 10
plan(multisession, workers = no_cores)

future_pmap(list(input_df$id,input_df$cap1,input_df$cap2,input_df$cap3,input_df$cap4,input_df$surf1,input_df$surf2,input_df$surf3,
                 input_df$surf4,input_df$pluie1,input_df$pluie2,input_df$pxpiment,input_df$pxaubergine,input_df$pxcarotte,input_df$pxchou,input_df$pxoignon,input_df$pxpdt,input_df$nbparties),
            ~system(paste("java -jar C:/omp210/jeux/jeu_niayesbis.jar",..1,..2,..3,..4,..5,..6,..7,..8,..9,..10,..11,..12,..13,..14,..15,..16,..17,..18)))


#system("java -jar C:/omp210/ocelet_oct2021/omp210_win64/workspace/calib_hydro.jar")
#system("java -jar C:/omp210/jeux/jeu_niayes_essai.jar")