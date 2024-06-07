


#simulations
df<- read.csv("../data/applatJoueur_simule_complet.csv")
#df<- read.csv("../data/applatJoueur_simule_SameInit.csv")
# Essai joueurs
real_gameSession <- read.csv("../data/gameSession_config_pluie_26.csv", sep = ";", encoding="latin1")

### PREPARATION DES DONNEES #############
# enlever le capital négatif...

df$capitalmodif <- ifelse(df$capital < 0, 0, df$capital)

agg_data_gini <- aggregate(cbind(capitalmodif, prelevement) ~ partie, data = df, FUN = function(x) {
  GINI = ineq(x)
}, na.action = na.pass)
colnames(agg_data_gini)<-c("partie","ginicap","giniprelev")
# ¨pour les vraies parties
irl_gini <-aggregate(cbind(Capital_final,Consommation_eau)~ID.partie, data=real_gameSession,FUN = function(x) {
  GINI = ineq(x)
}, na.action = na.pass)

colnames(irl_gini)<-c("ID.partie","ginicap","giniprelev")

# prélèvements
agg_data_prelevement<-aggregate(prelevement ~ partie, data = df, FUN = sum)
irl_prelevement<- aggregate(Consommation_eau ~ ID.partie, data = real_gameSession, FUN = sum)
# somme capital
agg_data_capital<- aggregate(capital ~ partie, data = df, FUN = sum)
irl_captot <-aggregate(Capital_final ~ID.partie, data=real_gameSession,FUN = sum)

# strat Culture
agg_data_stratculture<- aggregate(stratCulture ~ partie, data = df, FUN = sum)
irl_stratculture<-aggregate(strat_culture ~ID.partie, data=real_gameSession,FUN = sum)


# strat Parcelle
agg_data_stratParcelle<- aggregate(stratParcelle ~ partie, data = df, FUN = sum)
irl_stratParcelle<- aggregate(strat_parcelle ~ ID.partie, data = real_gameSession, FUN = sum)


# stratSeau
agg_data_stratSeau<- aggregate(stratSeau ~ partie, data = df, FUN = sum)
irl_stratSeau <- aggregate(stratSeau ~ ID.partie, data = real_gameSession, FUN = sum)

# "stratGG"    
agg_data_stratGG<- aggregate(stratGG ~ partie, data = df, FUN = sum)
irl_stratGG <- aggregate(strat_gag ~ ID.partie, data = real_gameSession, FUN = sum)


#"stratLance" 
agg_data_stratLance<- aggregate(stratLance ~ partie, data = df, FUN = sum)
irl_stratlance <- aggregate(strat_lance ~ ID.partie, data = real_gameSession, FUN = sum)


#"puits"  

agg_data_puits<- aggregate(puits ~ partie, data = df, FUN = sum)
irl_puits <- aggregate(puits_profmax ~ ID.partie, data = real_gameSession, FUN = sum)


#"agrandissement"
agg_data_foncier<- aggregate(agrandissement ~ partie, data = df, FUN = sum)
irl_foncier <- aggregate(agrandissement ~ ID.partie, data = real_gameSession, FUN = sum)


#"empechement"
agg_data_empechement<- aggregate(empechement ~ partie, data = df, FUN = sum)
irl_empechement <- aggregate(empechement ~ ID.partie, data = real_gameSession, FUN = sum)


# prof nap
agg_data_prof<- aggregate(profnappe ~ partie, data = df, FUN = mean)

irl_conso <- aggregate(Consommation_eau ~ ID.partie, data = real_gameSession, FUN = sum)
irl_pluies2<- aggregate(Pluie_annee2 ~ ID.partie, data = real_gameSession, FUN = mean)
irl_pluies3<- aggregate(Pluie_annee3 ~ ID.partie, data = real_gameSession, FUN = mean)
irl_profnappe <- irl_conso*0.039-irl_pluies2*0.039-irl_pluies3*0.039
colnames(irl_profnappe)<- c("ID.partie","profnappe")
# irl : grouper les prélèvement et pluies
# =E2*0.039-I2*0.039



# Tableau final
tabparties<-cbind(agg_data_gini,agg_data_prof$profnappe,agg_data_capital$capital,
                  agg_data_stratculture$stratCulture,agg_data_stratParcelle$stratParcelle,
                  agg_data_stratSeau$stratSeau,agg_data_stratGG$stratGG,agg_data_stratLance$stratLance,
                  agg_data_puits$puits,agg_data_empechement$empechement,agg_data_foncier$agrandissement,
                  agg_data_prelevement$prelevement
)

colnames(tabparties)<- c("partie","ginicap","giniprelev","profnappe","sum_cap","stratculture",
                         "stratParcelle","stratSeau","StratGG","stratLance","puits","empechement","foncier","prelevements")

# Tableau final irl
tabparties_irl<-cbind(irl_gini,irl_profnappe$profnappe,irl_captot$Capital_final, irl_stratculture$strat_culture, 
                      irl_stratParcelle$strat_parcelle,irl_stratSeau$stratSeau, irl_stratGG$strat_gag,
                      irl_stratlance$strat_lance, irl_puits$puits_profmax, 
                      irl_empechement$empechement,irl_foncier$agrandissement,
                      irl_prelevement$Consommation_eau
)


colnames(tabparties_irl)<-c("partie","ginicap","giniprelev","profnappe","sum_cap","stratculture",
                            "stratParcelle","stratSeau","StratGG","stratLance","puits","empechement","foncier","prelevements")

