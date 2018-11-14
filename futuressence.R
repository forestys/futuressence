
# Calcule le DEE (deficit en eau estival) et la temperature moyenne annuelle pour une periode
# de reference et pour un climat futur, sur des grilles de points rectangulaires.
# Donnees utilisees :
# - raster donnant la RUM du sol (reserve utile maximale) en mm, mesuree sur le terrain, sinon modelisee,
# - des rasters mensuels pour le climat de la periode de reference : ETP (evapotranspiration en mm/mois),
#   temperature en degre Celsius et precipitations en mm/mois,
# - les anomalies climatiques mensuelles pour le climat futur (constantes sur toute la zone) : 
#   temperature, precipitations en mm/s, rayonnement reel en W/m2.

# Liste des variables a saisir : les lieux du code concernes sont entre "" ci-apres. 
# stockage des librairies : "#install des packages"
# Repertoire de travail pour le calcul : "# Repertoire de travail"
# Definition de la grille des points de calcul : #Generation de la grille de points
# Fichier de la RUM modelisee par le Lerfob (au pas de 50 m voire de 1 km) : "# Extraction de la RUM"
# Fichier des mesures terrain de RUM : "# Extraction des donnees terrain de RUM"
####### Idem pour les donnees bioindiquees, fournies par le Lerfob ou mesur?es sur le terrain.
# Donnees climatiques de la periode de reference : "# Extraction des donnees climatiques"
# Anomalies climatiques pour la periode future : "# Lecture des anomalies climatiques"



#install des packages
library(raster)
library(sp)
library(SpatialTools)
library(rgdal)

# ETAPE 1 ###################################################################################
# Import de la base de calcul

# Repertoire de travail
setwd("E:/2_travail_calcul")

## TODO a commenter sur master
getwd()
dir()

# Generation de la grille de points, par leurs coordonn?es Lambert II epsg:27572
# Exemple pour g?n?rer un rectangle de coordonn?es X allant de 985870 ? 987130 et Y allant de 2461280 ? 2462270 avec une r?solution de 10 unit?s de carte (m).
# On peut aussi remplacer extent par une autre couche SIG [ex : grid <- raster(ext=extent(carte_france), res=5000)]
grid1 <- raster(ext=extent(1037220,1038480,6891300,6892310), res=10)
grid2 <- raster(ext=extent(1038510,1039890,6892110,6892840), res=10)
grid3 <- raster(ext=extent(1039250,1040000,6889930,6890600), res=10)
grid4 <- raster(ext=extent(1047000,1047960,6883320,6884240), res=10)

#Wgrid1 <- raster(ext=extent(985870,987130,2461280,2462270), res=10)
#grid2 <- raster(ext=extent(987170,988540,2462110,2462820), res=10)
#grid3 <- raster(ext=extent(987940,988690,2459930,2460580), res=10)
#grid4 <- raster(ext=extent(995720,997220,2453080,2454700), res=10)

#convertir en tableau r?unissant les diff?rentes grilles
coordonnees1 <- as.data.frame(rasterToPoints(grid1))
coordonnees2 <- as.data.frame(rasterToPoints(grid2))
coordonnees3 <- as.data.frame(rasterToPoints(grid3))
coordonnees4 <- as.data.frame(rasterToPoints(grid4))
coordonnees<- rbind(coordonnees1, coordonnees2, coordonnees3, coordonnees4)
colnames(coordonnees)<- c("Xl2", "Yl2")

# Sauvegarder ce tableau dans le dossier
write.table(coordonnees, file="E:/2_travail_calcul/coordonnees.csv",sep=";",dec=".")
      
# Pour visualiser sous R :
# grid1.x <- as(grid1, "SpatialGrid")
# plot(grid1.x)
      

# D?but de construction du tableau des donn?es pour le calcul
base<-read.csv('coordonnees.csv',header=TRUE, sep = ";", dec = ".")
dim(base)
#extraction des coordonn?es
coord<-base[,c("Xl2", "Yl2")]   
head(coord)

# Extraction de la RUM mod?lis?e par le Lerfob au pas de 50 m si disponible sinon de 1 km
rum_lerfob <- "E:/2_Donnees/modeles_distrib/variables/lorraine/rumlor50/hdr.adf" 
# Extraction des donn?es terrain de RUM (fichier vecteur rast?ris? au format ESRI.hdr Labelled)
rum_terrain <- "E:/1_Pro_Silva_2018_5a/Mesures/RUM_iti5.tif"

listacces<-c(rum_lerfob, rum_terrain)
listnom<-c( "rum_lerfob", "rum_terrain")

tableraster=as.data.frame(matrix(data=NA,nrow=nrow(base),ncol=length(listnom))) 
colnames(tableraster)=listnom
summary(tableraster)
dim(tableraster)

projLII<-CRS("+init=epsg:27572")    # param?trage projection lambert 2
projL93<-CRS("+init=epsg:2154")

for (i in 1:length(listnom) ) {
  rast=raster(listacces[i])
  projection(rast)=projLII
  tableraster[,i]=extract(x=rast,y=coord)
  print(paste(Sys.time()," - ",listnom[i],sep=""))
}
head(tableraster)
summary(tableraster)

# Quand rum_terrain est non vide alors remplacer rum_lerfob par rum_terrain
tablerasterrum=as.data.frame(matrix(data=NA,nrow=nrow(base),ncol=1)) 
colnames(tablerasterrum)=c("rum")
dim(tablerasterrum)
tablerasterrum[,1]<- ifelse (tableraster[,dimnames(tableraster)[[2]]=="rum_terrain"]==NA, tableraster[,dimnames(tableraster)[[2]]=="rum_lerfob"], tableraster[,dimnames(tableraster)[[2]]=="rum_terrain"])
######### (expression ? simplifier ?)
head(tablerasterrum)

# Inclure la RUM dans base
base<-cbind(base, tablerasterrum)
head(base)

######### Recommencer la m?me chose pour pH, C/N, engorgement temporaire et engorgement permanent.

# Extraction du pH mod?lis?e par le Lerfob au pas de 50 m si disponible sinon de 1 km
ph_lerfob <- "E:/2_Donnees/modeles_distrib/variables/lorraine/ph_lor_50/hdr.adf" 
# Extraction des donn?es terrain de pH (fichier vecteur rast?ris? au format ESRI.hdr Labelled)
ph_terrain <- "E:/1 Pro Silva 2018 5a/Mesures/PH_iti5_esri_hdr_labelled"

listacces<-c(ph_lerfob, ph_terrain)
listacces

listnom<-c( "ph_lerfob", "ph_terrain")
listnom

tableraster=as.data.frame(matrix(data=NA,nrow=nrow(base),ncol=length(listnom))) 
colnames(tableraster)=listnom
tableraster
dim(tableraster)

projLII<-CRS("+init=epsg:27572")    # param?trage projection lambert 2

for (i in 1:length(listnom) ) {
  rast=raster(listacces[i])
  projection(rast)=projLII
  tableraster[,i]=extract(x=rast,y=coord)
  print(paste(Sys.time()," - ",listnom[i],sep=""))
}
head(tableraster)

# Quand ph_terrain est non vide alors remplacer ph_lerfob par ph_terrain
tablerasterph=as.data.frame(matrix(data=NA,nrow=nrow(base),ncol=1)) 
colnames(tablerasterph)=c("ph")
tablerasterph
dim(tablerasterph)
tablerasterph[,1]<- ifelse (tableraster[,dimnames(tableraster)[[2]]=="ph_terrain"]=NA, tableraster[,dimnames(tableraster)[[2]]=="ph_lerfob"], tableraster[,dimnames(tableraster)[[2]]=="ph_terrain"])
######### (expression ? simplifier ?)

# Inclure le pH dans base
base<-cbind(base, tablerasterph)
head(base)

################################################"


# Extraction du rapport C/N mod?lis? par le Lerfob au pas de 50 m si disponible sinon de 1 km
cn_lerfob <- "E:/2_Donnees/modeles_distrib/variables/lorraine/cn_50_lor_cor/hdr.adf"    
# Extraction des donn?es terrain dd'eng. temp. (fichier vecteur rast?ris? au format ESRI.hdr Labelled)
cn_terrain <- "E:/1 Pro Silva 2018 5a/Mesures/CN_iti5_esri_hdr_labelled"

listacces<-c(cn_lerfob, cn_terrain)
listacces

listnom<-c( "cn_lerfob", "cn_terrain")
listnom

tableraster=as.data.frame(matrix(data=NA,nrow=nrow(base),ncol=length(listnom))) 
colnames(tableraster)=listnom
tableraster
dim(tableraster)

projLII<-CRS("+init=epsg:27572")    # param?trage projection lambert 2

for (i in 1:length(listnom) ) {
  rast=raster(listacces[i])
  projection(rast)=projLII
  tableraster[,i]=extract(x=rast,y=coord)
  print(paste(Sys.time()," - ",listnom[i],sep=""))
}
head(tableraster)

# Quand cn_terrain est non vide alors remplacer cn_lerfob par cn_terrain
tablerasteret=as.data.frame(matrix(data=NA,nrow=nrow(base),ncol=1)) 
colnames(tablerasteret)=c("cn")
tablerastercn
dim(tablerastercn)
tablerastercn[,1]<- ifelse (tableraster[,dimnames(tableraster)[[2]]=="cn_terrain"]=NA, tableraster[,dimnames(tableraster)[[2]]=="cn_lerfob"], tableraster[,dimnames(tableraster)[[2]]=="cn_terrain"])
######### (expression ? simplifier ?)

# Inclure l'eng. temp. dans base
base<-cbind(base, tablerastercn)
head(base)


################################################"

# Extraction de l'engorgement temporaire mod?lis? par le Lerfob au pas de 50 m si disponible sinon de 1 km
et_lerfob <- "E:/2_Donnees/modeles_distrib/variables/lorraine/modeleetbon/hdr.adf"
# Extraction des donn?es terrain d'eng. temp. (fichier vecteur rast?ris? au format ESRI.hdr Labelled)
et_terrain <- "E:/1 Pro Silva 2018 5a/Mesures/ET_iti5_esri_hdr_labelled"

listacces<-c(et_lerfob, et_terrain)
listacces

listnom<-c( "et_lerfob", "et_terrain")
listnom

tableraster=as.data.frame(matrix(data=NA,nrow=nrow(base),ncol=length(listnom))) 
colnames(tableraster)=listnom
tableraster
dim(tableraster)

projLII<-CRS("+init=epsg:27572")    # param?trage projection lambert 2

for (i in 1:length(listnom) ) {
  rast=raster(listacces[i])
  projection(rast)=projLII
  tableraster[,i]=extract(x=rast,y=coord)
  print(paste(Sys.time()," - ",listnom[i],sep=""))
}
head(tableraster)

# Quand et_terrain est non vide alors remplacer et_lerfob par et_terrain
tablerasteret=as.data.frame(matrix(data=NA,nrow=nrow(base),ncol=1)) 
colnames(tablerasteret)=c("et")
tablerasteret
dim(tablerasteret)
tablerasteret[,1]<- ifelse (tableraster[,dimnames(tableraster)[[2]]=="et_terrain"]=NA, tableraster[,dimnames(tableraster)[[2]]=="et_lerfob"], tableraster[,dimnames(tableraster)[[2]]=="et_terrain"])
######### (expression ? simplifier ?)

# Inclure l'eng. temp. dans base
base<-cbind(base, tablerasteret)
head(base)


################################################"


# Extraction de l'engorgement permanent mod?lis? par le Lerfob au pas de 50 m si disponible sinon de 1 km
ep_lerfob <- "E:/2_Donnees/modeles_distrib/variables/france1km/ep_ess2/hdr.adf"
# Extraction des donn?es terrain d'eng. perm. (fichier vecteur rast?ris? au format ESRI.hdr Labelled)
ep_terrain <- "E:/1 Pro Silva 2018 5a/Mesures/EP_iti5_esri_hdr_labelled"

listacces<-c(ep_lerfob, ep_terrain)
listacces

listnom<-c( "ep_lerfob", "ep_terrain")
listnom

tableraster=as.data.frame(matrix(data=NA,nrow=nrow(base),ncol=length(listnom))) 
colnames(tableraster)=listnom
tableraster
dim(tableraster)

projLII<-CRS("+init=epsg:27572")    # param?trage projection lambert 2

for (i in 1:length(listnom) ) {
  rast=raster(listacces[i])
  projection(rast)=projLII
  tableraster[,i]=extract(x=rast,y=coord)
  print(paste(Sys.time()," - ",listnom[i],sep=""))
}
head(tableraster)

# Quand ep_terrain est non vide alors remplacer ep_lerfob par ep_terrain
tablerasteret=as.data.frame(matrix(data=NA,nrow=nrow(base),ncol=1)) 
colnames(tablerasterep)=c("ep")
tablerasterep
dim(tablerasterep)
tablerasterep[,1]<- ifelse (tableraster[,dimnames(tableraster)[[2]]=="ep_terrain"]=NA, tableraster[,dimnames(tableraster)[[2]]=="ep_lerfob"], tableraster[,dimnames(tableraster)[[2]]=="ep_terrain"])
######### (expression ? simplifier ?)

# Inclure l'eng. perm. dans base
base<-cbind(base, tablerasterep)
head(base)


################################################"

# Extraction des donn?es climatiques de r?f?rence
# Exemple de chemin d'acc?s : F:/Climat/Digitalis_v1/etp_v1/etp_8610_1/w001001.adf
for (var in c("tmoy_", "prec_", "etp_")){
    for (mois in 1:12 ) {
      nomvar <- gsub(" ", "", paste(var, "8610_", mois), fixed = TRUE)
      PATH_VAR    <- gsub(" ", "", paste("F:/Climat/Digitalis_v1/", var, "v1/", nomvar, "/w001001.adf"), fixed = TRUE)
      print(PATH_VAR)
      col<- dim(base)[2] + 1
      rast=raster(PATH_VAR, sp=TRUE)
      projection(rast)=projLII
      base[,col]=extract(x=rast,y=coord)
      colnames(base)[col] = nomvar
      print(paste(Sys.time()," - ",var,sep=""))
   }
}

# Calcul de la temp?rature moyenne annuelle de r?f?rence
table_tmoy_mens=as.data.frame(matrix(data=NA, nrow=nrow(base), ncol=12))
for (mois in 1:12) {
  nomvar <-gsub(" ", "", paste("temp_8610_", mois), fixed = TRUE)
  table_tmoy_mens[,mois]=base[,dimnames(base)[[2]]==nomvar]
}
table_tmoy_an=as.data.frame(matrix(data=NA, nrow=nrow(base), ncol=1))
table_tmoy_an<-rowMeans(table_tmoy_mens, na.rm = FALSE,dims=1)
colnames(table_tmoy_an)="tmoy_8610"    ########## Syntaxe ?

# Inclure les temp?ratures annuelles de r?f?rence dans base
base<-cbind(base, table_tmoy_an)
head(base)
dim(base)
head(base [1,], n=1)    


# Lecture des anomalies climatiques
anomalies<-read.csv("E:/1 Pro Silva 2018 5a/2046_2065/anomalies_climat.csv", header = TRUE, sep = ";", dec = ",")   ############## V?rifier syntaxe

# Calcul des temp?ratures futures mensuelles
table_tmoy_mensf=as.data.frame(matrix(data=NA, nrow=nrow(base), ncol=12))
for (mois in 1:12) {
  nomvar <-gsub(" ", "", paste("temp_8610_", mois), fixed = TRUE)
  table_tmoy_mensf[,mois]=base[,dimnames(base)[[2]]==nomvar]+anomalies[dimnames(anomalies[[1]]==mois),dimnames(anomalies[[2]]=="tas")]
  nomvar <-gsub(" ", "", paste("temp_4665_", mois), fixed = TRUE)
  colnames(table_tmoy_mensf)[mois]= nomvar
}
# Calcul des temp?ratures futures annuelles
table_tmoy_anf=as.data.frame(matrix(data=NA, nrow=nrow(base), ncol=1))
table_tmoy_anf<-rowMeans(table_tmoy_mensf, na.rm = FALSE,dims=1)
colnames(table_tmoy_anf)="tmoy_4665"

# Inclure les temp?ratures futures annuelles dans base
base<-cbind(base, table_tmoy_anf)
head(base)

# Calcul des pr?cipitations futures mensuelles
table_prec_mensf=as.data.frame(matrix(data=NA, nrow=nrow(base), ncol=12))
for (mois in 1:12) {
  nomvar <-gsub(" ", "", paste("prec_8610_", mois), fixed = TRUE)
  table_prec_mensf[,mois]=base[,dimnames(base)[[2]]==nomvar]+anomalies[dimnames(anomalies[[1]]==mois),dimnames(anomalies[[2]]=="pr")]*2592000 # 1 mm/s = 2592000 mm/mois
  nomvar <-gsub(" ", "", paste("prec_4665_", mois), fixed = TRUE)
  colnames(table_prec_mensf)[mois]= nomvar
}
# Inclure les pr?cipitations futures dans base
base<-cbind(base, table_prec_mensf)
head(base)

# Calcul des etp futures
table_etp_mensf=as.data.frame(matrix(data=NA, nrow=nrow(base), ncol=12))
for (mois in 1:12) {
  nomvar <-gsub(" ", "", paste("etp_8610_", mois), fixed = TRUE)
  table_etp_mensf[,mois]=                                                      ############## formule ? ?crire
  nomvar <-gsub(" ", "", paste("etp_4665_", mois), fixed = TRUE)
  colnames(table_etp_mensf)[mois]= nomvar
}
# Inclure les etp futures dans base
base<-cbind(base, table_etp_mensf)
head(base)



write.table(base, file="base_etape1.csv",append=FALSE,quote=FALSE,sep=",",eol="\n",na="NA",dec=".",row.names=FALSE,col.names=TRUE,qmethod=c("escape","double"))

# Colonnes de base_etape1.csv : 
# . X, Y, rum, 4 param?tres de chimie du sol, 
# . temp?ratures mensuelles de r?f?rence, temp?rature moyenne annuelle de r?f., 
# . 12 pr?cipitations mensuelles de r?f?rence, 12 etp mensuelles de r?f?rence,
# . temp?rature moyenne annuelle future,
# . 12 pr?cipitations mensuelles futures, 12 etp mensuelles futures,





