
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
library(gam)

# ETAPE 1 ###################################################################################
# Import de la base de calcul

# TODO a mettre en tmp
# Repertoire de travail de la fonction
rep_travail <- "E:/2_travail_calcul"
# Repertoire du projet
rep_projet <- "E:/1_Pro_Silva_2018_5a"
# Repertoire des donnes du Lerfob
rep_data <- "E:/2_Donnees"
# Repertoire des donnes climatiques
rep_clim <- "F:/Climat"

# resolution de la grille
resol_grid <- 10
# parametrage projection lambert 2
projLII <- CRS("+init=epsg:27572")    
projL93 <- CRS("+init=epsg:2154")

# Se mettre dans le repertoire de travail
setwd(rep_travail)
## TODO a commenter sur master
getwd()
dir()

# TODO saisi des polygones de travail ou utiliser le point saisi
# rendre la resolution parametrable 10m par defaut
# extraire les grilles sur chaque tenement des multipolygones saisis

# Generation de la grille de points, par leurs coordonnees Lambert II epsg:27572
# Exemple pour generer un rectangle de coordonn?es X allant de 
# 985870 a 987130 et Y allant de 2461280 a 2462270 avec une resolution de 10 unites de carte (m).
# On peut aussi remplacer extent par une autre couche SIG :
# [ex : grid <- raster(ext=extent(carte_france), res=5000)]

grid1 <- raster(ext=extent(1037220,1038480,6891300,6892310), res=resol_grid)
grid2 <- raster(ext=extent(1038510,1039890,6892110,6892840), res=resol_grid)
grid3 <- raster(ext=extent(1039250,1040000,6889930,6890600), res=resol_grid)
grid4 <- raster(ext=extent(1047000,1047960,6883320,6884240), res=resol_grid)

# convertir en tableau reunissant les differentes grilles
coordonnees1 <- as.data.frame(rasterToPoints(grid1))
coordonnees2 <- as.data.frame(rasterToPoints(grid2))
coordonnees3 <- as.data.frame(rasterToPoints(grid3))
coordonnees4 <- as.data.frame(rasterToPoints(grid4))
base <- rbind(coordonnees1, coordonnees2, coordonnees3, coordonnees4)
colnames(coordonnees)<- c("Xl2", "Yl2")

# Sauvegarder ce tableau dans le dossier
write.table(base, file=paste0(rep_travail, "/coordonnees.csv"), sep=";", dec=".")

# TODO ajouter un parametre pour visualiser cette partie
# Pour visualiser sous R :
# grid1.x <- as(grid1, "SpatialGrid")
# plot(grid1.x)
      
# Debut de construction du tableau des donnees pour le calcul
# base <- read.csv(paste0(rep_travail, "coordonnees.csv"), header=TRUE, sep = ";", dec = ".")

dim(base)
# extraction des coordonn?es
coord <- base[,c("Xl2", "Yl2")]   
head(coord)

# Extraction de la RUM modelisee par le Lerfob au pas de 50 m si disponible sinon de 1 km
rum_lerfob <- paste0(rep_data, "/modeles_distrib/variables/lorraine/rumlor50/hdr.adf")
# Extraction des donnees terrain de RUM (fichier vecteur rasterise au format)
rum_terrain <- paste0(rep_projet, "/Mesures/RUM_iti5.tif")

listacces <- c(rum_lerfob, rum_terrain)
listnom <- c( "rum_lerfob", "rum_terrain")

# TODO optimiser le nommage des colonnes
tableraster <- as.data.frame(matrix(data = NA, nrow = nrow(base), ncol = length(listnom))) 
colnames(tableraster) = listnom

# TODO a commenter en master
summary(tableraster)
dim(tableraster)

for (i in 1:length(listnom)) {
  rast = raster(listacces[i])
  projection(rast) = projLII
  tableraster[,i] = extract(x = rast, y = coord)
  print(paste0(Sys.time()," - ", listnom[i]))
}

# TODO a commenter en master
head(tableraster)
summary(tableraster)

# Quand rum_terrain est non vide alors remplacer rum_lerfob par rum_terrain
tablerasterrum <- as.data.frame(matrix(data = NA, nrow = nrow(base), ncol = 1))
colnames(tablerasterrum)=c("rum")

# TODO a commenter dans master
dim(tablerasterrum)

# TODO a optimiser
tablerasterrum[,1]<- ifelse (tableraster[,dimnames(tableraster)[[2]]=="rum_terrain"]==NA, tableraster[,dimnames(tableraster)[[2]]=="rum_lerfob"], tableraster[,dimnames(tableraster)[[2]]=="rum_terrain"])

# TODO a commenter en master
######### (expression a simplifier ?)
head(tablerasterrum)

# Inclure la RUM dans base
base <- cbind(base, tablerasterrum) 

# TODO a commenter en master
head(base)

######### Recommencer la meme chose pour pH, C/N, engorgement temporaire et engorgement permanent.

# Extraction du pH modelisee par le Lerfob au pas de 50 m si disponible sinon de 1 km
ph_lerfob <- paste0(rep_data, "/modeles_distrib/variables/lorraine/ph_lor_50/hdr.adf")

# Extraction des donnees terrain de pH (fichier vecteur rasterise au format tif)
ph_terrain <- paste0(rep_projet, "/Mesures/PH.tif")

listacces <- c(ph_lerfob, ph_terrain)
listacces

listnom <- c( "ph_lerfob", "ph_terrain")
listnom

tableraster <- as.data.frame(matrix(data = NA, nrow = nrow(base), ncol = length(listnom))) 
colnames(tableraster) <- listnom

# TODO a commenter dans master
head(tableraster)
dim(tableraster)

for (i in 1:length(listnom) ) {
  rast = raster(listacces[i])
  projection(rast) = projLII
  tableraster[,i] = extract(x=rast,y=coord)
  print(paste0(Sys.time()," - ",listnom[i]))
}

# TODO a commenter dans master
head(tableraster)

# Quand ph_terrain est non vide alors remplacer ph_lerfob par ph_terrain
tablerasterph <- as.data.frame(matrix(data = NA, nrow = nrow(base), ncol = 1)) 
colnames(tablerasterph) <- c("ph")

# TODO a commenter dans master
tablerasterph
dim(tablerasterph)

tablerasterph[,1] <- ifelse (tableraster[,dimnames(tableraster)[[2]]=="ph_terrain"]=NA, tableraster[,dimnames(tableraster)[[2]]=="ph_lerfob"], tableraster[,dimnames(tableraster)[[2]]=="ph_terrain"])

######### (expression ? simplifier ?)

# Inclure le pH dans base
base <- cbind(base, tablerasterph)

# TODO a commenter dans master
head(base)

################################################"

# Extraction du rapport C/N modelise par le Lerfob au pas de 50 m si disponible sinon de 1 km
cn_lerfob <- paste0(rep_data, "/modeles_distrib/variables/lorraine/cn_50_lor_cor/hdr.adf")    
# Extraction des donn?es terrain dd'eng. temp. (fichier vecteur rast?ris? au format tif)
cn_terrain <- paste0(rep_projet, "/Mesures/CN.tif")

listacces <- c(cn_lerfob, cn_terrain)
listacces

listnom <- c("cn_lerfob", "cn_terrain")
listnom

tableraster <- as.data.frame(matrix(data = NA, nrow = nrow(base), ncol = length(listnom))) 
colnames(tableraster) <- listnom

# TODO a commenter dans master
tableraster
dim(tableraster)

for (i in 1:length(listnom) ) {
  rast = raster(listacces[i])
  projection(rast) = projLII
  tableraster[,i] = extract(x = rast, y = coord)
  print(paste0(Sys.time()," - ", listnom[i]))
}

# TODO a commenter dans master
head(tableraster)

# Quand cn_terrain est non vide alors remplacer cn_lerfob par cn_terrain
tablerasteret <- as.data.frame(matrix(data = NA, nrow = nrow(base), ncol = 1)) 
colnames(tablerasteret) <- c("cn")

# TODO a commenter dans master
tablerastercn
dim(tablerastercn)

tablerastercn[,1]<- ifelse (tableraster[,dimnames(tableraster)[[2]]=="cn_terrain"]=NA, tableraster[,dimnames(tableraster)[[2]]=="cn_lerfob"], tableraster[,dimnames(tableraster)[[2]]=="cn_terrain"])
######### (expression ? simplifier ?)

# Inclure l'eng. temp. dans base
base<-cbind(base, tablerastercn)

# TODO a commenter dans master
head(base)


################################################"

# Extraction de l'engorgement temporaire modelise par le Lerfob au pas de 50 m si disponible sinon de 1 km
et_lerfob <- paste0(rep_data, "/modeles_distrib/variables/lorraine/modeleetbon/hdr.adf")
# Extraction des donn?es terrain d'eng. temp. (fichier vecteur rast?ris? au format tif)
et_terrain <- paste0(rep_projet, "/Mesures/ET.tif")

listacces <- c(et_lerfob, et_terrain)
listacces

listnom <- c("et_lerfob", "et_terrain")
listnom

tableraster <- as.data.frame(matrix(data = NA, nrow = nrow(base), ncol = length(listnom))) 
colnames(tableraster) <- listnom

# TODO a commenter dans master
tableraster
dim(tableraster)

for (i in 1:length(listnom) ) {
  rast = raster(listacces[i])
  projection(rast) = projLII
  tableraster[,i] = extract(x=rast,y=coord)
  print(paste0(Sys.time()," - ",listnom[i]))
}

# TODO a commenter dans master
head(tableraster)

# Quand et_terrain est non vide alors remplacer et_lerfob par et_terrain
tablerasteret <- as.data.frame(matrix(data=NA,nrow=nrow(base),ncol=1)) 
colnames(tablerasteret) <- c("et")

# TODO a commenter dans master
tablerasteret
dim(tablerasteret)

tablerasteret[,1] <- ifelse (tableraster[,dimnames(tableraster)[[2]]=="et_terrain"]=NA, tableraster[,dimnames(tableraster)[[2]]=="et_lerfob"], tableraster[,dimnames(tableraster)[[2]]=="et_terrain"])
######### (expression ? simplifier ?)

# Inclure l'eng. temp. dans base
base <- cbind(base, tablerasteret)
head(base)


################################################"


# Extraction de l'engorgement permanent modelise par le Lerfob au pas de 50 m si disponible sinon de 1 km
ep_lerfob <- paste0(rep_data, "/modeles_distrib/variables/france1km/ep_ess2/hdr.adf")
# Extraction des donn?es terrain d'eng. perm. (fichier vecteur rast?ris? au format tif)
ep_terrain <- paste0(rep_projet, "/Mesures/EP.tif")

listacces <- c(ep_lerfob, ep_terrain)
listacces

listnom <- c("ep_lerfob", "ep_terrain")
listnom


tableraster <- as.data.frame(matrix(data = NA,nrow = nrow(base),ncol = length(listnom))) 
colnames(tableraster) <- listnom

# TODO a commenter dans master
tableraster
dim(tableraster)

for (i in 1:length(listnom) ) {
  rast = raster(listacces[i])
  projection(rast) = projLII
  tableraster[,i] = extract(x=rast,y=coord)
  print(paste0(Sys.time()," - ",listnom[i]))
}

# TODO a commenter dans master
head(tableraster)

# Quand ep_terrain est non vide alors remplacer ep_lerfob par ep_terrain
tablerasteret <- as.data.frame(matrix(data = NA, nrow = nrow(base), ncol = 1)) 
colnames(tablerasterep) <- c("ep")

# TODO a commenter dans master
tablerasterep
dim(tablerasterep)

tablerasterep[,1] <- ifelse (tableraster[,dimnames(tableraster)[[2]]=="ep_terrain"]=NA, tableraster[,dimnames(tableraster)[[2]]=="ep_lerfob"], tableraster[,dimnames(tableraster)[[2]]=="ep_terrain"])
######### (expression ? simplifier ?)

# Inclure l'eng. perm. dans base
base <- cbind(base, tablerasterep)
head(base)


################################################"

# Extraction des donnees climatiques de reference
# Exemple de chemin d'acces : F:/Climat/Digitalis_v1/etp_v1/etp_8610_1/w001001.adf
for (var in c("tmoy_", "prec_", "etp_")){
    for (mois in 1:12 ) {
      nomvar <- paste0(var, "8610_", mois)
      PATH_VAR <- paste0(rep_clim, "/Digitalis_v1/", var, "v1/", nomvar, "/w001001.adf")
      print(PATH_VAR)
      col <- dim(base)[2] + 1
      rast <- raster(PATH_VAR, sp=TRUE)
      projection(rast) <- projLII
      base[,col] <- extract(x = rast, y = coord)
      colnames(base)[col] = nomvar
      print(paste0(Sys.time()," - ", var))
   }
}

# Calcul de la temperature moyenne annuelle de reference
base$tmoy_8610 <- (base$tmoy_8610_1 + base$tmoy_8610_2 + base$tmoy_8610_3 + base$tmoy_8610_4 + base$tmoy_8610_5 + base$tmoy_8610_6 +
                     base$tmoy_8610_7 + base$tmoy_8610_8 + base$tmoy_8610_9 + base$tmoy_8610_10 + base$tmoy_8610_11 + base$tmoy_8610_12)/12

# TODO a commenter dans master
head(base, n=1)

# Lecture des anomalies climatiques genere par Georges pour le projet
# TODO creer un tif krige des anomalies clim
anomalies <- read.csv(paste0(rep_projet, "/2046_2065/Anomalies_climat.csv"), header = TRUE, sep = ";", dec = ",")

# TODO a commenter dans master
head(anomalies)


############################################################################################    Reprendre ici

# Calcul des temperatures futures mensuelles
for (mois in c(1:12)){
  value_anom <- anomalies[which(anomalies[,"mois"] == mois), "tas"]
  nom_colonne <- paste0("tmoy_8610_", mois)
  info_value <- base[ , nom_colonne]
  # Addition de l'anomalie climatique
  climat_futur <- info_value + value_anom
  # Ajout de climat futur dans base
  nom_colonne_futur <- paste0("tmoy_4665_", mois)
  base <- setNames(data.frame(base, climat_futur), c(colnames(base), nom_colonne_futur))
  
} # Fin de la boucle des mois

# Calcul des temperatures futures annuelles
moy_ano_tas_annuelle <- mean(anomalies[ , "tas"])
info_value_moy_ann <- base[ , "tmoy_8610"] + moy_ano_tas_annuelle
base <- setNames(data.frame(base, info_value_moy_ann), c(colnames(base), "tmoy_4665"))

# Calcul des precipitations futures mensuelles
for (mois in c(1:12)){
  value_anom <- anomalies[which(anomalies[ , "mois"] == mois), "pr"] * 2592000 # 1 mm/s = 2592000 mm/mois
  nom_colonne <- paste("prec_8610_", mois)
  info_value <- base[,nom_colonne]
  # Addition de l'anomalie climatique
  climat_futur <- info_value + value_anom
  # Ajout de climat futur dans base
  nom_colonne_futur <- paste("prec_4665_", mois)
  base = setNames(data.frame(base, climat_futur), c(colnames(base), nom_colonne_futur))
} # Fin de la boucle des mois


# Calcul des etp futures
for (mois in c(1:12)){
  etp8610_mois <- base[ , paste0("etp_8610_", mois)]
  tmoy8610_mois <- base[ , paste("tmoy_8610_", moi)]
  tmoy4665_mois <- base[ , paste("tmoy_4665_", mois)]
  value_anom <- anomalies[which(anomalies[ , "mois"] == mois),"rsds"] * 20.7 # 1 W/m2 = 20.7 kCal/cm2/jour
  # Calcul de l'etp futur
  climat_futur <- (etp8610_mois * 2.5 * ((tmoy8610_mois + 15)/tmoy8610_mois)) * 0.4 * (tmoy4665_mois / (tmoy4665_mois + 15))
  # Ajout de climat futur dans base
  nom_colonne_futur <- paste0("etp_4665_", mois)
  base <- setNames(data.frame(base, climat_futur), c(colnames(base), nom_colonne_futur))
} # Fin de la boucle des mois


write.table(base, file=paste0(rep_travail, "/base_etape1.csv"), append=FALSE, quote=FALSE, sep=",", eol="\n", na="NA", dec=".", 
            row.names=FALSE, col.names=TRUE, qmethod=c("escape","double"))

# Colonnes de base_etape1.csv : 
# . X, Y, rum, 4 param?tres de chimie du sol, 
# . temperatures mensuelles de reference, temperature moyenne annuelle de ref., 
# . 12 precipitations mensuelles de reference, 12 etp mensuelles de reference,
# . temperature moyenne annuelle future,
# . 12 precipitations mensuelles futures, 12 etp mensuelles futures,


# calcul  du BHE (RU, DE et ETR)

for (mois in 1:12) {
  print(mois)
  prec8610_mois <- base[ , paste0("prec_8610_", mois)]
  prec4665_mois <- base[ , paste0("prec_4665_", mois)]
  etp8610_mois <- base[ , paste0("etp_8610_", mois)]
  etp4665_mois <- base[ , paste0("etp_4665_", mois)]
  rum <- base[,"rum"]
  
  if(mois == 1){
    # RU present et futur
    value_p_ru <- ifelse (prec8610_mois >= etp8610_mois, 
                         pmax(pmin(rum, rum + prec8610_mois - etp8610_mois),0), 
                         pmin(pmax(0, (rum * exp((prec8610_mois - etp8610_mois)/rum))),rum))
    value_f_ru <- ifelse (prec4665_mois >= etp4665_mois, 
                        pmax(pmin(rum, rum + prec4665_mois - etp4665_mois),0), 
                        pmin(pmax(0, (rum * exp((prec4665_mois - etp4665_mois)/rum))),rum))
    # ETR present et futur
    value_p_etr <- ifelse (prec8610_mois >= etp8610_mois, etp8610_mois, rum + prec8610_mois - value_p_ru)
    value_f_etr <- ifelse (prec4665_mois >= etp4665_mois, etp4665_mois, rum + prec4665_mois - value_f_ru)
  } else {
    # RU present et futur
    ru_present_m1 <- base[ , paste0("ru_8610_", mois - 1)]
    ru_futur_m1 <- base[ , paste("ru_4665_", mois - 1)]
    value_p_ru <- ifelse (prec8610_mois >= etp8610_mois, 
                        pmax(pmin(rum, ru_present_m1 + prec8610_mois - etp8610_mois),0), 
                        pmin(pmax(0, (ru_present_m1 * exp((prec8610_mois - etp8610_mois) / rum))),rum))
    value_f_ru <- ifelse (prec4665_mois >= etp4665_mois, 
                        pmax(pmin(rum, ru_futur_m1 + prec4665_mois - etp4665_mois),0), 
                        pmin(pmax(0, (ru_futur_m1 * exp((prec4665_mois - etp4665_mois) / rum))),rum))
    
    # ETR present et futur    
    value_p_etr <- ifelse (prec8610_mois >= etp8610_mois, etp8610_mois, ru_present_m1 + prec8610_mois - value_p_ru)
    value_f_etr <- ifelse (prec4665_mois >= etp4665_mois, etp4665_mois, ru_futur_m1 + prec4665_mois - value_f_ru)
    
  }
  
  # Ajout de ru dans base
  base <- setNames(data.frame(base, value_p_ru), c(colnames(base), paste0("ru_8610_", mois)))
  base <- setNames(data.frame(base, value_f_ru), c(colnames(base), paste0("ru_4665_", mois)))
  # Ajout de etr dans base
  base <- setNames(data.frame(base, value_p_etr), c(colnames(base), paste0("etr_8610_", mois)))
  base <- setNames(data.frame(base, value_f_etr), c(colnames(base), paste0("etr_4665_", mois)))
  
  # DE present et futur    
  value_p_de <- pmax(0, etp8610_mois - value_p_etr)
  value_f_de <- pmax(0, etp4665_mois - value_f_etr)
  # Ajout de de dans base
  base <- setNames(data.frame(base, value_p_de), c(colnames(base), paste0("de_8610_", mois)))
  base <- setNames(data.frame(base, value_f_de), c(colnames(base), paste0("de_4665_", mois)))
  
} # Fin de la boucle des mois 

# Deficit en eau estival (juin ? Ao?t)
sum_de_ete_p  <- base[ , "de_8610_6"] + base[ , "de_8610_7"] + base[ , "de_8610_8"]
sum_de_ete_f  <- base[ , "de_4665_6"] + base[ , "de_4665_7"] + base[ , "de_4665_8"]

base <- setNames(data.frame(base, sum_de_ete_p), c(colnames(base), "de_8610_ete"))
base <- setNames(data.frame(base, sum_de_ete_f), c(colnames(base), "de_4665_ete"))

#---------------------------
# Distribution especes
#---------------------------

# lecture du fichier "tab6var.csv" (contenant les donnees IFN de presence et les variables environnementales pour caler les modeles GAM).
#setwd(paste0(rep_travail, "E:/2_Donnees/modeles_distrib")
data <- read.table(paste0(rep_data, "/tab6var.csv"), h=TRUE, sep=";", dec=",")
data2	<- data[complete.cases(data), ] # supprime les NA

# TODO a remettre avec prolL2 defini au-dessus
# Definition de la projection Lambert 2 en language R
crs_l2 = "+proj=lcc +lat_1=46.8 +lat_0=46.8 +lon_0=0 +k_0=0.99987742 +x_0=600000 +y_0=2200000 +a=6378249.2 +b=6356515 +towgs84=-168,-60,320,0,0,0,0 +pm=paris +units=m +no_defs"

# Creation des rasters des variables que l'on veut dans base
r_tmoy8610  <- rasterFromXYZ(base[,c("Xl2", "Yl2", "tmoy_8610")], crs = crs_l2)
r_tmoy4665  <- rasterFromXYZ(base[,c("Xl2", "Yl2", "tmoy_4665")], crs = crs_l2)
r_de8610  <- rasterFromXYZ(base[,c("Xl2", "Yl2", "de_8610_ete")], crs = crs_l2)
r_de4665  <- rasterFromXYZ(base[,c("Xl2", "Yl2", "de_4665_ete")], crs = crs_l2)
r_ph  <- rasterFromXYZ(base[,c("Xl2", "Yl2", "ph")], crs = crs_l2)
r_cn  <- rasterFromXYZ(base[,c("Xl2", "Yl2", "cn")], crs = crs_l2)
r_et  <- rasterFromXYZ(base[,c("Xl2", "Yl2", "et")], crs = crs_l2)
r_ep  <- rasterFromXYZ(base[,c("Xl2", "Yl2", "ep")], crs = crs_l2)

# Enregistrement des resultats
dossier_save =  paste0(rep_travail, "/2046_2065") ############## A Changer

#-----------------------
# fasy   Fagus sylvatica    (Hetre)

gamfasy <- gam(fasy~s(tmoy_an,4) + s(plot_deete,4) + s(plac_EP_ssess1et2,4) + s(plac_pH_ssess1et2,4), family = binomial, data = data2, na.action = na.omit)
RHS_p <- stack(r_tmoy8610, r_de8610, r_ep, r_ph)
RHS_f <- stack(r_tmoy4665, r_de4665, r_ep, r_ph)
names(RHS_p) <- c("tmoy_an", "plot_deete", "plac_EP_ssess1et2","plac_pH_ssess1et2") # fonction "names" ds nouvelle version
names(RHS_f) <- c("tmoy_an", "plot_deete", "plac_EP_ssess1et2","plac_pH_ssess1et2") # fonction "names" ds nouvelle version

# Enregistremenet de la prediction 
Predfasy_p <- predict(RHS_p, gamfasy, type = "response", progress = "text", filename = file.path(dossier_save, "fasy_8610.tif"), overwrite=TRUE)
Predfasy_f <- predict(RHS_f, gamfasy, type = "response", progress = "text", filename = file.path(dossier_save, "fasy_4665.tif"), overwrite=TRUE)

# Ratio futur sur present
fsurp <- Predfasy_f / Predfasy_p

# TODO a commenter dans master
plot(fsurp)

# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename = paste0(rep_projet, "/Tendance_fasy.tif"), format="GTiff", overwrite=TRUE)


#----------------
# piab   Picea abies    (Epicea)

gampiab	<- gam(piab ~ s(tmoy_an,4) + s(plot_deete,4) + s(plac_pH_ssess1et2,4), family = binomial, data = data2, na.action = na.omit)
RHS_p	<- stack(r_tmoy8610, r_de8610, r_ph)
RHS_f	<- stack(r_tmoy4665, r_de4665, r_ph)
names(RHS_p) <- c("tmoy_an", "plot_deete", "plac_pH_ssess1et2") 
names(RHS_f) <- c("tmoy_an", "plot_deete", "plac_pH_ssess1et2") 

# Enregistremenet de la prediction 
Predpiab_p <- predict(RHS_p, gampiab, type = "response", progress = "text", filename = file.path(dossier_save, "piab_8610.tif"),overwrite=TRUE)
Predpiab_f <- predict(RHS_f, gampiab, type="response",progress="text", filename=file.path(dossier_save, "piab_4665.tif"),overwrite=TRUE)

# Ratio futur sur present
fsurp = Predpiab_f / Predpiab_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet, "/Tendance_piab.tif"), format="GTiff", overwrite=TRUE)


#---------------------
# pisy   Pinus sylvestris    (Pin sylvestre)

gampisy	= gam(pisy~ s(plac_pH_ssess1et2,4) + s(tmoy_an,4) + s(plac_CN_ssess1et2,4) + s(plot_deete,4),family=binomial,data=data2,na.action=na.omit)
RHS_p = stack(r_ph, r_tmoy8610, r_cn, r_de8610)
RHS_f = stack(r_ph, r_tmoy4665, r_cn, r_de4665)
names(RHS_p) = c("plac_pH_ssess1et2", "tmoy_an", "plac_CN_ssess1et2" , "plot_deete") 
names(RHS_f) = c("plac_pH_ssess1et2", "tmoy_an", "plac_CN_ssess1et2" , "plot_deete") 

# Enregistremenet de la prediction 

Predpisy_p = predict(RHS_p,gampisy,type="response",progress="text",filename=file.path(dossier_save, "pisy_8610.tif"),overwrite=TRUE)
Predpisy_f = predict(RHS_f,gampisy,type="response",progress="text",filename=file.path(dossier_save, "pisy_4665.tif"),overwrite=TRUE)

# Ratio futur sur present
fsurp = Predpisy_f / Predpisy_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet, "/Tendance_pisy.tif"), format="GTiff", overwrite=TRUE)


#---------------------
#qupe   Quercus petraea    (Ch?ne sessile)

gamqupe	= gam(qupe~ s(tmoy_an,4) + s(plac_ET_ssess1et2,4) + s(plac_EP_ssess1et2,4) + s(plac_CN_ssess1et2,4),family=binomial,data=data2,na.action=na.omit)
RHS_p	= stack(r_tmoy8610, r_et, r_ep, r_cn)
RHS_f	= stack(r_tmoy4665, r_et, r_ep, r_cn)
names(RHS_p) = c("tmoy_an", "plac_ET_ssess1et2", "plac_EP_ssess1et2", "plac_CN_ssess1et2") 
names(RHS_f) = c("tmoy_an", "plac_ET_ssess1et2", "plac_EP_ssess1et2", "plac_CN_ssess1et2") 

# Enregistremenet de la prediction 

Predqupe_p = predict(RHS_p,gamqupe,type="response",progress="text",filename=file.path(dossier_save, "qupe_8610.tif"),overwrite=TRUE)
Predqupe_f = predict(RHS_f,gamqupe,type="response",progress="text",filename=file.path(dossier_save, "qupe_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predqupe_f / Predqupe_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet, "Tendance_qupe.tif"), format="GTiff", overwrite=TRUE)


#---------------------
# abal   Abies alba    (Sapin pectine)

gamabal = gam(abal~ s(tmoy_an,4) + s(plot_deete,4) + s(plac_CN_ssess1et2,4),family=binomial,data=data2,na.action=na.omit)
RHS_p	= stack(r_tmoy8610, r_de8610, r_cn)
RHS_f	= stack(r_tmoy4665, r_de4665, r_cn)
names(RHS_p)	= c("tmoy_an", "plot_deete", "plac_CN_ssess1et2")
names(RHS_f)	= c("tmoy_an", "plot_deete", "plac_CN_ssess1et2")

# Enregistremenet de la prediction 

Predabal_p = predict(RHS_p,gamabal,type="response",progress="text",filename=file.path(dossier_save, "abal_8610.tif"),overwrite=TRUE)
Predabal_f = predict(RHS_f,gamabal,type="response",progress="text",filename=file.path(dossier_save, "abal_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predabal_f / Predabal_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet, "Tendance_abal.tif"), format="GTiff", overwrite=TRUE)


#------------------------
# acca Acer carpestre    (Erable champetre)

gamacca	= gam(acca~ s(plac_pH_ssess1et2,4) + s(tmoy_an,4) + s(plac_EP_ssess1et2,4)+s(plac_CN_ssess1et2,4),family=binomial,data=data2,na.action=na.omit)
RHS_p	= stack(r_ph,r_tmoy8610, r_ep, r_cn)
RHS_f	= stack(r_ph,r_tmoy4665, r_ep, r_cn)
names(RHS_p) = c("plac_pH_ssess1et2", "tmoy_an", "plac_EP_ssess1et2", "plac_CN_ssess1et2")
names(RHS_f) = c("plac_pH_ssess1et2", "tmoy_an", "plac_EP_ssess1et2", "plac_CN_ssess1et2")

# Enregistremenet de la prediction 

Predacca_p = predict(RHS_p,gamacca,type="response",progress="text",filename=file.path(dossier_save, "acca_8610.tif"),overwrite=TRUE)
Predacca_f = predict(RHS_f,gamacca,type="response",progress="text",filename=file.path(dossier_save, "acca_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predacca_f / Predacca_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet, "Tendance_acca.tif"), format="GTiff", overwrite=TRUE)


#------------------------
# acmo   Acer monspessulanus    (Erable de Montpellier)

gamacmo	= gam(acmo~ s(plac_pH_ssess1et2,4) + s(tmoy_an,4) + s(plac_EP_ssess1et2,4)+s(plac_CN_ssess1et2,4),family=binomial,data=data2,na.action=na.omit)
RHS_p = stack(r_ph, r_tmoy8610, r_ep, r_cn)
RHS_f = stack(r_ph, r_tmoy4665, r_ep, r_cn)
names(RHS_p) = c("plac_pH_ssess1et2", "tmoy_an", "plac_EP_ssess1et2", "plac_CN_ssess1et2")
names(RHS_f) = c("plac_pH_ssess1et2", "tmoy_an", "plac_EP_ssess1et2", "plac_CN_ssess1et2")

# Enregistremenet de la prediction 

Predacmo_p = predict(RHS_p,gamacmo,type="response",progress="text",filename=file.path(dossier_save, "acmo_8610.tif"),overwrite=TRUE)
Predacmo_f = predict(RHS_f,gamacmo,type="response",progress="text",filename=file.path(dossier_save, "acmo_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predacmo_f / Predacmo_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet, "/Tendance_acmo.tif"), format="GTiff", overwrite=TRUE)


#------------------------
# acop   (? Acer opalus)    Erable a feuilles d'obier

gamacop	= gam(acop~ s(plac_ET_ssess1et2,4)+ s(plot_deete,4) + s(plac_pH_ssess1et2,4) + s(tmoy_an,4)+s(plac_EP_ssess1et2,4),family=binomial,data=data2,na.action=na.omit)
RHS_p = stack(r_et, r_de8610, r_ph, r_tmoy8610, r_ep)
RHS_f = stack(r_et, r_de4665, r_ph, r_tmoy4665, r_ep)
names(RHS_p) = c("plac_ET_ssess1et2", "plot_deete", "plac_pH_ssess1et2", "tmoy_an", "plac_EP_ssess1et2")
names(RHS_f) = c("plac_ET_ssess1et2", "plot_deete", "plac_pH_ssess1et2", "tmoy_an", "plac_EP_ssess1et2")

# Enregistremenet de la prediction 

Predacop_p = predict(RHS_p,gamacop,type="response",progress="text",filename=file.path(dossier_save, "acop_8610.tif"),overwrite=TRUE)
Predacop_f = predict(RHS_f,gamacop,type="response",progress="text",filename=file.path(dossier_save, "acop_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predacop_f / Predacop_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet, "/Tendance_acop.tif"), format="GTiff", overwrite=TRUE)


#------------------------
# acpl   Acer platanoides    (Erable plane)

gamacpl = gam(acpl~ s(plac_pH_ssess1et2,4)+ s(plot_deete,4) + s(tmoy_an,4) + s(plac_ET_ssess1et2,4)+s(plac_CN_ssess1et2,4),family=binomial,data=data2,na.action=na.omit)
RHS_p = stack(r_ph, r_de8610, r_tmoy8610, r_et,  r_cn)
RHS_f = stack(r_ph, r_de4665, r_tmoy4665, r_et,  r_cn)
names(RHS_p) = c("plac_pH_ssess1et2", "plot_deete", "tmoy_an", "plac_ET_ssess1et2", "plac_CN_ssess1et2")
names(RHS_f) = c("plac_pH_ssess1et2", "plot_deete", "tmoy_an", "plac_ET_ssess1et2", "plac_CN_ssess1et2")

# Enregistremenet de la prediction 

Predacpl_p = predict(RHS_p,gamacpl,type="response",progress="text",filename=file.path(dossier_save, "acpl_8610.tif"),overwrite=TRUE)
Predacpl_f = predict(RHS_f,gamacpl,type="response",progress="text",filename=file.path(dossier_save, "acpl_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predacpl_f / Predacpl_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet, "/Tendance_acpl.tif"), format="GTiff", overwrite=TRUE)


#------------------------
# acps   Acer pseudoplatanus    (Erable sycomore)

gamacps	= gam(acps~ s(tmoy_an,4) + s(plac_CN_ssess1et2,4) + s(plac_pH_ssess1et2,4)+ s(plot_deete,4) ,family=binomial,data=data2,na.action=na.omit)
RHS_p = stack(r_tmoy8610, r_cn, r_ph, r_de8610)
RHS_f = stack(r_tmoy4665, r_cn, r_ph, r_de4665)
names(RHS_p) = c("tmoy_an", "plac_CN_ssess1et2", "plac_pH_ssess1et2", "plot_deete")
names(RHS_f) = c("tmoy_an", "plac_CN_ssess1et2", "plac_pH_ssess1et2", "plot_deete")

# Enregistremenet de la prediction 

Predacps_p = predict(RHS_p,gamacps,type="response",progress="text",filename=file.path(dossier_save, "acps_8610.tif"),overwrite=TRUE)
Predacps_f = predict(RHS_f,gamacps,type="response",progress="text",filename=file.path(dossier_save, "acps_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predacps_f / Predacps_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet, "/Tendance_acps.tif"), format="GTiff", overwrite=TRUE)


#------------------------
# algl   Alnus glutinosa    (Aulne glutineux)

gamalgl	= gam(algl~ s(plac_EP_ssess1et2,4) + s(plot_deete,4) + s(tmoy_an,4) ,family=binomial,data=data2,na.action=na.omit)
RHS_p = stack(r_ep, r_de8610, r_tmoy8610)
RHS_f = stack(r_ep, r_de4665, r_tmoy4665)
names(RHS_p) = c("plac_EP_ssess1et2",  "plot_deete", "tmoy_an")
names(RHS_f) = c("plac_EP_ssess1et2",  "plot_deete", "tmoy_an")

# Enregistremenet de la prediction 

Predalgl_p = predict(RHS_p,gamalgl,type="response",progress="text",filename=file.path(dossier_save, "algl_8610.tif"),overwrite=TRUE)
Predalgl_f = predict(RHS_f,gamalgl,type="response",progress="text",filename=file.path(dossier_save, "algl_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predalgl_f / Predalgl_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet,"/Tendance_algl.tif"), format="GTiff", overwrite=TRUE)


#------------------------
# bepe   Betulus pendulata    (Bouleau verruqueux)

gambepe = gam(bepe~ s(plac_pH_ssess1et2,4) + s(tmoy_an,4) + s(plac_ET_ssess1et2,4)+ s(plac_EP_ssess1et2,4) ,family=binomial,data=data2,na.action=na.omit)
RHS_p = stack(r_ph, r_tmoy8610, r_et, r_ep)
RHS_f = stack(r_ph, r_tmoy4665, r_et, r_ep)
names(RHS_p) = c("plac_pH_ssess1et2", "tmoy_an",  "plac_ET_ssess1et2", "plac_EP_ssess1et2")
names(RHS_f) = c("plac_pH_ssess1et2", "tmoy_an",  "plac_ET_ssess1et2", "plac_EP_ssess1et2")

# Enregistremenet de la prediction 

Predbepe_p = predict(RHS_p,gambepe,type="response",progress="text",filename=file.path(dossier_save, "bepe_8610.tif"),overwrite=TRUE)
Predbepe_f = predict(RHS_f,gambepe,type="response",progress="text",filename=file.path(dossier_save, "bepe_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predbepe_f / Predbepe_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet,"/Tendance_bepe.tif"), format="GTiff", overwrite=TRUE)


#------------------------
# bepu   Betulus pubescens    (Bouleau pubescent)

gambepu	= gam(bepu~ s(plac_pH_ssess1et2,4) + s(plot_deete,4) + s(plac_EP_ssess1et2,4)+ s(plac_CN_ssess1et2,4) ,family=binomial,data=data2,na.action=na.omit)
RHS_p = stack(r_ph, r_de8610, r_ep, r_cn)
RHS_f = stack(r_ph, r_de4665, r_ep, r_cn)
names(RHS_p) = c("plac_pH_ssess1et2", "plot_deete",  "plac_EP_ssess1et2", "plac_CN_ssess1et2")
names(RHS_f) = c("plac_pH_ssess1et2", "plot_deete",  "plac_EP_ssess1et2", "plac_CN_ssess1et2")

# Enregistremenet de la prediction 

Predbepu_p = predict(RHS_p,gambepu,type="response",progress="text",filename=file.path(dossier_save, "bepu_8610.tif"),overwrite=TRUE)
Predbepu_f = predict(RHS_f,gambepu,type="response",progress="text",filename=file.path(dossier_save, "bepu_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predbepu_f / Predbepu_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet, "Tendance_bepu.tif"), format="GTiff", overwrite=TRUE)


#------------------------
# cabe   Carpinus betulus    (Charme commun)

gamcabe = gam(cabe~ s(tmoy_an,4) + s(plac_pH_ssess1et2,4) + s(plac_ET_ssess1et2,4) + s(plac_CN_ssess1et2,4) + s(plac_EP_ssess1et2,4) ,family=binomial,data=data2,na.action=na.omit)
RHS_p = stack(r_tmoy8610, r_ph, r_et, r_cn, r_ep)
RHS_f = stack(r_tmoy4665, r_ph, r_et, r_cn, r_ep)
names(RHS_p) = c("tmoy_an", "plac_pH_ssess1et2", "plac_ET_ssess1et2",  "plac_CN_ssess1et2", "plac_EP_ssess1et2")
names(RHS_f) = c("tmoy_an", "plac_pH_ssess1et2", "plac_ET_ssess1et2",  "plac_CN_ssess1et2", "plac_EP_ssess1et2")

# Enregistremenet de la prediction 

Predcabe_p = predict(RHS_p,gamcabe,type="response",progress="text",filename=file.path(dossier_save, "cabe_8610.tif"),overwrite=TRUE)
Predcabe_f = predict(RHS_f,gamcabe,type="response",progress="text",filename=file.path(dossier_save, "cabe_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predcabe_f / Predcabe_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet, "/Tendance_cabe.tif"), format="GTiff", overwrite=TRUE)


#------------------------
# casa   Castanea sativa    (Ch?taignier)

gamcasa	= gam(casa~ s(plac_pH_ssess1et2,4) + s(tmoy_an,4) +s(plac_CN_ssess1et2,4) + s(plac_EP_ssess1et2,4) ,family=binomial,data=data2,na.action=na.omit)
RHS_p = stack(r_ph, r_tmoy8610, r_cn, r_ep)
RHS_f = stack(r_ph, r_tmoy4665, r_cn, r_ep)
names(RHS_p) = c( "plac_pH_ssess1et2", "tmoy_an", "plac_CN_ssess1et2",  "plac_EP_ssess1et2")
names(RHS_f) = c( "plac_pH_ssess1et2", "tmoy_an", "plac_CN_ssess1et2",  "plac_EP_ssess1et2")

# Enregistremenet de la prediction 

Predcasa_p = predict(RHS_p,gamcasa,type="response",progress="text",filename=file.path(dossier_save, "casa_8610.tif"),overwrite=TRUE)
Predcasa_f = predict(RHS_f,gamcasa,type="response",progress="text",filename=file.path(dossier_save, "casa_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predcasa_f / Predcasa_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet, "/Tendance_casa.tif"), format="GTiff", overwrite=TRUE)


#---------------------------
# fran    Fraxinus angustifolia    (Fr?ne oxyphille)

gamfran	= gam(fran~ s(plac_pH_ssess1et2,4) + s(plot_deete,4) +s(tmoy_an,4) ,family=binomial,data=data2,na.action=na.omit)
RHS_p = stack(r_ph, r_de8610, r_tmoy8610)
RHS_f = stack(r_ph, r_de4665, r_tmoy4665)
names(RHS_p) = c( "plac_pH_ssess1et2", "plot_deete", "tmoy_an") 
names(RHS_f) = c( "plac_pH_ssess1et2", "plot_deete", "tmoy_an") 

# Enregistremenet de la prediction 

Predfran_p	= predict(RHS_p,gamfran,type="response",progress="text",filename=file.path(dossier_save, "fran_8610.tif"),overwrite=TRUE)
Predfran_f	= predict(RHS_f,gamfran,type="response",progress="text",filename=file.path(dossier_save, "fran_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predfran_f / Predfran_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet, "/Tendance_fran.tif"), format="GTiff", overwrite=TRUE)


#------------------------
# frex   Fraxinus exelsior    (Fr?ne commun)

gamfrex	= gam(frex~ s(plac_pH_ssess1et2,4) + s(plot_deete,4) +s(tmoy_an,4)+ s(plac_EP_ssess1et2,4) ,family=binomial,data=data2,na.action=na.omit)
RHS_p	= stack(r_ph, r_de8610, r_tmoy8610, r_ep)
RHS_f	= stack(r_ph, r_de4665, r_tmoy4665, r_ep)
names(RHS_p) = c( "plac_pH_ssess1et2", "plot_deete", "tmoy_an", "plac_EP_ssess1et2") 
names(RHS_f) = c( "plac_pH_ssess1et2", "plot_deete", "tmoy_an", "plac_EP_ssess1et2") 

# Enregistremenet de la prediction 

Predfrex_p	= predict(RHS_p,gamfrex,type="response",progress="text",filename=file.path(dossier_save, "frex_8610.tif"),overwrite=TRUE)
Predfrex_f	= predict(RHS_f,gamfrex,type="response",progress="text",filename=file.path(dossier_save, "frex_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predfrex_f / Predfrex_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet, "/Tendance_frex.tif"), format="GTiff", overwrite=TRUE)


#------------------------
# piha    Pinus halepensis     (Pin d'Alep)

gampiha	= gam(piha~ s(tmoy_an,4) + s(plot_deete,4) + s(plac_pH_ssess1et2,4) ,family=binomial,data=data2,na.action=na.omit)
RHS_p	= stack(r_tmoy8610, r_de8610, r_ph)
RHS_f	= stack(r_tmoy4665, r_de4665, r_ph)
names(RHS_p)	= c( "tmoy_an", "plot_deete", "plac_pH_ssess1et2")
names(RHS_f)	= c( "tmoy_an", "plot_deete", "plac_pH_ssess1et2")

# Enregistremenet de la prediction 

Predpiha_p	= predict(RHS_p,gampiha,type="response",progress="text",filename=file.path(dossier_save, "piha_8610.tif"),overwrite=TRUE)
Predpiha_f	= predict(RHS_f,gampiha,type="response",progress="text",filename=file.path(dossier_save, "piha_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predpiha_f / Predpiha_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet, "/Tendance_piha.tif"), format="GTiff", overwrite=TRUE)


#------------------------
# prav    Prunus avium    (Merisier)

gamprav	= gam(prav~ s(plac_pH_ssess1et2,4) + s(plot_deete,4)+ s(plac_EP_ssess1et2,4)+ s(tmoy_an,4) ,family=binomial,data=data2,na.action=na.omit)
RHS_p = stack(r_ph, r_de8610, r_ep, r_tmoy8610)
RHS_f = stack(r_ph, r_de4665, r_ep, r_tmoy4665)
names(RHS_p)	= c( "plac_pH_ssess1et2", "plot_deete", "plac_EP_ssess1et2", "tmoy_an")
names(RHS_f)	= c( "plac_pH_ssess1et2", "plot_deete", "plac_EP_ssess1et2", "tmoy_an")

# Enregistremenet de la prediction 

Predprav_p	= predict(RHS_p,gamprav,type="response",progress="text",filename=file.path(dossier_save, "prav_8610.tif"),overwrite=TRUE)
Predprav_f	= predict(RHS_f,gamprav,type="response",progress="text",filename=file.path(dossier_save, "prav_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predprav_f / Predprav_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet, "/Tendance_prav.tif"), format="GTiff", overwrite=TRUE)


#------------------------
# quil    Quercus ilex    (Ch?ne vert)

gamquil	= gam(quil~ s(plot_deete,4)+ s(plac_ET_ssess1et2,4) +  s(tmoy_an,4) + s(plac_EP_ssess1et2,4),family=binomial,data=data2,na.action=na.omit)
RHS_p	= stack(r_de8610, r_et, r_tmoy8610, r_ep)
RHS_f	= stack(r_de4665, r_et, r_tmoy4665, r_ep)
names(RHS_p) = c( "plot_deete", "plac_ET_ssess1et2",  "tmoy_an", "plac_EP_ssess1et2")
names(RHS_f) = c( "plot_deete", "plac_ET_ssess1et2",  "tmoy_an", "plac_EP_ssess1et2")

# Enregistremenet de la prediction 

Predquil_p	= predict(RHS_p,gamquil,type="response",progress="text",filename=file.path(dossier_save, "quil_8610.tif"),overwrite=TRUE)
Predquil_f	= predict(RHS_f,gamquil,type="response",progress="text",filename=file.path(dossier_save, "quil_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predquil_f / Predquil_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet,"/Tendance_quil.tif"), format="GTiff", overwrite=TRUE)


#------------------------
# qupu    Quercus pubescens    (Ch?ne pubescent)

gamqupu	= gam(qupu~ s(tmoy_an,4) + s(plot_deete, 4) + s(plac_ET_ssess1et2,4) + s(plac_EP_ssess1et2,4) + s(plac_pH_ssess1et2,4),family=binomial,data=data2,na.action=na.omit)
RHS_p	= stack(r_tmoy8610, r_de8610, r_et, r_ep, r_ph)
RHS_f	= stack(r_tmoy4665, r_de4665, r_et, r_ep, r_ph)
names(RHS_p)	= c( "tmoy_an",  "plot_deete", "plac_ET_ssess1et2",   "plac_EP_ssess1et2",   "plac_pH_ssess1et2")
names(RHS_f)	= c( "tmoy_an",  "plot_deete", "plac_ET_ssess1et2",   "plac_EP_ssess1et2",   "plac_pH_ssess1et2")

# Enregistremenet de la prediction 

Predqupu_p	= predict(RHS_p,gamqupu,type="response",progress="text",filename=file.path(dossier_save, "qupu_8610.tif"),overwrite=TRUE)
Predqupu_f	= predict(RHS_f,gamqupu,type="response",progress="text",filename=file.path(dossier_save, "qupu_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predqupu_f / Predqupu_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet, "/Tendance_qupu.tif"), format="GTiff", overwrite=TRUE)


#------------------------
# quro    Quercus robur    (Ch?ne p?doncul?)

gamquro	= gam(quro~ s(plac_ET_ssess1et2,4) + s(tmoy_an,4) + s(plac_pH_ssess1et2,4) + s(plac_EP_ssess1et2,4) + s(plot_deete, 4)  ,family=binomial,data=data2,na.action=na.omit)
RHS_p	= stack(r_et, r_tmoy8610, r_ph, r_ep, r_de8610)
RHS_f	= stack(r_et, r_tmoy4665, r_ph, r_ep, r_de4665)
names(RHS_p) = c( "plac_ET_ssess1et2", "tmoy_an",  "plac_pH_ssess1et2", "plac_EP_ssess1et2", "plot_deete")
names(RHS_f) = c( "plac_ET_ssess1et2", "tmoy_an",  "plac_pH_ssess1et2", "plac_EP_ssess1et2", "plot_deete")

# Enregistremenet de la prediction 

Predquro_p	= predict(RHS_p,gamquro,type="response",progress="text",filename=file.path(dossier_save, "quro_8610.tif"),overwrite=TRUE)
Predquro_f	= predict(RHS_f,gamquro,type="response",progress="text",filename=file.path(dossier_save, "quro_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predquro_f / Predquro_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet, "/Tendance_quro.tif"), format="GTiff", overwrite=TRUE)


#------------------------
# rops    Robinia pseudoacacia    (Robinier)

gamrops	= gam(rops~ s(plac_pH_ssess1et2,4) + s(tmoy_an,4) + s(plac_EP_ssess1et2,4) + s(plot_deete, 4)+ s(plac_CN_ssess1et2,4),family=binomial,data=data2,na.action=na.omit)
RHS_p	= stack(r_ph, r_tmoy8610, r_ep, r_de8610, r_cn)
RHS_f	= stack(r_ph, r_tmoy4665, r_ep, r_de4665, r_cn)
names(RHS_p)	= c( "plac_pH_ssess1et2", "tmoy_an",  "plac_EP_ssess1et2", "plot_deete", "plac_CN_ssess1et2")
names(RHS_f)	= c( "plac_pH_ssess1et2", "tmoy_an",  "plac_EP_ssess1et2", "plot_deete", "plac_CN_ssess1et2")

# Enregistremenet de la prediction 

Predrops_p	= predict(RHS_p,gamrops,type="response",progress="text",filename=file.path(dossier_save, "rops_8610.tif"),overwrite=TRUE)
Predrops_f	= predict(RHS_f,gamrops,type="response",progress="text",filename=file.path(dossier_save, "rops_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predrops_f / Predrops_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet, "/Tendance_rops.tif"), format="GTiff", overwrite=TRUE)


#------------------------
# saal    Salix alba    (Saule blanc)

gamsaal	= gam(saal~ s(plac_EP_ssess1et2,4) + s(plac_pH_ssess1et2,4) + s(tmoy_an,4),family=binomial,data=data2,na.action=na.omit)
RHS_p = stack(r_ep, r_ph, r_tmoy8610)
RHS_f = stack(r_ep, r_ph, r_tmoy4665)
names(RHS_p)	= c( "plac_EP_ssess1et2", "plac_pH_ssess1et2", "tmoy_an")
names(RHS_f)	= c( "plac_EP_ssess1et2", "plac_pH_ssess1et2", "tmoy_an")

# Enregistremenet de la prediction 

Predsaal_p	= predict(RHS_p,gamsaal,type="response",progress="text",filename=file.path(dossier_save, "saal_8610.tif"),overwrite=TRUE)
Predsaal_f	= predict(RHS_f,gamsaal,type="response",progress="text",filename=file.path(dossier_save, "saal_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predsaal_f / Predsaal_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet, "/Tendance_saal.tif"), format="GTiff", overwrite=TRUE)


#------------------------
# saca    Salix caprea    (Saule marsault)

gamsaca	= gam(saca~ s(plac_EP_ssess1et2,4) + s(tmoy_an,4) + s(plac_pH_ssess1et2,4) ,family=binomial,data=data2,na.action=na.omit)
RHS_p	= stack(r_ep, r_tmoy8610, r_ph)
RHS_f	= stack(r_ep, r_tmoy4665, r_ph)
names(RHS_p)	= c( "plac_EP_ssess1et2", "tmoy_an", "plac_pH_ssess1et2")
names(RHS_f)	= c( "plac_EP_ssess1et2", "tmoy_an", "plac_pH_ssess1et2")

# Enregistremenet de la prediction 

Predsaca_p	= predict(RHS_p,gamsaca,type="response",progress="text",filename=file.path(dossier_save, "saca_8610.tif"),overwrite=TRUE)
Predsaca_f	= predict(RHS_f,gamsaca,type="response",progress="text",filename=file.path(dossier_save, "saca_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predsaca_f / Predsaca_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet, "/Tendance_saca.tif"), format="GTiff", overwrite=TRUE)


#------------------------
# saci    Salix cinerea    (Saule cendr?)

gamsaci	= gam(saci~ s(plac_EP_ssess1et2,4) + s(plac_CN_ssess1et2,4) + s(tmoy_an,4) + s(plac_ET_ssess1et2,4) ,family=binomial,data=data2,na.action=na.omit)
RHS_p	= stack(r_ep, r_cn, r_tmoy8610, r_et)
RHS_f	= stack(r_ep, r_cn, r_tmoy4665, r_et)
names(RHS_p)	= c( "plac_EP_ssess1et2", "plac_CN_ssess1et2", "tmoy_an", "plac_ET_ssess1et2" )
names(RHS_f)	= c( "plac_EP_ssess1et2", "plac_CN_ssess1et2", "tmoy_an", "plac_ET_ssess1et2" )

# Enregistremenet de la prediction 

Predsaci_p	= predict(RHS_p,gamsaci,type="response",progress="text",filename=file.path(dossier_save, "saci_8610.tif"),overwrite=TRUE)
Predsaci_f	= predict(RHS_f,gamsaci,type="response",progress="text",filename=file.path(dossier_save, "saci_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predsaci_f / Predsaci_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet, "/Tendance_saci.tif"), format="GTiff", overwrite=TRUE)


#------------------------
# soar    Sorbus aria    (Alisier blanc)

gamsoar	= gam(soar~ s(tmoy_an,4) + s(plac_ET_ssess1et2,4) + s(plac_EP_ssess1et2,4) + s(plac_pH_ssess1et2,4),family=binomial,data=data2,na.action=na.omit)
RHS_p	= stack(r_tmoy8610, r_et, r_ep, r_ph)
RHS_f	= stack(r_tmoy4665, r_et, r_ep, r_ph)
names(RHS_p)	= c( "tmoy_an", "plac_ET_ssess1et2", "plac_EP_ssess1et2", "plac_pH_ssess1et2" )
names(RHS_f)	= c( "tmoy_an", "plac_ET_ssess1et2", "plac_EP_ssess1et2", "plac_pH_ssess1et2" )

# Enregistremenet de la prediction 

Predsoar_p	= predict(RHS_p,gamsoar,type="response",progress="text",filename=file.path(dossier_save, "soar_8610.tif"),overwrite=TRUE)
Predsoar_f	= predict(RHS_f,gamsoar,type="response",progress="text",filename=file.path(dossier_save, "soar_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predsoar_f / Predsoar_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet, "/Tendance_soar.tif"), format="GTiff", overwrite=TRUE)


#------------------------
# soau    Sorbus aucuparia    (Sorbier des oiseleurs)

gamsoau	=	gam(soau~ s(tmoy_an,4) + s(plac_CN_ssess1et2,4) + s(plot_deete,4),family=binomial,data=data2,na.action=na.omit)
RHS_p	= stack(r_tmoy8610, r_cn, r_de8610)
RHS_f	= stack(r_tmoy4665, r_cn, r_de4665)
names(RHS_p)	= c( "tmoy_an", "plac_CN_ssess1et2", "plot_deete")
names(RHS_f)	= c( "tmoy_an", "plac_CN_ssess1et2", "plot_deete")

# Enregistremenet de la prediction 

Predsoau_p	= predict(RHS_p,gamsoau,type="response",progress="text",filename=file.path(dossier_save, "soau_8610.tif"),overwrite=TRUE)
Predsoau_f	= predict(RHS_f,gamsoau,type="response",progress="text",filename=file.path(dossier_save, "soau_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predsoau_f / Predsoau_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet, "/Tendance_soau.tif"), format="GTiff", overwrite=TRUE)


#------------------------
# soto    Sorbus torminalis    (Alisier torminal)

gamsoto	= gam(soto~ s(plot_deete,4) + s(plac_pH_ssess1et2,4) + s(plac_EP_ssess1et2,4) + s(plac_ET_ssess1et2,4) +s(tmoy_an,4) ,family=binomial,data=data2,na.action=na.omit)
RHS_p	= stack(r_de8610, r_ph, r_ep, r_et, r_tmoy8610)
RHS_f	= stack(r_de4665, r_ph, r_ep, r_et, r_tmoy4665)
names(RHS_p)	= c( "plot_deete", "plac_pH_ssess1et2", "plac_EP_ssess1et2", "plac_ET_ssess1et2", "tmoy_an" )
names(RHS_f)	= c( "plot_deete", "plac_pH_ssess1et2", "plac_EP_ssess1et2", "plac_ET_ssess1et2", "tmoy_an" )

# Enregistremenet de la prediction 

Predsoto_p	= predict(RHS_p,gamsoto,type="response",progress="text",filename=file.path(dossier_save, "soto_8610.tif"),overwrite=TRUE)
Predsoto_f	= predict(RHS_f,gamsoto,type="response",progress="text",filename=file.path(dossier_save, "soto_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predsoto_f / Predsoto_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet, "/Tendance_soto.tif"), format="GTiff", overwrite=TRUE)


#------------------------
# tico    Tilia cordata    (Tilleul ? petites feuilles)

gamtico	= gam(tico~ s(plac_CN_ssess1et2,4) +s(tmoy_an,4) + s(plac_pH_ssess1et2,4) + s(plac_EP_ssess1et2,4) +s(plot_deete,4) ,family=binomial,data=data2,na.action=na.omit)
RHS_p	= stack(r_cn, r_tmoy8610, r_ph, r_ep, r_de8610)
RHS_f	= stack(r_cn, r_tmoy4665, r_ph, r_ep, r_de4665)
names(RHS_p)	= c( "plac_CN_ssess1et2", "tmoy_an", "plac_pH_ssess1et2",  "plac_EP_ssess1et2", "plot_deete")
names(RHS_f)	= c( "plac_CN_ssess1et2", "tmoy_an", "plac_pH_ssess1et2",  "plac_EP_ssess1et2", "plot_deete")

# Enregistremenet de la prediction 

Predtico_p	= predict(RHS_p,gamtico,type="response",progress="text",filename=file.path(dossier_save, "tico_8610.tif"),overwrite=TRUE)
Predtico_f	= predict(RHS_f,gamtico,type="response",progress="text",filename=file.path(dossier_save, "tico_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predtico_f / Predtico_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet, "/Tendance_tico.tif"), format="GTiff", overwrite=TRUE)


#------------------------
# tipl    Tilia platophyllos    (Tilleul ? grandes feuilles)

gamtipl	= gam(tipl~ s(plac_pH_ssess1et2,4) + s(plot_deete,4) +s(tmoy_an,4) + s(plac_EP_ssess1et2,4)  ,family=binomial,data=data2,na.action=na.omit)
RHS_p	= stack(r_ph, r_de8610, r_tmoy8610, r_ep)
RHS_f	= stack(r_ph, r_de4665, r_tmoy4665, r_ep)
names(RHS_p)	= c( "plac_pH_ssess1et2", "plot_deete", "tmoy_an", "plac_EP_ssess1et2")
names(RHS_f)	= c( "plac_pH_ssess1et2", "plot_deete", "tmoy_an", "plac_EP_ssess1et2")

# Enregistremenet de la prediction 

Predtipl_p	= predict(RHS_p,gamtipl,type="response",progress="text",filename=file.path(dossier_save, "tipl_8610.tif"),overwrite=TRUE)
Predtipl_f	= predict(RHS_f,gamtipl,type="response",progress="text",filename=file.path(dossier_save, "tipl_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predtipl_f / Predtipl_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet, "/Tendance_tipl.tif"), format="GTiff", overwrite=TRUE)


#------------------------
# ulgl    Ulmus glabra    (Orme de montagne)

gamulgl	= gam(ulgl~ s(plot_deete,4) +s(plac_CN_ssess1et2,4) + s(plac_ET_ssess1et2,4) ,family=binomial,data=data2,na.action=na.omit)
RHS_p	= stack(r_de8610, r_cn, r_et)
RHS_f	= stack(r_de4665, r_cn, r_et)
names(RHS_p)	= c( "plot_deete", "plac_CN_ssess1et2",  "plac_ET_ssess1et2")
names(RHS_f)	= c( "plot_deete", "plac_CN_ssess1et2",  "plac_ET_ssess1et2")

# Enregistremenet de la prediction 

Predulgl_p = predict(RHS_p,gamulgl,type="response",progress="text",filename=file.path(dossier_save, "ulgl_8610.tif"),overwrite=TRUE)
Predulgl_f = predict(RHS_f,gamulgl,type="response",progress="text",filename=file.path(dossier_save, "ulgl_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predtulgl_f / Predulgl_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet, "/Tendance_ulgl.tif"), format="GTiff", overwrite=TRUE)


#------------------------
# ulmi   Ulmus minor    (Orme champ?tre)

gamulmi	= gam(ulmi ~ s(plac_pH_ssess1et2,4) + s(tmoy_an,4)+ s(plac_EP_ssess1et2,4) + s(plot_deete,4) ,family=binomial,data=data2,na.action=na.omit)
RHS_p = stack(r_ph, r_tmoy8610, r_ep, r_de8610)
RHS_f = stack(r_ph, r_tmoy4665, r_ep, r_de4665)
names(RHS_p)	= c( "plac_pH_ssess1et2", "tmoy_an", "plac_EP_ssess1et2", "plot_deete" )
names(RHS_f)	= c( "plac_pH_ssess1et2", "tmoy_an", "plac_EP_ssess1et2", "plot_deete" )

# Enregistremenet de la prediction 

Predulmi_p	= predict(RHS_p,gamulmi,type="response",progress="text",filename=file.path(dossier_save, "ulmi_8610.tif"),overwrite=TRUE)
Predulmi_f	= predict(RHS_f,gamulmi,type="response",progress="text",filename=file.path(dossier_save, "ulmi_4665.tif"),overwrite=TRUE)

# Ratio futur sur present

fsurp = Predtulmi_f / Predulmi_p

# TODO a commenter dans master
plot(fsurp)
# Save en tif
# setwd(dossier_save)
writeRaster(fsurp, filename=paste0(rep_projet, "/Tendance_ulmi.tif"), format="GTiff", overwrite=TRUE)

#####       FIN DU SCRIPT     ######
