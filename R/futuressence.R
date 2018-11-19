#' @title Calcule les potentialites presentes et futures des essences forestieres
#'
#' @description La fonction permet de calculer les presences futures des essences forestieres
#' française
#'
#' @param fgeo = fichier geographique (points, lignes ou polygones) de l'etendue sur laquelle le calcul est realise
#' @param enreg = FASLE par defaut, specficie si des enregistrements intermediaires sont realises
#' @param rep_travail = /tmp par defaut specifie le repertoire dans lequel sont enregistres les resultats intermediaire
#' @param rep_projet = gewd() par dafaut, specifie le repertoire dans lequel les resultats sont enregistres
#' @param rep_data = specifie les repertoire dans lequel sont les data du Lerfob
#' @param rep_clim = specifie le repertoire dans lequel sont les data climatiques
#' @param resol_grid = 10 par defaut, specifie la resolution des grilles generees
#' @param buffer = 100 par defaut, specifie le buffer en unite de projection realise sur la zone de calcul
#'
#' @return La fonction renvoie un tableau, des graphes et des cartes
#' @importFrom raster rasterize rasterFromXYZ rasterToPoints projection raster crs values
#' @importFrom dplyr select
#' @importFrom sf st_coordinates st_crs st_intersects st_polygon st_read st_bbox st_as_sfc st_transform st_buffer
#' @importFrom ggplot2 ggtitle xlab ylab scale_x_log10 scale_y_log10 geom_point geom_abline ggplot aes
#' @importFrom ggrepel geom_label_repel
#' @importFrom stats complete.cases median setNames predict
#' @importFrom gam gam
#' @importFrom methods as
#' @importFrom utils head read.csv read.table write.table
#' @importFrom magrittr %>%
#'
#' @return A list of plot and table.
#'
#' @examples
#'
#' ### Calcul sur un shapefile multipolygones
#'
#' \dontrun{
#' res <- futuressence(fgeo='/home/pascal/Documents/forestys/data/groupe.shp',
#' rep_travail= '/media/pascal/data2/forestys/tmp/',
#' rep_projet='/media/pascal/data2/forestys/Essai/1_Pro_Silva_2018_5a',
#' rep_data='/media/pascal/data2/forestys/Essai/2_Donnees',
#' rep_clim='/media/pascal/data2/forestys/Climat')
#'
#' # afficher la grille de la presence de l'espece fagus sylvatice (fasy)
#' plot(res$species$fasy$present)
#'
#' # afficher la grille de la future presence de l'expece quercus robur (quro)
#' plot(res$species$quro$futur)
#'
#' # afficher le diffenretiel entre present et futur
#' plot(res$species$quro$fsurp)
#'
#' # afficher le stressogramme present-futur
#' res$stressogramme
#' }
#' @export

futuressence <- function(fgeo = NULL, enreg = F, rep_travail = tempdir(), rep_projet = NULL, rep_data = NULL, rep_clim = NULL, resol_grid = 10, buffer = 100) {

  # ETAPE 1 ###########
  # Import de la base de calcul
  # creation environnement pour les variables globales
  cacheEnv <- new.env()

  if (class(fgeo)[1] == "sf"){
    shape <- fgeo
  } else {
    shape <- sf::st_read(fgeo)
  }

  # ajoute un buffer de 10m
  shape <- shape %>% st_buffer(buffer)

  # parametrage projection lambert 2
  projLII <- crs("+init=epsg:27572")
  projL93 <- crs("+init=epsg:2154")

  # essence disponible dans tab6var.csv
  species <- c("abal", "piab", "piha", "pisy", "acca", "acmo", "acop", "acpl", "acps", "algl", "bepu", "cabe", "fasy", "frex", "quil", "qupe", "qupu", "quro",
      "rops", "saal", "saca", "saci", "soar", "soau", "soto", "tico", "tipl", "ulgl", "ulmi", "bepe", "fran", "prav", "casa")

  # Se mettre dans le repertoire de travail, le cree si necessaire
  if (!dir.exists(rep_travail)) {
    dir.create(rep_travail, showWarnings = TRUE, recursive = TRUE, mode = "0777")
  }
  setwd(rep_travail)

  ### calcul les grids a partir du shape
  base <- as.data.frame(rasterToPoints(rasterize(as(shape, "Spatial"), raster(as(shape, "Spatial"), res = resol_grid), progress = "text")))[, 1:2]
  # on supprime les data en doublon
  base <- unique(base)
  # on nomme les colonnes
  colnames(base) <- c("Xl2", "Yl2")

  if (enreg) {
      # Sauvegarder ce tableau dans le dossier
      write.table(base, file = paste0(rep_travail, "/coordonnees.csv"), sep = ";", dec = ".", row.names = FALSE)
      message(paste0("Result is saved in: ", paste0(dirname(rep_travail), "/coordonnees.csv")))
  }

  # Extraction des coordonnees
  coord <- base[, c("Xl2", "Yl2")]

  # Extraction de la RUM modelisee par le Lerfob au pas de 50 m si disponible sinon de 1 km
  rum_lerfob <- paste0(rep_data, "/modeles_distrib/variables/lorraine/rumlor50/hdr.adf")

  # Extraction des donnees terrain de RUM (fichier vecteur rasterise au format tif)
  rum_terrain <- paste0(rep_projet, "/Mesures/RUM.tif")
  listacces <- c(rum_lerfob, rum_terrain)
  listnom <- c("rum_lerfob", "rum_terrain")

  # TODO optimiser le nommage des colonnes
  tableraster <- as.data.frame(matrix(data = NA, nrow = nrow(base), ncol = length(listnom)))
  colnames(tableraster) = listnom

  head(tableraster)

  # Cree le raster pour le rum
  for (i in 1:length(listnom)) {
      rast <- raster(listacces[i])
      raster::projection(rast) <- projLII
      tableraster[, i] <- raster::extract(x = rast, y = coord, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, na.rm=TRUE)
      print(paste0(Sys.time(), " - ", listnom[i]))
  }

  # Quand rum_terrain est non vide alors remplacer rum_lerfob par rum_terrain
  tablerasterrum <- as.data.frame(matrix(data = NA, nrow = nrow(base), ncol = 1))
  colnames(tablerasterrum) <- c("rum")

  # TODO a optimiser
  tablerasterrum[, 1] <- ifelse(is.na(tableraster[, dimnames(tableraster)[[2]] == "rum_terrain"]),
                                tableraster[, dimnames(tableraster)[[2]] == "rum_lerfob"],
                                tableraster[, dimnames(tableraster)[[2]] == "rum_terrain"])

  # Inclure la RUM dans base
  base <- cbind(base, tablerasterrum)

  ######### Recommencer la meme chose pour pH, C/N, engorgement temporaire et engorgement permanent.

  # Extraction du pH modelisee par le Lerfob au pas de 50 m si disponible sinon de 1 km
  ph_lerfob <- paste0(rep_data, "/modeles_distrib/variables/lorraine/ph_lor_50/hdr.adf")

  # Extraction des donnees terrain de pH (fichier vecteur rasterise au format tif)
  ph_terrain <- paste0(rep_projet, "/Mesures/PH.tif")
  listacces <- c(ph_lerfob, ph_terrain)
  listnom <- c("ph_lerfob", "ph_terrain")

  tableraster <- as.data.frame(matrix(data = NA, nrow = nrow(base), ncol = length(listnom)))
  colnames(tableraster) <- listnom

  for (i in 1:length(listnom)) {
      rast = raster(listacces[i])
      raster::projection(rast) = projLII
      tableraster[, i] = raster::extract(x = rast, y = coord, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, na.rm=TRUE)
      print(paste0(Sys.time(), " - ", listnom[i]))
  }

  # Quand ph_terrain est non vide alors remplacer ph_lerfob par ph_terrain
  tablerasterph <- as.data.frame(matrix(data = NA, nrow = nrow(base), ncol = 1))
  colnames(tablerasterph) <- c("ph")

  tablerasterph[, 1] <- ifelse(is.na(tableraster[, dimnames(tableraster)[[2]] == "ph_terrain"]), tableraster[, dimnames(tableraster)[[2]] == "ph_lerfob"], tableraster[,
      dimnames(tableraster)[[2]] == "ph_terrain"])

  # Inclure le pH dans base
  base <- cbind(base, tablerasterph)

  ################################################'

  # Extraction du rapport C/N modelise par le Lerfob au pas de 50 m si disponible sinon de 1 km
  cn_lerfob <- paste0(rep_data, "/modeles_distrib/variables/lorraine/cn_50_lor_cor/hdr.adf")
  # Extraction des donn?es terrain dd'eng. temp. (fichier vecteur rast?ris? au format tif)
  cn_terrain <- paste0(rep_projet, "/Mesures/CN.tif")
  listacces <- c(cn_lerfob, cn_terrain)
  listnom <- c("cn_lerfob", "cn_terrain")

  tableraster <- as.data.frame(matrix(data = NA, nrow = nrow(base), ncol = length(listnom)))
  colnames(tableraster) <- listnom

  for (i in 1:length(listnom)) {
      rast = raster(listacces[i])
      raster::projection(rast) = projLII
      tableraster[, i] = raster::extract(x = rast, y = coord, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, na.rm=TRUE)
      print(paste0(Sys.time(), " - ", listnom[i]))
  }

  # Quand cn_terrain est non vide alors remplacer cn_lerfob par cn_terrain
  tablerastercn <- as.data.frame(matrix(data = NA, nrow = nrow(base), ncol = 1))
  colnames(tablerastercn) <- c("cn")

  tablerastercn[, 1] <- ifelse(is.na(tableraster[, dimnames(tableraster)[[2]] == "cn_terrain"]), tableraster[, dimnames(tableraster)[[2]] == "cn_lerfob"], tableraster[,
      dimnames(tableraster)[[2]] == "cn_terrain"])

  # Inclure l'eng. temp. dans base
  base <- cbind(base, tablerastercn)

  ################################################'

  # Extraction de l'engorgement temporaire modelise par le Lerfob au pas de 50 m si disponible sinon de 1 km
  et_lerfob <- paste0(rep_data, "/modeles_distrib/variables/lorraine/modeleetbon/hdr.adf")
  # Extraction des donn?es terrain d'eng. temp. (fichier vecteur rast?ris? au format tif)
  et_terrain <- paste0(rep_projet, "/Mesures/ET.tif")
  listacces <- c(et_lerfob, et_terrain)
  listnom <- c("et_lerfob", "et_terrain")
  tableraster <- as.data.frame(matrix(data = NA, nrow = nrow(base), ncol = length(listnom)))
  colnames(tableraster) <- listnom

  for (i in 1:length(listnom)) {
      rast = raster(listacces[i])
      raster::projection(rast) = projLII
      tableraster[, i] = raster::extract(x = rast, y = coord, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, na.rm=TRUE)
      print(paste0(Sys.time(), " - ", listnom[i]))
  }

  # Quand et_terrain est non vide alors remplacer et_lerfob par et_terrain
  tablerasteret <- as.data.frame(matrix(data = NA, nrow = nrow(base), ncol = 1))
  colnames(tablerasteret) <- c("et")

  tablerasteret[, 1] <- ifelse(is.na(tableraster[, dimnames(tableraster)[[2]] == "et_terrain"]), tableraster[, dimnames(tableraster)[[2]] == "et_lerfob"], tableraster[,
      dimnames(tableraster)[[2]] == "et_terrain"])

  # Inclure l'eng. temp. dans base
  base <- cbind(base, tablerasteret)

  ################################################'

  # Extraction de l'engorgement permanent modelise par le Lerfob au pas de 50 m si disponible sinon de 1 km
  ep_lerfob <- paste0(rep_data, "/modeles_distrib/variables/france1km/ep_ess2/hdr.adf")
  # Extraction des donn?es terrain d'eng. perm. (fichier vecteur rast?ris? au format tif)
  ep_terrain <- paste0(rep_projet, "/Mesures/EP.tif")
  listacces <- c(ep_lerfob, ep_terrain)
  listnom <- c("ep_lerfob", "ep_terrain")

  tableraster <- as.data.frame(matrix(data = NA, nrow = nrow(base), ncol = length(listnom)))
  colnames(tableraster) <- listnom

  for (i in 1:length(listnom)) {
      rast = raster(listacces[i])
      raster::projection(rast) = projLII
      tableraster[, i] = raster::extract(x = rast, y = coord, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, na.rm=TRUE)
      print(paste0(Sys.time(), " - ", listnom[i]))
  }

  # Quand ep_terrain est non vide alors remplacer ep_lerfob par ep_terrain
  tablerasterep <- as.data.frame(matrix(data = NA, nrow = nrow(base), ncol = 1))
  colnames(tablerasterep) <- c("ep")

  tablerasterep[, 1] <- ifelse(is.na(tableraster[, dimnames(tableraster)[[2]] == "ep_terrain"]), tableraster[, dimnames(tableraster)[[2]] == "ep_lerfob"], tableraster[,
      dimnames(tableraster)[[2]] == "ep_terrain"])
  ######### (expression a simplifier)

  # Inclure l'eng. perm. dans base
  base <- cbind(base, tablerasterep)

  ################################################'

  # Extraction des donnees climatiques de reference Exemple de chemin d'acces : F:/Climat/Digitalis_v1/etp_v1/etp_8610_1/w001001.adf
  for (var in c("tmoy_", "prec_", "etp_")) {
      for (mois in 1:12) {
          nomvar <- paste0(var, "8610_", mois)
          PATH_VAR <- paste0(rep_clim, "/Digitalis_v1/", var, "v1/", nomvar, "/w001001.adf")
          col <- dim(base)[2] + 1
          rast <- raster(PATH_VAR, sp = TRUE)
          raster::projection(rast) <- projLII
          base[, col] <- raster::extract(x = rast, y = coord, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, na.rm=TRUE)
          colnames(base)[col] = nomvar
          print(paste0(Sys.time(), " - ", var, " du mois ", mois, "/12."))
      }
  }

  # Calcul de la temperature moyenne annuelle de reference
  base$tmoy_8610 <- (base$tmoy_8610_1 + base$tmoy_8610_2 + base$tmoy_8610_3 + base$tmoy_8610_4 + base$tmoy_8610_5 + base$tmoy_8610_6 + base$tmoy_8610_7 + base$tmoy_8610_8 +
      base$tmoy_8610_9 + base$tmoy_8610_10 + base$tmoy_8610_11 + base$tmoy_8610_12)/12

  # Lecture des anomalies climatiques genere par Georges pour le projet TODO creer un tif krige des anomalies clim
  anomalies <- read.csv(paste0(rep_projet, "/2046_2065/Anomalies_climat.csv"), header = TRUE, sep = ";", dec = ",")

  ############################################################################################

  # Calcul des temperatures futures mensuelles
  for (mois in c(1:12)) {
      value_anom <- anomalies[which(anomalies[, "mois"] == mois), "tas"]
      nom_colonne <- paste0("tmoy_8610_", mois)
      info_value <- base[, nom_colonne]
      # Addition de l'anomalie climatique
      climat_futur <- info_value + value_anom
      # Ajout de climat futur dans base
      nom_colonne_futur <- paste0("tmoy_4665_", mois)
      base <- setNames(data.frame(base, climat_futur), c(colnames(base), nom_colonne_futur))

  }  # Fin de la boucle des mois

  # Calcul des temperatures futures annuelles
  moy_ano_tas_annuelle <- mean(anomalies[, "tas"])
  info_value_moy_ann <- base[, "tmoy_8610"] + moy_ano_tas_annuelle
  base <- setNames(data.frame(base, info_value_moy_ann), c(colnames(base), "tmoy_4665"))

  # Calcul des precipitations futures mensuelles
  for (mois in c(1:12)) {
      value_anom <- anomalies[which(anomalies[, "mois"] == mois), "pr"] * 2592000  # 1 mm/s = 2592000 mm/mois
      nom_colonne <- paste0("prec_8610_", mois)
      info_value <- base[, nom_colonne]
      # Addition de l'anomalie climatique
      climat_futur <- info_value + value_anom
      # Ajout de climat futur dans base
      nom_colonne_futur <- paste0("prec_4665_", mois)
      base = setNames(data.frame(base, climat_futur), c(colnames(base), nom_colonne_futur))
  }  # Fin de la boucle des mois


  # Calcul des etp futures
  for (mois in c(1:12)) {
      etp8610_mois <- base[, paste0("etp_8610_", mois)]
      tmoy8610_mois <- base[, paste0("tmoy_8610_", mois)]
      tmoy4665_mois <- base[, paste0("tmoy_4665_", mois)]
      value_anom <- anomalies[which(anomalies[, "mois"] == mois), "rsds"] * 20.7  # 1 W/m2 = 20.7 kCal/cm2/jour
      # Calcul de l'etp futur
      climat_futur <- (etp8610_mois * 2.5 * ((tmoy8610_mois + 15)/tmoy8610_mois)) * 0.4 * (tmoy4665_mois/(tmoy4665_mois + 15))
      # Ajout de climat futur dans base
      nom_colonne_futur <- paste0("etp_4665_", mois)
      base <- setNames(data.frame(base, climat_futur), c(colnames(base), nom_colonne_futur))
  }  # Fin de la boucle des mois

  if (enreg) {
      write.table(base, file = paste0(rep_travail, "/base_etape1.csv"), append = FALSE, quote = FALSE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE,
          col.names = TRUE, qmethod = c("escape", "double"))
      message(paste0("Result is saved in: ", paste0(dirname(rep_travail), "/base_etape1.csv")))
  }

  # Colonnes de base_etape1.csv : . X, Y, rum, 4 param?tres de chimie du sol, . temperatures mensuelles de reference, temperature moyenne annuelle de ref., . 12
  # precipitations mensuelles de reference, 12 etp mensuelles de reference, . temperature moyenne annuelle future, . 12 precipitations mensuelles futures, 12 etp
  # mensuelles futures,


  # calcul du BHE (RU, DE et ETR)

  for (mois in 1:12) {
      prec8610_mois <- base[, paste0("prec_8610_", mois)]
      prec4665_mois <- base[, paste0("prec_4665_", mois)]
      etp8610_mois <- base[, paste0("etp_8610_", mois)]
      etp4665_mois <- base[, paste0("etp_4665_", mois)]
      rum <- base[, "rum"]

      if (mois == 1) {
          # RU present et futur
          value_p_ru <- ifelse(prec8610_mois >= etp8610_mois, pmax(pmin(rum, rum + prec8610_mois - etp8610_mois), 0), pmin(pmax(0, (rum * exp((prec8610_mois -
              etp8610_mois)/rum))), rum))
          value_f_ru <- ifelse(prec4665_mois >= etp4665_mois, pmax(pmin(rum, rum + prec4665_mois - etp4665_mois), 0), pmin(pmax(0, (rum * exp((prec4665_mois -
              etp4665_mois)/rum))), rum))
          # ETR present et futur
          value_p_etr <- ifelse(prec8610_mois >= etp8610_mois, etp8610_mois, rum + prec8610_mois - value_p_ru)
          value_f_etr <- ifelse(prec4665_mois >= etp4665_mois, etp4665_mois, rum + prec4665_mois - value_f_ru)
      } else {
          # RU present et futur
          ru_present_m1 <- base[, paste0("ru_8610_", mois - 1)]
          ru_futur_m1 <- base[, paste0("ru_4665_", mois - 1)]
          value_p_ru <- ifelse(prec8610_mois >= etp8610_mois, pmax(pmin(rum, ru_present_m1 + prec8610_mois - etp8610_mois), 0), pmin(pmax(0, (ru_present_m1 * exp((prec8610_mois -
              etp8610_mois)/rum))), rum))
          value_f_ru <- ifelse(prec4665_mois >= etp4665_mois, pmax(pmin(rum, ru_futur_m1 + prec4665_mois - etp4665_mois), 0), pmin(pmax(0, (ru_futur_m1 * exp((prec4665_mois -
              etp4665_mois)/rum))), rum))

          # ETR present et futur
          value_p_etr <- ifelse(prec8610_mois >= etp8610_mois, etp8610_mois, ru_present_m1 + prec8610_mois - value_p_ru)
          value_f_etr <- ifelse(prec4665_mois >= etp4665_mois, etp4665_mois, ru_futur_m1 + prec4665_mois - value_f_ru)

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

  }  # Fin de la boucle des mois

  # Deficit en eau estival (juin a Aout)
  sum_de_ete_p <- base[, "de_8610_6"] + base[, "de_8610_7"] + base[, "de_8610_8"]
  sum_de_ete_f <- base[, "de_4665_6"] + base[, "de_4665_7"] + base[, "de_4665_8"]

  base <- setNames(data.frame(base, sum_de_ete_p), c(colnames(base), "de_8610_ete"))
  base <- setNames(data.frame(base, sum_de_ete_f), c(colnames(base), "de_4665_ete"))

  #---------------------------
  # Distribution especes
  #---------------------------

  # lecture du fichier 'tab6var.csv' (contenant les donnees IFN de presence et les variables environnementales pour caler les modeles GAM).
  # setwd(paste0(rep_travail, 'E:/2_Donnees/modeles_distrib')
  data <- read.table(paste0(rep_data, "/tab6var.csv"), header = TRUE, sep = ";", dec = ",")
  data2 <- data[complete.cases(data), ]  # supprime les NA

  # TODO a remettre avec prolL2 defini au-dessus Definition de la projection Lambert 2 en language R
  crs_l2 = "+proj=lcc +lat_1=46.8 +lat_0=46.8 +lon_0=0 +k_0=0.99987742 +x_0=600000 +y_0=2200000 +a=6378249.2 +b=6356515 +towgs84=-168,-60,320,0,0,0,0 +pm=paris +units=m +no_defs"

  # Creation des rasters des variables que l'on veut dans base
  r_tmoy8610 <- rasterFromXYZ(base[, c("Xl2", "Yl2", "tmoy_8610")], crs = crs_l2)
  r_tmoy4665 <- rasterFromXYZ(base[, c("Xl2", "Yl2", "tmoy_4665")], crs = crs_l2)
  r_de8610 <- rasterFromXYZ(base[, c("Xl2", "Yl2", "de_8610_ete")], crs = crs_l2)
  r_de4665 <- rasterFromXYZ(base[, c("Xl2", "Yl2", "de_4665_ete")], crs = crs_l2)
  r_ph <- rasterFromXYZ(base[, c("Xl2", "Yl2", "ph")], crs = crs_l2)
  r_cn <- rasterFromXYZ(base[, c("Xl2", "Yl2", "cn")], crs = crs_l2)
  r_et <- rasterFromXYZ(base[, c("Xl2", "Yl2", "et")], crs = crs_l2)
  r_ep <- rasterFromXYZ(base[, c("Xl2", "Yl2", "ep")], crs = crs_l2)

  # Enregistrement des resultats
  dossier_save = paste0(rep_travail, "/2046_2065")
  if (!dir.exists(dossier_save)) {
    dir.create(dossier_save, showWarnings = TRUE, recursive = TRUE, mode = "0777")
    message(paste0("Directory is created in: ", dossier_save))
  }

  # Boucle sur les especes
  out <- as.list(species)
  names(out) <- species
  nb <- 1

  for (sp in species) {
      print(paste0(Sys.time(), " - Calcul pour l'essence : ", sp, " (", nb, "/", length(species), ")."))
      # on evalue l expression du gam
      txt <- paste0(ifelse(sp %in% c("fasy", "piab", "pisy", "qupe", "abal", "acca", "acmo", "acop", "acpl", "acps", "algl", "bepe", "cabe", "casa", "fran", "frex",
          "piha", "prav", "quil", "qupu", "quro", "rops", "saal", "saca", "saci", "soar", "soau", "soto", "tico", "tipl", "ulmi"), " s(tmoy_an,4) +", ""), ifelse(sp %in%
          c("fasy", "piab", "pisy", "abal", "acop", "acpl", "acps", "algl", "bepu", "fran", "frex", "piha", "prav", "quil", "qupu", "quro", "rops", "soau", "soto",
              "tico", "tipl", "ulgl", "ulmi"), " s(plot_deete,4) +", ""), ifelse(sp %in% c("fasy", "qupe", "qupe", "acca", "acmo", "acop", "algl", "bepe", "bepu",
          "cabe", "casa", "frex", "prav", "quil", "qupu", "quro", "rops", "saal", "saca", "saci", "soar", "soto", "tico", "tipl", "ulmi"), " s(plac_EP_ssess1et2,4) +",
          ""), ifelse(sp %in% c("fasy", "piab", "pisy", "acca", "acmo", "acop", "acpl", "acps", "bepe", "bepu", "cabe", "casa", "fran", "frex", "piha", "prav",
          "qupu", "quro", "rops", "saal", "saca", "soar", "soto", "tico", "tipl", "ulmi"), " s(plac_pH_ssess1et2,4) +", ""), ifelse(sp %in% c("pisy", "qupe", "abal",
          "acca", "acmo", "acpl", "acps", "bepu", "cabe", "casa", "rops", "saci", "soau", "tico", "ulgl"), " s(plac_CN_ssess1et2,4) +", ""), ifelse(sp %in% c("acop",
          "acpl", "bepe", "cabe", "quil", "qupu", "quro", "saci", "soar", "soto", "ulgl"), " s(plac_ET_ssess1et2,4) +", ""))
      # on remplace l expression txt dans la formule du gamdf
      form <- paste("cacheEnv$gamdf <- gam(data2[ , sp] ~", substr(txt, 1, nchar(txt) - 2), ",family = binomial, data = data2, na.action = na.omit)")
      ### evlaue le gamdf
      eval(parse(text = substitute(form, list(form = form))))

      # on recherche les noms de colonne
      txt <- paste0(ifelse(sp %in% c("fasy", "piab", "pisy", "qupe", "abal", "acca", "acmo", "acop", "acpl", "acps", "algl", "bepe", "cabe", "casa", "fran", "frex",
          "piha", "prav", "quil", "qupu", "quro", "rops", "saal", "saca", "saci", "soar", "soau", "soto", "tico", "tipl", "ulmi"), "r_tmoy8610,", ""), ifelse(sp %in%
          c("fasy", "piab", "pisy", "abal", "acop", "acpl", "acps", "algl", "bepu", "fran", "frex", "piha", "prav", "quil", "qupu", "quro", "rops", "soau", "soto",
              "tico", "tipl", "ulgl", "ulmi"), "r_de8610,", ""), ifelse(sp %in% c("fasy", "qupe", "qupe", "acca", "acmo", "acop", "algl", "bepe", "bepu", "cabe",
          "casa", "frex", "prav", "quil", "qupu", "quro", "rops", "saal", "saca", "saci", "soar", "soto", "tico", "tipl", "ulmi"), "r_ep,", ""), ifelse(sp %in%
          c("fasy", "piab", "pisy", "acca", "acmo", "acop", "acpl", "acps", "bepe", "bepu", "cabe", "casa", "fran", "frex", "piha", "prav", "qupu", "quro", "rops",
              "saal", "saca", "soar", "soto", "tico", "tipl", "ulmi"), "r_ph,", ""), ifelse(sp %in% c("pisy", "qupe", "abal", "acca", "acmo", "acpl", "acps", "bepu",
          "cabe", "casa", "rops", "saci", "soau", "tico", "ulgl"), "r_cn,", ""), ifelse(sp %in% c("acop", "acpl", "bepe", "cabe", "quil", "qupu", "quro", "saci",
          "soar", "soto", "ulgl"), "r_et,", ""))

      form <- paste("RHS_p <- stack(", substr(txt, 1, nchar(txt) - 1), ")")
      # evalue RHS_p
      eval(parse(text = substitute(form, list(form = form))))
      # RHS_p <- stack(r_tmoy8610, r_de8610, r_ep, r_ph)

      # on recherche les noms de colonne
      txt <- paste0(ifelse(sp %in% c("fasy", "piab", "pisy", "qupe", "abal", "acca", "acmo", "acop", "acpl", "acps", "algl", "bepe", "cabe", "casa", "fran", "frex",
          "piha", "prav", "quil", "qupu", "quro", "rops", "saal", "saca", "saci", "soar", "soau", "soto", "tico", "tipl", "ulmi"), "r_tmoy4665,", ""), ifelse(sp %in%
          c("fasy", "piab", "pisy", "abal", "acop", "acpl", "acps", "algl", "bepu", "fran", "frex", "piha", "prav", "quil", "qupu", "quro", "rops", "soau", "soto",
              "tico", "tipl", "ulgl", "ulmi"), "r_de4665,", ""), ifelse(sp %in% c("fasy", "qupe", "qupe", "acca", "acmo", "acop", "algl", "bepe", "bepu", "cabe",
          "casa", "frex", "prav", "quil", "qupu", "quro", "rops", "saal", "saca", "saci", "soar", "soto", "tico", "tipl", "ulmi"), "r_ep,", ""), ifelse(sp %in%
          c("fasy", "piab", "pisy", "acca", "acmo", "acop", "acpl", "acps", "bepe", "bepu", "cabe", "casa", "fran", "frex", "piha", "prav", "qupu", "quro", "rops",
              "saal", "saca", "soar", "soto", "tico", "tipl", "ulmi"), "r_ph,", ""), ifelse(sp %in% c("pisy", "qupe", "abal", "acca", "acmo", "acpl", "acps", "bepu",
          "cabe", "casa", "rops", "saci", "soau", "tico", "ulgl"), "r_cn,", ""), ifelse(sp %in% c("acop", "acpl", "bepe", "cabe", "quil", "qupu", "quro", "saci",
          "soar", "soto", "ulgl"), "r_et,", ""))

      form <- paste("RHS_f <- stack(", substr(txt, 1, nchar(txt) - 1), ")")
      # evalue RHS_f
      eval(parse(text = substitute(form, list(form = form))))
      # RHS_f <- stack(r_tmoy4665, r_de4665, r_ep, r_ph)

      # on recherche les noms de colonne
      nomcol <- paste0("cacheEnv$nc <- c(", ifelse(sp %in% c("fasy", "piab", "pisy", "qupe", "abal", "acca", "acmo", "acop", "acpl", "acps", "algl", "bepe", "cabe", "casa",
          "fran", "frex", "piha", "prav", "quil", "qupu", "quro", "rops", "saal", "saca", "saci", "soar", "soau", "soto", "tico", "tipl", "ulmi"), "as.character(quote(tmoy_an)),",
          ""), ifelse(sp %in% c("fasy", "piab", "pisy", "abal", "acop", "acpl", "acps", "algl", "bepu", "fran", "frex", "piha", "prav", "quil", "qupu", "quro",
          "rops", "soau", "soto", "tico", "tipl", "ulgl", "ulmi"), "as.character(quote(plot_deete)),", ""), ifelse(sp %in% c("fasy", "qupe", "qupe", "acca", "acmo",
          "acop", "algl", "bepe", "bepu", "cabe", "casa", "frex", "prav", "quil", "qupu", "quro", "rops", "saal", "saca", "saci", "soar", "soto", "tico", "tipl",
          "ulmi"), "as.character(quote(plac_EP_ssess1et2)),", ""), ifelse(sp %in% c("fasy", "piab", "pisy", "acca", "acmo", "acop", "acpl", "acps", "bepe", "bepu",
          "cabe", "casa", "fran", "frex", "piha", "prav", "qupu", "quro", "rops", "saal", "saca", "soar", "soto", "tico", "tipl", "ulmi"), "as.character(quote(plac_pH_ssess1et2)),",
          ""), ifelse(sp %in% c("pisy", "qupe", "abal", "acca", "acmo", "acpl", "acps", "bepu", "cabe", "casa", "rops", "saci", "soau", "tico", "ulgl"), "as.character(quote(plac_CN_ssess1et2)),",
          ""), ifelse(sp %in% c("acop", "acpl", "bepe", "cabe", "quil", "qupu", "quro", "saci", "soar", "soto", "ulgl"), "as.character(quote(plac_ET_ssess1et2)),",
          ""))

      tt <- paste0(substr(nomcol, 1, nchar(nomcol) - 1), ")")
      # evalue le nom des colonnes
      eval(parse(text = substitute(tt, list(tt = tt))))

      names(RHS_p) <- cacheEnv$nc  # fonction 'names' ds nouvelle version
      names(RHS_f) <- cacheEnv$nc  # fonction 'names' ds nouvelle version
      # Enregistremenet present (p) de la prediction futur (f)
      Predsp_p <- predict(RHS_p, cacheEnv$gamdf, type = "response", progress = "text", filename = file.path(dossier_save, paste0(sp, "_8610.tif")), overwrite = TRUE)
      out[[sp]]$present <- Predsp_p
      out[[sp]]$median_present <- median(values(Predsp_p), na.rm = TRUE)
      Predsp_f <- predict(RHS_f, cacheEnv$gamdf, type = "response", progress = "text", filename = file.path(dossier_save, paste0(sp, "_4665.tif")), overwrite = TRUE)
      out[[sp]]$futur <- Predsp_f
      out[[sp]]$median_futur <- median(values(Predsp_f), na.rm = TRUE)
      # Ratio futur sur present
      out[[sp]]$fsurp <- Predsp_f/Predsp_p
      nb <- nb + 1
      # Save en tif writeRaster(fsurp, filename = paste0(rep_projet, '/tendance_', sp, '.tif'), format='GTiff', overwrite=TRUE)
  }

  df <- NULL
  for (i in 1:length(out)) {
      res <- c(out[[i]][[1]], as.numeric(out[[i]]$median_present), as.numeric(out[[i]]$median_futur))
      names(res) <- c("essence", "p", "f")
      df <- rbind(df, res)
  }
  dd <- as.data.frame(df, row.names = T)
  dd$p <- as.numeric(levels(dd$p))
  dd$f <- as.numeric(levels(dd$f))

  p1 <- ggplot(dd, ggplot2::aes(x = dd$p, y = dd$f, label = dd$essence)) + geom_abline(intercept = 0, color = "red") + geom_point(color = "red") + geom_label_repel() + scale_x_log10(limits = c(1e-10,
      1)) + scale_y_log10(limits = c(1e-10, 1)) + xlab(label = "Present") + ylab(label = "Futur") + ggtitle("Stressogramme present-futur")

  ## cree la liste des resultats
  turn <- list(out, p1)
  names(turn) <- c("species", "stressogramme")
  message("Calculation realized!")
  return(turn)

}  #####     FIN DU SCRIPT     ######
