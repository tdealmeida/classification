##############################################################
# SCRIPT : Data.R
# OBJECTIF : Importation + filtre des données FCT
# Données en sortie : 
# - data (métriques DGO brutes) 
# - roe
# - Data (métriques DGO filtrées)
# - surface_drainee (surface drainée)
# - meander_belt (meander belt)
# - chenal_forme (propriétés des chenaux)
# - chenal_labels (labels DGO)
# - iles_veget (îles végétalisées)
# - retenue (retenues)

##############################################################

# ============================================
# 1. Chargement des Bibliothèques
# ============================================
library(dplyr)
library(RPostgreSQL)
library(purrr)
library(tidyr)
library(sf)
library(ggplot2)
library(zoo)
library(patchwork)
library(circlize)
library(caret)
library(randomForest)
library(units)
library(concaveman)
library(changepoint)
library(stringr)
library(Rbeast)
library(lwgeom)       # st_split
library(qgisprocess)  # QGIS
library(rpart)
library(rpart.plot)
library(ggpubr)
library(forcats)
library(ggtext)
library(ggh4x)
library(corrplot)
library(scales)
library(ragg)
library(here)
# detach(qgisprocess::qgis_process, unload = TRUE) # pour éviter les conflits de fonctions avec d'autres packages)
# Sys.setenv(
  # R_QGISPROCESS_PATH = "C:/Program Files/QGIS 3.44.9/bin/qgis_process-qgis-ltr.bat",
  # QGIS_CUSTOM_CONFIG_PATH = tempdir()
# )

# qgisprocess::qgis_configure()
# qgisprocess::qgis_session_info()
# qgisprocess::qgis_enable_plugins()

# ============================================
# 2. Import des métriques depuis la base de données FCT  
# ============================================
# Connexion à la base de données PostgreSQL
con <- dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("DBMAPDO_HOST"),
  port = Sys.getenv("DBMAPDO_PORT"),
  dbname = Sys.getenv("DBMAPDO_NAME"),
  user = Sys.getenv("DBMAPDO_USER"),
  password  = Sys.getenv("DBMAPDO_PASS"))

# Import des données de l'Isère
# query <- "SELECT * FROM network_metrics WHERE gid_region IN ('11')" # pour l'Isère
# query <- "SELECT * FROM network_metrics WHERE gid_region IN ('11','33','16','31')" # pour bassin du Rhône
# query <- "SELECT * FROM network_metrics WHERE gid_region IN ('11','33','16','31','26')" # pour RMC
query <- "SELECT * FROM network_metrics" # pour France
data <- sf::st_read(dsn = con, query = query) 

# Import des ROE de RMC
# query <- "SELECT * FROM roe WHERE gid_region IN ('11')" # pour l'Isère
# query <- "SELECT * FROM roe WHERE gid_region IN ('11','33','16','31')" # pour bassin du Rhône
# query <- "SELECT * FROM roe WHERE gid_region IN ('11','33','16','31','26')" # pour RMC
query <- "SELECT * FROM roe" # pour France
roe <- sf::st_read(dsn = con, query = query) 
    
dbDisconnect(con) # Fermeture de la connexion à la base de données

# ------------------------------------------------
# 3. Filtrage des petits DGO NA (<20m) inter DGO
# ------------------------------------------------
# data <- data %>%
  # select(-fid)

Data <- data %>%
  mutate(length = as.numeric(st_length(geom))) %>%
  filter(!(is.na(measure_medial_axis) & length < 20)) %>%
  mutate(ID_DGO = fid)%>%
  select(-fid)

# ------------------------------------------------
# 4. Export des données
# ------------------------------------------------
# st_write(Data, "Data_DGO_fr.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile
st_write(Data, "data_DGO_final2.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile

# ------------------------------------------------
# 5. Import des surfaces drainées par bassin
# ------------------------------------------------
surface_drainee_rhone <- read.csv("Data/RMC/Rhone/DRAINAGE_AREA_rhone.csv")
surface_drainee_med <- read.csv("Data/RMC/med/DRAINAGE_AREA_med.csv")
surface_drainee_corse <- read.csv("Data/RMC/corse/DRAINAGE_AREA_corse.csv")
surface_drainee_rhin <- read.csv("Data/Rhin/DRAINAGE_AREA_rhin.csv")
surface_drainee_loire <- read.csv("Data/Loire/DRAINAGE_AREA_loire.csv")
surface_drainee_seine <- read.csv("Data/Seine/DRAINAGE_AREA_seine.csv")
surface_drainee_nord <- read.csv("Data/Nord/DRAINAGE_AREA_nords.csv")
surface_drainee_garonne <- read.csv("Data/Garonne/DRAINAGE_AREA_garonne.csv")
surface_drainee_adour <- read.csv("Data/Adour/DRAINAGE_AREA_adour.csv")
surface_drainee_charente <- read.csv("Data/Charente/DRAINAGE_AREA_charente.csv")
surface_drainee_moselle <- read.csv("Data/Moselle/DRAINAGE_AREA_moselle.csv")
surface_drainee_meuse <- read.csv("Data/Meuse/DRAINAGE_AREA_meuse.csv")
surface_drainee_bretagne <- read.csv("Data/Bretagne/DRAINAGE_AREA_bretagne.csv")

surface_drainee <- rbind(surface_drainee_rhone,
                         surface_drainee_med,
                         surface_drainee_corse,
                         surface_drainee_rhin,
                         surface_drainee_loire,
                         surface_drainee_seine,
                         surface_drainee_nord,
                         surface_drainee_garonne,
                         surface_drainee_adour,
                         surface_drainee_charente,
                         surface_drainee_moselle,
                         surface_drainee_meuse,
                         surface_drainee_bretagne
)

# st_write(surface_drainee, "surface_drainee.csv", delete_layer = TRUE) # Export des données nettoyées en shapefile)

# ------------------------------------------------
# 6. Import des meander belt par bassin
# ------------------------------------------------
meanderbelt_rmc <- st_read("Data/RMC/meanderbelt_RMC.gpkg") %>%
  st_drop_geometry() %>%
  select(M, AXIS_2, enveloppe,VALUE) %>%
  group_by(AXIS_2, M) %>%
  filter(if(n() > 1) VALUE == 2 else TRUE) %>%
  slice_max(enveloppe, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-VALUE)

meanderbelt_rhin <- st_read("Data/Rhin/meanderbelt_rhin.gpkg") %>%
  st_drop_geometry() %>%
  select(M, AXIS_2, enveloppe, VALUE) %>%
  group_by(AXIS_2, M) %>%
  filter(if(n() > 1) VALUE == 2 else TRUE) %>%
  slice_max(enveloppe, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-VALUE)

meanderbelt_loire <- st_read("Data/Loire/meanderbelt_loire.gpkg") %>%
  st_drop_geometry() %>%
  select(M, AXIS_2, enveloppe, VALUE) %>%
  group_by(AXIS_2, M) %>%
  filter(if(n() > 1) VALUE == 2 else TRUE) %>%
  slice_max(enveloppe, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-VALUE)

meanderbelt_seine <- st_read("Data/Seine/meanderbelt_seine.gpkg") %>%
  st_drop_geometry() %>%
  select(M, AXIS_2, enveloppe, VALUE) %>%
  group_by(AXIS_2, M) %>%
  filter(if(n() > 1) VALUE == 2 else TRUE) %>%
  slice_max(enveloppe, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-VALUE)

meanderbelt_nord <- st_read("Data/Nord/meanderbelt_nords.gpkg") %>%
  st_drop_geometry() %>%
  select(M, AXIS_2, enveloppe, VALUE) %>%
  group_by(AXIS_2, M) %>%
  filter(if(n() > 1) VALUE == 2 else TRUE) %>%
  slice_max(enveloppe, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-VALUE)

meanderbelt_garonne <- st_read("Data/Garonne/meanderbelt_garonne.gpkg") %>%
  st_drop_geometry() %>%
  select(M, AXIS_2, enveloppe, VALUE) %>%
  group_by(AXIS_2, M) %>%
  filter(if(n() > 1) VALUE == 2 else TRUE) %>%
  slice_max(enveloppe, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-VALUE)

meanderbelt_adour <- st_read("Data/Adour/meanderbelt_adour.gpkg") %>%
  st_drop_geometry() %>%
  select(M, AXIS_2, enveloppe, VALUE) %>%
  group_by(AXIS_2, M) %>%
  filter(if(n() > 1) VALUE == 2 else TRUE) %>%
  slice_max(enveloppe, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-VALUE)

meanderbelt_charente <- st_read("Data/Charente/meanderbelt_charente.gpkg") %>%
  st_drop_geometry() %>%
  select(M, AXIS_2, enveloppe, VALUE) %>%
  group_by(AXIS_2, M) %>%
  filter(if(n() > 1) VALUE == 2 else TRUE) %>%
  slice_max(enveloppe, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-VALUE)

meanderbelt_moselle <- st_read("Data/Moselle/meanderbelt_moselle.gpkg") %>%
  st_drop_geometry() %>%
  select(M, AXIS_2, enveloppe, VALUE) %>%
  group_by(AXIS_2, M) %>%
  filter(if(n() > 1) VALUE == 2 else TRUE) %>%
  slice_max(enveloppe, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-VALUE)

meanderbelt_meuse <- st_read("Data/Meuse/meanderbelt_meuse.gpkg") %>%
  st_drop_geometry() %>%
  select(M, AXIS_2, enveloppe, VALUE) %>%
  group_by(AXIS_2, M) %>%
  filter(if(n() > 1) VALUE == 2 else TRUE) %>%
  slice_max(enveloppe, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-VALUE)

meanderbelt_bretagne <- st_read("Data/Bretagne/meanderbelt_bretagne.gpkg") %>%
  st_drop_geometry() %>%
  select(M, AXIS_2, enveloppe, VALUE)%>%
  group_by(AXIS_2, M) %>%
  filter(if(n() > 1) VALUE == 2 else TRUE) %>%
  slice_max(enveloppe, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-VALUE)

meander_belt <- rbind(meanderbelt_rmc,
                         meanderbelt_rhin,
                         meanderbelt_loire,
                         meanderbelt_seine,
                         meanderbelt_nord,
                         meanderbelt_garonne,
                         meanderbelt_adour,
                         meanderbelt_charente,
                         meanderbelt_moselle,
                         meanderbelt_meuse,
                         meanderbelt_bretagne
)


# ------------------------------------------------
# 7. Import du confinement degree
# ------------------------------------------------
margins_VB <- st_read("margins_VB.gpkg") %>%
  st_drop_geometry() %>%
  distinct(AXIS, M, .keep_all = TRUE) %>%   # ← supprime les doublons axis + M
  rename(measure_medial_axis = M,
         axis = AXIS) %>%
  select(axis, measure_medial_axis, longueur_margins)

margins_VB_rmc <- st_read("margins_VB_rmc.gpkg") %>%
  st_drop_geometry() %>%
  distinct(AXIS, M, .keep_all = TRUE) %>%   # ← supprime les doublons axis + M
  rename(measure_medial_axis = M,
         axis = AXIS) %>%
  select(axis, measure_medial_axis, longueur_margins)

margins_VB_sum <- bind_rows(margins_VB, margins_VB_rmc)

# ------------------------------------------------
# 8. Import des chenal forms par bassin
# ------------------------------------------------
chenal_forme_rmc <- read.csv("Data/RMC/chenal_props_rmc.csv")
chenal_forme_rhin <- read.csv("Data/Rhin/chenal_props_rhin.csv")
# chenal_forme_loire <- read.csv("Data/Loire/chenal_props_loire.csv") manque loire
chenal_forme_seine <- read.csv("Data/Seine/chenal_props_seine.csv")
chenal_forme_nord <- read.csv("Data/Nord/chenal_props_nords.csv")
chenal_forme_garonne <- read.csv("Data/Garonne/chenal_props_garonne.csv")
chenal_forme_adour <- read.csv("Data/Adour/chenal_props_adour.csv")
chenal_forme_charente <- read.csv("Data/Charente/chenal_props_charente.csv")
chenal_forme_moselle <- read.csv("Data/Moselle/chenal_props_moselle.csv")
chenal_forme_meuse <- read.csv("Data/Meuse/chenal_props_meuse.csv")
chenal_forme_bretagne <- read.csv("Data/Bretagne/chenal_props_bretagne.csv")

chenal_forme <- rbind(chenal_forme_rmc,
                      chenal_forme_rhin,
                      # chenal_forme_loire,
                      chenal_forme_seine,
                      chenal_forme_nord,
                      chenal_forme_garonne,
                      chenal_forme_adour,
                      chenal_forme_charente,
                      chenal_forme_moselle,
                      chenal_forme_meuse,
                      chenal_forme_bretagne
)

# ------------------------------------------------
# 9. Import des labels pour le RF du multichenal
# ------------------------------------------------
chenal_labels <- st_read("DGO_label.shp") %>%
  rename(geom = geometry) %>%
  st_as_sf(sf_column_name = "geom") %>%
  select(AXIS, M, multi) %>%
  group_by(AXIS, M) %>%
  slice(1) %>%        # garder une seule ligne par couple (AXIS, M)
  ungroup()


# ------------------------------------------------
# 10. Import des îles végétalisées par bassin
# ------------------------------------------------
iles_veget_rmc <- st_read("Data/RMC/iles_veget_rmc.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M)
  
iles_veget_rhin <- st_read("Data/Rhin/iles_veget_rhin.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M)

iles_veget_loire <- st_read("Data/Loire/iles_veget_loire.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M)

iles_veget_seine <- st_read("Data/Seine/iles_veget_seine.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M) 

iles_veget_nord <- st_read("Data/Nord/iles_veget_nord.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M)

iles_veget_garonne <- st_read("Data/Garonne/iles_veget_garonne.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M)

iles_veget_adour <- st_read("Data/Adour/iles_veget_adour.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M)

iles_veget_charente <- st_read("Data/Charente/iles_veget_charente.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M)

iles_veget_moselle <- st_read("Data/Moselle/iles_veget_moselle.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M)

iles_veget_meuse <- st_read("Data/Meuse/iles_veget_meuse.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M) 

iles_veget_bretagne <- st_read("Data/Bretagne/iles_veget_bretagne.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M) 

iles_veget <- rbind(iles_veget_rmc,
                      iles_veget_rhin,
                      iles_veget_loire,
                      iles_veget_seine,
                      iles_veget_nord,
                      iles_veget_garonne,
                      iles_veget_adour,
                      iles_veget_charente,
                      iles_veget_moselle,
                      iles_veget_meuse,
                      iles_veget_bretagne
)
  

# ------------------------------------------------
# 10. Import des îles total par bassin
# ------------------------------------------------
iles_total_rmc <- st_read("Data/RMC/iles_total_rmc.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS_2, M_2) %>%
  rename(AXIS = AXIS_2,
         M = M_2)

iles_total_rhin <- st_read("Data/Rhin/iles_total_rhin.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M)

iles_total_loire <- st_read("Data/Loire/iles_total_loire.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M)

iles_total_seine <- st_read("Data/Seine/iles_total_seine.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M)

iles_total_nord <- st_read("Data/Nord/iles_total_nords.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M)

iles_total_garonne <- st_read("Data/Garonne/iles_total_garonne.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M)

iles_total_adour <- st_read("Data/Adour/iles_total_adour.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M)

iles_total_charente <- st_read("Data/Charente/iles_total_charente.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M)

iles_total_moselle <- st_read("Data/Moselle/iles_total_moselle.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M)

iles_total_meuse <- st_read("Data/Meuse/iles_total_meuse.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M)

iles_total_bretagne <- st_read("Data/Bretagne/iles_total_bretagne.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M)

iles_total <- rbind(iles_total_rmc,
                      iles_total_rhin,
                      iles_total_loire,
                      iles_total_seine,
                      iles_total_nord,
                      iles_total_garonne,
                      iles_total_adour,
                      iles_total_charente,
                      iles_total_moselle,
                      iles_total_meuse,
                      iles_total_bretagne
)
  
  
# ------------------------------------------------
# 11. Import des retenues par bassin
# ------------------------------------------------
retenue_rmc <- st_read("Data/RMC/retenue_rmc.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M) 

retenue_rhin <- st_read("Data/Rhin/retenue_rhin.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M)

retenue_loire <- st_read("Data/Loire/retenue_loire.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M)

retenue_seine <- st_read("Data/Seine/retenue_seine.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M)

retenue_nord <- st_read("Data/Nord/retenue_nords.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M)

retenue_garonne <- st_read("Data/Garonne/retenue_garonne.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M) 

retenue_adour <- st_read("Data/Adour/retenue_adour.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M)

retenue_charente <- st_read("Data/Charente/retenue_charente.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M)

retenue_moselle <- st_read("Data/Moselle/retenue_moselle.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M)

retenue_meuse <- st_read("Data/Meuse/retenue_meuse.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M)

retenue_bretagne <- st_read("Data/Bretagne/retenue_bretagne.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M)

retenu <- rbind(retenue_rmc,
                     retenue_rhin,
                     retenue_loire,
                     retenue_seine,
                     retenue_nord,
                     retenue_garonne,
                     retenue_adour,
                     retenue_charente,
                     retenue_moselle,
                     retenue_meuse,
                     retenue_bretagne
)

# ------------------------------------------------
# 12. Import des meander belt axis par bassin
# ------------------------------------------------
meanderbelt_axis_rmc <- st_read("Data/RMC/meanderbelt_axis_RMC_2.shp") %>%
  st_transform(2154) %>%
  rename(geom = geometry) %>%
  select(axis, toponyme)

meanderbelt_axis_rhin <- st_read("Data/Rhin/meanderbelt_axis_rhin_2.gpkg") %>%
  st_transform(2154)%>%
  select(axis, toponyme)

meanderbelt_axis_loire <- st_read("Data/Loire/meanderbelt_axis_loire_3.gpkg") %>%
  st_transform(2154)%>%
  select(axis, toponyme)

meanderbelt_axis_seine <- st_read("Data/Seine/meanderbelt_axis_seine_2.gpkg") %>%
  st_transform(2154)%>%
  select(axis, toponyme)

meanderbelt_axis_nord <- st_read("Data/Nord/meanderbelt_axis_nords_2.gpkg") %>%
  st_transform(2154)%>%
  select(axis, toponyme)

meanderbelt_axis_garonne <- st_read("Data/Garonne/meanderbelt_axis_garonne_3.gpkg") %>%
  st_transform(2154)%>%
  select(axis, toponyme)

meanderbelt_axis_adour <- st_read("Data/Adour/meanderbelt_axis_adour_3.gpkg") %>%
  st_transform(2154)%>%
  select(axis, toponyme)

meanderbelt_axis_charente <- st_read("Data/Charente/meanderbelt_axis_charente.gpkg") %>%
  st_transform(2154)%>%
  select(axis, toponyme)

meanderbelt_axis_moselle <- st_read("Data/Moselle/meanderbelt_axis_moselle_2.gpkg") %>%
  st_transform(2154)%>%
  select(axis, toponyme)

meanderbelt_axis_meuse <- st_read("Data/Meuse/meanderbelt_axis_meuse.gpkg") %>%
  st_transform(2154)%>%
  select(axis, toponyme)

meanderbelt_axis_bretagne <- st_read("Data/Bretagne/meanderbelt_axis_bretagne.gpkg") %>%
  st_transform(2154)%>%
  select(axis, toponyme)

meanderbelt_axis <- rbind(meanderbelt_axis_rmc,
                         meanderbelt_axis_rhin,
                         meanderbelt_axis_loire,
                         meanderbelt_axis_seine,
                         meanderbelt_axis_nord,
                         meanderbelt_axis_garonne,
                         meanderbelt_axis_adour,
                         meanderbelt_axis_charente,
                         meanderbelt_axis_moselle,
                         meanderbelt_axis_meuse,
                         meanderbelt_axis_bretagne
)

# ------------------------------------------------
# 13. supprimer de l'environnement les objets temporaires
# ------------------------------------------------
rm(query, con,pourcentage_df, toponymes_valides, surface_drainee_rhone, 
   surface_drainee_med, surface_drainee_corse, surface_drainee_rhin,
   surface_drainee_loire, surface_drainee_seine, surface_drainee_nord,
   surface_drainee_garonne, surface_drainee_adour, surface_drainee_charente,
   surface_drainee_moselle, surface_drainee_meuse, surface_drainee_bretagne,
   meanderbelt_rmc, meanderbelt_rhin,
   meanderbelt_loire, meanderbelt_seine, meanderbelt_nord,
   meanderbelt_garonne, meanderbelt_adour, meanderbelt_charente,
   meanderbelt_moselle, meanderbelt_meuse, meanderbelt_bretagne,
   margins_VB, margins_VB_rmc,
   chenal_forme_rmc, chenal_forme_rhin,
   chenal_forme_loire, chenal_forme_seine, chenal_forme_nord,
   chenal_forme_garonne, chenal_forme_adour, chenal_forme_charente,
   chenal_forme_moselle, chenal_forme_meuse, chenal_forme_bretagne,
   iles_veget_rmc, iles_veget_rhin, iles_veget_loire,
   iles_veget_seine, iles_veget_nord, iles_veget_garonne,
   iles_veget_adour, iles_veget_charente, iles_veget_moselle,
   iles_veget_meuse, iles_veget_bretagne,
   iles_total_rmc, iles_total_rhin, iles_total_loire,
   iles_total_seine, iles_total_nord, iles_total_garonne,
   iles_total_adour, iles_total_charente, iles_total_moselle,
   iles_total_meuse, iles_total_bretagne,
   retenue_rmc, retenue_rhin, retenue_loire,
   retenue_seine, retenue_nord, retenue_garonne,
   retenue_adour, retenue_charente, retenue_moselle,
   retenue_meuse, retenue_bretagne, 
   meanderbelt_axis_rmc, meanderbelt_axis_rhin, meanderbelt_axis_loire,
   meanderbelt_axis_seine, meanderbelt_axis_nord, meanderbelt_axis_garonne,
   meanderbelt_axis_adour, meanderbelt_axis_charente,
   meanderbelt_axis_moselle, meanderbelt_axis_meuse, meanderbelt_axis_bretagne
)




