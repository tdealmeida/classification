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
query <- "SELECT * FROM roe WHERE gid_region IN ('11','33','16','31','26')" # pour RMC
# query <- "SELECT * FROM roe" # pour France
roe <- sf::st_read(dsn = con, query = query) 
    
dbDisconnect(con) # Fermeture de la connexion à la base de données

# ------------------------------------------------
# 4. Filtrage des petits DGO NA (<20m) inter DGO
# ------------------------------------------------
Data <- data %>%
  mutate(length = as.numeric(st_length(geom))) %>%  
  filter(!(is.na(measure_medial_axis) & length < 20))

# ------------------------------------------------
# 5. Calcul du % de NA ou 0 par axe (sans géométrie)
# ------------------------------------------------
pourcentage_df <- Data %>%
  st_drop_geometry() %>%           # ⬅️ énorme gain de perf
  group_by(axis) %>%
  summarise(
    pourcentage_na_ou_zero =
      mean(is.na(active_channel_width) | active_channel_width == 0) * 100,
    .groups = "drop"
  )

# ------------------------------------------------
# 6. Sélection des axes valides
# ------------------------------------------------
toponymes_valides <- pourcentage_df$axis[
  pourcentage_df$pourcentage_na_ou_zero < 80
]

# ------------------------------------------------
# 7. Filtrage final (sans %in%)
# ------------------------------------------------
Data <- Data %>%
  semi_join(
    tibble(axis = toponymes_valides),
    by = "axis"
  ) %>%
  select(-fid) %>%
  left_join(pourcentage_df %>% select(axis, pourcentage_na_ou_zero),
            by = "axis") %>%
  arrange(axis, measure)

# ------------------------------------------------
# 7. Export des données
# ------------------------------------------------
st_write(Data, "Data_DGO_filtre.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile
write.csv(Data, "Data_DGO_filtre.csv", row.names = FALSE) # Export des données nettoyées en shapefile


# ------------------------------------------------
# 7. Import des surfaces drainées par bassin
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

# ------------------------------------------------
# 7. Import des meander belt par bassin
# ------------------------------------------------
meanderbelt_rmc <- st_read("Data/RMC/meanderbelt_RMC.gpkg") %>%
  st_drop_geometry() %>%
  select(M, AXIS_2, enveloppe)

meanderbelt_rhin <- st_read("Data/Rhin/meanderbelt_rhin.gpkg") %>%
  st_drop_geometry() %>%
  select(M, AXIS_2, enveloppe) 

meanderbelt_loire <- st_read("Data/Loire/meanderbelt_loire.gpkg") %>%
  st_drop_geometry() %>%
  select(M, AXIS_2, enveloppe) 

meanderbelt_seine <- st_read("Data/Seine/meanderbelt_seine.gpkg") %>%
  st_drop_geometry() %>%
  select(M, AXIS_2, enveloppe) 

meanderbelt_nord <- st_read("Data/Nord/meanderbelt_nords.gpkg") %>%
  st_drop_geometry() %>%
  select(M, AXIS_2, enveloppe) 

meanderbelt_garonne <- st_read("Data/Garonne/meanderbelt_garonne.gpkg") %>%
  st_drop_geometry() %>%
  select(M, AXIS_2, enveloppe) 

meanderbelt_adour <- st_read("Data/Adour/meanderbelt_adour.gpkg") %>%
  st_drop_geometry() %>%
  select(M, AXIS_2, enveloppe) 

meanderbelt_charente <- st_read("Data/Charente/meanderbelt_charente.gpkg") %>%
  st_drop_geometry() %>%
  select(M, AXIS_2, enveloppe) 

meanderbelt_moselle <- st_read("Data/Moselle/meanderbelt_moselle.gpkg") %>%
  st_drop_geometry() %>%
  select(M, AXIS_2, enveloppe) 

meanderbelt_meuse <- st_read("Data/Meuse/meanderbelt_meuse.gpkg") %>%
  st_drop_geometry() %>%
  select(M, AXIS_2, enveloppe) 

meanderbelt_bretagne <- st_read("Data/Bretagne/meanderbelt_bretagne.gpkg") %>%
  st_drop_geometry() %>%
  select(M, AXIS_2, enveloppe)

mmeander_belt <- rbind(meanderbelt_rmc,
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
# 7. Import des chenal forms par bassin
# ------------------------------------------------
chenal_forme_rhone <- read.csv("Data/RMC/chenal_props_rmc.csv")
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

chenal_forme <- rbind(chenal_forme_rhone,
                      chenal_forme_med,
                      chenal_forme_corse,
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
# 7. Import des DGO label
# ------------------------------------------------
chenal_labels <- st_read("DGO_label.shp") %>%
  rename(geom = geometry) %>%
  st_as_sf(sf_column_name = "geom") %>%
  select(AXIS, M, multi) %>%
  group_by(AXIS, M) %>%
  slice(1) %>%        # garder une seule ligne par couple (AXIS, M)
  ungroup()

# ------------------------------------------------
# 7. Import des îles végétalisées par bassin
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
# 7. Import des retenues par bassin
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

retenue <- rbind(retenue_rmc,
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
# 7. supprimer de l'environnement les objets temporaires
# ------------------------------------------------
rm(query, con,pourcentage_df, toponymes_valides, surface_drainee_rhone, 
   surface_drainee_med, surface_drainee_corse, surface_drainee_rhin,
   surface_drainee_loire, surface_drainee_seine, surface_drainee_nord,
   surface_drainee_garonne, surface_drainee_adour, surface_drainee_charente,
   surface_drainee_moselle, surface_drainee_meuse, surface_drainee_bretagne,
   meander_belt_rhone, meander_belt_med, meander_belt_corse, meander_belt_rhin,
   meander_belt_loire, meander_belt_seine, meander_belt_nord,
   meander_belt_garonne, meander_belt_adour, meander_belt_charente,
   meander_belt_moselle, meander_belt_meuse, meander_belt_bretagne,
   chenal_forme_rhone, chenal_forme_med, chenal_forme_corse, chenal_forme_rhin,
   chenal_forme_loire, chenal_forme_seine, chenal_forme_nord,
   chenal_forme_garonne, chenal_forme_adour, chenal_forme_charente,
   chenal_forme_moselle, chenal_forme_meuse, chenal_forme_bretagne,
   iles_veget_rmc, iles_veget_rhin, iles_veget_loire,
   iles_veget_seine, iles_veget_nord, iles_veget_garonne,
   iles_veget_adour, iles_veget_charente, iles_veget_moselle,
   iles_veget_meuse, iles_veget_bretagne,
   retenue_rmc, retenue_rhin, retenue_loire,
   retenue_seine, retenue_nord, retenue_garonne,
   retenue_adour, retenue_charente, retenue_moselle,
   retenue_meuse, retenue_bretagne
)








# # ============================================
# # 3. Préparation des Données : Filtrage
# # ============================================
# Data <- data %>%
#   mutate(length = as.numeric(st_length(geom))) %>%  # conversion en numeric
#   filter(!(is.na(measure_medial_axis) & length < 20))   # suppression des segments <20m sans données
# # filter(!(is.na(measure_medial_axis)))
# 
# pourcentage_df <- Data %>% # Sélection des axes avec données (+80% de données valides)
#   mutate(is_na_or_zero = is.na(active_channel_width) | active_channel_width == 0) %>%
#   group_by(axis) %>%
#   summarise(pourcentage_na_ou_zero = mean(is_na_or_zero, na.rm = TRUE) * 100)
# 
# # st_write(pourcentage_df, "pourcentage_na_ou_zero.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile
# 
# toponymes_valides <- pourcentage_df %>% # Filtrage des toponymes avec moins de 80% de NA ou de zéros
#   filter(pourcentage_na_ou_zero < 80) %>%
#   pull(axis)
# 
# # Data <- Data %>%
# #   filter(axis %in% toponymes_valides) %>%
# #   select(-fid) %>% # Suppression de la colonne 'fid' si elle existe
# #   filter(!is.na(valley_bottom_width)) # Bug dans la carte de conitnuité qui oblige de supprimer l'amont des rivières les + à l'est
# 
# 
# Data <- Data %>%
#   filter(axis %in% toponymes_valides) %>%
#   select(-fid) %>%  # Suppression de la colonne 'fid' si elle existe
#   arrange(axis, measure)









