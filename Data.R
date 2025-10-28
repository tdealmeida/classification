##############################################################
# SCRIPT : Data.R
# OBJECTIF : Importation + filtre des données FCT
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
# 2. Chargement des Données
# ============================================

con<-dbConnect(
  RPostgres::Postgres(),
  dbname = 'mapdoapp',
  host = 'lxc-pgdb-dev.evs.ens-lyon.fr',
  port = '5435',
  user = 'reader',
  password = 'LaGeoCbi1!')

# Import des données de l'Isère
# query <- "SELECT * FROM network_metrics WHERE gid_region = '11'" # Requête SQL pour récupérer les données
# Import des données RMC
# query <- "SELECT * FROM network_metrics WHERE gid_region IN ('11','33','16','31','26')" # pour ajouter le tour de RMC
query <- "SELECT * FROM network_metrics WHERE gid_region IN ('11','33','16','31')" # Requête SQL pour récupérer les données
data <- sf::st_read(dsn = con, query = query) # Lecture des données à partir de la base de données PostgreSQL

# Import des ROE de RMC
query <- "SELECT * FROM roe WHERE gid_region IN ('11','33','16','31','26')" # Requête SQL pour récupérer les données"
roe <- sf::st_read(dsn = con, query = query) # Lecture des données à partir de la base de données PostgreSQL

dbDisconnect(con) # Fermeture de la connexion à la base de données



# ============================================
# 3. Préparation des Données : Filtrage
# ============================================
Data <- data %>%
  mutate(length = as.numeric(st_length(geom))) %>%  # conversion en numeric
  filter(!(is.na(measure_medial_axis) & length < 20))
  # filter(!(is.na(measure_medial_axis)))
  
pourcentage_df <- Data %>% # Sélection des axes avec données (+80% de données valides)
  mutate(is_na_or_zero = is.na(active_channel_width) | active_channel_width == 0) %>%
  group_by(axis) %>%
  summarise(pourcentage_na_ou_zero = mean(is_na_or_zero, na.rm = TRUE) * 100)

toponymes_valides <- pourcentage_df %>% # Filtrage des toponymes avec moins de 80% de NA ou de zéros
  filter(pourcentage_na_ou_zero < 80) %>%
  pull(axis)

# Data <- Data %>%
#   filter(axis %in% toponymes_valides) %>%
#   select(-fid) %>% # Suppression de la colonne 'fid' si elle existe
#   filter(!is.na(valley_bottom_width)) # Bug dans la carte de conitnuité qui oblige de supprimer l'amont des rivières les + à l'est


Data <- Data %>%
  filter(axis %in% toponymes_valides) %>%
  select(-fid) %>%  # Suppression de la colonne 'fid' si elle existe
  arrange(axis, measure)


# supprimer de l'environnement les objets temporaires
rm(query, con,pourcentage_df, toponymes_valides)


st_write(Data, "Data_DGO_filtre.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile






