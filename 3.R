
# =====================================================================
# Chargement des librairies nécessaires
# =====================================================================
library(dplyr)        # Manipulation de données
library(sf)           # Gestion et traitement de données spatiales
library(lwgeom)  # pour st_split
library(qgisprocess)  # Exécution d'algorithmes QGIS depuis R)

# =====================================================================
# Lecture des données sources
# =====================================================================
# Lecture du shapefile contenant les axes médians
# medial_axis_VB <- st_read("MEDIAL_AXIS_SIMPL_DEFAULT.shp") %>%
#   st_as_sf() 

meander_belt_axis <- st_read("meander_belt_axis_Vfinal.gpkg") %>%
  rename(AXIS = axis)  # Harmonisation du nom de la colonne "axis" -> "AXIS"

rupture_points <- Data_points %>%
  ungroup() %>%   # Retirer tout groupement éventuel
  mutate(ID = seq_len(n())) %>%
  rename(AXIS = axis,
         geometry = geom) %>%
  select(ID,ID_segment, toponyme,AXIS,geometry) %>%
  st_as_sf()

# =====================================================================
# SNAP : Alignement des ruptures sur l’axe médian VB
# =====================================================================
# Liste vide pour stocker les résultats intermédiaires (par AXIS)
# rupture_snap_sf_list <- list()
# 
# # Extraction des valeurs uniques d’AXIS
# axes_unique <- unique(rupture_points$AXIS)
# total_axes <- length(axes_unique)
# 
# # Boucle sur chaque valeur d’AXIS
# for(i in seq_along(axes_unique)) {
#   
#   # AXIS courant
#   axis_value <- axes_unique[i]
#   
#   # Sélection des ruptures et axe correspondant
#   rupture_group <- rupture_points %>% filter(AXIS == axis_value)
#   medial_axis_group <- medial_axis_VB %>% filter(AXIS == axis_value)
#   
#   # Message de suivi dans la console
#   cat(sprintf("Traitement de l'AXIS %s (%d sur %d)\n",
#               axis_value, i, total_axes))
#   
#   # Exécution de l'algorithme QGIS : Snap
#   snap_result <- qgis_run_algorithm(
#     "native:snapgeometries",
#     INPUT = rupture_group,              # Géométries à aligner
#     REFERENCE_LAYER = medial_axis_group, # Référence : medial axis
#     TOLERANCE = 10000,                  # Distance de tolérance (en unités projetées)
#     BEHAVIOR = 3                        # Stratégie de snap
#   )
#   
#   # Lecture du résultat et ajout de l’AXIS
#   snap_sf <- st_read(snap_result$OUTPUT) %>%
#     mutate(AXIS = axis_value)
#   
#   # Stockage du résultat dans la liste
#   rupture_snap_sf_list[[as.character(axis_value)]] <- snap_sf
# }
# 
# # Combinaison des résultats individuels en un seul sf
# rupture_snap_VB <- do.call(rbind, rupture_snap_sf_list)




# =====================================================================
# SNAP : Alignement des ruptures sur l’axe médian Meander_belt
# =====================================================================

# Liste vide pour stocker les résultats intermédiaires (par AXIS)
rupture_snap_sf_list <- list()

# Extraction des valeurs uniques d’AXIS
axes_unique <- unique(rupture_points$AXIS)
total_axes <- length(axes_unique)

# Boucle sur chaque valeur d’AXIS
for(i in seq_along(axes_unique)) {
  
  # AXIS courant
  axis_value <- axes_unique[i]
  
  # Sélection des ruptures et axe correspondant
  rupture_group <- rupture_points %>% filter(AXIS == axis_value)
  medial_axis_group <- meander_belt_axis %>% filter(AXIS == axis_value)
  
  # Message de suivi dans la console
  cat(sprintf("Traitement de l'AXIS %s (%d sur %d)\n",
              axis_value, i, total_axes))
  
  # Exécution de l'algorithme QGIS : Snap
  snap_result <- qgis_run_algorithm(
    "native:snapgeometries",
    INPUT = rupture_group,              # Géométries à aligner
    REFERENCE_LAYER = medial_axis_group, # Référence : medial axis
    TOLERANCE = 10000,                  # Distance de tolérance (en unités projetées)
    BEHAVIOR = 3                        # Stratégie de snap
  )
  
  # Lecture du résultat et ajout de l’AXIS
  snap_sf <- st_read(snap_result$OUTPUT) %>%
    mutate(AXIS = axis_value)
  
  # Stockage du résultat dans la liste
  rupture_snap_sf_list[[as.character(axis_value)]] <- snap_sf
}

# Combinaison des résultats individuels en un seul sf
rupture_snap_meander <- do.call(rbind, rupture_snap_sf_list)


# =====================================================================
# Zone tampon autour des points de rupture
# =====================================================================
# rupture_buffer_VB <- st_buffer(rupture_snap_VB, dist = 1)
rupture_buffer_meander <- st_buffer(rupture_snap_meander, dist = 1)

# =====================================================================
# Découpage de l'axe médian VB aux points de rupture
# =====================================================================
# result <- qgis_run_algorithm(
#   "native:splitwithlines",
#   INPUT = medial_axis_VB,        # lignes à couper
#   LINES  = rupture_buffer_VB,   # points de découpe
# )
# medial_axis_segments_sf <- st_read(result$OUTPUT)
# 
# medial_VB <- medial_axis_segments_sf %>%
#   mutate(length_VB = as.numeric(st_length(geom))) %>%
#   filter(length_VB > 10)   # filtrer les segments trop petits

# =====================================================================


result <- qgis_run_algorithm(
  "native:splitwithlines",
  INPUT = meander_belt_axis,        # lignes à couper
  LINES  = rupture_buffer_meander,   # points de découpe
)
medial_axis_segments_sf <- st_read(result$OUTPUT)

medial_meander <- medial_axis_segments_sf %>%
  mutate(length_meander = as.numeric(st_length(geom))) %>%
  filter(length_meander > 10)   # filtrer les segments trop petits


# =====================================================================
# Attribution des segments découpés aux segments homogènes
# =====================================================================
TGH <- st_transform(TGH, 2154)

# medial_VB <- medial_VB %>%
#   rename(axis = AXIS) %>%
#   filter(axis %in% unique(TGH$axis)) %>%
#   group_by(axis) %>%
#   mutate(ID_segment = row_number()) %>%
#   st_drop_geometry()

medial_meander <- medial_meander %>%
  rename(axis = AXIS) %>%
  filter(axis %in% unique(TGH$axis)) %>%
  group_by(axis) %>%
  mutate(ID_segment = row_number()) %>%
  st_drop_geometry() %>%
  # mutate(ID_segment = if_else(axis == 2000803225, dplyr::row_number(dplyr::desc(row_number())), ID_segment)) %>%
  ungroup()
  




TGH_ID <- TGH %>%
  # left_join(medial_VB %>% select(axis, ID_segment, length_VB),
  #           by = c("axis", "ID_segment")) %>%
  left_join(medial_meander %>% select(axis, ID_segment, length_meander),
            by = c("axis", "ID_segment")) 

TGH_ID <- TGH_ID %>%
  mutate(
        # Sinuosity_VB = as.numeric(sum_length) / as.numeric(length_VB),
        Sinuosity_meander = as.numeric(sum_length) / as.numeric(length_meander)
         )

st_write(TGH_ID, "TGH.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile


rm(medial_axis_group, medial_axis_segments_sf, result, rupture_group, rupture_snap_sf_list, 
   snap_result, snap_sf, axes_unique, axis_value, i, total_axes, rupture_buffer_meander, 
   rupture_buffer_VB)



