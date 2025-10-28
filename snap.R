# =====================================================================
# Chargement des librairies nécessaires
# =====================================================================
library(dplyr)        # Manipulation de données
library(qgisprocess)  # Exécution d'algorithmes QGIS depuis R
library(sf)           # Gestion et traitement de données spatiales

# =====================================================================
# Lecture des données sources
# =====================================================================
# Lecture du shapefile contenant les axes médians
medial_axis_VB <- st_read("MEDIAL_AXIS_SIMPL_DEFAULT.shp")
meander_belt_axis <- st_read("meander_belt_axis_param_base.gpkg") %>%
  rename(AXIS = axis)  # Harmonisation du nom de la colonne "axis" -> "AXIS"

# Lecture des midpoints (géopackage)
midpoints <- st_read("te.gpkg") %>%
  rename(AXIS = axis)  # Harmonisation du nom de la colonne "axis" -> "AXIS"
  # select(ID_segment, toponyme,AXIS,geom)
  # mutate(ID = seq_len(n()))

# st_write(midpoints, "midpoints.gpkg", delete_layer = TRUE) # Export des midpoints avec ID



rupture <- Data_points %>%
  ungroup() %>%   # Retirer tout groupement éventuel
  mutate(ID = seq_len(n())) %>%
  rename(AXIS = axis) %>%
  select(ID,ID_segment, toponyme,AXIS,geom)
  
# =====================================================================
# SNAP : Alignement des ruptures sur l’axe médian VB
# =====================================================================

# Liste vide pour stocker les résultats intermédiaires (par AXIS)
rupture_snap_sf_list <- list()
st_crs(rupture) 
# Extraction des valeurs uniques d’AXIS
axes_unique <- unique(rupture$AXIS)
total_axes <- length(axes_unique)

# Boucle sur chaque valeur d’AXIS
for(i in seq_along(axes_unique)) {
  
  # AXIS courant
  axis_value <- axes_unique[i]
  
  # Sélection des ruptures et axe correspondant
  rupture_group <- rupture %>% filter(AXIS == axis_value)
  medial_axis_group <- medial_axis_VB %>% filter(AXIS == axis_value)
  
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
rupture_snap_final <- do.call(rbind, rupture_snap_sf_list)

# Export au format GeoPackage
st_write(rupture_snap_final, "rupture_snap_axisVB.gpkg", delete_layer = TRUE)

# =====================================================================
# SNAP : Alignement des midpoints sur l’axe médian VB
# =====================================================================

# Liste vide pour stocker les résultats intermédiaires (par AXIS)
midpoint_snap_sf_list <- list()

# Extraction des valeurs uniques d’AXIS
axes_unique <- unique(midpoints$AXIS)
total_axes <- length(axes_unique)

# Boucle sur chaque valeur d’AXIS
for(i in seq_along(axes_unique)) {
  
  # AXIS courant
  axis_value <- axes_unique[i]
  
  # Sélection des midpoints et axe correspondant
  midpoint_group <- midpoints %>% filter(AXIS == axis_value)
  medial_axis_group <- medial_axis_VB %>% filter(AXIS == axis_value)
  
  # Message de suivi
  cat(sprintf("Traitement de l'AXIS %s (%d sur %d)\n",
              axis_value, i, total_axes))
  
  # Exécution de l'algorithme QGIS : Snap
  snap_result <- qgis_run_algorithm(
    "native:snapgeometries",
    INPUT = midpoint_group,              # Géométries à aligner
    REFERENCE_LAYER = medial_axis_group, # Référence : medial axis
    TOLERANCE = 10000,                   # Distance de tolérance
    BEHAVIOR = 3                         # Stratégie de snap
  )
  
  # Lecture du résultat et ajout de l’AXIS
  snap_sf <- st_read(snap_result$OUTPUT) %>%
    mutate(AXIS = axis_value)
  
  # Stockage du résultat dans la liste
  midpoint_snap_sf_list[[as.character(axis_value)]] <- snap_sf
}

# Combinaison des résultats en un seul sf
midpoint_snap_final <- do.call(rbind, midpoint_snap_sf_list)

# Export au format GeoPackage
st_write(midpoint_snap_final, "midpoint_snap.gpkg", delete_layer = TRUE)





# =====================================================================
# SNAP : Alignement des ruptures sur l’axe médian Meander_belt
# =====================================================================

# Liste vide pour stocker les résultats intermédiaires (par AXIS)
rupture_snap_sf_list <- list()

# Extraction des valeurs uniques d’AXIS
axes_unique <- unique(rupture$AXIS)
total_axes <- length(axes_unique)

# Boucle sur chaque valeur d’AXIS
for(i in seq_along(axes_unique)) {
  
  # AXIS courant
  axis_value <- axes_unique[i]
  
  # Sélection des ruptures et axe correspondant
  rupture_group <- rupture %>% filter(AXIS == axis_value)
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
rupture_snap_final <- do.call(rbind, rupture_snap_sf_list)

# Export au format GeoPackage
st_write(rupture_snap_final, "rupture_snap_axis_meanderbelt.gpkg", delete_layer = TRUE)

# =====================================================================
# SNAP : Alignement des midpoints sur l’axe médian VB
# =====================================================================

# Liste vide pour stocker les résultats intermédiaires (par AXIS)
midpoint_snap_sf_list <- list()

# Extraction des valeurs uniques d’AXIS
axes_unique <- unique(midpoints$AXIS)
total_axes <- length(axes_unique)

# Boucle sur chaque valeur d’AXIS
for(i in seq_along(axes_unique)) {
  
  # AXIS courant
  axis_value <- axes_unique[i]
  
  # Sélection des midpoints et axe correspondant
  midpoint_group <- midpoints %>% filter(AXIS == axis_value)
  medial_axis_group <- meander_belt_axis %>% filter(AXIS == axis_value)
  
  # Message de suivi
  cat(sprintf("Traitement de l'AXIS %s (%d sur %d)\n",
              axis_value, i, total_axes))
  
  # Exécution de l'algorithme QGIS : Snap
  snap_result <- qgis_run_algorithm(
    "native:snapgeometries",
    INPUT = midpoint_group,              # Géométries à aligner
    REFERENCE_LAYER = medial_axis_group, # Référence : medial axis
    TOLERANCE = 10000,                   # Distance de tolérance
    BEHAVIOR = 3                         # Stratégie de snap
  )
  
  # Lecture du résultat et ajout de l’AXIS
  snap_sf <- st_read(snap_result$OUTPUT) %>%
    mutate(AXIS = axis_value)
  
  # Stockage du résultat dans la liste
  midpoint_snap_sf_list[[as.character(axis_value)]] <- snap_sf
}

# Combinaison des résultats en un seul sf
midpoint_snap_final <- do.call(rbind, midpoint_snap_sf_list)

# Export au format GeoPackage
st_write(midpoint_snap_final, "midpoint_snap_axis_meanderbelt.gpkg", delete_layer = TRUE)
