
library(qgisprocess)  # Exécution d'algorithmes QGIS depuis R)

meander_belt_axis <- st_read("meander_belt_axis_Vfinal.gpkg") 

vertices <- st_cast(metrique, "POINT") %>%
  st_transform(2154) %>%
  mutate(ID = row_number())


# =====================================================================
# SNAP : Alignement des ruptures sur l’axe médian Meander_belt
# =====================================================================

# Liste vide pour stocker les résultats intermédiaires (par AXIS)
rupture_snap_sf_list <- list()

# Extraction des valeurs uniques d’AXIS
axes_unique <- unique(vertices$axis)
total_axes <- length(axes_unique)

# Boucle sur chaque valeur d’AXIS
for(i in seq_along(axes_unique)) {
  
  # AXIS courant
  axis_value <- axes_unique[i]
  
  # Sélection des ruptures et axe correspondant
  rupture_group <- vertices %>% filter(axis == axis_value)
  medial_axis_group <- meander_belt_axis %>% filter(axis == axis_value)
  
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
    mutate(axis = axis_value)
  
  # Stockage du résultat dans la liste
  rupture_snap_sf_list[[as.character(axis_value)]] <- snap_sf
}

# Combinaison des résultats individuels en un seul sf
rupture_snap_meander <- do.call(rbind, rupture_snap_sf_list)




# --- Étape 1 : joindre sans géométrie ---
df1 <- st_drop_geometry(vertices)
df2 <- st_drop_geometry(rupture_snap_meander)

joined <- df1 %>%
  inner_join(df2, by = "ID", suffix = c("_1", "_2"))

# --- Étape 2 : rattacher les géométries ---
joined$geometry_1 <- vertices$geom[match(joined$ID, vertices$ID)]
joined$geometry_2 <- rupture_snap_meander$geom[match(joined$ID, rupture_snap_meander$ID)]

joined$distance <- as.numeric(st_distance(joined$geometry_1, joined$geometry_2, by_element = TRUE))



df_summary <- joined %>%
  group_by(axis_1, measure_1) %>%      # regroupe par axis et measure
  summarise(mean_distance = mean(distance, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(axis = axis_1, measure = measure_1) 

metrique <- metrique %>%
  left_join(df_summary %>% 
              select(axis, measure, mean_distance), by = c("axis", "measure"))



