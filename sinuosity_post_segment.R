
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

meander_belt_axis <- st_read("meanderbelt_RMC.gpkg") 

rupture_points <- Data_points %>%
  ungroup() %>%   # Retirer tout groupement éventuel
  mutate(ID = seq_len(n())) %>%
  select(ID,ID_segment, toponyme,axis,geom) %>%
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
#     BEHAVIOR = 1                        # Stratégie de snap
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
axes_unique <- unique(rupture_points$axis)
total_axes <- length(axes_unique)

# Boucle sur chaque valeur d’AXIS
for(i in seq_along(axes_unique)) {
  
  # AXIS courant
  axis_value <- axes_unique[i]
  
  # Sélection des ruptures et axe correspondant
  rupture_group <- rupture_points %>% filter(axis == axis_value)
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
    BEHAVIOR = 1                        # Stratégie de snap
  )
  
  # Lecture du résultat et ajout de l’AXIS
  snap_sf <- st_read(snap_result$OUTPUT) %>%
    mutate(axis = axis_value)
  
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
  filter(axis %in% unique(TGH$axis)) %>%
  group_by(axis) %>%
  # mutate(ID_segment = row_number()) %>%
  mutate(ID_segment = n() - row_number() + 1) %>%
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





# TGH_0.4_ampl <- TGH_ID 
# st_write(TGH_0.4_ampl, "TGH_04_ampl.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile

st_write(TGH_ID, "TGH.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile



rm(medial_axis_group, medial_axis_segments_sf, result, rupture_group, rupture_snap_sf_list, 
   snap_result, snap_sf, axes_unique, axis_value, i, total_axes, rupture_buffer_meander, 
   rupture_buffer_VB)





TGH_classif <- TGH_ID %>%
  mutate(
    noeud1 = case_when(
      mean_AC > 4 ~ "Lit",
      mean_AC <= 4x   ~ "Pas de Lit"
    ),
    noeud2 = case_when(
      noeud1 == "Lit" & mean_WC > 4 ~ "Lit en eau",
      noeud1 == "Lit" & mean_WC <= 4 ~ "Lit sans eau",
      TRUE ~ noeud1
    ),
    noeud3 = case_when(
      noeud2 == "Lit en eau" & retenue > 0.4 ~ "Retenue",
      noeud2 == "Lit en eau" & retenue <= 0.4 ~ "rivière",
      noeud2 == "Lit sans eau" & mean_ACW_star < 5.5 ~ "Tresse intermittent",
      noeud2 == "Lit sans eau" & mean_ACW_star <= 5.5 ~ "Intermittent",
      TRUE ~ noeud2
    ),
    noeud4 = case_when(
      noeud3 == "Lit en eau" & mean_idx_water > 0.95 ~ "Lit à banc",
      noeud3 == "Lit en eau" & mean_idx_water <= 0.95 ~ "Lit sans bancs",
      TRUE ~ noeud3
    ),
    noeud5 = case_when(
      noeud4 == "Lit à banc" & mean_ACW_star > 5.5 & mean_idx_water > 0.25 & mean_ACW_star < 30 ~ "Tresse",
      noeud4 == "Lit à banc" & mean_ACW_star <= 5.5 & mean_idx_water >= 0.25 & mean_ACW_star >= 30 ~ "Non tresse",
      noeud4 == "Lit sans bancs" & iles_veget < 0.5 ~ "Multi à ilots boisés",
      noeud4 == "Lit sans bancs" & iles_veget <= 0.5 ~ "Chenal unique",
      TRUE ~ noeud4
    ),
    noeud6 = case_when(
      noeud5 == "Tresse" & iles_veget > 0.4 ~ "Tresse végétalisée",
      noeud5 == "Tresse" & iles_veget <= 0.4 ~ "Tresse",
      noeud5 == "Non tresse" & mean_ACW_star < 3 & mean_ACW_star < 15 ~ "Divagant",
      noeud5 == "Non tresse" & mean_ACW_star <= 3 & mean_ACW_star > 15 ~ "Bancs",
      noeud5 == "Chenal unique" & Sinuosity_meander < 1.25 ~ "Méandre passive",
      noeud5 == "Chenal unique" & Sinuosity_meander <= 1.25 ~ "Non méandre",
      TRUE ~ noeud5
    ),
    noeud7 = case_when(
      noeud6 == "Bancs" & Sinuosity_meander > 1.25 ~ "Sinueux à bancs",
      noeud6 == "Bancs" & Sinuosity_meander <= 1.25 ~ "Rectligne à bancs",
      noeud6 == "Non méandre" & iles_veget < 1.25 ~ "Sinueux",
      noeud6 == "Non méandre" & iles_veget <= 1.25 ~ "Rectiligne",
      TRUE ~ noeud6
  )
  )




