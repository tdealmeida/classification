##############################################################
# SCRIPT : Metrique.R (Refactorisé)
# OBJECTIF : Importation et calcul des métriques par fonctions
##############################################################

# ============================================
# 0. Chargement des Bibliothèques
# ============================================
library(dplyr)
library(RPostgreSQL)
library(purrr)
library(tidyr)
library(sf)
library(ggplot2)
library(caret)
library(randomForest)
library(units)
library(concaveman)
library(zoo) 

# ============================================
# 1. Métriques de base
# ============================================
calculer_metriques_base <- function(Data) {
  
  metriques_base %>%
    mutate(
      axis  = as.numeric(axis),
      measure_medial_axis = as.numeric(measure_medial_axis),
      AC    = active_channel_width,
      VB    = valley_bottom_width,
      WC    = water_channel_width,
      idx_conf = idx_confinement,
      Slope_talweg = ifelse(talweg_slope < 0, 0.0001, talweg_slope),
      Slope_VB = ifelse(floodplain_slope < 0, 0.0001, floodplain_slope),
      elevation = talweg_elevation_min,
      disconnected_pc_corrige = 100 - water_channel_pc - gravel_bars_pc - riparian_corridor_pc - semi_natural_pc - reversible_pc - built_environment_pc,
      active_channel_pc = water_channel_pc + gravel_bars_pc,
      sum_pc = water_channel_pc + gravel_bars_pc + riparian_corridor_pc + semi_natural_pc + reversible_pc + built_environment_pc + disconnected_pc_corrige,
    )
  
  return(metriques_base)
}

# ============================================
# 2. Drainage Area
# ============================================
calculer_drainage_area <- function(surface_drainee) {
  
    drainage_area <- surface_draine %>%
      rename(measure_medial_axis = measure)
    
  return(drainage_area)
}

# ============================================
# 3. Sinuosité
# ============================================
calculer_sinuosite <- function(Data) {
  # Préparation SF
  Data_sf <- Data %>%
    st_as_sf() %>%
    mutate(
      geom = st_sfc(purrr::map(geom, ~ {
        coords <- st_coordinates(.x)
        st_point(coords[nrow(coords), 1:2])
      }), crs = st_crs(.))
    ) %>%
    st_zm(drop = TRUE, what = "ZM") %>%
    st_transform(2154) %>%
    mutate(axis = as.numeric(axis)) # Assurer le type
  
  SIN <- Data_sf %>%
    group_by(axis) %>%
    group_modify(~ {
      if (nrow(.x) >= 3) {
        helper_calculate_angles_sf(.x)
      } else {
        .x %>% mutate(angle_rad = NA_real_, angle_deg = NA_real_)
      }
    }) %>%
    ungroup() %>%
    select(axis, measure, angle_deg) %>%
    st_drop_geometry()
  
  return(SIN)
}

# ============================================
# 4. Enveloppe de méandrage
# ============================================
calculer_enveloppe <- function(meander_belt) {

  meander_belt <- meander_belt %>%
    rename(axis = AXIS_2,
           measure_medial_axis = M,
           meander_belt = enveloppe) 

  return(enveloppe_meandrage)
}

# ============================================
# 5. Multi-chenal (Random Forest)
# ============================================
calculer_multichenal <- function(chenal_prop, DGO_label) {
  # 1. Préparation données
  chenal_labels <- DGO_label %>%
    rename(geom = geometry) %>%
    st_as_sf(sf_column_name = "geom") %>%
    select(AXIS, M, multi) %>%
    group_by(AXIS, M) %>%
    slice(1) %>%
    ungroup()
  
  # 2. Préparation Modèle
  chenal_complet <- chenal_forme_total %>%
    left_join(chenal_labels, by = c("AXIS", "M"))
  
  colonnes_a_exclure <- c("AXIS", "M", "geom")
  
  donnees_connues <- chenal_complet %>%
    st_drop_geometry() %>%
    filter(multi %in% c(0, 1)) %>%
    filter(rowSums(!is.na(select(., -all_of(c("multi", colonnes_a_exclure))))) > 0) %>%
    select(-all_of(colonnes_a_exclure))
  
  set.seed(123)
  
  # Entraînement complet (pas de séparation train/test ici pour le code de prod, on utilise tout pour prédire)
  donnees_connues$multi <- as.factor(donnees_connues$multi)
  
  modele_foret <- randomForest(
    multi ~ ., 
    data = donnees_connues,
    na.action = na.roughfix,
    importance = FALSE # Pas besoin pour la prod
  )
  
  # 3. Prédiction
  donnees_propres <- chenal_complet %>%
    select(-all_of(colonnes_a_exclure)) %>%
    st_drop_geometry()
  
  predictions_complet <- predict(modele_foret, newdata = donnees_propres)
  
  chenal_resultats <- chenal_complet %>%
    mutate(multi_chenaux = predictions_complet) %>%
    select(AXIS, M, multi_chenaux) %>%
    st_drop_geometry()
  
  return(chenal_resultats)
}

# ============================================
# 6. Distance à un barrage
# ============================================
# calculer_distance_barrage <- function(Data, roe) {
#   
#   # Filtrer les barrages dans le référentiel ROE fourni
#   barrage <- roe %>%
#     filter(lbtypeouvr %in% c("Barrage",
#                              "Barrage mobile",
#                              "Barrage en remblais",
#                              "Barrage poids",
#                              "Barrage poids voûte",
#                              "Barrage voûte",
#                              "Barrage à voûtes multiple",
#                              "Autre sous-type de barrage",
#                              "Barrage à contreforts")) %>%
#     select(gid, axis, distance_axis)%>%
#     rename(measure = distance_axis)
#   
#   # Fonction de calcul locale
#   calcul_distance_local <- function(discon, points, colname) {
#     colname_sym <- rlang::sym(colname)
#     
#     discon %>%
#       dplyr::rowwise() %>%
#       dplyr::mutate(
#         # distance au ROE aval (donc roe.measure <= metrique.measure)
#         aval = {
#           c <- points$measure[points$axis == axis & points$measure <= measure]
#           if (length(c)) min(measure - c) else NA_real_
#         },
#         # on ignore l’amont
#         .distance = aval
#       ) %>%
#       dplyr::ungroup() %>%
#       dplyr::select(-aval) %>%
#       dplyr::rename(!!colname_sym := .distance)
#   }
#   
#   dist_barrage <- calcul_distance_local(discon = Data, points = barrage, colname = "roe")
#   
#   return(dist_barrage)
# }

# ============================================
# 7. Présence/absence d'îles végétalisées
# ============================================
calculer_iles <- function(Data, iles_vegetalise) {
  
  iles_veget <- iles_vegetalise %>%
    rename(axis = AXIS, measure_medial_axis = M) %>%
    distinct() 
  
  return(iles_veget)
}

# ============================================
# 8. Présence/absence de retenue
# ============================================
calculer_retenue <- function(retenue) {
  
  retenue <- retenue %>%
    rename(axis = AXIS, measure_medial_axis = M) %>%
    distinct() 

  return(retenue)
}

# # ============================================
# # 9. Amplitude (sinuosité)
# # ============================================
# calculer_amplitude <- function(Data, amplitude) {
#   ampli <- amplitude %>%
#     st_drop_geometry() %>%
#     select(axis, measure, amplitude)
#   
#   res <- Data %>% st_drop_geometry() %>%
#     select(axis, measure) %>%
#     mutate(axis = as.numeric(axis)) %>%
#     inner_join(ampli, by = c("axis", "measure"))
#   
#   return(res)
# }


# ============================================
# 10. Stream Power
# ============================================
# calculer_stream_power<- function(Data) {
#   
#   # 1. Obtenir les données de base nécessaires (Pente, AC)
#   base_info <- Data %>%
#     st_drop_geometry() %>%
#     select(axis, measure, measure_medial_axis, active_channel_width, talweg_slope) %>%
#     mutate(
#       axis = as.numeric(axis),
#       measure_medial_axis = as.numeric(measure_medial_axis),
#       AC = active_channel_width,
#       # Même logique que base :
#       Slope_talweg = ifelse(talweg_slope < 0, 0.0001, talweg_slope)
#     )
#   
#   # 2. Charger Drainage Area (indépendamment)
#   drainage <- helper_load_drainage_data()
#   
#   # 3. Charger Bassin Stream Params
#   bassin <- st_read("bassin_stream.gpkg", quiet = TRUE) %>% st_drop_geometry()
#   
#   # 4. Jointures
#   df_calc <- base_info %>%
#     left_join(drainage %>% select(axis, measure_medial_axis, drainage_area), 
#               by = c("axis", "measure_medial_axis")) %>%
#     left_join(bassin, by = "axis")
#   
#   # 5. Calculs
#   df_calc <- df_calc %>%
#     mutate(
#       ACW_star = AC / (drainage_area^0.44),
#       Q = A * (drainage_area^B),
#       pente_mm = Slope_talweg / 100
#     ) %>%
#     group_by(axis) %>%
#     mutate(
#       pente_m_moy = rollapply(pente_mm, width = 5, FUN = mean, align = "center", fill = NA),
#       pente_m_moy = ifelse(is.na(pente_m_moy), pente_mm, pente_m_moy)
#     ) %>%
#     ungroup() %>%
#     mutate(
#       stream_power = ((9800 * Q * pente_mm) / AC),
#       stream_power_mm = ((9800 * Q * pente_m_moy) / AC)
#     )
#   
#   return(df_calc %>% select(axis, measure, ACW_star, Q, stream_power, stream_power_mm))
# }





# ============================================
# 3. Calcul des données
# ============================================
# Métriques de base
df_base <- calculer_metriques_base(Data)

# Drainage Area
df_drainage <- calculer_drainage_area(surface_drainee)

# Sinuosité
df_sin <- calculer_sinuosite(Data)

# Enveloppe de méandrage
df_env <- calculer_meander_belt(meander_belt)

# Multi-chenal
df_multi <- calculer_multichenal(chenal_forme, DGO_label)

# Distance à un barrage
df_barrage <- calculer_distance_barrage(Data, roe)

# Présence/absence d'îles végétalisées
df_iles <- calculer_iles(iles_veget)

# Présence/absence de retenue
df_retenue <- calculer_retenue(retenue)

# Amplitude (sinuosité)
# df_ampli <- calculer_amplitude(Data_input)

# Stream Power
# df_sp <- calculer_stream_power(Data_input)
  
# ============================================
# 4. Jointures successives sur df_base
# ============================================
metrique <- df_base %>%
  
  left_join(
    df_drainage %>% select(axis, measure_medial_axis, drainage_area),
    by = c("axis", "measure_medial_axis")
  ) %>%
  
  left_join(
    df_sin %>% select(axis, measure, angle_deg),
    by = c("axis", "measure")
  ) %>%
  
  left_join(
    df_env,
    by = c("axis", "measure_medial_axis")
  ) %>%
  
  left_join(
    df_multi %>% select(AXIS, M, multi_chenaux),
    by = c("axis" = "AXIS", "measure_medial_axis" = "M")
  ) %>%
  mutate(
    multi_chenaux = as.numeric(multi_chenaux),
    multi_chenaux = ifelse(is.na(multi_chenaux), 1, multi_chenaux)
  ) %>%
  
  # left_join(df_barrage, by = c("axis", "measure")) %>%   # ← OK de commenter comme ça
  
  left_join(
    df_iles %>% mutate(ile_vege = 2),
    by = c("axis", "measure_medial_axis")
  ) %>%
  mutate(
    ile_vege = ifelse(is.na(ile_vege), 1, ile_vege)
  ) %>%
  
  left_join(
    df_retenue %>% mutate(reservoir = 2),
    by = c("axis", "measure_medial_axis")
  ) %>%
  mutate(
    reservoir = ifelse(is.na(reservoir), 1, reservoir)
  )

# %>%
  # inner_join(df_ampli, by = c("axis", "measure")) %>% # inner_join comme dans l'original
# %>%
  # left_join(df_sp, by = c("axis", "measure"))



# ============================================
# 4. Interpolations et auxtres métriques
# ============================================
metrique <- metrique %>%
  group_by(axis) %>%
  mutate(across(where(is.numeric) & !matches("measure_medial_axis"),
                ~ na.approx(.x, na.rm = FALSE))) %>%
  filter(!is.na(AC)) %>%
  ungroup()
  
# Idx_eau
metrique <- metrique %>%
  mutate(
    idx_water = case_when(
      AC == 0 & WC == 0 ~ -1,    # pas de données → ignorer pour les moyennes
      WC == 0 & AC > 0 ~ 0,      # vrai 0
      TRUE ~ WC / AC             # ratio normal
    )
  )

# step AC
metrique <- metrique %>%
  group_by(axis) %>%
  # arrange(measure) %>%
  mutate(Delta_AC = lag(AC) - AC) %>%
  ungroup()

# ACW*
metrique <- metrique %>%
  mutate(ACW_star = AC / (drainage_area^0.44))

