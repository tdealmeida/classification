##############################################################
# SCRIPT : Metrique.R
# OBJECTIF : Importation et calcul des métriques 
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
library(caret)
library(randomForest)
library(units)
library(concaveman)

# options(scipen = 999)
# options(scipen = 0)

# Data$axis <- as.numeric(Data$axis)
Data <- st_read("Data_DGO.gpkg")

# ============================================
# 2. Génération d'un tableau 
# ============================================
metrique <- Data %>%
  # select(toponyme,axis,measure,measure_medial_axis,strahler)%>%
  mutate(axis = as.numeric(axis),
         measure_medial_axis = as.numeric(measure_medial_axis),
         disconnected_pc_corrige = 100 - water_channel_pc - gravel_bars_pc - riparian_corridor_pc - semi_natural_pc - reversible_pc - built_environment_pc,
         active_channel_pc = water_channel_pc + gravel_bars_pc,
         sum_pc = water_channel_pc + gravel_bars_pc + riparian_corridor_pc + semi_natural_pc + reversible_pc + built_environment_pc + disconnected_pc_corrige
         )



# ============================================
# 3. AC
# ============================================
metrique <- metrique %>%
  mutate(AC = Data$active_channel_width)

# ============================================
# 4. VB
# ============================================
metrique <- metrique %>%
  mutate(VB = Data$valley_bottom_width)

# ============================================
# 5. Idx_confi
# ============================================
metrique <- metrique %>%
  mutate(idx_conf = Data$idx_confinement)

# ============================================
# 6. WC
# ============================================
metrique <- metrique %>%
  mutate(WC = Data$water_channel_width)


# ============================================
# 6. Slope Talweg
# ============================================
metrique <- metrique %>%
  mutate(Slope_talweg = ifelse(Data$talweg_slope < 0, 0.0001, Data$talweg_slope))

# ============================================
# 6. slope_VB
# ============================================
metrique <- metrique %>%
  mutate(Slope_VB = Data$floodplain_slope)

# ============================================
# 6. elevation
# ============================================
metrique <- metrique %>%
  mutate(elevation = Data$talweg_elevation_min)



# ============================================
# 9. Surface drainée
# ============================================
# Import du fichier CSV contenant les surfaces drainées
surface_drainee_rhone <- read.csv("DRANAGE_AREA_rhone.csv")
surface_drainee_med <- read.csv("DRAINAGE_AREA_med.csv")
surface_drainee_corse <- read.csv("DRAINAGE_AREA_corse.csv")

surface_drainee_totale <- rbind(surface_drainee_rhone,
                                surface_drainee_med,
                                surface_drainee_corse)

# Renommer la colonne "measure" en "measure_medial_axis" pour la jointure
surface_drainee <- surface_drainee_totale %>% 
  rename(measure_medial_axis = measure) 

# Jointure avec le tableau metrique pour ajouter la surface drainée
metrique <- metrique %>%
  left_join(
    surface_drainee %>% select(axis, measure_medial_axis, drainage_area),
    by = c("axis", "measure_medial_axis")
  )


# ============================================
# 11. Sinuosité
# ============================================
calculate_angles_sf <- function(points_sf) {
  coords <- sf::st_coordinates(points_sf)
  if (nrow(coords) < 3) {
    stop("At least 3 points are necessary to calculate angles")
  }
  angles <- sapply(2:(nrow(coords) - 1), function(i) {
    p1 <- coords[i - 1, ]
    p2 <- coords[i, ]
    p3 <- coords[i + 1, ]
    v1 <- p1 - p2
    v2 <- p3 - p2
    dot_product <- sum(v1 * v2)
    norm_v1 <- sqrt(sum(v1^2))
    norm_v2 <- sqrt(sum(v2^2))
    angle <- acos(dot_product / (norm_v1 * norm_v2))
    return(angle)
  })
  
  # Crée un vecteur complet avec NA pour les premiers/derniers points
  angle_full <- rep(NA, nrow(coords))
  angle_full[2:(nrow(coords)-1)] <- angles
  
  angle_sf <- points_sf
  angle_sf$angle_rad <- abs(angle_full - pi) # écart à un angle plat
  angle_sf$angle_deg <- angle_sf$angle_rad * 180 / pi # conversion en degrés
  
  return(angle_sf)
}

Data_sf <- metrique %>%
  st_as_sf()

# 2. Extraction du point aval pour chaque ligne
Data_sf <- Data_sf %>%
  mutate(
    geom = st_sfc(map(geom, ~ {
      coords <- st_coordinates(.x)
      st_point(coords[nrow(coords), 1:2])
    }), crs = st_crs(.))
  ) %>%
  mutate(geom = st_zm(geom, drop = TRUE, what = "ZM")) %>%
  st_as_sf()

# 3. Reprojection
Data_sf_proj <- st_transform(Data_sf, 2154)

SIN <- Data_sf_proj %>%
  group_by(axis) %>%
  # arrange(measure) %>%
  group_modify(~ {
    # .x = groupe par toponyme (un sf)
    if (nrow(.x) >= 3) {
      angles <- calculate_angles_sf(.x)
    } else {
      angles <- .x %>%
        mutate(angle_rad = NA_real_, angle_deg = NA_real_)
    }
    return(angles)
  }) %>%
  ungroup()

metrique <- metrique %>%
  left_join(
    SIN %>% select(axis, measure, angle_deg),
    by = c("axis", "measure")
  )

# ============================================
# 12. Enveloppe Méandrage
# ============================================
enveloppe <- st_read("enveloppe_concave.gpkg") 

# %>%
  st_drop_geometry() %>%
  dplyr::select(M,VALUE,AXIS_2, enveloppe) %>%
  group_by(AXIS_2, M) %>%
  filter(if(n() > 1) VALUE == 2 else TRUE) %>%
  slice_max(enveloppe, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  dplyr::select(-VALUE) %>%
  rename(meander_belt = enveloppe)

metrique <- metrique %>%
  left_join(enveloppe, by = c("axis"= "AXIS_2", "measure_medial_axis" ="M"))

# ============================================
# 13. Multi-chenal
# ============================================
# Import du fichier CSV contenant les métriques de forme
chenal_forme_rhone <- read.csv("chenal_props_rhone.csv")
chenal_forme_med <- read.csv("chenal_props_med.csv")
chenal_forme_corse <- read.csv("chenal_props_corse.csv")

chenal_forme_total <- rbind(chenal_forme_rhone,
                            chenal_forme_med,
                            chenal_forme_corse)

# Import du shapefile avec labels DGO
chenal_labels <- st_read("DGO_label.shp") %>%
  rename(geom = geometry) %>%
  st_as_sf(sf_column_name = "geom") %>%
  select(AXIS, M, multi) %>%
  group_by(AXIS, M) %>%
  slice(1) %>%        # garder une seule ligne par couple (AXIS, M)
  ungroup()

# Fusion des propriétés de forme et des labels
chenal_complet <- chenal_forme_total %>%
  left_join(chenal_labels, by = c("AXIS", "M"))

# Colonnes à exclure des prédicteurs
colonnes_a_exclure <- c("AXIS", "M", "geom")

# Jeu d'apprentissage (multi connu = 0 : chenal unique ; 1 : chenal multiple)
donnees_connues <- chenal_complet %>%
  st_drop_geometry() %>%
  filter(multi %in% c(0, 1)) %>%
  # garder uniquement les lignes avec au moins un prédicteur non manquant
  filter(rowSums(!is.na(select(., -all_of(c("multi", colonnes_a_exclure))))) > 0) %>%
  select(-all_of(colonnes_a_exclure))

# Jeu à prédire (multi manquant)
donnees_inconnues <- chenal_complet %>%
  filter(is.na(multi)) %>%
  select(-all_of(colonnes_a_exclure))

set.seed(123)

# Séparation apprentissage / test
index_entrainement <- createDataPartition(donnees_connues$multi, p = 0.7, list = FALSE)
jeu_entrainement   <- donnees_connues[index_entrainement, ]
jeu_test           <- donnees_connues[-index_entrainement, ]

# Conversion de la variable réponse en facteur
jeu_entrainement$multi <- as.factor(jeu_entrainement$multi)
jeu_test$multi         <- as.factor(jeu_test$multi)

# Entraînement du modèle Random Forest
modele_foret <- randomForest(
  multi ~ ., 
  data = jeu_entrainement,
  na.action = na.roughfix,   # imputation simple des NA
  importance = TRUE
)

# Importance des variables
# varImpPlot(modele_foret)

# Évaluation sur le jeu de test
predictions_test <- predict(modele_foret, newdata = jeu_test)
matrice_confusion <- confusionMatrix(predictions_test, jeu_test$multi)
print(matrice_confusion)

# Prédiction sur l’ensemble du jeu
donnees_propres <- chenal_complet %>%
  select(-all_of(colonnes_a_exclure)) %>%
  st_drop_geometry()

predictions_complet <- predict(modele_foret, newdata = donnees_propres)

# Résultat final avec prédictions Random Forest
chenal_resultats <- chenal_complet %>%
  mutate(multi_chenaux = predictions_complet) 

# Jointure avec le tableau metrique pour ajouter la variable multi
metrique <- metrique %>%
  left_join(
    chenal_resultats %>% 
      select(AXIS, M, multi_chenaux),
    by = c("axis" = "AXIS", "measure_medial_axis" = "M")
  )

metrique <- metrique %>%
  mutate(multi_chenaux = as.numeric(multi_chenaux), # conversion en numérique
         multi_chenaux = ifelse(is.na(multi_chenaux), 1, multi_chenaux)) # remplacer NA par 1 (chenal unique)

# ============================================
# 14. Dist_barrage
# ============================================
barrage <- roe %>%
  filter(lbtypeouvr %in% c("Barrage",
                           "Barrage mobile",
                           "Barrage en remblais",
                           "Barrage poids",
                           "Barrage poids voûte",
                           "Barrage voûte",
                           "Barrage à voûtes multiple",
                           "Autre sous-type de barrage",
                           "Barrage à contreforts")) %>%
  select(gid, axis, distance_axis)%>%
  rename(measure = distance_axis)

calcul_distance <- function(discon, points, colname) {

  colname_sym <- rlang::sym(colname)

  discon %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      # distance au ROE aval (donc roe.measure <= metrique.measure)
      aval = {
        c <- points$measure[points$axis == axis & points$measure <= measure]
        if (length(c)) min(measure - c) else NA_real_
      },
      # on ignore l’amont
      .distance = aval
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-aval) %>%
    dplyr::rename(!!colname_sym := .distance)
}

metrique <- calcul_distance(discon=metrique, points=barrage, colname = "roe" )


# ============================================
# . longueur
# ============================================
metrique <- metrique %>%
  mutate(length = as.numeric(st_length(geom)))


# ============================================
# . îles végétalisés
# ============================================
iles <- st_read("iles_vegetalise.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M) %>%
  rename(axis = AXIS, measure_medial_axis = M) %>%
  distinct()

metrique <- metrique %>%
  left_join(iles %>% 
              mutate(ile_veget = 2), 
            by = c("axis", "measure_medial_axis")) %>%
  mutate(ile_veget = ifelse(is.na(ile_veget), 1, ile_veget))



# ============================================
# . Retenue
# ============================================
retenue <- st_read("retenue.gpkg") %>%
  st_drop_geometry() %>%
  select(AXIS, M) %>%
  rename(axis = AXIS, measure_medial_axis = M) %>%
  distinct()

metrique <- metrique %>%
  left_join(retenue %>% 
              mutate(reservoir = 2), 
            by = c("axis", "measure_medial_axis")) %>%
  mutate(reservoir = ifelse(is.na(reservoir), 1, reservoir))


# ============================================
# . amplitude
# ============================================
# ampli <- st_read("amplitude.gpkg") %>%
#   st_drop_geometry() %>%
#   select(axis, measure, amplitude) 
# 
# metrique <- metrique %>%
#   inner_join(ampli, by = c("axis", "measure"))



# ============================================
# 13. Interpolation des NA
# ============================================

metrique <- metrique %>%
  group_by(axis) %>%
  mutate(across(where(is.numeric) & !matches("measure_medial_axis"),
              ~ na.approx(.x, na.rm = FALSE)))%>%
  filter(!is.na(AC))%>%
  ungroup()

  
# ============================================
# 7. Idx_eau
# ============================================
metrique <- metrique %>%
  mutate(
    idx_water = case_when(
      AC == 0 & WC == 0 ~ -1,    # pas de données → ignorer pour les moyennes
      WC == 0 & AC > 0 ~ 0,      # vrai 0
      TRUE ~ WC / AC             # ratio normal
    )
  )

# ============================================
# 8. step AC
# ============================================
metrique <- metrique %>%
  group_by(axis) %>%
  # arrange(measure) %>%
  mutate(Delta_AC = lag(AC) - AC) %>%
  ungroup()

# ============================================ 
# 10. ACW*
# ============================================
metrique <- metrique %>%
  mutate(ACW_star = AC / (drainage_area^0.44))


# ============================================ 
# 10. Stream power
# ============================================
bassin <- st_read("bassin_stream.gpkg") %>%
  st_drop_geometry() 

debit <- metrique %>%
  left_join(bassin, by = c("axis"))

debit <- debit %>%
  mutate(Q = A * (drainage_area^B),
         pente_mm = Slope_talweg / 100, # conversion en m/m
         pente_cmkm = pente_mm * 100000) # conversion en cm/km

debit <- debit %>%
  group_by(axis) %>%
  mutate(
    pente_m_moy = rollapply(
      pente_mm,
      width = 5,
      FUN = mean,
      align = "center",
      fill = NA
    ),
    # si pente_m_moy est NA, on met pente_mm
    pente_m_moy = ifelse(is.na(pente_m_moy), pente_mm, pente_m_moy)
  ) %>%
  ungroup() %>%
  mutate(stream_power = ((9800*Q*pente_mm)/AC),
         stream_power_mm = ((9800*Q*pente_m_moy)/AC))
  

metrique <- metrique %>%
  mutate(stream_power = debit$stream_power,
         stream_power_mm = debit$stream_power_mm,
         Q = debit$Q)


# metrique <- metrique %>%
#   filter(!is.na(stream_power) & !is.infinite(stream_power))
  
  
# sum(is.na(metrique$AC), na.rm = TRUE) # vérifier qu'il n'y a plus de NA
# sum(is.na(metrique$VB), na.rm = TRUE) # vérifier qu'il n'y a plus de NA
# sum(is.na(metrique$measure_medial_axis), na.rm = TRUE) # vérifier qu'il n'y a plus de NA

# ============================================
# 14. supprimer les df temporaires
# ============================================
rm(chenal_complet, chenal_resultats, chenal_forme_corse, chenal_forme_med,
   chenal_forme_rhone, chenal_forme_total ,chenal_labels, donnees_connues, 
   donnees_inconnues,donnees_propres, jeu_entrainement, jeu_test, modele_foret, 
   predictions_complet, predictions_test, surface_drainee, surface_drainee_corse,
  surface_drainee_med, surface_drainee_rhone,surface_drainee_totale, SIN, Data_sf, Data_sf_proj,
   barrage, enveloppe, matrice_confusion, index_entrainement, colonnes_a_exclure,
   calculate_angles_sf, calcul_distance,iles, retenue, debit, bassin, ampli)
  

# ============================================
# 14. Export des métriques
# ============================================
st_write(metrique, "Metrics_DGO.gpkg", delete_layer = TRUE)
# write.csv(metrique, "metrique.csv", row.names = FALSE)



