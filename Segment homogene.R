library(changepoint)
library(stringr)

length(unique(metrique$axis))

metrique <- metrique %>%
  mutate(
    log_AC = log(ifelse(AC == 0, 0.1, AC)+1) ,
    log_VB = log(ifelse(VB == 0, 0.1, VB)+1),
    log_SIN = log(ifelse(angle_deg == 0, 0.1, angle_deg)+1)
  )

AC <- metrique %>%
  group_by(axis) %>%
  arrange(measure) %>%
  summarize(run_pelt_meanvar(log_AC)) %>%
  filter(cpt > 0) %>%
  mutate(source = "AC")

VB <- metrique %>%
  group_by(axis) %>%
  arrange(measure) %>%
  summarize(run_pelt_meanvar(log_VB)) %>%
  filter(cpt > 0) %>%
  mutate(source = "VB")

SIN <- metrique %>%
  group_by(axis) %>%
  arrange(measure) %>%
  summarize(run_pelt_meanvar(na.omit(log_SIN))) %>%
  filter(cpt > 0) %>%
  mutate(source = "SIN")


filter_tol_sources <- function(cp, source, tol = 3) {
  df <- data.frame(cp = cp, source = source, stringsAsFactors = FALSE)
  df <- df[order(df$cp), ]
  
  result <- df[1, , drop = FALSE]
  
  if (nrow(df) > 1) {
    for (i in 2:nrow(df)) {
      # comparer au dernier cp retenu
      if (abs(df$cp[i] - tail(result$cp, 1)) <= tol) {
        # fusionner les sources
        last_idx <- nrow(result)
        merged_sources <- unique(c(strsplit(result$source[last_idx], "\\+")[[1]], df$source[i]))
        result$source[last_idx] <- paste(merged_sources, collapse = "+")
      } else {
        result <- rbind(result, df[i, , drop = FALSE])
      }
    }
  }
  
  return(result)
}

sum(unlist(AC$cpt))
sum(unlist(VB$cpt))
sum(unlist(SIN$cpt))


# combined <- bind_rows(SIN)
combined <- bind_rows(AC, VB, SIN)

rupture <- combined %>%
  group_by(axis) %>%
  summarise(
    data = list(filter_tol_sources(unlist(cp), rep(source, lengths(cp)), tol = 3)),
    .groups = "drop"
  ) %>%
  unnest(data)


# 1️⃣ Filtrer les toponymes présents dans AC
axis_list <- unique(rupture$axis)
metrique_filtre <- metrique %>%
  filter(axis %in% axis_list)

# # # Fonction pour majorité
# majority_cat <- function(x) {
#   counts <- tabulate(x, nbins = 2)
#   ifelse(counts[1] > counts[2], 1, 2)
# }

classe_dominante <- function(x) {
  x <- na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

value_pct <- function(x, val = 2) {
  sum(x == val, na.rm = TRUE) / length(x)
}

safe_mean_idx_water <- function(x) {
  vals <- x[x != -1]  # on retire les -1
  if (length(vals) == 0) {
    return(-1)         # si que des -1 → on garde -1
  } else {
    return(mean(vals, na.rm = TRUE))  # sinon moyenne normale
  }
}

# 2️⃣ Boucler sur chaque toponyme
process_toponyme <- function(top) {
  
  # Extraire ruptures pour cet axis
  rupture_top <- rupture %>%
    filter(axis == top)
  
  subdata <- metrique_filtre %>%
    filter(axis == top) %>%
    arrange(measure)  # ⚡ trier par measure avant de créer les segments
  
  # Préparer vecteurs
  segment <- rep(0, nrow(subdata))
  source_segment <- rep(NA_character_, nrow(subdata))
  
  # Marquer ruptures AC ou VB
  for (i in seq_len(nrow(rupture_top))) {
    cp_i <- rupture_top$cp[i]
    src_i <- rupture_top$source[i]
    
    if (!is.na(cp_i) && cp_i <= nrow(subdata)) {
      segment[cp_i] <- 1
      source_segment[cp_i] <- src_i
    }
  }
  
  # Construire les segments
  subdata <- subdata %>%
    mutate(
      ID_segment = cumsum(segment) + 1 - segment,
      source_cpt = source_segment
    )
  
  # Résumé par segment
  res <- subdata %>%
    group_by(ID_segment) %>%
    summarise(
      toponyme = first(toponyme),
      nb_DGO = n(),
      axis = first(axis),
      gid_region = first(gid_region),
      measure = last(measure),
      source = paste(na.omit(unique(source_cpt)), collapse = "+"),
      strahler = classe_dominante(strahler),
      mean_idx_water = safe_mean_idx_water(idx_water),
      across(c(AC, VB, WC, 
               Slope_talweg, Slope_VB, elevation, idx_conf,
               ACW_star, meander_belt,
               # amplitude, 
               angle_deg,
               # stream_power,
               water_channel_pc, gravel_bars_pc, natural_open_pc, 
               forest_pc, grassland_pc, crops_pc, diffuse_urban_pc, 
               dense_urban_pc, infrastructures_pc, active_channel_pc, 
               riparian_corridor_pc, semi_natural_pc, reversible_pc, 
               disconnected_pc_corrige, built_environment_pc
               ), 
             ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}"),
      nb_na = sum(is.na(WC)),
      na_pct = nb_na / nb_DGO * 100,
      multi_chenal = value_pct(multi_chenaux, val = 2),   #
      iles_veget = value_pct(ile_veget, val = 2),       #
      retenue = value_pct(reservoir, val = 2),       #
      # roe = min(roe, na.rm = TRUE),               # ✅ min au lieu de moyenne
      sum_length = sum(length, na.rm = TRUE),
      sum_conf_degree = sum(conf_degree, na.rm = TRUE),
      drainage_area = max(drainage_area, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(ID_segment) %>%  # pour que lag() ait du sens
    mutate(Delta_AC = lag(mean_AC) - mean_AC,
           Delta_WC = lag(mean_WC) - mean_WC,
           Lag_AC = mean_AC - lead(mean_AC),
           Delta_AC_relatif = ((lag(mean_AC)-mean_AC) / mean_AC) * 100,  # variation relative vs ligne précédente
           Lag_AC_relatif = ((mean_AC - lead(mean_AC)) / lead(mean_AC)) * 100      # variation relative vs ligne suivante
    )

  list(res = res, subdata = subdata)
}

# Appliquer la fonction sur tous les toponymes
all_results <- map(axis_list, process_toponyme)

# 3️⃣ Combiner les résultats
TGH <- bind_rows(map(all_results, "res"))
ALL_SUBDATA <- bind_rows(map(all_results, "subdata"))

# str(TGH)
# st_write(TGH, "TGH.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile
# write.csv(TGH, "TGH.csv", row.names = FALSE) # Export des données nettoyées en shapefile
# st_write(ALL_SUBDATA, "ALL_SUBDATA.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile

Data_points <- ALL_SUBDATA %>%
  group_by(axis, ID_segment) %>%
  arrange(desc(measure)) %>%
  slice_head(n = 1) %>%
  st_as_sf() %>%
  mutate(
    geom = st_sfc(map(geom, ~ {
      coords <- st_coordinates(.x)
      st_point(coords[1, 1:2]) # prend le 1er point
      # st_point(coords[nrow(coords), 1:2]) # prend le dernier point
    }), crs = st_crs(.))
  ) %>%
  st_as_sf() %>%
  mutate(geom = st_zm(geom, drop = TRUE, what = "ZM")) %>%
  ungroup() %>%
  group_by(axis) %>%
  slice_head(n = -1) %>%  #
  # slice(-1) %>%   # <-- supprime le dernier point car sinon crée un point de rupture pour le premier segment amont
  ungroup() %>%
  st_transform(2154)


st_write(Data_points, "Rupture.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile


Data_points_mid <- ALL_SUBDATA %>%
  group_by(axis, ID_segment) %>%
  arrange(desc(measure)) %>%
  slice(ceiling(n() / 2)) %>%
  st_as_sf() %>%
  mutate(
    geom = st_sfc(map(geom, ~ {
      coords <- st_coordinates(.x)
      mid <- ceiling(nrow(coords) / 2)
      st_point(coords[mid, 1:2])
    }), crs = st_crs(.))
  ) %>%
  st_as_sf() %>%
  mutate(geom = st_zm(geom, drop = TRUE, what = "ZM")) %>%
  ungroup() %>%
  st_transform(2154) %>%
  select(axis, ID_segment, toponyme)


st_write(Data_points_mid, "midpoints.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile








df_source <- ALL_SUBDATA %>%
  st_drop_geometry() %>%
  filter(!is.na(source_cpt)) %>%
  distinct(axis, measure, .keep_all = TRUE) %>%
  mutate(axis = as.numeric(axis)) %>%
  select(axis, measure, source_cpt)

df2_new <- data %>%
  mutate(axis = as.numeric(axis)) %>%
  left_join(df_source, by = c("axis", "measure"))

df2_new <- df2_new %>%
  arrange(axis, measure) %>%
  group_by(axis) %>%
  mutate(segment_id = 1 + cumsum(lag(!is.na(source_cpt), default = FALSE))) %>%
  ungroup()

df2_group <- df2_new %>%
  st_transform(2154) %>% # exemple Lambert 
  mutate(longueur_a = st_length(geom)) %>%
  st_drop_geometry() %>%
  summarise(
    longueur = sum(longueur_a),
    .by = c(axis, segment_id)
  ) 

df2_group <- df2_group %>%
  mutate(longueur_data = as.numeric(longueur)) %>%
  select(-longueur)

TGH <- TGH %>%
  left_join(df2_group, by = c("axis", "ID_segment" = "segment_id"))

st_write(TGH, "TGH.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile






rm(all_results, axis_list, combined, i, process_toponyme, rupture_top,
  filter_tol_sources, metrique_filtre, segment, source_segment, subdata, top,
  majority_cat, value_pct, safe_mean_idx_water)



# 
# Test <- SIN %>%
#   filter(axis == "2000800882")
# test <- metrique %>%
#   filter(axis == "2000800882") %>%
#   arrange(desc(measure)) %>%
#   mutate(ID = row_number()) 
# 
# ggplot(test, aes(x = ID, y = angle_deg)) +
#   geom_line() +
#   geom_vline(xintercept = unlist(Test$cp), color = "red", linetype = "dashed") +
#   theme_minimal()
# 
# 
# ggplot(test, aes(x = ID, y = VB)) +
#   geom_line() +
#   geom_vline(xintercept = unlist(Test$cp), color = "red", linetype = "dashed") +
#   scale_x_continuous(
#     name = "measure",
#     breaks = c(50, 100),                                  # positions sur ID
#     labels = round(test$measure[c(50, 100)], 2)            # valeurs correspondantes
#   ) +
#   theme_minimal()
# 
# 
# length(test$VB)
# 0.5*log(500)
# write.csv(test, "test_sinuosity.csv", row.names = FALSE)

