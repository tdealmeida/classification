test<- metrique %>%
  filter(axis == "2000801955") %>%
  mutate(id = row_number())

values <- na.omit(test$log_AC)
cpt_result_pelt <- cpt.mean(
  values,
  method = "PELT",
  penalty = "Manual",
  # pen.value = 1,
  pen.value = "0.5*log(n)",  # BIC
  minseglen = 4  # 4 pour avoir au moins 3 segments valides
)

# Extraire les points de changement
cpt_pelt <- cpt_result_pelt@cpts
cpt_pelt <- cpt_pelt[-length(cpt_pelt)]  # retirer le dernier, c'est juste la fin
length(cpt_pelt)


ggplot() +
  geom_line(data = test, aes(x = id, y = AC), color = "grey") +
  geom_point(data = test, aes(x = id, y = AC), color = "black") +
  geom_vline(aes(xintercept = rupture$cp), linetype = "dashed", color = "red") +
  labs(title = "Changement de comportement de la métrique d'angle pour l'Ain (SIN)",
       x = "Step",
       y = "Measure") +
  theme_minimal()







run_pelt_meanvar <- function(values) {
  result <- tryCatch({
    # Analyse de changement de moyenne avec PELT
    cpt_result_pelt <- cpt.mean(
      values,
      method = "PELT",
      penalty = "Manual",
      # pen.value = 1,
      pen.value = "0.5*log(n)",  # BIC      minseglen = 4  # 4 pour avoir au moins 3 segments valides
    )
    
    # Extraire les points de changement
    cpt_pelt <- cpt_result_pelt@cpts
    cpt_pelt <- cpt_pelt[-length(cpt_pelt)]  # retirer le dernier, c'est juste la fin
    
    # Retourner un tibble
    tibble(
      cp = list(cpt_pelt),
      cpt = length(cpt_pelt)
    )
    
  }, error = function(e) {
    # En cas d'erreur -> pas de changements
    tibble(
      cp = list(integer(0)),
      cpt = 0
    )
  })
  
  return(result)
}


SIN <- test %>%
  group_by(axis) %>%
  arrange(measure) %>%
  summarize(run_pelt_meanvar(na.omit(log_AC))) %>%
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


combined <- bind_rows(SIN)
# combined <- bind_rows(AC, VB, SIN)

rupture <- combined %>%
  group_by(axis) %>%
  summarise(
    data = list(filter_tol_sources(unlist(cp), rep(source, lengths(cp)), tol = 3)),
    .groups = "drop"
  ) %>%
  unnest(data)


# 1️⃣ Filtrer les toponymes présents dans AC
axis_list <- unique(rupture$axis)
metrique_filtre <- test %>%
  filter(axis %in% axis_list)

# Fonction pour majorité
majority_cat <- function(x) {
  counts <- tabulate(x, nbins = 2)
  ifelse(counts[1] > counts[2], 1, 2)
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
      source = paste(na.omit(unique(source_cpt)), collapse = "+"),
      across(c(angle_deg), 
             ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}"),
      .groups = "drop"
    ) %>%
    arrange(ID_segment) 
  
  list(res = res, subdata = subdata)
}

# Appliquer la fonction sur tous les toponymes
all_results <- map(axis_list, process_toponyme)

# 3️⃣ Combiner les résultats
TGH <- bind_rows(map(all_results, "res"))
ALL_SUBDATA <- bind_rows(map(all_results, "subdata"))

# str(TGH)
st_write(TGH, "TGH_sin.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile


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


st_write(Data_points, "rupture_sin.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile


nrow(Data_points)



