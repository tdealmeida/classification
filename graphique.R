palette_lits <- c(
  "rectiligne" = "#aac1cf",
  "rectiligne bars" = "#6baed6",
  # "Pas de lit" = "#202020",
  # "intermittent" = "#a7a7a7",
  "sinueux" = "#2f76aa",
  "sinueux ba" = "#08519c",
  "meandre passif" = "#756bb1",
  "meandre actif" = "#54278f",
  "tresse" = "#ffd500",
  # "tresse intermittent" = "#eae9ab",
  "tresse vegetal" = "#9acd32",
  "divagant" = "#e69f00",
  "retenue" = "#2227db",
  "anamostose" = "#b410d5",
  "diffus" = "#fcb4b1"
)


palette_ordre_conf <- c(
  # Rectiligne
  "rectiligne confine"        = "#8fa9bb",
  "rectiligne semi-confine"   = "#aac1cf",
  "rectiligne non confine"    = "#c7d8e3",
  
  # Rectiligne bars
  "rectiligne bars confine"      = "#4f97c6",
  "rectiligne bars semi-confine" = "#6baed6",
  "rectiligne bars non confine"  = "#9fcceb",
  
  # Sinueux
  "sinueux confine"          = "#245b83",
  "sinueux semi-confine"     = "#2f76aa",
  "sinueux non confine"      = "#6fa6cf",
  
  # Sinueux bars
  "sinueux bars confine"        = "#063b73",
  "sinueux bars semi-confine"   = "#08519c",
  "sinueux bars non confine"    = "#3f7fc0",
  
  # Méandre passif
  "meandre passif confine"        = "#5e5798",
  "meandre passif semi-confine"   = "#756bb1",
  "meandre passif non confine"    = "#a29ad0",
  
  # Méandre actif
  "meandre actif confine"        = "#3f1d6b",
  "meandre actif semi-confine"   = "#54278f",
  "meandre actif non confine"    = "#8a5fc0",
  
  # Divagant (wandering)
  "wandering confine"        = "#c67f00",
  "wandering semi-confine"   = "#e69f00",
  "wandering non confine"    = "#f2c44d",
  
  # Tresse
  "tresse confine"           = "#d4b200",
  "tresse semi-confine"      = "#ffd500",
  "tresse non confine"       = "#ffea66",
  
  # Tresse végétal
  "tresse vegetal confine"        = "#6f9f24",
  "tresse vegetal semi-confine"   = "#9acd32",
  "tresse vegetal non confine"    = "#c6e68a",
  
  # Anamostose
  "anamostosed confine"        = "#8a0aa4",
  "anamostosed semi-confine"   = "#b410d5",
  "anamostosed non confine"    = "#db7ef0",
  
  # Classes simples
  "tresse intermittent" = "#eae9ab",
  "intermittent"        = "#a7a7a7",
  "retenue"             = "#2227db",
  "Pas de lit"           = "#202020"
)

ordre_conf <- c(
  "rectiligne confine",
  "rectiligne semi-confine",
  "rectiligne non confine",
  "rectiligne bars confine",
  "rectiligne bars semi-confine",
  "rectiligne bars non confine",
  "sinueux confine",
  "sinueux semi-confine",
  "sinueux non confine",
  "sinueux bars confine",
  "sinueux bars semi-confine",
  "sinueux bars non confine",
  "meandre passif confine",
  "meandre passif semi-confine",
  "meandre passif non confine",
  "meandre actif confine",
  "meandre actif semi-confine",
  "meandre actif non confine",
  "wandering confine",
  "wandering semi-confine",
  "wandering non confine",
  "tresse confine",
  "tresse semi-confine",
  "tresse non confine",
  "tresse vegetal confine",
  "tresse vegetal semi-confine",
  "tresse vegetal non confine",
  "anamostosed confine",
  "anamostosed semi-confine",
  "anamostosed non confine",
  "tresse intermittent",
  "intermittent",
  "retenue",
  "Pas de lit"
)

ordre_lits <- c(
  "rectiligne",
  "rectiligne bars",
  "sinueux",
  "sinueux ba",
  "meandre passif",
  "meandre actif",
  "divagant",
  "tresse",
  "tresse vegetal",
  "anamostose",
  "diffus",
  # "tresse intermittent",
  # "intermittent",
  "retenue"
  # "Pas de lit"
)

# histogramme du nb de sgments homogènes par classe
resultat_final %>%
  mutate(Prediction = factor(Prediction, levels = ordre_lits)) %>%
  ggplot() +
  aes(x = Prediction) +
  geom_bar(aes(fill = Prediction)) +
  scale_fill_manual(values = palette_lits) +
  theme_minimal() + 
  labs(y = "Number of Homogeneous River Reaches", 
       x = "Planform") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = "none"
  )


resultat_final %>%
  mutate(noeudfinal = factor(noeudfinal, levels = ordre_lits)) %>%
  # filter(!Prediction == "Pas de lit") %>%
  ggplot() +
  aes(x = noeudfinal) +
  geom_bar(aes(fill = noeudfinal)) +
  scale_fill_manual(values = palette_lits) +
  theme_minimal() + 
  labs(y = "Nombre de segments homogènes", 
       x = "Type de lit") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = "none"
  )






test1 <- resultat_conf %>%
  mutate(style_confinement = factor(style_confinement)) %>%
  filter(!style_confinement == "Pas de lit",
         !style_confinement == "non contraint",
         !style_confinement == "partially contraint",
         !style_confinement == "contraint",
         !style_confinement == "intermittent confined",
         !style_confinement == "intermittent semi-confined",
         !style_confinement == "intermittent no confined",
         !style_confinement == "free braided"
  ) %>%
  group_by(style_confinement) %>%
  summarize(long = sum(sum_length)/1000)



test1 %>%
  # filter(Prediction != "Pas de lit") %>%
  ggplot(aes(x = style_confinement, y = long, fill = style_confinement)) +
  geom_col() +   # ou geom_bar(stat = "identity") si long contient déjà des valeurs
  scale_fill_manual(values = palette_ordre_conf) +
  theme_minimal() + 
  labs(y = "km", 
       x = "Type de lit") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = "none"
  )








# histogramme du nb de sgments homogènes par classe
test1 <- resultat_final %>%
  mutate(Prediction = factor(Prediction, levels = ordre_lits)) %>%
  group_by(Prediction) %>%
  summarize(long = sum(sum_length)/1000)

test1 %>%
  # filter(Prediction != "Pas de lit") %>%
  ggplot(aes(x = Prediction, y = long, fill = Prediction)) +
  geom_col() +   # ou geom_bar(stat = "identity") si long contient déjà des valeurs
  scale_fill_manual(values = palette_lits) +
  theme_minimal() + 
  labs(y = "km", 
       x = "Type de lit") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = "none"
  )


# camenbert pour la longueur par classe
pie_data <- test1 %>%
  mutate(Prediction = factor(Prediction, levels = ordre_lits)) %>%
  # filter(Prediction != "Pas de lit") %>%
  group_by(Prediction) %>%
  summarise(total_long = sum(long, na.rm = TRUE)) %>%  # somme de long par catégorie
  ungroup() %>%
  mutate(percent = total_long / sum(total_long) * 100)  # pourcentage par rapport au total

ggplot(pie_data, aes(x = "", y = percent, fill = Prediction)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = palette_lits) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) 






df_facet <- resultat_final %>%
  mutate(Prediction = factor(Prediction, levels = ordre_lits)) %>%
  mutate(
    gid_region_name = case_when(
      gid_region == 31 ~ "la Saône",
      gid_region == 16 ~ "la Durance",
      gid_region == 11 ~ "l'Isère",
      gid_region == 33 ~ "le Rhône hors autre bassin",
      gid_region == 26 ~ "Côtiers méditerranéens"
    )
  )

df_total <- df_facet %>%
  mutate(gid_region_name = "Total")

df_plot <- bind_rows(df_facet, df_total)

ggplot(df_plot, aes(x = "Total", fill = Prediction)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = palette_lits) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~ gid_region_name, nrow  = 1) +
  theme_minimal() +
  labs(
    y = "Pourcentage de segments homogènes",
    x = NULL,
    fill = "Type de lit"
  ) +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )








df_plot <- ALL_SUBDATA %>%
  left_join(
    sf::st_drop_geometry(resultat_final) %>% 
      select(axis, ID_segment, Prediction),
    by = c("axis", "ID_segment")
  ) %>%
  filter(toponyme == "la Drôme") %>%
  arrange(measure_medial_axis)

bg_df <- df_plot %>%
  mutate(
    grp = cumsum(Prediction != dplyr::lag(Prediction, default = first(Prediction))),
    xmin = measure_medial_axis,
    xmax = dplyr::lead(measure_medial_axis)
  ) %>%
  filter(!is.na(xmax)) %>%
  group_by(grp, Prediction) %>%
  summarise(
    xmin = min(xmin),
    xmax = max(xmax),
    .groups = "drop"
  )

ggplot() +
  geom_rect(
    data = bg_df,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = Prediction),
    alpha = 0.5
  ) +
  geom_line(
    data = df_plot,
    aes(x = measure_medial_axis, y = active_channel_width),
    color = "black",
    linewidth = 1
  ) +
  scale_fill_manual(values = palette_lits, name = "Type de lit") +
  theme_minimal() + 
  labs(
    y = "Largeur du chenal actif (m)",
    x = "Distance depuis la source (m)"
  )

























tttt <- resultat_final %>%
  st_drop_geometry() %>%
  select(axis, ID_segment, Prediction) 

transitions <- tttt %>%
  arrange(axis, ID_segment) %>%          # ordre correct
  group_by(axis) %>%                     # transitions par trajectoire
  mutate(next_pred = lead(Prediction)) %>%
  ungroup() %>%
  filter(!is.na(next_pred)) %>%
  count(Prediction, next_pred, name = "count")

data_long <- tttt %>%
  arrange(axis, ID_segment) %>%
  group_by(axis) %>%
  mutate(next_pred = lead(Prediction)) %>%
  ungroup() %>%
  filter(!is.na(next_pred)) %>%
  filter(Prediction != next_pred) %>%          # ⬅️ garde seulement les changements
  count(Prediction, next_pred, name = "value") %>%
  group_by(Prediction) %>%
  mutate(value = 100 * value / sum(value)) %>% # % par classe source
  ungroup() %>%
  rename(rowname = Prediction, key = next_pred)

labels_wrap <- c(
  "rectiligne bars" = "rectiligne\nbars",
  "meandre passif" = "meandre\npassif",
  "meandre actif" = "meandre\nactif",
  "tresse intermittent" = "tresse\nintermittent",
  "tresse vegetal" = "tresse\nvegetal"
)

data_long$rowname <- recode(data_long$rowname, !!!labels_wrap)
data_long$key     <- recode(data_long$key, !!!labels_wrap)

palette_lits <- c(
  "rectiligne" = "#aac1cf",
  "rectiligne\nbars" = "#6baed6",
  "Pas de lit" = "#202020",
  "intermittent" = "#a7a7a7",
  "sinueux" = "#2f76aa",
  "sinueux ba" = "#08519c",
  "meandre\npassif" = "#756bb1",
  "meandre\nactif" = "#54278f",
  "tresse" = "#ffd500",
  "tresse\nintermittent" = "#eae9ab",
  "tresse\nvegetal" = "#9acd32",
  "divagant" = "#e69f00",
  "retenue" = "#2227db",
  "anamostose" = "#b410d5"
)

classes <- union(data_long$rowname, data_long$key)
grid_cols <- palette_lits[classes]

circos.clear()
circos.par(
  start.degree = 90,
  gap.degree = 10,
  track.margin = c(-0.1, 0.1),
  points.overflow.warning = FALSE
)

par(mar = rep(0, 4))

chordDiagram(
  x = data_long,
  grid.col = grid_cols,
  transparency = 0.25,
  directional = 1,
  direction.type = c("arrows", "diffHeight"),
  diffHeight = -0.04,
  annotationTrack = "grid",
  annotationTrackHeight = c(0.05, 0.1),
  link.arr.type = "big.arrow",
  link.sort = TRUE,
  link.largest.ontop = TRUE
)

circos.trackPlotRegion(
  track.index = 1,
  bg.border = NA,
  panel.fun = function(x, y) {
    
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    
    circos.text(
      x = mean(xlim),
      y = 3.5,
      labels = sector.index,
      facing = "clockwise",
      niceFacing = TRUE,
      cex = 0.65
    )
  }
)



















palette_continuité <- c(
  # "Espace_eau" = "#0050c8",
  "Espace_bancs" = "#8cc1da",
  "Espace_naturel" = "#1d8641",
  "Espace_semi_naturel" = "#d2e68a",
  "Espace_agricole_connecté" = "#ffff99",
  "Espace_déconnecté" = "#f2f2f2",
  "Espace_artificialisé" = "#cecece"
)

ordre_continuité <- c(
  # "Espace_eau",
  "Espace_bancs",
  "Espace_naturel",
  "Espace_semi_naturel",
  "Espace_agricole_connecté",
  "Espace_déconnecté",
  "Espace_artificialisé"
)

  
test_continuité <- resultat_conf %>%
  filter(
         !style_confinement == "Pas de lit",
         !style_confinement == "non contraint",
         !style_confinement == "partially contraint",
         !style_confinement == "contraint",
         !style_confinement == "intermittent confined",
         !style_confinement == "intermittent semi-confined",
         !style_confinement == "intermittent no confined",
         !style_confinement == "free braided",
         !style_confinement == "retenue"
  ) %>%
  group_by(style_confinement) %>%
  summarize(
    # Espace_eau = mean(mean_water_channel_pc, na.rm = TRUE),
    Espace_bancs = mean(mean_gravel_bars_pc, na.rm = TRUE),
    Espace_naturel = mean(mean_riparian_corridor_pc, na.rm = TRUE),
    Espace_semi_naturel = mean(mean_semi_natural_pc, na.rm = TRUE),
    Espace_agricole_connecté = mean(mean_reversible_pc, na.rm = TRUE),
    Espace_déconnecté = mean(mean_disconnected_pc_corrige, na.rm = TRUE),
    Espace_artificialisé = mean(mean_built_environment_pc, na.rm = TRUE)
  ) %>%
  st_drop_geometry() %>%   # ✅ sans argument
  pivot_longer(
    cols = starts_with("Espace_"),
    names_to = "classe",
    values_to = "surface"
  ) %>%
  mutate(
    classe = factor(classe, levels = ordre_continuité),
    style_confinement = factor(style_confinement),
    surface = ifelse(surface < 0, 0, surface)
  )

ggplot(test_continuité, aes(x = style_confinement, y = surface, fill = classe)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = palette_continuité, drop = FALSE) +  # ✅ bonne palette
  scale_y_continuous(labels = scales::percent) +
  labs(
    y = "Relative proportion (%)",
    x = "Confinement type",
    fill = "lateral continuity"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )








palette_landuse <- c(
  "Espace_eau" = "#ccdaeb",
  "Espace_bancs" = "#e2e2e2",
  "Espace_naturel" = "#daf188",
  "Espace_forêt" = "#adc86e",
  "Espace_prairie" = "#ffefa1",
  "Espace_cultures" = "#ffffc5",
  "Espace_urbain_diffus" = "#fcb4b9",
  "Espace_urbain_dense" = "#fc7982",
  "Espace_infrastructures" = "#fc7aa7"
)

ordre_landuse <- c(
  "Espace_eau",
  "Espace_bancs",
  "Espace_naturel",
  "Espace_forêt",
  "Espace_prairie",
  "Espace_cultures",
  "Espace_urbain_diffus",
  "Espace_urbain_dense",
  "Espace_infrastructures"
)

test_landuse <- resultat_conf %>%
  filter(
    !style_confinement == "Pas de lit",
    !style_confinement == "non contraint",
    !style_confinement == "partially contraint",
    !style_confinement == "contraint",
    !style_confinement == "intermittent confined",
    !style_confinement == "intermittent semi-confined",
    !style_confinement == "intermittent no confined",
    !style_confinement == "free braided",
    !style_confinement == "retenue"
  ) %>%
  group_by(style_confinement) %>%
  summarize(Espace_eau = mean(mean_water_channel_pc, na.rm = TRUE),
            Espace_bancs = mean(mean_gravel_bars_pc, na.rm = TRUE),
            Espace_forêt = mean(mean_forest_pc, na.rm = TRUE),
            Espace_cultures = mean(mean_crops_pc, na.rm = TRUE),
            Espace_urbain_diffus = mean(mean_diffuse_urban_pc, na.rm = TRUE),
            Espace_urbain_dense = mean(mean_dense_urban_pc, na.rm = TRUE),
            Espace_infrastructures = mean(mean_infrastructures_pc, na.rm = TRUE),
            Espace_naturel = mean(mean_natural_open_pc, na.rm = TRUE),
            Espace_prairie = mean(mean_grassland_pc, na.rm = TRUE)
  ) %>%
  st_drop_geometry() %>%   # ✅ sans argument
  pivot_longer(
    cols = starts_with("Espace_"),
    names_to = "classe",
    values_to = "surface"
  ) %>%
  mutate(
    classe = factor(classe, levels = ordre_landuse),
    style_confinement = factor(style_confinement),
    surface = ifelse(surface < 0, 0, surface)
  )

ggplot(test_landuse, aes(x = style_confinement, y = surface, fill = classe)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = palette_landuse, drop = FALSE) +  # ✅ bonne palette
  scale_y_continuous(labels = scales::percent) +
  labs(
    y = "Relative proportion (%)",
    x = "Confinement type",
    fill = "LandUse"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )
  









resultat_conf2 <- resultat_conf %>%
  filter(style_confinement_simple %in% c(
    "braided confined",
    "braided semi-confined",
    "free braided",
    "incised meander",
    "meander semi-confined",
    "free meander",
    "rectiligne confined",
    "rectiligne semi-confined",
    "rectiligne endigué",
    "sinuous confined",
    "sinuous semi-confined",
    "sinuous no confined",
    "wandering confined",
    "wandering semi-confined",
    "free wandering"
  )) %>%
  mutate(
    style_conf_label = style_confinement_simple
  )

resultat_conf2 <- resultat_conf2 %>%
  mutate(
    planform_group = case_when(
      Prediction %in% c("rectiligne", "rectiligne bars") ~ "Rectilinear",
      Prediction %in% c("sinueux", "sinueux ba") ~ "Sinuous",
      Prediction %in% c("meandre passif", "meandre actif") ~ "Meandering",
      Prediction %in% c("tresse", "tresse vegetal") ~ "Braided",
      Prediction %in% c("divagant") ~ "Wandering",
      Prediction %in% c("anamostose") ~ "Anastomosed",
      TRUE ~ "Other"
    )
  )

palette_planform <- c(
  "Rectilinear" = "#8c510a",
  "Sinuous" = "#dfc27d",
  "Meandering" = "#80cdc1",
  "Braided" = "#018571",
  "Wandering" = "#a6bddb",
  "Anastomosed" = "#542788"
)

ggplot(resultat_conf2,
       aes(x = style_conf_label, y = mean_AC, fill = planform_group)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.85) +
  scale_fill_manual(values = palette_planform) +
  labs(
    x = "Planform × confinement classes",
    y = "Active channel width (%)",
    fill = "Planform type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "bottom"
  )

ggplot(resultat_conf2,
       aes(x = confinement, y = mean_AC, fill = confinement)) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~ planform_group, scales = "free_y", nrow = 1) +
  labs(
    x = NULL,
    y = "Active channel width (%)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_blank(),
  )







metrics <- c(
  "mean_AC",
  "mean_VB",
  "mean_Slope_talweg",
  "Sinuosity_meander",
  "drainage_area",
  "mean_elevation",
  "mean_riparian_corridor_pc",
  "mean_built_environment_pc"
)

resultat_long <- resultat_conf2 %>%
  filter(Sinuosity_meander < 5,
         Sinuosity_meander > 0.8,
         mean_Slope_talweg > 0) %>%
  pivot_longer(
    cols = all_of(metrics),
    names_to = "metric",
    values_to = "value"
  )

resultat_long <- resultat_long %>%
  mutate(metric_label = case_when(
    metric == "mean_AC" ~ "Active channel width (%)",
    metric == "mean_Slope_talweg" ~ "Talweg slope",
    metric == "mean_VB" ~ "Valley Bottom width",
    metric == "Sinuosity_meander" ~ "Sinuosity",
    metric == "drainage_area" ~ "Drainage area (m²)",
    metric == "mean_elevation" ~ "Elevation (m)",
    metric == "mean_riparian_corridor_pc" ~ "Riparian corridor (%)",
    metric == "mean_built_environment_pc" ~ "Built environment (%)",
    TRUE ~ metric
  ))

ggplot(resultat_long,
       aes(x = confinement, y = value, fill = confinement)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  facet_grid(
    rows = vars(metric_label),
    cols = vars(planform_group),
    scales = "free_y",
    switch = "y"   # 🔥 labels à gauche du plot
  ) +
  scale_y_log10()+
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    legend.position = "bottom",
    strip.placement = "outside",
    panel.spacing = unit(1, "lines")
  )

















data_box <- resultat_conf %>%
  filter(Sinuosity_meander < 4,
         Sinuosity_meander > 0.8,
         mean_Slope_talweg > 0) %>%
  st_drop_geometry() %>%
  select(
    style_confinement_2,
    # Prediction,
    mean_AC,
    mean_ACW_star,
    mean_idx_water,
    mean_VB,
    mean_Slope_talweg,
    mean_elevation,
    mean_idx_conf,
    drainage_area,
    Sinuosity_meander,
    mean_disconnected_pc_corrige,
    mean_riparian_corridor_pc,
    mean_built_environment_pc
  ) %>%
  pivot_longer(
    cols = -style_confinement_2,
    names_to = "metric",
    values_to = "value"
  )

data_box <- data_box %>%
  mutate(
    metric = recode(metric,
                    mean_AC = "Active channel width (m)",
                    Sinuosity_meander = "Sinuosity",
                    mean_riparian_corridor_pc = "Riparian corridor (%)",
                    mean_built_environment_pc = "Built environment (%)"
    ),
    metric = factor(metric, levels = c(
      "Active channel width (m)",
      "Sinuosity",
      "Riparian corridor (%)",
      "Built environment (%)"
    ))
  )

ggplot(data_box, aes(x = style_confinement_2, y = value)) +
  geom_boxplot(outlier.alpha = 0.3) +
  facet_wrap(~ metric, scales = "free_y", ncol = 3) +
  labs(
    x = "Planform type",
    y = "Metric value"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    strip.text = element_text(face = "bold")
  )



ggplot(resultat_conf, aes(x = style_confinement_2, y = mean_AC)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = palette_landuse, drop = FALSE) +  # ✅ bonne palette
  scale_y_continuous(labels = scales::percent) +
  labs(
    y = "",
    x = "Confinement type",
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )










