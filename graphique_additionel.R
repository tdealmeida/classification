table(resultat_conf$conf_detaille)

# ============================================
# histogramme du nombre de segments homogènes par classe
# ============================================
resultat_final %>%
  mutate(Prediction_en = factor(Prediction_en, levels = ordre_lits)) %>%
  ggplot() +
  aes(x = Prediction_en) +
  geom_bar(aes(fill = Prediction_en)) +
  scale_fill_manual(values = palette_lits) +
  theme_minimal() + 
  labs(y = "Number of Homogeneous River Reaches", 
       x = "Planform") +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = "none"
  )

# ============================================
# histogramme de km par classe RMC
# ============================================
resultat_final %>%
  # filter(gid_region %in% c(31, 16, 11, 33, 26)) %>%
  filter(!is.na(Prediction_en)) %>%   # enlève les NA
  mutate(Prediction_en = factor(Prediction_en, levels = ordre_lits),
         Prediction_en = fct_rev(Prediction_en)) %>%
  group_by(Prediction_en) %>%
  summarize(long = sum(sum_length)/1000) %>%
  ggplot(aes(y = long, x = Prediction_en, fill = Prediction_en)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = palette_lits) +
  theme_minimal() +
  labs(
    x = "river length (km)",
    y = NULL
  ) +
  theme(
    legend.position = "none",
    # panel.background = element_rect(fill = "transparent", colour = NA),
    # plot.background = element_rect(fill = "transparent", colour = NA),
    # panel.grid = element_blank(),
    axis.text = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 16, face = "bold")
  )

# ============================================
# histogramme de km par classe FRANCE
# ============================================
df_plot <- resultat_conf %>%
  filter(!is.na(conf_detaille)) %>%
  mutate(
    conf_detaille = factor(conf_detaille, levels = ordre_conf),
    conf_short = recode(conf_simple,
                        "confined" = "C",
                        "partly confined" = "PC",
                        "unconfined" = "U",
                        .missing = "PC"),
    x_label = conf_short
  ) %>%
  group_by(conf_detaille, x_label, Prediction_en) %>%
  summarise(long = sum(sum_length)/1000, .groups = "drop") %>%
  mutate(long = long + 1e-6)

# ajouter Anastomosing confined si absent
if (!"Anastomosed confined" %in% df_plot$conf_detaille) {
  
  df_plot <- bind_rows(
    df_plot,
    data.frame(
      conf_detaille = "Anastomosed confined",
      x_label = "C",
      Prediction_en = "Anastomosed",
      long = 0
    )
  )
}

df_plot$conf_detaille <- factor(df_plot$conf_detaille, levels = ordre_conf)

# créer position x avec espace entre groupes
df_plot <- df_plot %>%
  arrange(conf_detaille) %>%
  mutate(
    id = as.numeric(conf_detaille),
    group_id = ceiling(id / 3),
    x = id + (group_id - 1) * 1
  )

# labels groupes
group_labels <- df_plot %>%
  filter(x_label == "PC") %>%
  group_by(Prediction_en) %>%
  summarise(x = mean(x), .groups = "drop")

# séparations groupes
group_breaks <- df_plot %>%
  group_by(Prediction_en) %>%
  summarise(max_x = max(x), .groups = "drop") %>%
  slice(-n()) %>%
  mutate(x = max_x + 0.5)

ggplot(df_plot, aes(x = x, y = long, fill = conf_detaille)) +
  
  geom_col(width = 0.7) +
  
  geom_text(
    data = group_labels,
    aes(x = x, y = -max(df_plot$long)*0.1, label = Prediction_en),
    hjust = 0.5,
    angle = -10,
    size = 3,
    inherit.aes = FALSE
  ) +
  
  geom_segment(
    data = group_breaks,
    aes(x = x+0.5, xend = x+0.5,
        y = max(df_plot$long)*-0.01,
        yend = max(df_plot$long)*0.01),
    inherit.aes = FALSE,
    linewidth = 0.4
  ) +
  
  scale_fill_manual(values = palette_conf) +
  
  scale_x_continuous(
    breaks = df_plot$x,
    labels = df_plot$x_label
  ) +
  
  coord_cartesian(clip = "off") +
  
  theme_minimal() +
  labs(
    x = NULL,
    y = "river length (km)"
  ) +
  
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 10, color = "black", vjust = 25),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank()
    # plot.margin = ggplot2::margin(5.5, 60, 5.5, 5.5)
  )

# ============================================
# histogramme de km par classe FRANCE log échelle
# ============================================
df_plot <- resultat_conf %>%
  filter(!is.na(conf_detaille)) %>%
  mutate(
    conf_detaille = factor(conf_detaille, levels = ordre_conf),
    conf_short = recode(conf_simple,
                        "confined" = "C",
                        "partly confined" = "PC",
                        "unconfined" = "U",
                        .missing = "PC"),
    x_label = conf_short
  ) %>%
  group_by(conf_detaille, x_label, Prediction_en) %>%
  summarise(long = sum(sum_length)/1000, .groups = "drop") %>%
  mutate(long = long + 1e-6)

# ajouter Anastomosing confined si absent
if (!"Anastomosed confined" %in% df_plot$conf_detaille) {
  
  df_plot <- bind_rows(
    df_plot,
    data.frame(
      conf_detaille = "Anastomosed confined",
      x_label = "C",
      Prediction_en = "Anastomosed",
      long = 0
    )
  )
}

df_plot$conf_detaille <- factor(df_plot$conf_detaille, levels = ordre_conf)

# créer position x avec espace entre groupes
df_plot <- df_plot %>%
  arrange(conf_detaille) %>%
  mutate(
    id = as.numeric(conf_detaille),
    group_id = ceiling(id / 3),
    x = id + (group_id - 1) * 1
  )

# labels groupes
group_labels <- df_plot %>%
  filter(x_label == "PC") %>%
  group_by(Prediction_en) %>%
  summarise(x = mean(x), .groups = "drop")

# séparations groupes
group_breaks <- df_plot %>%
  group_by(Prediction_en) %>%
  summarise(max_x = max(x), .groups = "drop") %>%
  slice(-n()) %>%
  mutate(x = max_x + 0.5)


y_base <- 0.1 

ggplot(df_plot, aes(x = x, y = long, fill = conf_detaille)) +
  
  annotation_logticks(sides = "l", color = "black", linewidth = 0.5) +
  
  geom_col(width = 0.7) +
  
  # Échelle log corrigée
  scale_y_log10(
    breaks = scales::breaks_log(n = 6),
    labels = scales::label_log(), # <--- Correction ici
    limits = c(y_base, max(df_plot$long) * 1.5)
  ) +
  
  # LABELS : Placer à une valeur fixe en log (ex: 0.05)
  geom_text(
    data = group_labels,
    aes(x = x, y = y_base * 4.5, label = Prediction_en), # Positionné juste sous l'axe
    hjust = 0.5,
    angle = -10,
    size = 3,
    inherit.aes = FALSE
  ) +
  
  # SEGMENTS : Placer entre deux valeurs très petites
  geom_segment(
    data = group_breaks,
    aes(x = x + 0.5, xend = x + 0.5,
        y = y_base * 0.3,      # Début du segment en log
        yend = y_base * 1.5),  # Fin du segment en log
    inherit.aes = FALSE,
    linewidth = 0.4
  ) +
  
  scale_fill_manual(values = palette_conf) +
  
  scale_x_continuous(
    breaks = df_plot$x,
    labels = df_plot$x_label
  ) +
  
  # IMPORTANT : clip = "off" permet d'afficher les textes qui sortent des limites
  coord_cartesian(clip = "off") +
  
  theme_minimal() +
  labs(
    x = NULL,
    y = "river length (km)"
  ) +
  
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    # panel.grid = element_blank(),
    # --- GRILLE LOG ---
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5), # Lignes 10^x
    panel.grid.minor.y = element_line(color = "grey95", linewidth = 0.2), # Lignes intermédiaires
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    # --- AJOUT DES LIGNES ET TICKS Y ---
    axis.line.y = element_line(color = "black", linewidth = 0.5), # Ligne verticale
    axis.ticks.y = element_line(color = "black"),                # Petits traits
    axis.ticks.length.y = unit(0.1, "cm"),                      # Longueur des traits
    # Ajuster vjust pour que les lettres C, PC, U soient proches de l'axe
    axis.text.x = element_text(size = 10, color = "black", vjust = 38), 
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank()
    # Ajouter de la marge en bas pour ne pas couper les labels Prediction_en
    # plot.margin = margin(5.5, 5.5, 40, 5.5) 
  )


  #============================================
  # Graphique combiné : Longueur par Prediction_en 
  # avec segmentation par Confinement (Stacked Bar)
  # ============================================
# 1. Sommes
ttt_sum <- ttt %>%
  group_by(Prediction_en, conf_simple) %>%
  summarise(sum_length = sum(sum_length), .groups = "drop")

# 2. Total par Prediction
ttt_sum <- ttt_sum %>%
  group_by(Prediction_en) %>%
  mutate(
    total = sum(sum_length),
    prop = sum_length / total
  ) %>%
  ungroup()

# 3. log du total
ttt_sum <- ttt_sum %>%
  mutate(log_total = log10(total))

# 4. hauteur de chaque segment = proportion * log(total)
ttt_sum <- ttt_sum %>%
  mutate(height = prop * log_total)

ttt_sum <- ttt_sum %>%
  mutate(fill_group = ifelse(Prediction_en == "Reservoir",
                             "Reservoir",
                             paste(Prediction_en, conf_simple)))

ggplot(ttt_sum, aes(x = Prediction_en, y = height, fill = fill_group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = palette_conf, guide = "none") +  # supprime légende
  scale_y_continuous(
    breaks = pretty(ttt_sum$height),
    labels = function(x) parse(text = paste0("10^", round(x, 1)))
  ) +
  labs(
    x = "Prediction",
    y = "Longueur totale"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# ============================================
# Graphique combiné : Longueur par Prediction_en 
# avec segmentation par Confinement (Stacked Bar)
# ============================================
# library(ggplot2)
# library(ggbreak)
# library(dplyr)
# library(forcats)


# 1. Transformation des données (on garde la logique de compression)
df_plot <- resultat_conf %>%
  filter(!is.na(Prediction_en), !is.na(conf_detaille)) %>%
  mutate(
    Prediction_en = factor(Prediction_en, levels = ordre_lits),
    # On enlève fct_rev car en vertical l'ordre des levels suit l'axe X de gauche à droite
    conf_detaille = factor(conf_detaille, levels = ordre_conf)
  ) %>%
  group_by(Prediction_en, conf_detaille) %>%
  summarize(long = sum(sum_length) / 1000, .groups = "drop")

# Paramètres de compression
break_start <- 15000
break_end <- 40000
compression_factor <- 0.1

trans_break <- function(x) {
  ifelse(x <= break_start, x,
         ifelse(x <= break_end, 
                break_start + (x - break_start) * compression_factor,
                break_start + (break_end - break_start) * compression_factor + (x - break_end)))
}

df_plot <- df_plot %>% mutate(long_trans = trans_break(long))

ggplot(df_plot, aes(x = Prediction_en, y = long_trans, fill = conf_detaille)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.1) +
  scale_fill_manual(values = palette_conf) +
  
  # Configuration de l'axe Y (anciennement X)
  scale_y_continuous(
    breaks = trans_break(c(0, 5000, 10000, 15000, 30000)),
    labels = c("0", "5 000", "10 000", "15 000", "30 000"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  # Ajout de l'encoche // sur l'axe Y
  # x = 0.4 place l'encoche à gauche de la première barre
  annotate("text", x = 0.4, 
           y = trans_break(break_start + (break_end - break_start) / 2), 
           label = "//", size = 5, fontface = "bold", angle = 0) +
  
  theme_minimal() +
  labs(y = "river length (km)", x = NULL) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    # On inverse les grilles majeures
    panel.grid.major.y = element_line(color = "grey92"),
    panel.grid.major.x = element_blank(),
    axis.line.y = element_line(color = "black"),
    axis.text.y = element_text(size = 10),
    # Rotation des noms de routes si elles sont trop longues
    axis.text.x = element_text(size = 10, color = "black", angle = -15, hjust = 0.5)
  )


# ============================================
# camenbert pour la longueur par classe
# ============================================
resultat_final %>%
  mutate(Prediction_en = factor(Prediction_en, levels = ordre_lits)) %>%
  group_by(Prediction_en) %>%
  summarise(long = sum(sum_length, na.rm = TRUE) / 1000) %>%
  ungroup() %>%
  mutate(percent = long / sum(long) * 100) %>%
  ggplot(aes(x = "", y = percent, fill = Prediction_en)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = palette_lits) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())



# ============================================
#mélande pei chart et de donut : fonctione pas
# ============================================
library(webr)
ttt <- resultat_conf %>%
  st_drop_geometry() %>% 
  select(Prediction_en, conf_simple, sum_length) %>%
  filter(!is.na(Prediction_en) & !is.na(conf_simple) & !is.na(sum_length)) %>%
  filter(Prediction_en %in% c("Passive meandering", "Active meandering", "Sinuous")) 
head(ttt)

df <- ttt %>%
  group_by(Prediction_en, conf_simple) %>%
  summarise(value = sum(sum_length), .groups = "drop")

PieDonut(
  df, 
  aes(pies = Prediction_en, donuts = conf_simple, count = value),
  title = "Répartition des prédictions par type de confinement",
  ratioByGroup = FALSE,  # Calcule le pourcentage des donuts par rapport au total global (et non au groupe parent)
  showPieName = FALSE,   # Masque le nom de la variable "Prediction_en" au centre pour un rendu plus épuré
  explode = 1,           # Détache légèrement la première part pour la mettre en valeur (optionnel)
  r0 = 0.3,              # Rayon du trou central
  r1 = 0.9               # Rayon du cercle extérieur
)

# ============================================
# comparaison classe intra rmc planform
# ============================================
df_facet <- resultat_final %>%
  filter(gid_region %in% c(31, 16, 11, 33, 26)) %>%
  mutate(Prediction_en = factor(Prediction_en, levels = ordre_lits)) %>%
  mutate(
    gid_region_name = case_when(
      gid_region == 31 ~ "la Saône",
      gid_region == 16 ~ "la Durance",
      gid_region == 11 ~ "l'Isère",
      gid_region == 33 ~ "le Rhône hors autre bassin",
      gid_region == 26 ~ "Côtiers méditerranéens"
    )
  ) %>%
  group_by(gid_region_name, Prediction_en) %>%
  summarise(km = sum(sum_length, na.rm = TRUE) / 1000) %>%
  ungroup()

df_total <- df_facet %>%
  group_by(Prediction_en) %>%
  summarise(km = sum(km)) %>%
  mutate(gid_region_name = "Total")

df_plot <- bind_rows(df_facet, df_total)

ggplot(df_plot, aes(x = "Total", y = km, fill = Prediction_en)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = palette_lits) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~ gid_region_name, nrow = 1, strip.position = "bottom") +
  theme_minimal() +
  labs(
    y = "% of total river length (km)",
    x = NULL,
    fill = "Planform"
  ) +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

# ============================================
# comparaison classe selon variables
# ============================================
df_box <- resultat_conf %>%
  filter(Prediction_en != "Reservoir") %>%
  filter(!is.na(Prediction_en)) %>%
  mutate(mean_Slope_talweg = ifelse(mean_Slope_talweg < 0.0001, 0.0001, mean_Slope_talweg),
         mean_Slope_VB = ifelse(mean_Slope_VB < 0.0001, 0.0001, mean_Slope_VB),
         mean_ACW_star = ifelse(mean_ACW_star < 0.1, 0.1, mean_ACW_star),
         mean_elevation = ifelse(mean_elevation < 1, 1, mean_elevation),
         drainage_area = ifelse(drainage_area < 1, 1, drainage_area),
         mean_idx_water = case_when(
           mean_AC == 0 & mean_WC == 0 ~ 1,    # pas de données → ignorer pour les moyennes
           mean_WC == 0 & mean_AC > 0 ~ 0,      # vrai 0
           TRUE ~ mean_WC / mean_AC             # ratio normal
         ),
         measure = measure / 1000
  ) %>%
  select(Prediction_en, mean_Slope_talweg, mean_elevation,
         drainage_area, mean_ACW_star, mean_idx_water, measure, 
         , multi_chenaux_index, sum_length) %>%
  pivot_longer(
    cols = c(mean_Slope_talweg, mean_elevation, measure, sum_length,
             drainage_area, mean_ACW_star, mean_idx_water, multi_chenaux_index),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    variable = recode(
      variable,
      mean_Slope_talweg = "Talweg slope (%)",
      mean_elevation = "Elevation (m)",
      measure = "Distance from source (km)",
      drainage_area = "Drainage area (km²)",
      mean_ACW_star = "Normalized active channel width (m)",
      mean_idx_water = "Water index",
      sum_length = "Segment length (m)",
      multi_chenaux_index = "Multi-channel index"
    ),
    Prediction_en = factor(Prediction_en, levels = ordre_lits)
  )

ggplot(df_box, aes(x = Prediction_en, y = value)) +
  geom_boxplot(aes(color = Prediction_en),
               fill = NA,
               outlier.alpha = 0.3) +
  ggh4x::facet_wrap2(~ variable, scales = "free_y", nrow = 4) +
  scale_color_manual(values = palette_lits) +
  ggh4x::facetted_pos_scales(
    y = list(
      variable %in% c("Talweg slope (%)", "Elevation (m)", "Drainage area (km²)",
                      "Normalized active channel width (m)", "Segment length (m)",
                      "Distance from source (km)", "Multi-channel index") ~ 
        scale_y_log10(labels = scales::label_number())
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = "none"
  ) +
  labs(x = NULL, y = NULL)




# ============================================
# comparaison classe selon variable et confinement en X
# ============================================
df_box <- resultat_conf %>%
  filter(Prediction_en != "Reservoir") %>%
  filter(!is.na(Prediction_en)) %>%
  mutate(
    mean_Slope_talweg = ifelse(mean_Slope_talweg < 0.0001, 0.0001, mean_Slope_talweg),
    drainage_area = ifelse(drainage_area < 1, 1, drainage_area),
    measure = measure / 1000
  ) %>%
  select(Prediction_en, conf_simple,
         mean_Slope_talweg, drainage_area, measure) %>%
  pivot_longer(
    cols = c(mean_Slope_talweg, measure, drainage_area),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    variable = recode(
      variable,
      mean_Slope_talweg = "Talweg slope (%)",
      measure = "Distance from source (km)",
      drainage_area = "Drainage area (km²)"
    ),
    conf_simple = recode(conf_simple,
                         "confined" = "Confined",
                         "partly confined" = "Partly confined",
                         "unconfined" = "Unconfined",
    ),
    Prediction_en = factor(Prediction_en, levels = ordre_lits),
    conf_simple = factor(conf_simple)   # important
  )

ggplot(df_box, aes(x = Prediction_en, y = value)) +
  geom_boxplot(aes(color = Prediction_en),
               fill = NA,
               outlier.alpha = 0.3) +
  ggh4x::facet_grid2(
    rows = vars(variable),
    cols = vars(conf_simple),
    scales = "free_y",
    # independent = "y",
    switch = "y"
  ) +
  scale_color_manual(values = palette_lits) +
  ggh4x::facetted_pos_scales(
    y = list(
      variable %in% c("Talweg slope (%)", "Drainage area (km²)",
                      "Distance from source (km)") ~ 
        scale_y_log10(labels = scales::label_number())
    )
  ) +
  theme_minimal() +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    # axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    # axis.text.y = element_text(size = 8),
    # axis.ticks.y = element_line(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom"
  ) +
  labs(x = NULL, y = NULL, color = "Planform")


# ============================================
# comparaison classe intra rmc plandform + conf
# ============================================
df_facet <- resultat_conf %>%
  filter(!is.na(conf_detaille)) %>% # supprime les na = reservoir
  filter(gid_region %in% c(31, 16, 11, 33, 26)) %>%
  mutate(conf_detaille = factor(conf_detaille, levels = ordre_conf)) %>%
  mutate(
    gid_region_name = case_when(
      gid_region == 31 ~ "la Saône",
      gid_region == 16 ~ "la Durance",
      gid_region == 11 ~ "l'Isère",
      gid_region == 33 ~ "le Rhône hors autre bassin",
      gid_region == 26 ~ "Côtiers méditerranéens"
    )
  ) %>%
  group_by(gid_region_name, conf_detaille) %>%
  summarise(km = sum(sum_length, na.rm = TRUE) / 1000,
            .groups = "drop"
  )

df_total <- df_facet %>%
  group_by(conf_detaille) %>%
  summarise(km = sum(km)) %>%
  mutate(gid_region_name = "Total")

df_plot <- bind_rows(df_facet, df_total)

ggplot(df_plot, aes(x = "Total", y = km, fill = conf_detaille)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = palette_conf) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~ gid_region_name, nrow = 1) +
  theme_minimal() +
  labs(
    y = "% of total river length (km)",
    x = NULL,
    fill = "Planform"
  ) +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )


# ============================================
# Classif arrière plan test Drome sur ACW
# ============================================
# ALL_SUBDATA <- st_read("ALL_SUBDATA.gpkg")
# df_plot <- ALL_SUBDATA %>%
#   left_join(
#     sf::st_drop_geometry(resultat_conf) %>%
#       select(axis, ID_segment, Prediction_en),
#     by = c("axis", "ID_segment")
#   ) %>%
#   filter(toponyme == "la Drôme") %>%
#   arrange(measure_medial_axis)
cols <- c("mean_water_channel_pc", "mean_gravel_bars_pc", "mean_natural_open_pc",
          "mean_forest_pc", "mean_grassland_pc", "mean_crops_pc",
          "mean_diffuse_urban_pc", "mean_dense_urban_pc", "mean_infrastructures_pc")

resultat_conf <- resultat_conf %>%
  # filter(axis == 2000789104) %>%
  rowwise() %>%
  mutate(
    max_col = cols[which.max(c_across(all_of(cols)))]
  ) %>%
  ungroup() %>%
  mutate(
    max_col = gsub("^mean_|_pc$", "", max_col)
  )

resultat_conf <- resultat_conf %>%
  mutate(
    landcover = recode(max_col,
                       "water_channel"     = "Espace eau",
                       "gravel_bars"       = "Espace bancs",
                       "natural_open"      = "Espace naturel",
                       "forest"            = "Espace forêt",
                       "grassland"         = "Espace prairie",
                       "crops"             = "Espace culture",
                       "diffuse_urban"     = "Espace urbain diffus",
                       "dense_urban"       = "Espace urbain dense",
                       "infrastructures"   = "Espace infrastructure"    
    ),
    confinement = recode(conf_simple,
                         "confined" = "Confined",
                         "partly confined" = "Partly confined",
                         "unconfined" = "Unconfined",
                         .missing = "NA"
    )
  )


df_plot <- resultat_conf %>%
  filter(axis == 2000789104) %>%
  select(ID_segment, Prediction_en, measure, mean_AC, mean_elevation,
         confinement,landcover)

bg_df <- df_plot %>%
  mutate(
    grp = cumsum(Prediction_en != dplyr::lag(Prediction_en, default = first(Prediction_en))),
    xmin = measure,
    xmax = dplyr::lead(measure)
  ) %>%
  filter(!is.na(xmax)) %>%
  group_by(grp, Prediction_en) %>%
  summarise(
    xmin = min(xmin),
    xmax = max(xmax),
    .groups = "drop"
  )

# ggplot() +
#   geom_rect(
#     data = bg_df,
#     aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = Prediction_en),
#     alpha = 0.5
#   ) +
#   geom_line(
#     data = df_plot,
#     aes(x = measure, y = mean_AC),
#     color = "black",
#     linewidth = 1
#   ) +
#   scale_fill_manual(values = palette_lits, name = "Type de lit") +
#   theme_minimal() + 
#   labs(
#     y = "Largeur du chenal actif (m)",
#     x = "Distance depuis la source (m)"
#   )

ggplot() +
  # 1. On trace la ligne principale d'abord
  geom_line(
    data = df_plot,
    aes(x = measure, y = mean_AC),
    color = "black",
    linewidth = 1
  ) +
  # 2. On trace les rectangles en dessous
  geom_rect(
    data = bg_df,
    aes(xmin = xmin, xmax = xmax, ymin = -100, ymax = -5, fill = Prediction_en), 
    alpha = 1 # L'opacité peut être à 1 car ce n'est plus un arrière-plan
  ) +
  scale_fill_manual(values = palette_lits, name = "Type de lit") +
  theme_minimal() + 
  labs(
    y = "Largeur du chenal actif (m)",
    x = "Distance depuis la source (m)"
  ) +
  # Optionnel : Forcer les limites de l'axe Y pour bien voir le bandeau
  coord_cartesian(ylim = c(-25, max(df_plot$mean_AC, na.rm = TRUE)))






bg_pred <- df_plot %>%
  mutate(
    value = Prediction_en,
    grp = cumsum(value != dplyr::lag(value, default = first(value))),
    xmin = measure,
    xmax = dplyr::lead(measure)
  ) %>%
  filter(!is.na(xmax)) %>%
  group_by(grp, value) %>%
  summarise(
    xmin = min(xmin),
    xmax = max(xmax),
    .groups = "drop"
  )

bg_conf <- df_plot %>%
  mutate(
    value = confinement,
    grp = cumsum(value != dplyr::lag(value, default = first(value))),
    xmin = measure,
    xmax = dplyr::lead(measure)
  ) %>%
  filter(!is.na(xmax)) %>%
  group_by(grp, value) %>%
  summarise(
    xmin = min(xmin),
    xmax = max(xmax),
    .groups = "drop"
  )

bg_max <- df_plot %>%
  mutate(
    value = landcover,
    grp = cumsum(value != dplyr::lag(value, default = first(value))),
    xmin = measure,
    xmax = dplyr::lead(measure)
  ) %>%
  filter(!is.na(xmax)) %>%
  group_by(grp, value) %>%
  summarise(
    xmin = min(xmin),
    xmax = max(xmax),
    .groups = "drop"
  )

ggplot() +
  
  geom_line(
    data = df_plot,
    aes(x = measure, y = mean_elevation),
    color = "black",
    linewidth = 1
  ) +
  
  geom_rect(
    data = bg_pred,
    aes(xmin = xmin, xmax = xmax, ymin = -50, ymax = 0, fill = value),
    alpha = 1
  ) +
  
  geom_rect(
    data = bg_conf,
    aes(xmin = xmin, xmax = xmax, ymin = -110, ymax = -60, fill = value),
    alpha = 1
  ) +
  
  geom_rect(
    data = bg_max,
    aes(xmin = xmin, xmax = xmax, ymin = -170, ymax = -120, fill = value),
    alpha = 1
  ) +
  
  scale_fill_manual(values = palette_axe) +
  
  scale_x_continuous(
    labels = function(x) round(x / 1000, 1)
  )+
  
  theme_minimal() +
  labs(
    y = "??",
    x = "Distance downstream from the source (km)"
  ) 

# coord_cartesian(
#   ylim = c(0, max(df_plot$mean_elevation, na.rm = TRUE)),
#   clip = "off"   # 🔥 clé ici
# ) 
# 
# theme(
#   plot.margin = ggplot2::margin(t = 5, r = 5, b = 120, l = 5)
# )



# ============================================
# comparaison tresse confiné ve tresse confiné 
# ============================================
df_box <- resultat_conf %>%
  filter(gid_region %in% c(31, 16, 11, 33, 26)) %>%
  filter(Prediction_en %in% c("Braided")) %>%
  # mutate(
  #   gid_region_name = case_when(
  #     gid_region == 31 ~ "la Saône",
  #     gid_region == 16 ~ "la Durance",
  #     gid_region == 11 ~ "l'Isère",
  #     gid_region == 33 ~ "le Rhône hors autre bassin",
  #     gid_region == 26 ~ "Côtiers méditerranéens"
  #   )
  # ) %>%
  select(conf_detaille, mean_Slope_talweg, mean_elevation, mean_Slope_VB,
         drainage_area, mean_AC, mean_idx_water) %>%
  pivot_longer(
    cols = c(mean_Slope_talweg, mean_elevation, mean_Slope_VB,
             drainage_area, mean_AC, mean_idx_water),
    names_to = "variable",
    values_to = "value"
  )

ggplot(df_box, aes(x = conf_detaille, y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y", nrow = 2) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  labs(
    x = NULL,
    y = "Value"
  )



# ============================================
# comparaison meandre
# ============================================
df_box <- resultat_conf %>%
  filter(Prediction_en %in% c("Passive meandering", "Active meandering")) %>%
  mutate(
    Prediction_en = recode(
      Prediction_en,
      "Passive meandering" = "Passive",
      "Active meandering" = "Active"
    )
  ) %>%
  select(Prediction_en, conf_simple,
         mean_Slope_talweg, mean_elevation, mean_Slope_VB,
         drainage_area, mean_AC, mean_VB,
         measure, mean_meander_belt) %>%
  pivot_longer(
    cols = c(mean_Slope_talweg, mean_elevation, mean_Slope_VB,
             drainage_area, mean_AC, mean_VB,
             measure, mean_meander_belt),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    category = paste(Prediction_en, conf_simple, sep = " - "),
    category = factor(category, levels = c(
      "Active - confined",
      "Active - partly confined",
      "Active - unconfined",
      "Passive - confined",
      "Passive - partly confined",
      "Passive - unconfined"
    ))
  ) %>%
  mutate(
    variable = recode(
      variable,
      mean_Slope_talweg = "Talweg slope (%)",
      mean_elevation = "Elevation (m)",
      mean_Slope_VB = "Valley bottom slope (%)",
      drainage_area = "Drainage area (km²)",
      mean_AC = "Active channel width (m)",
      mean_VB = "Valley bottom width (m)",
      measure = "Distance from source (m)",
      mean_meander_belt = "Meander belt width (m)"
    )
  )


ggplot(df_box, aes(x = Prediction_en, y = value, fill = category)) +
  geom_boxplot(
    position = position_dodge(width = 0.75),
    width = 0.65,
    outlier.alpha = 0.3
  ) +
  facet_wrap(~ variable, scales = "free_y", nrow = 4) +
  theme_minimal() +
  labs(
    x = NULL,
    y = NULL,
    fill = "Confinement"
  ) +
  scale_fill_manual(
    values = c(
      "Active - confined" = "#a45006",
      "Active - partly confined" = "#e08214",
      "Active - unconfined" = "#fdb863",
      "Passive - confined" = "#074889",
      "Passive - partly confined" = "#3182be",
      "Passive - unconfined" = "#6baed6"
    )
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_y_log10(labels = scales::label_number())+
  theme(
    text = element_text(size = 16),   # taille globale
    legend.position = "top",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15),
    strip.text = element_text(size = 15, face = "bold")
  )


# ============================================
# circle transition planform France
# ============================================
tttt <- resultat_final %>%
  st_drop_geometry() %>%
  # filter(gid_region %in% c(31, 16, 11, 33, 26)) %>%
  # filter(gid_region %in% c(11)) %>%
  select(axis, ID_segment, Prediction_en) %>%
  filter(!Prediction_en %in% c("Reservoir", NA)) %>%
  arrange(axis, desc(ID_segment))

data_long <- tttt %>%
  group_by(axis) %>%
  mutate(next_pred = lead(Prediction_en)) %>%
  ungroup() %>%
  filter(!is.na(next_pred)) %>%
  filter(Prediction_en != next_pred) %>%
  count(Prediction_en, next_pred, name = "value") %>%
  group_by(Prediction_en) %>%
  mutate(value = 100 * value / sum(value)) %>%
  ungroup() %>%
  rename(from = Prediction_en, to = next_pred) %>%
  filter(value >= 10)   # ⬅️ SUPPRESSION PETITS FLUX

classes <- union(data_long$from, data_long$to)

link_cols <- palette_lits[as.character(data_long$from)]
grid_cols <- palette_lits[as.character(classes)]
# data_long$link_col <- palette_lits[data_long$from]

ragg::agg_png(
  here("transitions_chord_fr.png"),
  width = 8,
  height = 8,
  units = "in",
  res = 500,
  background = "white"
)

circos.clear()

circos.par(
  start.degree = 90,
  gap.degree = 6,
  track.margin = c(0.01, 0.01),
  cell.padding = c(0, 0, 0, 0)
)

par(
  mar = rep(0, 4),
  bg = "white"
)

chordDiagram(
  x = data_long,
  grid.col = grid_cols,
  col = link_cols,
  transparency = 0.2,
  directional = 1,
  direction.type = c("arrows", "diffHeight"),
  diffHeight = -0.05,
  link.arr.type = "big.arrow",
  link.sort = FALSE,
  link.largest.ontop = TRUE,
  annotationTrack = "grid",
  annotationTrackHeight = 0.07
)

circos.trackPlotRegion(
  track.index = 1,
  bg.border = NA,
  panel.fun = function(x, y) {
    
    sector.name = get.cell.meta.data("sector.index")
    xlim = get.cell.meta.data("xlim")
    
    circos.text(
      x = mean(xlim),
      y = 1.5,
      labels = sector.name,
      facing = "bending.inside",
      niceFacing = TRUE,
      adj = c(0.5, 0.5),
      cex = 0.7,
      font = 2
    )
  }
)

dev.off()

# library(dplyr)
# library(circlize)
# 
# tttt <- resultat_final %>%
#   st_drop_geometry() %>%
#   # filter(gid_region %in% c(31, 16, 11, 33, 26)) %>%
#   # filter(gid_region %in% c(11)) %>%
#   select(axis, ID_segment, Prediction_en) %>%
#   filter(!Prediction_en %in% c("Reservoir", NA)) %>%
#   arrange(axis, desc(ID_segment))
# 
# # --- 1. PRÉPARATION DES DONNÉES (Filtrage + Astuce visuelle 80/20 + Sécurité) ---
# data_transitions <- tttt %>%
#   arrange(axis, desc(ID_segment)) %>%
#   group_by(axis) %>%
#   mutate(to = lead(Prediction_en)) %>%
#   ungroup() %>%
#   filter(!is.na(to)) %>%
#   rename(from = Prediction_en) %>%
#   count(from, to) %>%
#   group_by(from) %>%
# 
#   # 1. On trie pour mettre le flux interne en premier, puis les plus gros flux sortants
#   arrange(from, desc(from == to), desc(n)) %>%
# 
#   # 2. On ne garde que les 5 plus gros flux par origine
#   slice_head(n = 5) %>%
# 
#   # 3. Le trick visuel SÉCURISÉ
#   mutate(
#     a_un_interne = any(from == to),         # Vérifie si la classe a un flux interne
#     sum_sortants = sum(n[from != to]),      # Calcule le total des flux sortants
# 
#     prob = case_when(
#       # Cas 1 : Que de l'interne (100% de la place)
#       from == to & sum_sortants == 0 ~ 1.0,
# 
#       # Cas 2 : Interne + Externe -> L'interne prend 80%
#       from == to & sum_sortants > 0 ~ 0.8,
# 
#       # Cas 3 : Interne + Externe -> L'externe se partage les 20% restants
#       a_un_interne & from != to ~ (n / sum_sortants) * 0.2,
# 
#       # Cas 4 : QUE de l'externe (pas d'interne du tout) -> L'externe prend 100% de la place
#       !a_un_interne & from != to ~ n / sum_sortants
#     )
#   ) %>%
#   select(from, to, prob) %>%
#   ungroup()
# 
# # --- 2. ORDRE POUR LA COLLINE SUR LE CÔTÉ ---
# df_interne <- data_transitions %>% filter(from == to)
# df_externe <- data_transitions %>% filter(from != to)
# df_final <- rbind(df_externe, df_interne)
# 
# # --- 3. CRÉATION DU GRAPHIQUE AFFINÉ ---
# png("chord_morpho_thin_arrows_final.png", width = 2500, height = 2500, res = 300)
# 
# circos.clear()
# circos.par(start.degree = 90, gap.degree = 4)
# 
# chordDiagram(df_final,
#              grid.col = palette_lits,
#              directional = 1,
#              direction.type = "arrows",
#              link.arr.type = "big.arrow",
#              link.lwd = 0.1,               # Traits fins
#              self.link = 1,
#              link.sort = FALSE,
#              h.ratio = 0.7,
#              transparency = 0.5,           # Plus transparent pour alléger
#              annotationTrack = c("name", "grid", "axis"),
#              annotationTrackHeight = c(0.06, 0.05))
# 
# dev.off()
# circos.clear()

# ============================================
# comparaison region selon propriétés 
# ============================================
df_box <- resultat_conf %>%
  filter(gid_region %in% c(31, 16, 11, 33, 26)) %>%
  mutate(
    gid_region_name = case_when(
      gid_region == 31 ~ "la Saône",
      gid_region == 16 ~ "la Durance",
      gid_region == 11 ~ "l'Isère",
      gid_region == 33 ~ "le Rhône hors autre bassin",
      gid_region == 26 ~ "Côtiers méditerranéens"
    )
  ) %>%
  select(gid_region_name, mean_Slope_talweg, mean_elevation, mean_Slope_VB) %>%
  pivot_longer(
    cols = c(mean_Slope_talweg, mean_elevation, mean_Slope_VB),
    names_to = "variable",
    values_to = "value"
  )

ggplot(df_box, aes(x = gid_region_name, y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y", nrow = 1) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  labs(
    x = NULL,
    y = "Value"
  )

# ============================================
# comparaison classe selon conintuité lateral
# ============================================
resultat_continuite <- resultat_conf %>%
  st_drop_geometry() %>%   # ✅ sans argument
  filter(gid_region %in% c(31, 16, 11, 33, 26)) %>%
  filter(Prediction_en != "reservoir") %>%
  filter(!is.na(Prediction_en)) %>%
  group_by(Prediction_en) %>%
  summarize(
    # Espace_eau = mean(mean_water_channel_pc, na.rm = TRUE),
    Espace_bancs = mean(mean_gravel_bars_pc, na.rm = TRUE),
    Espace_naturel = mean(mean_riparian_corridor_pc, na.rm = TRUE),
    Espace_semi_naturel = mean(mean_semi_natural_pc, na.rm = TRUE),
    Espace_agricole_connecté = mean(mean_reversible_pc, na.rm = TRUE),
    Espace_déconnecté = mean(mean_disconnected_pc_corrige, na.rm = TRUE),
    Espace_artificialisé = mean(mean_built_environment_pc, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = starts_with("Espace_"),
    names_to = "classe",
    values_to = "surface"
  ) %>%
  mutate(
    classe = factor(classe, levels = ordre_continuité),
    Prediction_en = factor(Prediction_en),
    surface = ifelse(surface < 0, 0, surface)
  )

ggplot(resultat_continuite, aes(x = Prediction_en, y = surface, fill = classe)) +
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


# ============================================
# comparaison classe selon conintuité lateral
# ============================================
resultat_landuse <- resultat_conf %>%
  st_drop_geometry() %>%   # ✅ sans argument
  filter(gid_region %in% c(31, 16, 11, 33, 26)) %>%
  filter(Prediction_en != "reservoir") %>%
  group_by(Prediction_en) %>%
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
  pivot_longer(
    cols = starts_with("Espace_"),
    names_to = "classe",
    values_to = "surface"
  ) %>%
  mutate(
    classe = factor(classe, levels = ordre_landuse),
    Prediction_en = factor(Prediction_en),
    surface = ifelse(surface < 0, 0, surface)
  )

ggplot(resultat_landuse, aes(x = Prediction_en, y = surface, fill = classe)) +
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



# gaph length prediction
df_bar <- resultat_conf %>%
  filter(!is.na(Prediction_en), !is.na(conf_detaille)) %>%
  select(Prediction_en, conf_simple, conf_detaille, sum_length)%>%
  mutate(
    sum_length = sum_length /1000,  # pour éviter les problèmes de log(0)
  )%>%
  mutate(
    conf_simple = if_else(
      Prediction_en %in% c("Reservoir","Intermittent"),
      NA_character_,   # ⚠️ important : type caractère
      conf_simple
    )
  )

df_agg <- df_bar %>%
  st_drop_geometry() %>%   # IMPORTANT (sinon bugs ggplot)
  group_by(Prediction_en, conf_simple) %>%
  summarise(km = sum(sum_length), .groups = "drop") %>%
  mutate(
    Prediction_en = factor(Prediction_en, levels = ordre_lits)
  )

totaux <- df_agg %>%
  group_by(Prediction_en) %>%
  summarise(total = sum(km), .groups = "drop") %>%
  mutate(
    Prediction_en = factor(Prediction_en, levels = ordre_lits)
  ) %>%
  arrange(Prediction_en) %>%   # 👈 C'EST ÇA QUI CONTRÔLE L'AXE X
  mutate(
    xmin = lag(cumsum(total), default = 0),
    xmax = xmin + total,
    xmid = (xmin + xmax) / 2
  )

df_plot <- df_agg %>%
  left_join(totaux, by = "Prediction_en") %>%
  group_by(Prediction_en) %>%
  mutate(prop = km / total) %>%
  arrange(conf_simple, .by_group = TRUE) %>%
  mutate(
    ymin = lag(cumsum(prop), default = 0),
    ymax = ymin + prop
  ) %>%
  ungroup()


totaux_labels <- totaux %>%
  arrange(xmid) %>%
  mutate(
    y_label = xmid
  )

espacement <- 0.03 * max(totaux_labels$xmax)


# 👉 correction simple anti-overlap
for (i in (nrow(totaux_labels)-1):1) {
  if (totaux_labels$y_label[i+1] - totaux_labels$y_label[i] < espacement) {
    totaux_labels$y_label[i] <- totaux_labels$y_label[i+1] - espacement
  }
}

ggplot(df_plot) +
  geom_rect(aes(
    ymin = xmin, ymax = xmax,
    xmin = ymin, xmax = ymax,
    fill = conf_simple
  ),
  color = "grey30", linewidth = 0.3) +
  
  # 👉 segments de liaison (propre)
  geom_segment(
    data = totaux_labels,
    aes(
      x = 1,
      xend = 1.03,
      y = xmid,
      yend = y_label
    ),
    linewidth = 0.4,
    color = "black"
  ) +
  
  # 👉 labels décalés sans overlap
  geom_text(
    data = totaux_labels,
    aes(
      x = 1.04,
      y = y_label,
      label = Prediction_en
    ),
    hjust = 0,
    size = 5
  ) +
  
  scale_fill_manual(
    values = c(
      "confined" = "#FFA02E",
      "partly confined" = "#FFEF91",
      "unconfined" = "#9AD872"
    ),
    name = "Confinement"
  ) +
  
  scale_y_continuous(
    name = "River length (km)",
    expand = c(0, 0.5),
    breaks = scales::breaks_pretty(n = 5),
    labels = scales::label_number(big.mark = " ")
  ) +
  
  scale_x_continuous(
    name = NULL,
    labels = scales::percent_format(accuracy = 1),
    # limits = c(0, 1.15),
    expand = expansion(mult = c(0, 0))  # 👈 petit espace à droite
  ) +
  
  coord_cartesian(clip = "off") +
  
  
  theme_classic(base_size = 17) +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_line(color = "black"),
    # panel.grid.major.y = element_line(color = "grey85"),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    plot.margin = ggplot2::margin(10, 150, 10, 10)
    
  )





df_nuage <- resultat_conf %>%
  filter(Prediction_en %in% c("Braided", "Wandering",
                              "Passive meandering",
                              "Alternate bars",
                              "Sinuous with bars",
                              "Anabranching",
                              "Anastomosed",
                              "Active meandering"
  ),
  !(Prediction_en == "Braided" & !gid_region %in% c(31, 16, 11, 33, 26)),
  # conf_simple %in% c("unconfined")
  ) %>%
  mutate(drainage_area = drainage_area,
         drainage_area = ifelse(drainage_area < 1, 1, drainage_area)) %>%
  mutate(
    mean_Slope_talweg = ifelse(mean_Slope_talweg < 0.0001, 0.0001, mean_Slope_talweg))

# st_write(df_nuage, "df_nuage1.gpkg")

ggplot(df_nuage, aes(x = drainage_area, y = mean_Slope_talweg, color = Prediction_en)) +
  
  # geom_point(
  #   size = 1.8,
  #   alpha = 0.5,
  #   stroke = 0
  # ) +
  
  geom_point(
    size = 1.5,
    alpha = 0.4,
    stroke = 0
  )+
  
  # # 👉 Tendance (très important pour Nature)
  # geom_smooth(
  #   method = "loess",
  #   se = TRUE,
  #   linewidth = 0.8,
  #   alpha = 0.15
  # ) +
  
  scale_x_log10(
    breaks = scales::breaks_log(n = 5),
    labels = scales::label_number(accuracy = 1)
  )+
  
  scale_y_log10(
    breaks = scales::breaks_log(n = 5),
    labels = scales::label_number(accuracy = 1)
  )+
  
  scale_color_manual(values = c(
    "Braided" = "#e69f00",
    "Wandering" = "#b012d4",
    "Active meandering" = "#080867",
    "Passive meandering" = "#009e73",
    "Anabranching" = "#56b4e9",
    "Anastomosed" = "#d55e00",
    "Alternate bars" = "#0072b2",
    "Sinuous with bars" = "#cc79a7"
  )) +
  
  # 👉 Labels propres (unités !)
  labs(
    x = "Drainage area (km²)",
    y = "Active channel width (m)",
    color = NULL
  ) +
  
  geom_density_2d(linewidth = 0.4)+  
  # 👉 Thème publication
  theme_classic(base_size = 15) +
  theme(
    # legend.position = "top",
    
    legend.text = element_text(size = 15),
    
    axis.text = element_text(color = "black"),
    axis.title = element_text(size = 20),
    
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank()
  )



