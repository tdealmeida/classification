library(circlize)
library(forcats)
library(ggtext)
library(ggh4x)


"#2d2d2d"
"#656565"
"#b1b1b1"

"#bb0e0e"
"#a9a904"
"#0b5707"


palette_lits <- c(
  "Straight" = "#4988C4",         
  "Sinuous" = "#1C4D8D",           
  "Passive meandering" = "#807dba", 
  "Alternate bars" = "#b3cde3",    
  "Sinuous with bars" = "#629FAD",  
  "Active meandering" = "#542788",  
  "Braided" = "#e69f00",
  "Wandering" = "#b012d4",
  "Anastomosed" = "#b2df8a",
  "Anabranching" = "#33a02c",
  "Reservoir" = "#9e9e9e",
  "Intermittent" = "#363232",
  "NA" = "black"
)

palette_conf <- c(
  "Straight confined"        = "#2F5D8A",
  "Straight partly confined" = "#4988C4",
  "Straight unconfined"      = "#A9C7E8",
  
  "Sinuous confined"        = "#12345F",
  "Sinuous partly confined" = "#1C4D8D",
  "Sinuous unconfined"      = "#7FA6D6",
  
  "Passive meandering confined"        = "#54528A",
  "Passive meandering partly confined" = "#807dba",
  "Passive meandering unconfined"      = "#B7B5DC",
  
  "Alternate bars confined"        = "#7F9FB8",
  "Alternate bars partly confined" = "#b3cde3",
  "Alternate bars unconfined"      = "#E3EEF7",
  
  "Sinuous with bars confined"        = "#41707A",
  "Sinuous with bars partly confined" = "#629FAD",
  "Sinuous with bars unconfined"      = "#B5D3DA",
  
  "Active meandering confined"        = "#381A5C",
  "Active meandering partly confined" = "#542788",
  "Active meandering unconfined"      = "#A992C9",
  
  "Braided confined"        = "#A67000",
  "Braided partly confined" = "#e69f00",
  "Braided unconfined"      = "#F3C766",
  
  "Wandering confined"        = "#7A0D94",
  "Wandering partly confined" = "#b012d4",
  "Wandering unconfined"      = "#D580EB",
  
  "Anastomosed confined"        = "#789B5E",
  "Anastomosed partly confined" = "#b2df8a",
  "Anastomosed unconfined"      = "#D9F0C2",
  
  "Anabranching confined"        = "#236E1E",
  "Anabranching partly confined" = "#33a02c",
  "Anabranching unconfined"      = "#8FD18A",
  
  "Reservoir" = "#9e9e9e"
)

ordre_conf <- c(
  "Straight confined",
  "Straight partly confined",
  "Straight unconfined",
  
  "Sinuous confined",
  "Sinuous partly confined",
  "Sinuous unconfined",
  
  "Passive meandering confined",
  "Passive meandering partly confined",
  "Passive meandering unconfined",
  
  "Anabranching confined",
  "Anabranching partly confined",
  "Anabranching unconfined",
  
  "Alternate bars confined",
  "Alternate bars partly confined",
  "Alternate bars unconfined",
  
  "Sinuous with bars confined",
  "Sinuous with bars partly confined",
  "Sinuous with bars unconfined",
  
  "Active meandering confined",
  "Active meandering partly confined",
  "Active meandering unconfined",
  
  "Wandering confined",
  "Wandering partly confined",
  "Wandering unconfined",
  
  "Braided confined",
  "Braided partly confined",
  "Braided unconfined",
  
  "Anastomosed confined",
  "Anastomosed partly confined",
  "Anastomosed unconfined",
  
  "Reservoir"
)

ordre_meandering <- c(
  "Active meandering confined",
  "Active meandering partly confined",
  "Active meandering unconfined",
  
  "Passive meandering confined",
  "Passive meandering partly confined",
  "Passive meandering unconfined"
)


palette_meandering <- c(
  "Active – confined" = "#a20000",          # orange rouge (fort)
  "Active – partly confined" = "#D55E00",   # orange
  "Active – unconfined" = "#009E73",        # vert
  
  "Passive – confined" = "#f64541",         # 👈 rouge pâle (fix)
  "Passive – partly confined" = "#F6D55C",  # jaune doux
  "Passive – unconfined" = "#7FCDBB"        # vert clair
)


ordre_lits <- c(
  "Straight",
  "Sinuous",
  "Passive meandering",
  "Anastomosed",
  "Alternate bars",
  "Sinuous with bars",
  "Active meandering",
  "Wandering",
  "Braided",
  "Anabranching",
  "Reservoir",
  "Intermittent"
)

palette_axe <- c(
  "Straight" = "#4988C4",
  "Sinuous" = "#1C4D8D",           
  "Passive meandering" = "#807dba", 
  "Alternate bars" = "#b3cde3",    
  "Sinuous with bars" = "#629FAD", 
  "Active meandering" = "#542788",  
  "Braided" = "#e69f00",
  "Wandering" = "#b012d4",
  "Anastomosed" = "#b2df8a",
  "Anabranching" = "#33a02c",
  "Reservoir" = "#9e9e9e",
  "Confined" = "#bb0e0e",
  "Partly confined" = "#a9a904",
  "Unconfined" = "#0b5707",
  "Espace eau" = "#ccdaeb",
  "Espace bancs" = "#e2e2e2",
  "Espace naturel" = "#daf188",
  "Espace forêt" = "#adc86e",
  "Espace prairie" = "#ffefa1",
  "Espace culture" = "#ffffc5",
  "Espace urbain diffus" = "#fcb4b9",
  "Espace urbain dense" = "#fc7982",
  "Espace infrastructures" = "#fc7aa7"
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

ordre_bassins <- c(
  "France",
  "RMC (f)",
  "Adour - Garonne (e)",
  "Loire - Bretagne (d)",
  "Seine - Normandie (c)",
  "Rhin - Meuse (b)",
  "Artois - Picardie (a)"
)

labels_wrap <- c(
  "RMC (f)" = "RMC (f)",
  "Loire - Bretagne (d)" = "Loire -\nBretagne (d)",
  "Seine - Normandie (c)" = "Seine -\nNormandie (c)",
  "Adour - Garonne (e)" = "Adour -\nGaronne (e)",
  "Rhin - Meuse (b)" = "Rhin -\nMeuse (b)",
  "Artois - Picardie (a)" = "Artois -\nPicardie (a)",
  "France" = "France"
)

# ============================================
# Longueur par Prediction_en avec segmentation par Confinement 1000x750
# ============================================
df_bar <- resultat_conf %>%
  filter(!is.na(Prediction_en), !is.na(conf_detaille)) %>%
  select(Prediction_en, conf_simple, conf_detaille, sum_length) %>%
  mutate(
    sum_length = sum_length / 1000
  ) %>%
  mutate(
    conf_simple = if_else(
      Prediction_en %in% c("Reservoir", "Intermittent"),
      NA_character_,
      conf_simple
    )
  )


# AGREGATION
df_agg <- df_bar %>%
  st_drop_geometry() %>%
  group_by(Prediction_en, conf_simple) %>%
  summarise(
    km = sum(sum_length, na.rm = TRUE),
    .groups = "drop"
  )

# ORDRE PAR FREQUENCE
ordre_freq <- df_agg %>%
  group_by(Prediction_en) %>%
  summarise(
    total_km = sum(km),
    .groups = "drop"
  ) %>%
  
  mutate(
    Prediction_en = as.character(Prediction_en)
  ) %>%
  
  filter(
    !Prediction_en %in% c("Reservoir", "Intermittent")
  ) %>%
  
  arrange(desc(total_km)) %>%
  
  pull(Prediction_en)

# ajouter en dessous
ordre_freq <- c(
  "Intermittent",
  "Reservoir",
  ordre_freq
)

# APPLIQUER ORDRE
df_agg <- df_agg %>%
  mutate(
    Prediction_en = factor(
      as.character(Prediction_en),
      levels = ordre_freq
    )
  )


# TOTAUX
totaux <- df_agg %>%
  group_by(Prediction_en) %>%
  summarise(
    total = sum(km),
    .groups = "drop"
  ) %>%
  
  arrange(match(as.character(Prediction_en), ordre_freq)) %>%
  
  mutate(
    xmin = lag(cumsum(total), default = 0),
    xmax = xmin + total,
    xmid = (xmin + xmax) / 2
  )

# DONNEES PLOT
df_plot <- df_agg %>%
  left_join(totaux, by = "Prediction_en") %>%
  
  group_by(Prediction_en) %>%
  
  mutate(
    prop = km / total
  ) %>%
  
  arrange(conf_simple, .by_group = TRUE) %>%
  
  mutate(
    ymin = lag(cumsum(prop), default = 0),
    ymax = ymin + prop
  ) %>%
  
  ungroup()


# LABELS
totaux_labels <- totaux %>%
  arrange(xmid) %>%
  mutate(
    y_label = xmid
  )

espacement <- 0.03 * max(totaux_labels$xmax)

for (i in (nrow(totaux_labels)-1):1) {
  
  if (
    totaux_labels$y_label[i+1] -
    totaux_labels$y_label[i] < espacement
  ) {
    
    totaux_labels$y_label[i] <-
      totaux_labels$y_label[i+1] - espacement
  }
}


# PLOT
ggplot(df_plot) +
  
  geom_rect(
    aes(
      ymin = xmin,
      ymax = xmax,
      xmin = ymin,
      xmax = ymax,
      fill = conf_simple
    ),
    color = "grey30",
    linewidth = 0.3
  ) +
  
  # segments
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
  
  # labels
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
    expand = expansion(mult = c(0, 0))
  ) +
  
  coord_cartesian(clip = "off") +
  
  theme_classic(base_size = 17) +
  
  theme(
    axis.line = element_blank(),
    axis.ticks = element_line(color = "black"),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    plot.margin = ggplot2::margin(10, 150, 10, 10)
  )



# ============================================
# comparaison classe par region fr 500x1000
# ============================================
df_facet <- resultat_final %>%
  filter(!is.na(Prediction_en), !is.na(gid_region)) %>%
  
  mutate(
    gid_bassin = case_when(
      gid_region %in% c(31, 16, 11, 33, 26) ~ "RMC (f)",
      gid_region %in% c(23, 30, 15, 27, 29) ~ "Loire - Bretagne (d)",
      gid_region %in% c(22, 25, 20, 18) ~ "Seine - Normandie (c)",
      gid_region %in% c(24, 21, 10, 14) ~ "Adour - Garonne (e)",
      gid_region %in% c(28, 32) ~ "Rhin - Meuse (b)",
      gid_region %in% c(19, 12) ~ "Artois - Picardie (a)",
      TRUE ~ NA_character_
    )
  ) %>%
  
  # 👉 🔥 AJOUT IMPORTANT
  filter(!is.na(gid_bassin)) %>%
  
  mutate(gid_bassin = factor(gid_bassin, levels = ordre_bassins)) %>% 
  group_by(gid_bassin, Prediction_en) %>%
  summarise(km = sum(sum_length, na.rm = TRUE) / 1000, .groups = "drop")

df_total <- df_facet %>%
  group_by(Prediction_en) %>%
  summarise(km = sum(km)) %>%
  mutate(gid_bassin = "France")

df_plot <- bind_rows(df_facet, df_total)

df_plot <- df_plot %>%
  mutate(
    gid_bassin = factor(gid_bassin, levels = ordre_bassins),
    Prediction_en = factor(Prediction_en, levels = ordre_lits)
  )

# df_plot$gid_bassin <- ifelse(
#   df_plot$gid_bassin == "Total",
#   "Total*",
#   df_plot$gid_bassin
# )


p1_base <- ggplot(df_plot, aes(x = km, y = gid_bassin, fill = Prediction_en)) +
  
  geom_col(
    position = "fill",
    color = "black",
    linewidth = 0.2
  ) +
  
  scale_fill_manual(values = palette_lits) +
  
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  scale_y_discrete(
    position = "right",
    labels = labels_wrap
  ) +  
  
  labs(
    x = "Proportion of total river length",
    y = NULL,
    fill = "Planform"
  ) +
  
  theme_classic(base_size = 14) +
  
  theme(
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    
    axis.line.y = element_blank(),
    axis.line.x = element_line(linewidth = 0.4),
    
    axis.text.y = element_text(
      # face = "bold",
      size = 18
      # margin = margin(r = 8)
    ),
    axis.text.x = element_text(
      # face = "bold",
      size = 18
      # margin = margin(r = 8)
    ),
    plot.margin = ggplot2::margin(10, 10, 10, 20)
    
  )

p1_no_legend <- p1_base +
  theme(legend.position = "none")


p1_with_legend <- p1_base +
  guides(
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE,
      title.position = "top"
    )
  ) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.direction = "horizontal",
    legend.key.height = unit(0.5, "cm"),
    legend.text = element_text(size = 8)
  )

p1_no_legend
p1_with_legend

# ============================================
# comparaison classe meandering France 400x1000
# ============================================
df_facet <- resultat_conf %>%
  filter(!is.na(Prediction_en)) %>%
  filter(Prediction_en %in% c("Passive meandering", "Active meandering")) %>%
  # mutate(Prediction_en = factor(Prediction_en, levels = ordre_meandering)) %>%
  group_by(conf_detaille) %>%
  summarise(km = sum(sum_length, na.rm = TRUE) / 1000, .groups = "drop")


df_facet <- df_facet %>%
  mutate(
    conf_detaille = recode(conf_detaille,
                           "Active meandering confined" = "Active – confined",
                           "Active meandering partly confined" = "Active – partly confined",
                           "Active meandering unconfined" = "Active – unconfined",
                           "Passive meandering confined" = "Passive – confined",
                           "Passive meandering partly confined" = "Passive – partly confined",
                           "Passive meandering unconfined" = "Passive – unconfined"
    )
  )

df_facet$conf_detaille <- factor(
  df_facet$conf_detaille,
  levels = c(
    "Active – confined",
    "Active – partly confined",
    "Active – unconfined",
    "Passive – confined",
    "Passive – partly confined",
    "Passive – unconfined"
  )
)

p2_base <- ggplot(df_facet, aes(x = "Total", y = km, fill = conf_detaille)) +
  
  geom_col(
    position = "fill",
    width = 0.6,
    color = "black",
    linewidth = 0.2
  ) +
  
  scale_fill_manual(
    values = palette_meandering,
    breaks = names(palette_meandering),
    drop = FALSE
  )+
  
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0))
  ) +
  
  labs(
    y = "Proportion of total river length",
    x = NULL,
    fill = "Meandering channels"
  ) +
  
  theme_classic(base_size = 20) +
  
  # guides(
  #   fill = guide_legend(
  #     nrow = 2,
  #     byrow = TRUE,
  #     title.position = "top"
  #   )
  # ) +
  
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    
    # grille propre
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    
    # légende clean
    # legend.position = "top",
    # legend.title = element_text(face = "bold"),
    # legend.direction = "horizontal",
    # legend.key.height = unit(0.5, "cm"),
    # legend.text = element_text(size = 8)
    
    # 👉 IMPORTANT pour séparation avec p1
    # plot.margin = margin(10, 10, 10, 40)
  )

p2_no_legend <- p2_base +
  theme(legend.position = "none")

# ajout légende
p2_with_legend <- p2_base +
  guides(
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE,
      title.position = "top"
    )
  ) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.direction = "horizontal",
    legend.key.height = unit(0.5, "cm"),
    legend.text = element_text(size = 8)
  )

p2_no_legend
p2_with_legend


# ============================================
# combinaison
# ============================================
library(patchwork)

(p1_no_legend | plot_spacer() | p2_no_legend) +
  plot_layout(widths = c(5, 1, 1))


#==========================================
# comparaison classe selon variable et confinement en Y 1000x600
# ============================================
df_box <- resultat_conf %>%
  st_drop_geometry() %>%
  filter(    
    Prediction_en != "Reservoir",
    Prediction_en != "Intermittent",
    !(Prediction_en == "Braided" & !gid_region %in% c(31, 16, 11, 33, 26))
    ) %>%
  filter(!is.na(Prediction_en)) %>%
  mutate(
    mean_Slope_talweg = ifelse(mean_Slope_talweg < 0.0001, 0.0001, mean_Slope_talweg),
    drainage_area = ifelse(drainage_area < 1, 1, drainage_area),
    # measure = measure,
    # distance depuis la source
  ) %>%
  group_by(axis) %>%
  mutate( 
    longueur_totale = sum(longueur_data, na.rm = TRUE),
    measure_inverse = (longueur_totale - measure) ,
    measure_max = max(measure, na.rm = TRUE) - measure, 
    measure = measure_max / 1000
  ) %>%
  ungroup() %>%
  # filter(axis == "2000784404") %>%
  # select(axis, measure, measure_inverse, measure_max,longueur_data)
  
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

# 👉 calcul distance à la médiane (par groupe)
df_box2 <- df_box %>%
  group_by(Prediction_en, conf_simple, variable) %>%
  mutate(
    med = median(value, na.rm = TRUE),
    dist_med = abs(value - med),
    alpha_val = scales::rescale(dist_med, to = c(1, 0.4))  # proche médiane = opaque
  ) %>%
  ungroup()

ggplot(df_box2, aes(x = Prediction_en, y = value)) +
  
  # 👉 BOXPLOT propre avec notch
  geom_boxplot(
    aes(color = Prediction_en),
    width = 0.5,
    # notch = TRUE,
    linewidth = 0.6,
    fill = NA,
    outlier.shape = NA
  ) +
  
  # 👉 POINTS (info clé)
  # geom_jitter(
  #   aes(color = Prediction_en, alpha = alpha_val),
  #   width = 0.15,
  #   size = 1,
  #   stroke = 0
  # ) +
  
  ggh4x::facet_grid2(
    rows = vars(conf_simple),
    cols = vars(variable),
    scales = "free_y",
    independent = "y",
    switch = "y"
  ) +
  
  scale_color_manual(values = palette_lits) +
  scale_alpha_identity() +
  
  ggh4x::facetted_pos_scales(
    y = list(
      variable %in% c("Talweg slope (%)", "Drainage area (km²)",
                      "Distance from source (km)") ~ 
        scale_y_log10(labels = scales::label_number())
    )
  ) +
  
  labs(x = NULL, y = NULL) +
  
  theme_classic(base_size = 11) +
  

  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(size = 10, face = "bold"),
    
    axis.text.x = element_text(angle = -45, hjust = 0, size = 9),
    axis.text.y = element_text(size = 8),
    
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    
    panel.spacing = unit(1, "lines"),
    
    legend.position = "none"
  )

# ============================================
# circle transition planform France
# ============================================
tttt <- resultat_final %>%
  st_drop_geometry() %>%
  # filter(gid_region %in% c(31, 16, 11, 33, 26)) %>%
  # filter(gid_region %in% c(11)) %>%
  filter(!Prediction_en %in% c("Reservoir","Intermittent", NA),
         !(Prediction_en == "Braided" & !gid_region %in% c(31, 16, 11, 33, 26))
  ) %>%
  select(axis, ID_segment, Prediction_en) %>%
  arrange(axis, desc(ID_segment))
# --- 1 & 2. CALCUL DES DONNÉES (IDEM) ---
data_counts <- tttt %>%
  arrange(axis, desc(ID_segment)) %>%
  group_by(axis) %>%
  mutate(to = lead(Prediction_en)) %>%
  ungroup() %>%
  filter(!is.na(to)) %>%
  rename(from = Prediction_en) %>%
  filter(from != to) %>% 
  count(from, to)

total_sortant <- data_counts %>% group_by(from) %>% summarize(sum_out = sum(n))
total_entrant <- data_counts %>% group_by(to) %>% summarize(sum_in = sum(n))

df_final <- data_counts %>%
  left_join(total_sortant, by = "from") %>%
  left_join(total_entrant, by = "to") %>%
  mutate(
    prop_out = n / sum_out,
    prop_in  = n / sum_in
  ) %>%
  select(from, to, prop_out, prop_in)

# --- 3. GESTION DES COULEURS (FORÇAGE PAR NOM) ---
# On crée le vecteur de couleurs en s'assurant de la correspondance exacte des noms
link_cols <- sapply(1:nrow(df_final), function(i) {
  
  # 1. On récupère le nom du départ
  cat_depart <- as.character(df_final$from[i])
  
  # 2. On extrait la couleur de la palette qui porte ce NOM précis
  # (Si palette_lits["Straight"] est bleu, alors base_color sera bleu)
  base_color <- palette_lits[cat_depart]
  
  # 3. Application de la transparence selon ton critère d'épaisseur
  # Si la flèche est fine (ex: < 5% au départ ET à l'arrivée), on l'invisibilise
  if(df_final$prop_out[i] < 0.1 & df_final$prop_in[i] < 0.1) {
    return(add_transparency(base_color, 0.9)) # Quasi invisible
  } else {
    return(add_transparency(base_color, 0.4))  # Transparence standard
  }
})

# --- 4. SECTEURS ET PLOT ---
all_sectors <- unique(c(df_final$from, df_final$to))
xmax <- setNames(rep(1, length(all_sectors)), all_sectors)

png("chord_final_fixed_colors.png", width = 2000, height = 2000, res = 300)

circos.clear()
circos.par(start.degree = 90, gap.degree = 6, track.margin = c(0.01, 0.01))

chordDiagram(df_final,
             grid.col = palette_lits, 
             col = link_cols,         # Les couleurs des flèches sont maintenant forcées
             directional = 1,
             direction.type = c("diffHeight", "arrows"),
             link.arr.type = "big.arrow",
             h.ratio = 0.75,
             diffHeight = mm_h(2),
             link.border = NA,
             xmax = xmax,
             link.sort = TRUE,        # Les flèches opaques passent devant
             annotationTrack = c("name", "grid", "axis"),
             annotationTrackHeight = c(0.03, 0.01, 0.01))

dev.off()
circos.clear()


# ============================================
# Surrogate tree à partir du jeu test du RF
# ============================================
# TRAIN (80%)
surrogate_train <- train %>%
  mutate(
    rf_pred = predict(modele_foret, newdata = train),
    rf_pred = dplyr::recode(
      rf_pred,
      "rectiligne"      = "Straight",
      "rectiligne bars" = "Alternate bars",
      "sinueux"         = "Sinuous",
      "sinueux bars"    = "Sinuous with bars",
      "meandre actif"   = "Active meandering",
      "meandre passif"  = "Passive meandering",
      "tresse"          = "Braided",
      "divagant"        = "Wandering",
      "anastomose"      = "Anastomosed",
      "anabranche"      = "Anabranching",
      "reservoir"       = "Reservoir",
      "intermittent"    = "Intermittent"
    )
  ) %>%
  # dplyr::filter(!rf_pred %in% c("Reservoir", "Intermittent")) %>%
  select(-label) %>%
  dplyr::rename(
    "Water index" = idx_water_segment,
    "Sinuosity index" = Sinuosity_meander_2,
    "Normalized active channel width" = mean_ACW_star,
    "Vegetated islands" = iles_veget,
    "Water channel width" = mean_WC,
    "Multi-threading index" = multi_chenaux_index,
    "Delta water channel" = step_WC
  )

# TEST (20%)
surrogate_test <- testset %>%
  mutate(
    rf_pred = predict(modele_foret, newdata = testset),
    rf_pred = dplyr::recode(
      rf_pred,
      "rectiligne"      = "Straight",
      "rectiligne bars" = "Alternate bars",
      "sinueux"         = "Sinuous",
      "sinueux bars"    = "Sinuous with bars",
      "meandre actif"   = "Active meandering",
      "meandre passif"  = "Passive meandering",
      "tresse"          = "Braided",
      "divagant"        = "Wandering",
      "anastomose"      = "Anastomosed",
      "anabranche"      = "Anabranching",
      "retenue"         = "Reservoir",
      "intermittent"    = "Intermittent"
    )
  ) %>%
  # dplyr::filter(!rf_pred %in% c("Reservoir", "Intermittent")) %>%
  dplyr::rename(
    "Water index" = idx_water_segment,
    "Sinuosity index" = Sinuosity_meander_2,
    "Normalized active channel width" = mean_ACW_star,
    "Vegetated islands" = iles_veget,
    "Water channel width" = mean_WC,
    "Multi-threading index" =  multi_chenaux_index,
    "Delta water channel" = step_WC
  )

# entraînement surrogate
surrogate_tree_1 <- rpart(
  rf_pred ~ .,  # 👉 on apprend le RF
  data = surrogate_train,
  method = "class",
  control = rpart.control(
    maxdepth = 7,     # 👉 arbre simple = interprétable
    minsplit = 30,
    cp = 0.001
  )
)

ragg::agg_png(
  here("a.png"),
  width = 10,
  height = 8,
  units = "in",
  res = 500,
  background = "white"
)

rpart.plot(
  surrogate_tree_1,
  type = 0,
  # branch = 1,
  fallen.leaves = TRUE,
  box.palette = 0,
  nn = FALSE,
  extra = 0,
  shadow.col = NA,
  branch.col = "black",
  col = "black",
  border.col = "black"
  # varlen = 0
)

dev.off()


# prédiction surrogate sur RF du test
surrogate_pred_test <- predict(
  surrogate_tree_1,
  surrogate_test,
  type = "class"
)

conf_fidelity_1 <- confusionMatrix(
  surrogate_pred_test,
  surrogate_test$rf_pred
)

# conf_fidelity_1 <- confusionMatrix(
#   factor(surrogate_pred_test),
#   factor(surrogate_test$rf_pred)
# )

print(conf_fidelity_1)

cat("Surrogate 1 - Fidelity vs RF:",
    conf_fidelity_1$overall["Accuracy"], "\n")



# ============================
# annexe : matrice de confusion + MDA
# ============================


# Dictionnaire de renommage
labels_map <- c(
  "rectiligne"        = "Straight",
  "sinueux"           = "Sinuous",
  "meandre passif"    = "Passive meandering",
  "rectiligne bars"   = "Alternate bars",
  "sinueux ba"        = "Sinuous with bars",
  "meandre actif"     = "Active meandering",
  "divagant"          = "Wandering",
  "tresse"            = "Braided",
  "anastomose"        = "Anastomosed",
  "anabranchement"    = "Anabranching",
  "retenue"           = "Reservoir",
  "intermittent"      = "Intermittent"
)

conf_df <- conf_df %>%
  mutate(
    Reference = recode(Reference, !!!labels_map),
    Prediction = recode(Prediction, !!!labels_map)
  )

conf_df <- conf_df %>%
  group_by(Reference) %>%
  mutate(Percent = Freq / sum(Freq))

# CONFUSION MATRIX PLOT
conf_df <- conf_df %>%
  mutate(text_color = ifelse(Percent > 0.5, "white", "black"))

plot_conf <- ggplot(conf_df, aes(x = Reference, y = Prediction, fill = Percent)) +
  
  geom_tile(color = "white", linewidth = 0.4) +
  
  geom_text(aes(label = Freq, color = text_color),
            size = 4, fontface = "bold") +
  
  scale_color_identity() +
  
  scale_fill_gradient(
    low = "grey95",
    high = "grey20",
    guide = "none"
  ) +
  
  labs(
    x = "Observed class",
    y = "Predicted class"
  ) +
  
  coord_equal() +
  
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = ggplot2::margin(10, 10, 10, 10)
  )

plot_conf

dev.off()  # Ferme la fenêtre de plot pour éviter les problèmes d'affichage dans RStudio

# IMPORTANCE AVEC INCERTITUDE (BOOTSTRAP) type = 1 (MDA)
set.seed(123)
n_iter <- 30

all_importance <- list()

for(i in 1:n_iter){
  
  index <- createDataPartition(test_clean$label, p = 0.7, list = FALSE)
  train_i <- test_clean[index, ]
  
  model_i <- randomForest(
    label ~ .,
    data = train_i,
    importance = TRUE,
    ntree = 300
  )
  
  imp_i <- importance(model_i, type = 1)
  
  imp_df_i <- data.frame(
    Variable = rownames(imp_i),
    Importance = imp_i[,1],
    Iteration = i
  )
  
  all_importance[[i]] <- imp_df_i
}

imp_all <- bind_rows(all_importance)

# Moyenne + SD
imp_summary <- imp_all %>%
  group_by(Variable) %>%
  summarise(
    Mean = mean(Importance),
    SD = sd(Importance),
    .groups = "drop"
  ) %>%
  arrange(Mean)

var_map <- c(
  "Sinuosity_meander_2" = "Sinuosity index",
  "mean_idx_water" = "Water index",
  "mean_ACW_star" = "ACW*",
  # "mean_AC" = "AC",
  "mean_WC" = "Water Channel width",
  "multi_chenaux_index" = "Multi-channel index",
  "iles_veget" = "Vegetated islands occurence",
  "step_AC_na" = "Active channel delta"
)

imp_summary <- imp_summary %>%
  mutate(
    Variable = recode(Variable, !!!var_map)
  )

plot_imp <- ggplot(imp_summary, aes(x = Mean, y = reorder(Variable, Mean))) +
  
  geom_point(size = 2.5, color = "black") +
  
  geom_errorbarh(
    aes(xmin = Mean - SD, xmax = Mean + SD),
    height = 0.15,
    color = "black",
    linewidth = 0.5
  ) +
  
  scale_y_discrete(position = "right")+
  
  
  labs(
    x = "Mean Decrease in Accuracy",
    y = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_text(
      face = "bold",
      hjust = 0
    ),    
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

plot_imp


# VARIABLE IMPORTANCE type = 2 (MDI) 
set.seed(123)
n_iter <- 30

all_importance_gini <- list()

for(i in 1:n_iter){
  
  index <- createDataPartition(test_clean$label, p = 0.7, list = FALSE)
  train_i <- test_clean[index, ]
  
  model_i <- randomForest(
    label ~ .,
    data = train_i,
    importance = TRUE,
    ntree = 300
  )
  
  imp_i <- importance(model_i, type = 2)
  
  imp_df_i <- data.frame(
    Variable = rownames(imp_i),
    Importance = imp_i[,"MeanDecreaseGini"],
    Iteration = i
  )
  
  all_importance_gini[[i]] <- imp_df_i
}

imp_all_gini <- bind_rows(all_importance_gini)

imp_summary_gini <- imp_all_gini %>%
  group_by(Variable) %>%
  summarise(
    Mean = mean(Importance),
    SD = sd(Importance),
    .groups = "drop"
  ) %>%
  arrange(Mean)

var_map <- c(
  "Sinuosity_meander_2" = "Sinuosity index",
  "mean_idx_water" = "Water index",
  "mean_ACW_star" = "ACW*",
  "mean_WC" = "Water Channel width",
  "multi_chenaux_index" = "Multi-channel index",
  "iles_veget" = "Vegetated islands occurence",
  "step_AC_na" = "Active channel delta"
)

imp_summary_gini <- imp_summary_gini %>%
  mutate(Variable = recode(Variable, !!!var_map))

plot_imp_gini <- ggplot(imp_summary_gini, aes(x = Mean, y = reorder(Variable, Mean))) +
  geom_point(size = 2.5, color = "black") +
  geom_errorbarh(
    aes(xmin = Mean - SD, xmax = Mean + SD),
    height = 0.15,
    color = "black",
    linewidth = 0.5
  ) +
  
  scale_y_discrete(position = "right")+

  labs(
    x = "Mean Decrease in Gini",
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_text(
      face = "bold",
      hjust = 0
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
    
  )

plot_imp_gini


# combinaison 1000 x750

final_plot <- plot_conf + (plot_imp / plot_imp_gini) +
  plot_layout(widths = c(2, 1)) +
  plot_annotation(tag_levels = "A")

final_plot




# ============================================
# nuage de point pente vs ACW 1000x750
# ============================================
df_nuage <- resultat_conf %>%
  filter(Prediction_en %in% c("Braided", "Wandering",
                              "Passive meandering",
                              # "Alternate bars",
                              "Sinuous with bars",
                              "Anabranching",
                              # "Anastomosed",
                              "Active meandering"
                              ),
         !(Prediction_en == "Braided" & !gid_region %in% c(31, 16, 11, 33, 26)),
         conf_simple %in% c("unconfined")
         ) %>%
  mutate(drainage_area = drainage_area,
    drainage_area = ifelse(drainage_area < 1, 1, drainage_area))

# st_write(df_nuage, "df_nuage1.gpkg")

ggplot(df_nuage, aes(x = drainage_area, y = mean_AC, color = Prediction_en)) +
  
  # geom_point(
  #   size = 1.8,
  #   alpha = 0.5,
  #   stroke = 0
  # ) +
  
  geom_point(
    size = 1,
    alpha = 0.25,
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








df_resume <- resultat_conf %>%
  filter(
    Prediction_en %in% c("Braided", "Wandering", "Active meandering",
                         "Passive meandering"),
    conf_simple %in% c("unconfined")
  ) %>%
  mutate(drainage_area = drainage_area / 1000) %>%
  group_by(Prediction_en) %>%
  summarise(
    mean_x = mean(drainage_area, na.rm = TRUE),
    sd_x   = sd(drainage_area, na.rm = TRUE),
    mean_y = mean(mean_AC, na.rm = TRUE),
    sd_y   = sd(mean_AC, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(df_resume, aes(x = mean_x, y = mean_y, color = Prediction_en)) +
  geom_errorbarh(aes(xmin = mean_x - sd_x, xmax = mean_x + sd_x),
                 height = 0) +
  geom_errorbar(aes(ymin = mean_y - sd_y, ymax = mean_y + sd_y),
                width = 0) +
  geom_point(size = 3)




df_nuage <- resultat_conf %>%
  filter(gid_region %in% c(31, 16, 11, 33, 26))
  
ggplot(df_nuage, aes(x = drainage_area/1000, y = mean_AC, color = Prediction_en)) +
  geom_point(alpha = 0.6) 







