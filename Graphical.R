library(circlize)
library(forcats)
library(ggtext)
library(ggh4x)


# library(MASS)
library(dplyr)

# # Détacher MASS
# if ("package:MASS" %in% search()) {
#   detach("package:MASS", unload = TRUE)
# }
# 
# # Détacher dplyr
# if ("package:dplyr" %in% search()) {
#   detach("package:dplyr", unload = TRUE)
# }



"#2d2d2d"
"#656565"
"#b1b1b1"

"#bb0e0e"
"#a9a904"
"#0b5707"


# palette_lits <- c(
#   "Straight" = "#4988C4",
#   "Sinuous" = "#1C4D8D",
#   "Passive meandering" = "#807dba",
#   "Alternate bars" = "#b3cde3",
#   "Sinuous with bars" = "#629FAD",
#   "Active meandering" = "#542788",
#   "Braided" = "#e69f00",
#   "Wandering" = "#b012d4",
#   "Anastomosed" = "#b2df8a",
#   "Anabranching" = "#33a02c",
#   "Reservoir" = "#9e9e9e",
#   "Intermittent" = "#363232",
#   "NA" = "black"
# )



palette_lits <- c(
  # Couleurs froides
  "Straight" = "#4BB8FA",              # bleu
  "Sinuous" = "#2C5EAD",               # bleu foncé
  "Passive meandering" = "#121358",    # bleu clair
  "Anastomosed" = "#3E7B27",           # vert froid
  "Anabranching" = "#956d3e",          # vert foncé froid
  "Sparse islands" = "#93DA97",          # vert moyen froid
  
  # Couleurs chaudes
  "Straight with alternate bars" = "#E7D283",        # orange clair
  "Sinuous with bars" = "#fcb429",     # orange-rouge
  "Active meandering" = "#d96c0e",     # orange-rouge foncé
  "Braided" = "#DD0303",               # orange
  "Wandering" = "#7a1073",             # brun chaud

  # Autres
  "Reservoir" = "#9E9E9E",
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
  
  "Straight with alternate bars confined"        = "#7F9FB8",
  "Straight with alternate bars confined" = "#b3cde3",
  "Straight with alternate bars unconfined"      = "#E3EEF7",
  
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
  
  "Straight with alternate bars confined",
  "Straight with alternate bars partly confined",
  "Straight with alternate bars unconfined",
  
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


# palette_meandering <- c(
#   "Active – confined" = "#a20000",          # orange rouge (fort)
#   "Active – partly confined" = "#D55E00",   # orange
#   "Active – unconfined" = "#009E73",        # vert
#   
#   "Passive – confined" = "#f64541",         # 👈 rouge pâle (fix)
#   "Passive – partly confined" = "#F6D55C",  # jaune doux
#   "Passive – unconfined" = "#7FCDBB"        # vert clair
# )

palette_meandering <- c(
  # Active = couleurs chaudes
  "Active – confined" = "#A50026",          # rouge foncé
  "Active – partly confined" = "#F46D43",   # orange-rouge
  "Active – unconfined" = "#FDB863",        # orange clair
  
  # Passive = couleurs froides
  "Passive – confined" = "#084081",         # bleu foncé
  "Passive – partly confined" = "#2B8CBE",  # bleu moyen
  "Passive – unconfined" = "#7BCCC4"        # bleu-vert clair
)


ordre_lits <- c(
  "Straight",
  "Sinuous",
  "Passive meandering",
  "Sparse islands",
  "Anastomosed",
  "Straight with alternate bars",
  "Sinuous with bars",
  "Active meandering",
  "Wandering",
  "Anabranching",
  "Braided",
  "Reservoir",
  "Intermittent"
)

palette_axe <- c(
  "Straight" = "#4988C4",
  "Sinuous" = "#1C4D8D",           
  "Passive meandering" = "#807dba", 
  "Straight with alternate bars" = "#b3cde3",    
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
  "Rhône - Mediterranean (f)",
  "Garonne - Adour (e)",
  "Loire (d)",
  "Seine (c)",
  "Rhine - Meuse (b)",
  "Scheldt - Somme (a)"
)

labels_wrap <- c(
  "France" = "France",
  "Rhône - Mediterranean (f)" = "Rhône -\nMediterranean (f)",
  "Garonne - Adour (e)" = "Garonne -\nAdour (e)",
  "Loire (d)" = "Loire (d)",
  "Seine (c)" = "Seine (c)",
  "Rhine - Meuse (b)" = "Rhine -\nMeuse (b)",
  "Scheldt - Somme (a)" = "Scheldt -\nSomme (a)"
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
  
  # scale_fill_manual(
  #   values = c(
  #     "confined" = "#FFA02E",
  #     "partly confined" = "#FFEF91",
  #     "unconfined" = "#9AD872"
  #   ),
  #   name = "Confinement"  
  # ) +
  
  scale_fill_manual(
    values = c(
      "confined" = "#FFA02E",
      "partly confined" = "#FFEF91",
      "unconfined" = "#9AD872"
    ),
    breaks = c(
      "confined",
      "partly confined",
      "unconfined"
    ),
    na.value = "grey80",
    name = "Confinement"
  )+
  
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
    plot.margin = ggplot2::margin(10, 180, 10, 10)
  )



# ============================================
# comparaison classe par region fr 500x1000
# ============================================
df_facet <- resultat_final_filtre %>%
  filter(!is.na(Prediction_en), !is.na(gid_region)) %>%
  
  mutate(
    gid_bassin = case_when(
      gid_region %in% c(31, 16, 11, 33, 26) ~ "Rhône - Mediterranean (f)",
      gid_region %in% c(23, 30, 15, 27, 29) ~ "Loire (d)",
      gid_region %in% c(22, 25, 20, 18) ~ "Seine (c)",
      gid_region %in% c(24, 21, 10, 14) ~ "Garonne - Adour (e)",
      gid_region %in% c(28, 32) ~ "Rhine - Meuse (b)",
      gid_region %in% c(19, 12) ~ "Scheldt - Somme (a)",
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
      size = 22
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
      mean_Slope_talweg = "Talweg slope (m/m)",
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
      variable %in% c("Talweg slope (m/m)", "Drainage area (km²)",
                      "Distance from source (km)") ~ 
        scale_y_log10(labels = scales::label_number())
    )
  ) +
  
  labs(x = NULL, y = NULL) +
  
  theme_classic(base_size = 11) +
  # scale_x_discrete(expand = expansion(add = 0.3))+
  coord_cartesian(clip = "off")+

  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(size = 10, face = "bold"),
    
    axis.text.x = element_text(angle = -45, hjust = 0, size = 9),
    axis.text.y = element_text(size = 8),
    
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    
    panel.spacing = unit(1, "lines"),
    plot.margin = ggplot2::margin(10, 20, 10, 10),
    
    legend.position = "none"
  )

# ============================================
# circle transition planform France
# ============================================
tttt <- resultat_final_filtre %>%
  st_drop_geometry() %>%
  # filter(gid_region %in% c(31, 16, 11, 33, 26)) %>%
  # filter(gid_region %in% c(11)) %>%
  filter(!Prediction_en %in% c("Reservoir","Intermittent", NA),
         Prediction_en != "Braided" | gid_region %in% c(16, 11, 33, 26)
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
  if(df_final$prop_out[i] < 0.15 & df_final$prop_in[i] < 0.15) {
    return(add_transparency(base_color, 0.9)) # Quasi invisible
  } else {
    return(add_transparency(base_color, 0.4))  # Transparence standard
  }
})

# --- 4. SECTEURS ET PLOT ---
all_sectors <- unique(c(df_final$from, df_final$to))
xmax <- setNames(rep(1, length(all_sectors)), all_sectors)

svg(
  "Chord_fixed2.svg",
  width = 8.7,
  height = 8.7
)

circos.clear()
circos.par(start.degree = 90, gap.degree = 6, track.margin = c(0.01, 0.01)
           )

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
# circle transition upalnds
# ============================================
tttt <- resultat_conf %>%
  st_drop_geometry() %>%
  filter(
    !Prediction_en %in% c(
      "Reservoir",
      "Intermittent",
      NA
    ),
    Prediction_en != "Braided" |
      gid_region %in% c(
        16,11,33,26
      )
  ) %>%
  filter(
    Prediction_en %in% c(
      "Straight",
      "Sinuous",
      "Straight with alternate bars",
      "Sinuous with bars",
      "Active meandering",
      "Wandering",
      "Anabranching",
      "Braided"
    )
  ) %>%
  arrange(
    axis,
    desc(ID_segment)
  ) %>%
  group_by(axis) %>%
  mutate(
    
    Prediction_tmp = replace_na(
      Prediction_en,
      "__NA__"
    ),
    
    ID_style = c(
      1,
      cumsum(
        Prediction_tmp[-1] !=
          Prediction_tmp[-n()]
      ) + 1
    )
    
  ) %>%
  ungroup()

# Regroupement des segments d'un même style
tttt <- tttt %>%
  group_by(
    axis,
    ID_style
  ) %>%
  summarise(
    
    Prediction_en = first(Prediction_en),
    
    conf_simple = names(
      which.max(
        table(conf_simple)
      )
    ),
    
    ID_segment = first(ID_segment),
    
    .groups = "drop"
    
  ) %>%
  arrange(
    axis,
    desc(ID_segment)
  )

# Abréviations du confinement
conf_lab <- c(
  "confined" = "C",
  "partly confined" = "PC",
  "unconfined" = "UC"
)

# Création des secteurs
tttt <- tttt %>%
  mutate(
    secteur = paste0(
      Prediction_en,
      "\n",
      conf_lab[conf_simple]
    )
  )

# Transitions
data_counts <- tttt %>%
  group_by(axis) %>%
  arrange(
    desc(ID_segment),
    .by_group = TRUE
  ) %>%
  mutate(
    secteur_to = lead(secteur)
  ) %>%
  ungroup() %>%
  filter(!is.na(secteur_to)) %>%
  rename(
    secteur_from = secteur
  ) %>%
  filter(
    secteur_from != secteur_to
  ) %>%
  count(
    secteur_from,
    secteur_to,
    name = "n"
  )

# Proportions
total_sortant <- data_counts %>%
  group_by(secteur_from) %>%
  summarise(
    sum_out = sum(n),
    .groups = "drop"
  )

total_entrant <- data_counts %>%
  group_by(secteur_to) %>%
  summarise(
    sum_in = sum(n),
    .groups = "drop"
  )

df_final <- data_counts %>%
  left_join(
    total_sortant,
    by = "secteur_from"
  ) %>%
  left_join(
    total_entrant,
    by = "secteur_to"
  ) %>%
  mutate(
    prop_out = n / sum_out,
    prop_in = n / sum_in
  )

# Ordre des styles
styles <- c(
  "Straight",
  "Straight with alternate bars",
  "Sinuous",
  "Sinuous with bars",
  "Active meandering",
  "Wandering",
  "Anabranching",
  "Braided"
)

confs <- c(
  "C",
  "PC",
  "UC"
)

ordre_secteurs <- unlist(
  lapply(
    styles,
    function(st){
      paste(
        st,
        confs,
        sep = "\n"
      )
    }
  ),
  use.names = FALSE
)

# Ne conserver que les secteurs présents
all_sectors <- ordre_secteurs[
  ordre_secteurs %in%
    unique(
      c(
        df_final$secteur_from,
        df_final$secteur_to
      )
    )
]

# Palette
palette24 <- c()

for(st in styles){
  
  cols <- rep(
    palette_lits[st],
    3
  )
  
  names(cols) <- paste(
    st,
    confs,
    sep = "\n"
  )
  
  palette24 <- c(
    palette24,
    cols
  )
  
}

palette24_use <- palette24[
  all_sectors
]

# xmax
xmax <- setNames(
  rep(
    1,
    length(all_sectors)
  ),
  all_sectors
)

# Espacement entre secteurs
style_nom <- sub(
  "\n.*",
  "",
  all_sectors
)

gap.after <- rep(
  1,
  length(all_sectors)
)

if(length(style_nom) > 1){
  
  for(i in seq_len(length(style_nom)-1)){
    
    if(style_nom[i] != style_nom[i+1]){
      
      gap.after[i] <- 8
      
    }
    
  }
  
}

gap.after[length(gap.after)] <- 12

# Couleurs des liens
link_cols <- sapply(
  seq_len(nrow(df_final)),
  function(i){
    
    style_depart <- sub(
      "\n.*",
      "",
      df_final$secteur_from[i]
    )
    
    base_color <- palette_lits[style_depart]
    
    if(df_final$prop_out[i] < 0.15 &
       df_final$prop_in[i] < 0.15){
      
      add_transparency(
        base_color,
        0.90
      )
      
    }else{
      
      add_transparency(
        base_color,
        0.40
      )
      
    }
    
  }
)

#Diagramme chord
svg(
  "Chord_24_secteurs2.svg",
  width = 8.7,
  height = 8.7
)

# png(
#   "Chord_24_secteurs.png",
#   width = 8.7,
#   height = 8.7,
#   units = "in",
#   res = 300
# )

circos.clear()

circos.par(
  start.degree = 90,
  gap.after = gap.after,
  track.margin = c(0.01, 0.01)
)

chordDiagram(
  x = df_final %>%
    dplyr::select(
      secteur_from,
      secteur_to,
      prop_out
    ),
  order = all_sectors,
  grid.col = palette24_use,
  col = link_cols,
  xmax = xmax,
  directional = 1,
  direction.type = c(
    "diffHeight",
    "arrows"
  ),
  link.arr.type = "big.arrow",
  diffHeight = mm_h(2),
  h.ratio = 0.75,
  link.sort = TRUE,
  link.border = NA,
  annotationTrack = c(
    "name",
    "grid",
    "axis"
  ),
  annotationTrackHeight = c(
    0.05,
    0.015,
    0.015
  ),
  transparency = 0
)

dev.off()

circos.clear()



svg("Chord_combined.svg",
    width = 17.4,
    height = 8.7)

par(mfrow = c(1, 2), mar = c(1, 1, 1, 1))

# -----------------
# Diagramme 1
# -----------------
circos.clear()
circos.par(
  start.degree = 90,
  gap.degree = 6,
  track.margin = c(0.01, 0.01)
)

chordDiagram(
  ...
)

# ============================================
# Surrogate tree à partir du jeu test du RF
# ============================================
# -----------------
# Diagramme 2
# -----------------
circos.clear()
circos.par(
  start.degree = 90,
  gap.after = gap.after,
  track.margin = c(0.01, 0.01)
)

chordDiagram(
  ...
)

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
      "rectiligne bars" = "Straight with alternate bars",
      "sinueux"         = "Sinuous",
      "sinueux bars"    = "Sinuous with bars",
      "meandre actif"   = "Active meandering",
      "meandre passif"  = "Passive meandering",
      "tresse"          = "Braided",
      "divagant"        = "Wandering",
      "anastomose"      = "Anastomosed",
      "anabranche"      = "Anabranching",
      "reservoir"       = "Reservoir",
      "ile eparses"      = "Sparse islands",
      "intermittent"    = "Intermittent"
    )
  ) %>%
  # dplyr::filter(!rf_pred %in% c("Reservoir", "Intermittent")) %>%
  select(-label) %>%
  dplyr::rename(
    "Water index" = idx_water_segment,
    "Sinuosity index" = Sinuosity_meander_2,
    "Normalized active channel width" = mean_ACW_star,
    "Vegetated islands frequency" = iles_veget,
    "Water channel width" = mean_WC,
    "Multi-channel index" = multi_chenaux_index,
    "Delta water channel" = step_WC
  )

# TEST (20%)
surrogate_test <- testset %>%
  mutate(
    rf_pred = predict(modele_foret, newdata = testset),
    rf_pred = dplyr::recode(
      rf_pred,
      "rectiligne"      = "Straight",
      "rectiligne bars" = "Straight with alternate bars",
      "sinueux"         = "Sinuous",
      "sinueux bars"    = "Sinuous with bars",
      "meandre actif"   = "Active meandering",
      "meandre passif"  = "Passive meandering",
      "tresse"          = "Braided",
      "divagant"        = "Wandering",
      "anastomose"      = "Anastomosed",
      "anabranche"      = "Anabranching",
      "reservoir"         = "Reservoir",
      "ile eparses"      = "Sparse islands",
      "intermittent"    = "Intermittent"
    )
  ) %>%
  # dplyr::filter(!rf_pred %in% c("Reservoir", "Intermittent")) %>%
  dplyr::rename(
    "Water index" = idx_water_segment,
    "Sinuosity index" = Sinuosity_meander_2,
    "Normalized active channel width" = mean_ACW_star,
    "Vegetated islands frequency" = iles_veget,
    "Water channel width" = mean_WC,
    "Multi-channel index" =  multi_chenaux_index,
    "Delta water channel" = step_WC
  )

# entraînement surrogate
# surrogate_tree_1 <- rpart(
#   rf_pred ~ .,  # 👉 on apprend le RF
#   data = surrogate_train,
#   method = "class",
#   control = rpart.control(
#     maxdepth = 7,     # 👉 arbre simple = interprétable
#     minsplit = 30,
#     cp = 0.001
#   )
# )

surrogate_tree_1 <- rpart(
  rf_pred ~ .,  # 👉 on apprend le RF
  data = surrogate_train,
  method = "class",
  control = rpart.control(
    maxdepth = 5,     # 👉 arbre simple = interprétable
    minsplit = 25,
    cp = 0.005
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

surrogate_pred_test <- predict(
  surrogate_tree_1,
  newdata = surrogate_test,
  type = "class"
)

# Niveaux communs à utiliser
all_levels <- union(
  levels(factor(surrogate_pred_test)),
  levels(factor(surrogate_test$rf_pred))
)

surrogate_pred_test <- factor(
  surrogate_pred_test,
  levels = all_levels
)

rf_pred_test <- factor(
  surrogate_test$rf_pred,
  levels = all_levels
)

conf_fidelity_1 <- confusionMatrix(
  data = surrogate_pred_test,
  reference = rf_pred_test
)

print(conf_fidelity_1)

cat("Surrogate 1 - Fidelity vs RF:",
    conf_fidelity_1$overall["Accuracy"], "\n")



# ============================
# matrice de confusion 
# ============================
# Dictionnaire FR -> EN
labels_map <- c(
  "rectiligne" = "Straight",
  "rectiligne bars" = "Straight with alternate bars",
  "sinueux" = "Sinuous",
  "sinueux bars" = "Sinuous with bars",
  "meandre actif" = "Active meandering",
  "meandre passif" = "Passive meandering",
  "divagant" = "Wandering",
  "tresse" = "Braided",
  "anastomose" = "Anastomosed",
  "anabranche" = "Anabranching",
  "ile eparses" = "Sparse islands",
  "reservoir" = "Reservoir",
  "intermittent" = "Intermittent"
)

# Matrice de confusion
conf_df <- as.data.frame(confusion$table)

colnames(conf_df) <- c("Reference", "Prediction", "Freq")

conf_df <- conf_df %>%
  mutate(
    Reference = recode(as.character(Reference), !!!labels_map),
    Prediction = recode(as.character(Prediction), !!!labels_map)
  ) %>%
  group_by(Reference) %>%
  mutate(
    Percent = 100 * Freq / sum(Freq),
    text_color = ifelse(Percent > 50, "white", "black")
  ) %>%
  ungroup()

# ordre des classes
ordre <- c(
  "Straight",
  "Straight with alternate bars",
  "Sinuous",
  "Sinuous with bars",
  "Passive meandering",
  "Active meandering",
  "Wandering",
  "Braided",
  "Anabranching",
  "Anastomosed",
  "Sparse islands",
  "Reservoir",
  "Intermittent"
)

conf_df$Reference <- factor(conf_df$Reference, levels = ordre)
conf_df$Prediction <- factor(conf_df$Prediction, levels = ordre)

# Figure
ggplot(conf_df,
       aes(x = Reference,
           y = Prediction,
           fill = Percent)) +
  
  geom_tile(
    colour = "white",
    linewidth = 0.5
  ) +
  
  geom_text(
    aes(label = Freq,
        colour = text_color),
    size = 4,
    fontface = "bold"
  ) +
  
  scale_colour_identity() +
  
  scale_fill_gradient(
    low = "grey95",
    high = "grey20",
    limits = c(0, 100),
    guide = "none"
  ) +
  
  coord_equal() +
  
  labs(
    x = "Observed class",
    y = "Predicted class"
  ) +
  
  theme_classic(base_size = 13) +
  
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    axis.text = element_text(colour = "black"),
    axis.title = element_text(size = 14)
  )



# ============================================
# nuage de point pente vs ACW 800x500
# ============================================
df_nuage <- resultat_conf %>%
  filter(Prediction_en %in% c("Braided", "Wandering",
                              # "Passive meandering",
                              # "Alternate bars",
                              # "Sinuous with bars",
                              # "Anabranching",
                              # "Anastomosed",
                              "Active meandering"
                              ),
         !(Prediction_en == "Braided" & !gid_region %in% c(16, 11, 33, 26)),
         mean_AC > 5,
         conf_simple %in% c("unconfined")
         ) %>%
  mutate(drainage_area = drainage_area,
    drainage_area = ifelse(drainage_area < 1, 1, drainage_area))

df_nuage <- df_nuage %>%
  mutate(
    Prediction_en = factor(
      Prediction_en,
      levels = c(
        "Braided",
        "Wandering",
        "Active meandering"
        # "Anabranching"
      )
    )
  )

table(df_nuage$Prediction_en)
# st_write(df_nuage, "df_nuage1.gpkg")

ggplot(df_nuage, aes(x = drainage_area, y = mean_AC, color = Prediction_en)) +
  # geom_smooth(
  #   method = "lm",
  #   se = FALSE,
  #   linewidth = 1.2
  # ) +
  # 
  geom_point(
    size = 1.8,
    alpha = 0.5,
    stroke = 0
  ) +
  
  # geom_point(
  #   size = 1,
  #   alpha = 0.25,
  #   stroke = 0
  # )+
  
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
    "Braided" = "#FFEB00",
    "Wandering" = "#FADA7A",
    "Active meandering" = "#5EABD6",
    # "Passive meandering" = "#009e73",
    "Anabranching" = "#113F67"
    # "Anastomosed" = "#93DA97",
    # "Alternate bars" = "#E7D283",
    # "Sinuous with bars" = "#fcb429"
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



# ============================================
# nuage de point pente vs ACW 800x600 avec ligne frontiere
# ============================================
library(MASS)
## Fonction pour une frontière LDA
lda_line <- function(data, class1, class2){
  
  df <- data %>%
    filter(Prediction_en %in% c(class1, class2)) %>%
    mutate(
      groupe = factor(Prediction_en),
      logA = log10(drainage_area),
      logW = log10(mean_AC)
    )
  
  fit <- lda(groupe ~ logA + logW, data = df)
  
  w <- as.numeric(fit$scaling)
  
  mu1 <- as.numeric(fit$means[1, ])
  mu2 <- as.numeric(fit$means[2, ])
  
  midpoint <- (mu1 + mu2) / 2
  
  cst <- sum(w * midpoint)
  
  x <- seq(min(df$logA),
           max(df$logA),
           length.out = 300)
  
  y <- (cst - w[1] * x) / w[2]
  
  data.frame(
    drainage_area = 10^x,
    mean_AC = 10^y,
    comparaison = paste(class1, "vs", class2)
  )
}

## Calcul des 3 frontières
droites <- bind_rows(
  lda_line(df_nuage, "Braided", "Wandering"),
  lda_line(df_nuage, "Wandering", "Active meandering")
  # lda_line(df_nuage, "Braided", "Anabranching")
)

## Graphique
ggplot(df_nuage,
       aes(x = drainage_area,
           y = mean_AC,
           colour = Prediction_en)) +
  
  geom_point(
    size = 1.8,
    alpha = 0.6,
    stroke = 0
  ) +
  
  geom_line(
    data = droites,
    aes(x = drainage_area,
        y = mean_AC,
        linetype = comparaison),
    inherit.aes = FALSE,
    colour = "black",
    linewidth = 0.8
  ) +
  
  scale_x_log10(
    breaks = scales::breaks_log(n = 5),
    labels = scales::label_number(accuracy = 1)
  ) +
  
  scale_y_log10(
    breaks = scales::breaks_log(n = 5),
    labels = scales::label_number(accuracy = 1)
  ) +
  
  scale_color_manual(values = c(
    "Braided" = "#FFEB00",
    "Wandering" = "#FFCB61",
    "Active meandering" = "#799EFF",
    "Anabranching" = "#113F67"
  )) +
  
  scale_linetype_manual(values = c(
    "Braided vs Wandering" = "solid",
    "Wandering vs Active meandering" = "dashed"
    # "Braided vs Anabranching" = "dotdash"
  )) +
  
  labs(
    x = "Drainage area (km²)",
    y = "Active channel width (m)",
    colour = NULL,
    linetype = ""
  ) +
  
  guides(
    colour = guide_legend(nrow = 3, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  
  theme_classic(base_size = 15) +
  
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.text = element_text(size = 12),
    axis.text = element_text(color = "black"),
    axis.title = element_text(size = 18),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank()
  )



# ============================================
# nuage de point pente vs braided vegeatted et non 800x500
# ============================================
# # Détacher MASS
# if ("package:MASS" %in% search()) {
#   detach("package:MASS", unload = TRUE)
# }
# 
# # Détacher dplyr
# if ("package:dplyr" %in% search()) {
#   detach("package:dplyr", unload = TRUE)
# }
library(dplyr)

df_nuage <- resultat_conf %>%
  filter(
    Prediction_en == "Braided",
    gid_region %in% c(16, 11, 33, 26)
  ) %>%
  mutate(
    Vegetated_islands = ifelse(
      iles_veget > 0.1,
      "Present",
      "Absent"
    )
  )

vars <- c(
  "mean_Slope_talweg",
  "mean_ACW_star"
)

var_labels <- c(
  mean_Slope_talweg = "Talweg slope (m/m)",
  mean_ACW_star = "Normalized active channel width (W*)"
)

df_long <- df_nuage %>%
  select(Vegetated_islands, all_of(vars)) %>%
  pivot_longer(
    cols = all_of(vars),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    variable = recode(variable, !!!var_labels)
  )

ggplot(df_long,
       aes(x = Vegetated_islands,
           y = value)) +
  
  geom_boxplot(
    width = 0.5,
    outlier.shape = NA,
    linewidth = 0.4
  ) +
  
  geom_jitter(
    width = 0.15,
    alpha = 0.35,
    size = 1
  ) +
  
  scale_y_log10(
    breaks = scales::breaks_log(n = 5),
    labels = scales::label_number()
  ) +
  
  facet_wrap(
    ~variable,
    scales = "free_y",
    nrow = 1
  ) +
  
  labs(
    x = "Vegetated islands",
    y = NULL
  ) +
  
  theme_bw(base_size = 12) +
  
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title.x = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.5
    )
  )

