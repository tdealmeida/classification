library(circlize)
library(forcats)
library(ggtext)
library(ggh4x)


# palette_lits <- c(
#   "Straight" = "#a6cee3",
#   # "Straight with bars" = "#6baed6",
#   # "No channel" = "#202020",
#   # "intermittent" = "#a7a7a7",
#   "Sinuous" = "#1f78b4",
#   "Alternate bars" = "#5aa6c8",
#   # "Sinuous with bars" = "#2171b5",
#   "Passive meandering" = "#08519c",
#   "Active meandering" = "#08306b",
#   "Braided" = "#ffd500",
#   # "braided intermittent" = "#eae9ab",
#   "Vegetated braided" = "#e69f00",
#   "Wandering" = "#b410d5",
#   "Reservoir" = "#a7a7a7",
#   "Anastomosing" = "#9acd32"
#   # "Diffuse" = "#fcb4b1"
# )
# 
# 
# palette_conf <- c(
#   # Straight
#   "Straight confined"        = "#6baed6",
#   "Straight partly confined" = "#a6cee3",
#   "Straight unconfined"      = "#deebf7",
#   
#   # Sinuous
#   "Sinuous confined"        = "#08519c",
#   "Sinuous partly confined" = "#1f78b4",
#   "Sinuous unconfined"      = "#6baed6",
#   
#   # Alternate bars (NEW)
#   "Alternate bars confined"        = "#3c8dbc",
#   "Alternate bars partly confined" = "#5aa6c8",
#   "Alternate bars unconfined"      = "#9ecae1",
#   
#   # Passive meandering
#   "Passive meandering confined"        = "#084594",
#   "Passive meandering partly confined" = "#2171b5",
#   "Passive meandering unconfined"      = "#6baed6",
#   
#   # Active meandering
#   "Active meandering confined"        = "#08306b",
#   "Active meandering partly confined" = "#08519c",
#   "Active meandering unconfined"      = "#6baed6",
#   
#   # Braided
#   "Braided confined"        = "#b97700",
#   "Braided partly confined" = "#e69f00",
#   "Braided unconfined"      = "#f5c04a",
#   
#   # Vegetated braided
#   "Vegetated braided confined"        = "#c2a500",
#   "Vegetated braided partly confined" = "#ffd92f",
#   "Vegetated braided unconfined"      = "#fff176",
#   
#   # Wandering
#   "Wandering confined"        = "#7a0ca3",
#   "Wandering partly confined" = "#b012d4",
#   "Wandering unconfined"      = "#d580eb",
#   
#   # Anastomosing
#   "Anastomosing confined"        = "#4d7f1a",
#   "Anastomosing partly confined" = "#66a61e",
#   "Anastomosing unconfined"      = "#a6d96a",
#   
#   # Reservoir
#   "Reservoir" = "#9e9e9e"
# )

palette_lits <- c(
  "Straight" = "#6baed6",          # très clair
  "Sinuous" = "#2171b5",           # bleu soutenu
  "Passive meandering" = "#084594", # 🔵 bleu foncé réintroduit
  "Alternate bars" = "#c6dbef",    # intermédiaire
  "Active meandering" = "#6a51a3",  # violet (évite confusion)
  "Braided" = "#e69f00",
  "Vegetated braided" = "#ffd92f",
  "Wandering" = "#b012d4",
  "Anastomosing" = "#66a61e",
  "Reservoir" = "#9e9e9e"
)

palette_conf <- c(
  # Straight
  "Straight confined"        = "#4a98c9",
  "Straight partly confined" = "#6baed6",
  "Straight unconfined"      = "#cfe5f3",
  
  # Sinuous
  "Sinuous confined"        = "#185a94",
  "Sinuous partly confined" = "#2171b5",
  "Sinuous unconfined"      = "#7fb6e6",
  
  # Passive meandering (bleu foncé → très clair)
  "Passive meandering confined"        = "#062e5c",
  "Passive meandering partly confined" = "#084594",
  "Passive meandering unconfined"      = "#6f9fd8",
  
  # Alternate bars
  "Alternate bars confined"        = "#9fbcd1",
  "Alternate bars partly confined" = "#c6dbef",
  "Alternate bars unconfined"      = "#edf4fb",
  
  # Active meandering (violet)
  "Active meandering confined"        = "#4a3580",
  "Active meandering partly confined" = "#6a51a3",
  "Active meandering unconfined"      = "#b3a2d6",
  
  # Braided (orange)
  "Braided confined"        = "#b97700",
  "Braided partly confined" = "#e69f00",
  "Braided unconfined"      = "#f5c04a",
  
  # Vegetated braided (jaune)
  "Vegetated braided confined"        = "#c2a500",
  "Vegetated braided partly confined" = "#ffd92f",
  "Vegetated braided unconfined"      = "#fff176",
  
  # Wandering (violet rose)
  "Wandering confined"        = "#7a0ca3",
  "Wandering partly confined" = "#b012d4",
  "Wandering unconfined"      = "#d580eb",
  
  # Anastomosing (vert)
  "Anastomosing confined"        = "#4d7f1a",
  "Anastomosing partly confined" = "#66a61e",
  "Anastomosing unconfined"      = "#a6d96a",
  
  # Reservoir
  "Reservoir" = "#9e9e9e"
)
ordre_conf <- c(
  "Straight unconfined",
  "Straight partly confined",
  "Straight confined",
  
  # "Straight with bars unconfined",
  # "Straight with bars partly confined",
  # "Straight with bars confined",
  
  "Sinuous unconfined",
  "Sinuous partly confined",
  "Sinuous confined",
  
  # "Sinuous with bars unconfined",
  # "Sinuous with bars partly confined",
  # "Sinuous with bars confined",
  
  "Passive meandering unconfined",
  "Passive meandering partly confined",
  "Passive meandering confined",
  
  "Alternate bars unconfined",
  "Alternate bars partly confined",
  "Alternate bars confined",
  
  "Active meandering unconfined",
  "Active meandering partly confined",
  "Active meandering confined",
  
  "Wandering unconfined",
  "Wandering partly confined",
  "Wandering confined",
  
  "Braided unconfined",
  "Braided partly confined",
  "Braided confined",
  
  "Vegetated braided unconfined",
  "Vegetated braided partly confined",
  "Vegetated braided confined",
  
  "Anastomosing unconfined",
  "Anastomosing partly confined",
  "Anastomosing confined",
  
  "Reservoir"
)

# ordre_conf_gap <- c(
#   "straight confined",
#   "straight partly confined",
#   "straight unconfined",
#   "gap1",
#   "straight with bars confined",
#   "straight with bars partly confined",
#   "straight with bars unconfined",
#   "gap2",
#   "sinuous confined",
#   "sinuous partly confined",
#   "sinuous unconfined",
#   "gap3",
#   "sinuous with alternate bars confined",
#   "sinuous with alternate bars partly confined",
#   "sinuous with alternate bars unconfined",
#   "gap4",
#   "passive meandering confined",
#   "passive meandering partly confined",
#   "passive meandering unconfined",
#   "gap5",
#   "active meandering confined",
#   "active meandering partly confined",
#   "active meandering unconfined",
#   "gap6",
#   "wandering confined",
#   "wandering partly confined",
#   "wandering unconfined",
#   "gap7",
#   "braided confined",
#   "braided partly confined",
#   "braided unconfined",
#   "gap8",
#   "vegetated braided confined",
#   "vegetated braided partly confined",
#   "vegetated braided unconfined",
#   "gap9",
#   "anastomosing confined",
#   "anastomosing partly confined",
#   "anastomosing unconfined",
#   # "braided intermittent",
#   # "intermittent",
#   "reservoir"
#   # "No channel"
# )

ordre_lits <- c(
  "Straight",
  "Sinuous",
  "Passive meandering",
  # "Straight with bars",
  # "Sinuous with bars",
  "Alternate bars",
  "Active meandering",
  "Wandering",
  "Braided",
  "Vegetated braided",
  "Anastomosing",
  # "Diffuse",
  # "braided intermittent",
  # "intermittent",
  "Reservoir"
  # "No channel"
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


# ============================================
# histogramme du nb de sgments homogènes par classe
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
  ggplot(aes(x = long, y = Prediction_en, fill = Prediction_en)) +
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
    panel.grid = element_blank(),
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
  summarise(long = sum(sum_length)/1000, .groups = "drop")

# ------------------------------------------------
# ajouter Anastomosing confined si absent
if (!"Anastomosing confined" %in% df_plot$conf_detaille) {
  
  df_plot <- bind_rows(
    df_plot,
    data.frame(
      conf_detaille = "Anastomosing confined",
      x_label = "C",
      Prediction_en = "Anastomosing",
      long = 0
    )
  )
}

df_plot$conf_detaille <- factor(df_plot$conf_detaille, levels = ordre_conf)

# ------------------------------------------------
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

# ============================================
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
# Graphique combiné : Longueur par Prediction_en 
# avec segmentation par Confinement (Stacked Bar)
# ============================================
library(ggplot2)
library(ggbreak)
library(dplyr)
library(forcats)


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

# ============================================
# Création du graphique VERTICAL
# ============================================
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
# comparaison classe par region fr 
# ============================================
df_facet <- resultat_final %>%
  mutate(
    gid_bassin = case_when(
      gid_region %in% c(31, 16, 11, 33, 26) ~ "RMC",
      gid_region %in% c(23, 30, 15, 27, 29) ~ "Loire - Bretagne",
      gid_region %in% c(22, 25, 20, 18) ~ "Seine - Normandie",
      gid_region %in% c(24, 21, 10, 14) ~ "Adour - Garonne",
      gid_region %in% c(28, 32) ~ "Rhin - Meuse",
      gid_region %in% c(19, 12) ~ "Artois - Picardie",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(Prediction_en = factor(Prediction_en, levels = ordre_lits)) %>%
  group_by(gid_bassin, Prediction_en) %>%
  summarise(km = sum(sum_length, na.rm = TRUE) / 1000, .groups = "drop")

df_total <- df_facet %>%
  group_by(Prediction_en) %>%
  summarise(km = sum(km)) %>%
  mutate(gid_bassin = "Total")

df_plot <- bind_rows(df_facet, df_total)



ggplot(df_plot, aes(x = "Total", y = km, fill = Prediction_en)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = palette_lits) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~ gid_bassin, nrow = 1, strip.position = "bottom") +
  theme_minimal() +
  
  labs(
    y = "% of total river length (km)",
    x = NULL,
    fill = "Planform"
  ) +
  
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
    # strip.text = element_text(angle = 0, hjust = 1)
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

ggplot(df_box, aes(x = Prediction_en, y = value, fill = Prediction_en)) +
  geom_boxplot(outlier.alpha = 0.3) +
  ggh4x::facet_wrap2(~ variable, scales = "free_y", nrow = 4) +
  scale_fill_manual(values = palette_lits) +
  ggh4x::facetted_pos_scales(
    y = list(
      variable %in% c("Talweg slope (%)", "Elevation (m)", "Drainage area (km²)",
                      "Normalized active channel width (m)", "Segment length (m)",
                      "Distance from source (km)", "Multi-channel index") ~ scale_y_log10(labels = scales::label_number())
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = "none"
  ) +
  labs(
    x = NULL,
    y = NULL
  )



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
# library(circlize)
# library(dplyr)
# library(ragg)
# library(here)
# 
tttt <- resultat_final %>%
  st_drop_geometry() %>%
  # filter(gid_region %in% c(31, 16, 11, 33, 26)) %>%
  # filter(gid_region %in% c(11)) %>%
  select(axis, ID_segment, Prediction_en) %>%
  filter(!Prediction_en %in% c("Reservoir", NA)) %>%
  arrange(axis, desc(ID_segment))

head(tttt)

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

head(data_long)

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



# # # ============================================
# # # circle transition planform France
# # # ============================================
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
  

# ============================================
# Surrogate tree
# ============================================
library(rpart)
library(rpart.plot)
library(ragg)
library(here)

X_all <- test_clean %>%
  dplyr::select(-label)
rf_pred <- predict(modele_foret, X_all)

rf_pred_en <- dplyr::recode(
  rf_pred,
  "rectiligne"      = "Straight",
  "rectiligne bars" = "Straight with bars",
  "sinueux"         = "Sinuous",
  "sinueux ba"      = "Sinuous with bars",
  "alternant"       = "Alternate bars",
  "meandre actif"   = "Active meandering",
  "meandre passif"  = "Passive meandering",
  "tresse"          = "Braided",
  "tresse vegetal"  = "Vegetated braided",
  "divagant"        = "Wandering",
  "anastomose"      = "Anastomosing",
  "retenue"         = "Reservoir"
)

surrogate_data <- X_all
surrogate_data$rf_pred <- rf_pred_en

surrogate_data <- surrogate_data %>%
  dplyr::rename(
    "Water index" = mean_idx_water,
    "Sinuosity index" = Sinuosity_meander_2,
    "Normalized active channel width" = mean_ACW_star,
    "Vegetated islands" = iles_veget
    # "Active channel width" = mean_AC
  )

surrogate_tree <- rpart(
  rf_pred ~ .,
  data = surrogate_data,
  method = "class",
  control = rpart.control(
    maxdepth = 10,
    minsplit = 30,
    cp = 0.001
  )
)

ragg::agg_png(
  here("surrogate_tree_rf.png"),
  width = 10,
  height = 8,
  units = "in",
  res = 500,
  background = "white"
)

rpart.plot(
  surrogate_tree,
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



# =========================
# eval surrogate
# =========================
X_train <- train %>% dplyr::select(-label)
rf_pred_train <- predict(modele_foret, X_train)

surrogate_train <- X_train
surrogate_train$rf_pred <- rf_pred_train

X_test <- testset %>% dplyr::select(-label)
rf_pred_test <- predict(modele_foret, X_test)

surrogate_test <- X_test
surrogate_test$rf_pred <- rf_pred_test


surrogate_tree <- rpart(
  rf_pred ~ .,
  data = surrogate_train,
  method = "class",
  control = rpart.control(
    maxdepth = 10,
    minsplit = 30,
    cp = 0.005
  )
)

# Prédictions du surrogate
surrogate_pred_test <- predict(surrogate_tree, surrogate_test, type = "class")

conf_surrogate <- confusionMatrix(
  surrogate_pred_test,
  surrogate_test$rf_pred
)

print(conf_surrogate)

cat("Accuracy surrogate (vs RF) :", conf_surrogate$overall["Accuracy"], "\n")

mean(conf_surrogate$byClass[, "Sensitivity"], na.rm = TRUE)
mean(conf_surrogate$byClass[, "Pos Pred Value"], na.rm = TRUE)
mean(conf_surrogate$byClass[, "F1"], na.rm = TRUE)




# =========================
# DATASET SURROGATE GLOBAL
# =========================

surrogate_data <- resultat_final %>%
  st_drop_geometry() %>%
  select(-label, -Probabilite)   # on garde uniquement X + Prediction

surrogate_data$Prediction <- factor(surrogate_data$Prediction)

# =========================
# ENTRAINEMENT SURROGATE GLOBAL
# =========================

surrogate_tree <- rpart(
  Prediction ~ .,
  data = surrogate_data,
  method = "class",
  control = rpart.control(
    maxdepth = 10,
    minsplit = 30,
    cp = 0.005
  )
)

surrogate_pred <- predict(surrogate_tree, surrogate_data, type = "class")

conf_surrogate <- confusionMatrix(
  surrogate_pred,
  surrogate_data$Prediction
)

print(conf_surrogate)

cat("Accuracy surrogate (vs RF global) :", conf_surrogate$overall["Accuracy"], "\n")























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










