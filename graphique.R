library(circlize)
library(forcats)
library(ggtext)
library(ggh4x)


palette_lits <- c(
  "Straight" = "#9ecae1",
  "Straight with bars" = "#6baed6",
  # "No channel" = "#202020",
  # "intermittent" = "#a7a7a7",
  "Sinuous" = "#4292c6",
  "Sinuous with bars" = "#2171b5",
  "Passive meandering" = "#084594",
  "Active meandering" = "#08306b",
  "Braided" = "#e69f00",
  # "braided intermittent" = "#eae9ab",
  "Vegetated braided" = "#ffd500",
  "Wandering" = "#b410d5",
  "Reservoir" = "#a7a7a7",
  "Anastomosing" = "#9acd32"
  # "Diffuse" = "#fcb4b1"
)


palette_conf <- c(
  # Straight
  "Straight confined"        = "#6f9fb8",
  "Straight partly confined" = "#9ecae1",
  "Straight unconfined"      = "#cfe5f2",
  
  # Straight with bars
  "Straight with bars confined"        = "#4fa1c7",
  "Straight with bars partly confined" = "#6baed6",
  "Straight with bars unconfined"      = "#a9d0eb",
  
  # Sinuous
  "Sinuous confined"        = "#2f78a8",
  "Sinuous partly confined" = "#4292c6",
  "Sinuous unconfined"      = "#7fb8de",
  
  # Sinuous with bars
  "Sinuous with bars confined"        = "#185a94",
  "Sinuous with bars partly confined" = "#2171b5",
  "Sinuous with bars unconfined"      = "#5a9ad4",
  
  # Passive meandering
  "Passive meandering confined"        = "#06386f",
  "Passive meandering partly confined" = "#084594",
  "Passive meandering unconfined"      = "#3d6fb5",
  
  # Active meandering
  "Active meandering confined"        = "#062a57",
  "Active meandering partly confined" = "#08306b",
  "Active meandering unconfined"      = "#3c5c94",
  
  # Braided
  "Braided confined"        = "#c78700",
  "Braided partly confined" = "#e69f00",
  "Braided unconfined"      = "#f3c64d",
  
  # Vegetated braided
  "Vegetated braided confined"        = "#c6a700",
  "Vegetated braided partly confined" = "#ffd500",
  "Vegetated braided unconfined"      = "#ffe766",
  
  # Wandering
  "Wandering confined"        = "#8d0aa6",
  "Wandering partly confined" = "#b410d5",
  "Wandering unconfined"      = "#d46beb",
  
  # Anastomosing
  "Anastomosing confined"        = "#6c9c23",
  "Anastomosing partly confined" = "#9acd32",
  "Anastomosing unconfined"      = "#c8e78a",
  
  # Reservoir
  "Reservoir" = "#a7a7a7"
)

ordre_conf <- c(
  "Straight unconfined",
  "Straight partly confined",
  "Straight confined",
  
  "Straight with bars unconfined",
  "Straight with bars partly confined",
  "Straight with bars confined",
  
  "Sinuous unconfined",
  "Sinuous partly confined",
  "Sinuous confined",
  
  "Sinuous with bars unconfined",
  "Sinuous with bars partly confined",
  "Sinuous with bars confined",
  
  "Passive meandering unconfined",
  "Passive meandering partly confined",
  "Passive meandering confined",
  
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
  "Straight with bars",
  "Sinuous with bars",
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
  filter(gid_region %in% c(31, 16, 11, 33, 26)) %>%
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
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
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
         drainage_area, mean_ACW_star, mean_idx_water, measure, sum_length) %>%
  pivot_longer(
    cols = c(mean_Slope_talweg, mean_elevation, measure, sum_length,
             drainage_area, mean_ACW_star, mean_idx_water),
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
      sum_length = "Segment length (m)"
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
                      "Distance from source (km)") ~ scale_y_log10(labels = scales::label_number())
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = "none"
  ) +
  labs(
    x = NULL,
    y = "Value"
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
    variable = recode(
      variable,
      mean_Slope_talweg = "Talweg slope",
      mean_elevation = "Elevation",
      mean_Slope_VB = "Valley bottom slope",
      drainage_area = "Drainage area",
      mean_AC = "Active channel width",
      mean_VB = "Valley bottom width",
      measure = "Distance from source",
      mean_meander_belt = "Meander belt width"
    )
  )


ggplot(df_box, aes(x = Prediction_en, y = value, fill = conf_simple)) +
  geom_boxplot(
    position = position_dodge(width = 0.75),
    width = 0.65,
    outlier.alpha = 0.3
  ) +
  facet_wrap(~ variable, scales = "free_y", nrow = 3) +
  theme_minimal() +
  labs(
    x = NULL,
    y = "Value",
    fill = "Confinement"
  ) +
  scale_fill_manual(
    values = c(
      "unconfined" = "#A7D477",
      "partly confined" = "#c1c182",
      "confined" = "#EE4E4E"
    )
  ) +
  scale_y_log10(labels = scales::label_number())+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 11),
    strip.text = element_text(face = "bold")
  )




# ============================================
# circle transition planform France
# ============================================
library(circlize)
library(dplyr)
library(ragg)
library(here)

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



# ============================================
# circle transition planform France
# ============================================
library(dplyr)
library(circlize)

# --- 1. PRÉPARATION DES DONNÉES (Filtrage + Astuce visuelle 80/20 + Sécurité) ---
data_transitions <- tttt %>%
  arrange(axis, desc(ID_segment)) %>%
  group_by(axis) %>%
  mutate(to = lead(Prediction_en)) %>%
  ungroup() %>%
  filter(!is.na(to)) %>%
  rename(from = Prediction_en) %>%
  count(from, to) %>%
  group_by(from) %>%
  
  # 1. On trie pour mettre le flux interne en premier, puis les plus gros flux sortants
  arrange(from, desc(from == to), desc(n)) %>%
  
  # 2. On ne garde que les 5 plus gros flux par origine
  slice_head(n = 5) %>% 
  
  # 3. Le trick visuel SÉCURISÉ
  mutate(
    a_un_interne = any(from == to),         # Vérifie si la classe a un flux interne
    sum_sortants = sum(n[from != to]),      # Calcule le total des flux sortants
    
    prob = case_when(
      # Cas 1 : Que de l'interne (100% de la place)
      from == to & sum_sortants == 0 ~ 1.0,  
      
      # Cas 2 : Interne + Externe -> L'interne prend 80%
      from == to & sum_sortants > 0 ~ 0.8,   
      
      # Cas 3 : Interne + Externe -> L'externe se partage les 20% restants
      a_un_interne & from != to ~ (n / sum_sortants) * 0.2, 
      
      # Cas 4 : QUE de l'externe (pas d'interne du tout) -> L'externe prend 100% de la place
      !a_un_interne & from != to ~ n / sum_sortants         
    )
  ) %>%
  select(from, to, prob) %>%
  ungroup()

# --- 2. ORDRE POUR LA COLLINE SUR LE CÔTÉ ---
df_interne <- data_transitions %>% filter(from == to)
df_externe <- data_transitions %>% filter(from != to)
df_final <- rbind(df_externe, df_interne)

# --- 3. CRÉATION DU GRAPHIQUE AFFINÉ ---
png("chord_morpho_thin_arrows_final.png", width = 2500, height = 2500, res = 300)

circos.clear()
circos.par(start.degree = 90, gap.degree = 4)

chordDiagram(df_final,
             grid.col = palette_lits,
             directional = 1,
             direction.type = "arrows",
             link.arr.type = "big.arrow",
             link.lwd = 0.1,               # Traits fins
             self.link = 1,
             link.sort = FALSE,
             h.ratio = 0.7,                
             transparency = 0.5,           # Plus transparent pour alléger
             annotationTrack = c("name", "grid", "axis"),
             annotationTrackHeight = c(0.06, 0.05))

dev.off()
circos.clear()


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










