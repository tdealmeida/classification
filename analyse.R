TGH_test <- subset(TGH_ID, Sinuosity_meander <= 5)

# 2. Modèle linéaire sur le nouveau DF
model <- lm(Sinuosity_meander ~ mean_angle_deg, data = TGH_test)
r2 <- summary(model)$r.squared

# 3. Graphique avec R²
ggplot(TGH_test, aes(x = mean_angle_deg, y = Sinuosity_meander)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  annotate("text",
           x = Inf, y = Inf,
           label = paste0("R² = ", round(r2, 3)),
           hjust = 1.1, vjust = 1.5)







axe <- TGH %>%
  group_by(axis) %>%
  summarise(total_length = sum(sum_length, na.rm=TRUE),
            count_segments = n()
            ) %>%
  mutate(mean_length = total_length/count_segments)

ggplot(axe,aes(x=total_length,y=count_segments))+
  geom_point()+
  theme_minimal()+
  geom_smooth(method='lm')





ggplot(Data_points,aes(x=factor(strahler),y=angle_deg))+
  geom_boxplot()+
  theme_minimal()


leng <- TGH %>%
  group_by(axis) %>%
  summarise(total_length = sum(sum_length, na.rm=TRUE))%>%
  st_drop_geometry()

test <- Data_points %>%
  left_join(leng, by="axis") %>%
  mutate(length_exutoire = (measure/total_length)*100) %>%
  filter(length_exutoire <= 100)


ggplot(test,aes(y=length_exutoire))+
  geom_histogram()+
  theme_minimal()

ggplot(test,aes(x=factor(strahler),y=length_exutoire))+
  geom_boxplot()+
  theme_minimal()




label <- st_read("label_v5.gpkg") %>%
  drop_na() %>%
  filter(label != "retenue")

unique(label$label)

label$label[label$label == "rectligne"] <- "rectiligne"
label$label[label$label %in% c("tresse vegetal ", "tresse vegetal")] <- "tresse vegetal"

table(label$label)


# Préparation des données pour le boxplot
df_plot <- label %>%
  st_drop_geometry() %>%
  select(label, mean_ACW_star, Sinuosity_meander, multi_chenal, iles_veget)

# Mise en format long (tidy)
df_long <- df_plot %>%
  pivot_longer(cols = -label, names_to = "variable", values_to = "valeur")

# Boxplot ggplot
ggplot(df_long, aes(x = label, y = valeur, fill = label)) +
  geom_boxplot(outlier.shape = 16, outlier.alpha = 0.4) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  labs(title = "Distribution des métriques par label",
       x = "style", y = "Valeur") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # rotation verticale
        strip.text = element_text(face = "bold"))























library(stringr)


test <- st_read("label.gpkg")

test <- test %>%
  mutate(label_2 = str_trim(label_2)) %>%   # enlève espaces
  filter(
    !is.na(label_2),
    !label_2 %in% c(
      "divagant - tresse",
      "divagant - rectiligne bars",
      "divagant - tresse vegetal",
      "fausse divagantfausse divagant",
      ""
    )
  ) %>%
  mutate(
    label_2 = case_when(
      # rectiligne
      label_2 %in% c("rectiligne", "rectiligne ") ~ "rectiligne",
      
      # rectiligne bars
      label_2 %in% c("rectiligne bars", "rectiligne bars ") ~ "rectiligne bars",
      
      # anastomose
      label_2 %in% c("anastomose", "anamostose") ~ "anastomose",
      
      # sinueux
      label_2 %in% c("sinueux", "sinueux ") ~ "sinueux",

      
      # sinon : garder les autres valeurs intactes
      TRUE ~ label_2
    )
  )

unique(test$label_2)

table(test$label_2)


test <- st_read("label_aic.gpkg") %>%
  filter(!is.na(label)) %>%
  rename(label_2 = label)


# Préparation des données pour le boxplot
df_plot <- test %>%
  st_drop_geometry() %>%
  select(label_2, mean_ACW_star, Sinuosity_meander, multi_chenal, iles_veget,
         mean_AC,mean_idx_water,mean_Slope_talweg)



# Mise en format long (tidy)
df_long <- df_plot %>%
  pivot_longer(cols = -label_2, names_to = "variable", values_to = "valeur")

# Boxplot ggplot
ggplot(df_long, aes(x = label_2, y = valeur, fill = label_2)) +
  geom_boxplot(outlier.shape = 16, outlier.alpha = 0.4) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  labs(title = "Distribution des métriques par cluster (CAH)",
       x = "Cluster", y = "Valeur") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # rotation verticale
        strip.text = element_text(face = "bold"))












plot_comparaison <- function(test, classes, variables){
  
  # Vérification basique
  if(!all(classes %in% unique(test$label_2))){
    stop("Certaines classes n'existent pas dans test$label_2.")
  }
  if(!all(variables %in% colnames(test))){
    stop("Certaines variables n'existent pas dans le tableau.")
  }
  
  df_plot <- test %>%
    filter(label_2 %in% classes) %>%
    st_drop_geometry() %>%
    select(label_2, all_of(variables))
  
  df_long <- df_plot %>%
    pivot_longer(cols = -label_2, names_to = "variable", values_to = "valeur")
  
  ggplot(df_long, aes(x = label_2, y = valeur, fill = label_2)) +
    geom_boxplot(outlier.shape = 16, outlier.alpha = 0.4) +
    facet_wrap(~ variable, scales = "free_y", ncol = 2) +
    labs(title = paste("Comparaison :", paste(classes, collapse = " vs ")),
         x = "Type", y = "Valeur") +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      strip.text = element_text(face = "bold"))+
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 6)   # <-- 8 graduations, ajustable
    )
    
}

plot_comparaison(
  test,
  classes = c("rectiligne","sinueux", "meandre passif","sinueux ba", "rectiligne bars", "meandre actif"),
  variables = c("Sinuosity_meander")
)


plot_comparaison(
  test,
  classes = c("sinueux ba", "rectiligne bars", "meandre actif", "divagant", "tresse", "tresse vegetal"),
  variables = c("mean_ACW_star", "mean_idx_water")
)




plot_comparaison(
  test,
  classes = c( "tresse", "tresse vegetal"),
  variables = c("mean_ACW_star", "mean_idx_water", "iles_veget", "multi_chenal")
)

plot_comparaison(
  test,
  classes = c("tresse","tresse vegetal", "meandre actif", "anamostose"),
  variables = c("mean_ACW_star", "mean_idx_water", "iles_veget", "multi_chenal")
)

plot_comparaison(
  test,
  classes = c("reservoir"),
  variables = c("retenue")
)

plot_comparaison(
  test,
  classes = c("fausse divagant", "divagant"),
  variables = c("mean_ACW_star", "Sinuosity_meander", "multi_chenal", "iles_veget",
         "mean_AC","mean_idx_water","mean_Slope_talweg", "mean_WC")
)

plot_comparaison(
  test,
  classes = c("fausse tresse", "tresse"),
  variables = c("mean_ACW_star", "Sinuosity_meander", "multi_chenal", "iles_veget",
                "mean_AC","mean_idx_water","mean_Slope_talweg", "mean_WC")
)













map <- testset %>%
  left_join(resultat_final %>% st_drop_geometry() %>% 
              select(mean_Slope_talweg,mean_AC, noeudfinal, Prediction),
            by = c("mean_Slope_talweg", "mean_AC")
  )


confusion <- table(map$label, map$Prediction)

map$label <- factor(map$label)
map$Prediction <- factor(
  map$Prediction,
  levels = levels(map$label)
)

confusion <- confusionMatrix(
  data = map$Prediction,
  reference = map$label
)

conf_df <- as.data.frame(confusion$table)

colnames(conf_df) <- c("Reference", "Prediction", "Freq")

conf_df <- conf_df %>%
  group_by(Reference) %>%
  mutate(Percent = Freq / sum(Freq) * 100)

ggplot(conf_df, aes(x = Reference, y = Prediction, fill = Percent)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5) +
  scale_fill_gradient(low = "#f7fbff", high = "#08306b") +
  labs(
    title = "Matrice de confusion – Random Forest",
    x = "Vrai label",
    y = "Label prédit",
    fill = "% par classe"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    legend.position = "right"
  )




confusion <- table(map$label, map$noeudfinal)

map$label <- factor(map$label)
map$Prediction <- factor(
  map$noeudfinal,
  levels = levels(map$label)
)
unique(map$label)
unique(map$noeudfinal)
setdiff(unique(map$noeudfinal), unique(map$label))
setdiff(unique(map$label), unique(map$noeudfinal))
levels_communs <- sort(unique(map$label))
map$label   <- factor(map$label, levels = levels_communs)
map$noeudfinal <- factor(map$noeudfinal, levels = levels_communs)


confusion <- confusionMatrix(
  data = map$noeudfinal,
  reference = map$label
)

conf_df <- as.data.frame(confusion$table)

colnames(conf_df) <- c("Reference", "Prediction", "Freq")

conf_df <- conf_df %>%
  group_by(Reference) %>%
  mutate(Percent = Freq / sum(Freq) * 100)

ggplot(conf_df, aes(x = Reference, y = Prediction, fill = Percent)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 5) +
  scale_fill_gradient(low = "#f7fbff", high = "#08306b") +
  labs(
    title = "Matrice de confusion – Arbre Heuristique",
    x = "Vrai label",
    y = "Label prédit",
    fill = "% par classe"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    legend.position = "right"
  )


