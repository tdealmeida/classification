tgh_stre <- TGH_ID %>%
  # mutate(mean_idx_water = ifelse(mean_idx_water < 0, 0, mean_idx_water)) %>%
  filter(Sinuosity_meander <= 4, 
         # mean_AC >= 4,
         # retenue <= 0.4,
         # nb_WC_0_or_na / nb_DGO <= 0.35
  )

vars <- tgh_stre %>%
  select(mean_idx_water, Sinuosity_meander, mean_ACW_star, 
         , iles_veget, retenue , 
         mean_idx_conf, mean_VB
         # mean_Slope_talweg, mean_WC , multi_chenal , mean_elevation
         # , mean_stream_power
         ) %>%
  st_drop_geometry()





# vars <- tgh_stre %>%
#   select(mean_idx_water, Sinuosity_meander, mean_ACW_star, 
#           iles_veget, mean_idx_conf, retenue, 
#          # mean_Slope_talweg,
#          # , mean_stream_power
#   ) %>%
#   st_drop_geometry()


vars_scaled <- scale(vars)

cor_mat <- cor(vars, use = "complete.obs")

corrplot::corrplot(cor_mat, method = "color")

# Calcul et plot de la ACH
d <- dist(vars_scaled, method = "euclidean")
hc <- hclust(d, method = "ward.D2")
# hc <- hclust(d, method = "average")

plot(hc, labels = FALSE, hang = -1, main = "Classification hiérarchique (CAH)")

clusters <- cutree(hc, k = 12)

tgh_stre$cluster <- as.factor(clusters)

st_write(tgh_stre, "TGH_ID_cluster.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile





# Méthode du coude pour Kmeans
wss <- sapply(2:15, function(k){
  kmeans(vars_scaled, centers = k, nstart = 25)$tot.withinss
})

plot(2:15, wss, type = "b", pch = 19,
     xlab = "Nombre de clusters (k)",
     ylab = "Inertie intra-classe",
     main = "Méthode du coude - Kmeans")

k <- 7   # à adapter selon le coude
km <- kmeans(vars_scaled, centers = k, nstart = 50)

tgh_stre$cluster_kmeans <- as.factor(km$cluster)

st_write(tgh_stre, "TGH_ID_cluster_kmeans.gpkg", delete_layer = TRUE)


# DBSCAN
library(dbscan)

set.seed(123)
hdb <- hdbscan(vars_scaled, minPts = 5)   # à ajuster selon la taille
tgh_stre$cluster_hdbscan <- as.factor(hdb$cluster)
table(tgh_stre$cluster_hdbscan)

st_write(tgh_stre, "TGH_ID_cluster_hdbscan.gpkg", delete_layer = TRUE)











#boxplot des variables par cluster
library(ggplot2)

# Préparation des données pour le boxplot
df_plot <- tgh_stre %>%
  st_drop_geometry() %>%
  select(cluster, mean_idx_water, mean_Slope_talweg, 
         Sinuosity_meander, mean_ACW_star
         # ,mean_stream_power
         )

# Mise en format long (tidy)
df_long <- df_plot %>%
  pivot_longer(cols = -cluster, names_to = "variable", values_to = "valeur")

# Boxplot ggplot
ggplot(df_long, aes(x = cluster, y = valeur, fill = cluster)) +
  geom_boxplot(outlier.shape = 16, outlier.alpha = 0.4) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  labs(title = "Distribution des métriques par cluster (CAH)",
       x = "Cluster", y = "Valeur") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"))


























library(sf)
library(dplyr)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)

TGH_ID <- st_read("TGH_ID_fr.gpkg")



TGH_ID <- TGH_ID %>%
  left_join(TGH %>% st_drop_geometry() %>% select(axis,ID_segment, multi_chenaux_index),
            by = c("axis", "ID_segment"))%>%
  mutate(multi_chenaux_index = ifelse(multi_chenaux_index == 0, 1, multi_chenaux_index))

Segment_homogene <- TGH_ID %>%
  select(-source,-mean_meander_belt,-mean_angle_deg,-mean_water_channel_pc,
         -mean_gravel_bars_pc, -mean_natural_open_pc, -mean_forest_pc,
         -mean_forest_pc , -mean_grassland_pc, -mean_crops_pc, -mean_diffuse_urban_pc,
         -mean_dense_urban_pc, -mean_infrastructures_pc, -mean_active_channel_pc,
         -mean_riparian_corridor_pc, -mean_semi_natural_pc, -mean_reversible_pc,
         -mean_built_environment_pc, -nb_na, -na_pct, -multi_chenal, -retenue,
         -sum_length, -Delta_AC, -Delta_AC_relatif, -length_meander, -Sinuosity_meander_1,
         -mean_disconnected_pc_corrige, -Lag_AC, -Lag_AC_relatif) %>%
  rename(water_index = mean_idx_water,
         sinuosity_index = Sinuosity_meander_2,
         ACW_star = mean_ACW_star,
         AC = mean_AC,
         WC = mean_WC,
         conf_index = mean_idx_conf,
         VB = mean_VB,
         slope_talweg = mean_Slope_talweg, 
         slope_VB = mean_Slope_VB,
         multi_index = multi_chenaux_index,
         length = longueur_data,
         elevation = mean_elevation,
         iles_veget = iles_veget,
         conf_degree = sum_conf_degree
  )
# st_write(Segment_homogene, "Segment_homogene.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile

# ----------------------------
# 1️⃣ Lecture + nettoyage
# ----------------------------
# label <- st_read("label_aic.gpkg")
label <- st_read("label_fr.gpkg") %>%
  # st_drop_geometry() %>%
  select(-label) %>%
  rename(label = label_3) %>%
  filter(!label == "diffus") %>%
  filter(!label == "a voir") %>%
  # filter(!label == "anastomose") %>%
  filter(!label == "rectiligne bars") %>%
  filter(!label == "sinueux ba") %>%
  filter(!label == "tresse intermittent")


test <- TGH_ID %>%
  left_join(label %>% st_drop_geometry() %>% select(axis,ID_segment, label),
            by = c("axis", "ID_segment")
  ) %>%
  # arrange(axis, desc(ID_segment)) %>%
  # group_by(axis) %>%
  # mutate(Delta_WC = lead(mean_WC) - mean_WC) %>%
  ungroup() %>%
  mutate(step_AC_na = ifelse(is.na(Delta_AC), lag(Delta_AC), Delta_AC),
         mean_idx_water = case_when(
           mean_AC == 0 & mean_WC == 0 ~ 1,    # pas de données → ignorer pour les moyennes
           mean_WC == 0 & mean_AC > 0 ~ 0,      # vrai 0
           TRUE ~ mean_WC / mean_AC             # ratio normal
         )
         # step_WC_na = ifelse(is.na(Delta_WC), lead(Delta_WC), Delta_WC)
  )



table(test$label)
# Colonnes inutiles
colonnes_a_exclure <- c(
  "ID_segment", "toponyme", "nb_DGO", "axis", "source",
  "sum_length", "length_meander", 
  "drainage_area",
  # "roe",
  "mean_meander_belt", "Delta_AC", "Lag_AC",
  "Delta_AC_relatif", "Lag_AC_relatif", "measure",
  "mean_angle_deg" , "mean_idx_conf", "mean_VB",
  "mean_active_channel_pc", "mean_water_channel_pc",
  "mean_forest_pc", "mean_grassland_pc", "mean_crops_pc",
  "mean_diffuse_urban_pc", "mean_dense_urban_pc", "mean_infrastructures_pc",
  "mean_riparian_corridor_pc", "mean_semi_natural_pc", "mean_reversible_pc",
  "mean_disconnected_pc_corrige", "mean_built_environment_pc", "mean_natural_open_pc",
  "mean_gravel_bars_pc", "gid_region", "strahler", 
  "nb_na", "mean_Slope_VB" , "na_pct",
  # "Planform", "Process", 
  "mean_elevation", "Sinuosity_meander_1", "longueur_data",
  "sum_conf_degree",
  "retenue", "multi_chenal", 
  # "length_original", "Sinuosity_original", 
  "mean_Slope_talweg",
  "mean_AC"
  # "mean_WC"
  )



#e ajout de nouvelle colonne depuis TGH
test <- test %>%
  mutate(nb_na = TGH_ID$nb_na,
         na_pct = nb_na / nb_DGO * 100
  )



# 
# 
# # test en filtrant donnée
# test <- st_read("label_aic.gpkg")
# 
# test <- test %>%
#   mutate(nb_0_na = TGH$nb_WC_0_or_na,
#          zero_na_pct = nb_0_na / nb_DGO * 100
#   )
# 
# test <- test %>%
#   filter(!mean_AC < 4,
#          !retenue > 0.4,
#          !zero_na_pct > 34,
#          !label == "Retenue",
#          !label == "Pas de lit",
#          !label == "Lit sans eau"
#          )
# 
# # Colonnes inutiles
# colonnes_a_exclure <- c(
#   "ID_segment", "toponyme", "nb_DGO", "axis", "source",
#   "sum_length", "length_meander", "drainage_area", "roe",
#   "mean_enveloppe", "Delta_AC", "Lag_AC",
#   "Delta_AC_relatif", "Lag_AC_relatif", "measure",
#   "mean_angle_deg", "nb_0_na" , "mean_idx_conf", "mean_VB",
#   "mean_amplitude" , "retenue", "mean_AC", "zero_na_pct",
#   "mean_elevation", "mean_Slope_VB"
# )


# 
# 
# test en filtrant donnée
# label <- st_read("label_aic.gpkg")
# 
# test <- TGH_ID %>%
#   left_join(label %>% st_drop_geometry() %>% select(axis,ID_segment, label),
#             by = c("axis", "ID_segment")
#   )
# 
# test <- test %>%
#   mutate(nb_0_na = TGH$nb_WC_0_or_na,
#          zero_na_pct = nb_0_na / nb_DGO * 100
#   )
# 
# test <- test %>%
#   filter(
#     mean_AC >= 4,
#     retenue <= 0.35,
#     zero_na_pct <= 35,
#     !label %in% c("Retenue", "retenue", "Pas de lit",
#                   "intermittent", "tresse intermittent")
#   )
# 
# # Colonnes inutiles
# colonnes_a_exclure <- c(
#   "ID_segment", "toponyme", "nb_DGO", "axis", "source",
#   "sum_length", "length_meander", "drainage_area", "roe",
#   "mean_enveloppe", "Delta_AC", "Lag_AC",
#   "Delta_AC_relatif", "Lag_AC_relatif", "measure",
#   "mean_angle_deg", "nb_0_na" , "mean_idx_conf", "mean_VB",
#   "mean_amplitude" , "mean_active_channel_pc", "mean_water_channel_pc",
#   "mean_forest_pc", "mean_grassland_pc", "mean_crops_pc",
#   "mean_diffuse_urban_pc", "mean_dense_urban_pc", "mean_infrastructures_pc",
#   "mean_riparian_corridor_pc", "mean_semi_natural_pc", "mean_reversible_pc",
#   "mean_disconnected_pc_corrige", "mean_built_environment_pc", "mean_natural_open_pc",
#   "mean_gravel_bars_pc", "gid_region", "strahler", "noeudfinal","retenue",
#   "zero_na_pct", "mean_elevation", "mean_Slope_VB", "nb_WC_0_or_na"
# )















test_clean <- test %>% 
  st_drop_geometry() %>%
  drop_na(label) %>%
  select(-all_of(colonnes_a_exclure)) %>%
  drop_na()    # <-- enlève les NA dans toutes les colonnes

test_clean$label <- factor(test_clean$label)

# ----------------------------
# 2️⃣ Split train/test
# ----------------------------
set.seed(123)
index <- createDataPartition(test_clean$label, p = 0.8, list = FALSE)

train <- test_clean[index, ]
testset <- test_clean[-index, ]

table(test$label)


# ----------------------------
# 3️⃣ Random Forest
# ----------------------------
modele_foret <- randomForest(
  label ~ .,
  data = train,
  importance = TRUE,
  ntree = 500,
  mtry = floor(sqrt(ncol(train) - 1))
)


# ----------------------------
# 4️⃣ Évaluation
# ----------------------------
pred_test <- predict(modele_foret, newdata = testset)

confusion <- confusionMatrix(pred_test, testset$label)
print(confusion)
# confusion$byClass
mean(confusion$byClass[, "Sensitivity"], na.rm = TRUE)

mean(confusion$byClass[, "Pos Pred Value"], na.rm = TRUE)
mean(confusion$byClass[, "F1"], na.rm = TRUE)
mean(confusion$byClass[, "Specificity"], na.rm = TRUE)

# Convertir la matrice de confusion en data frame ggplot-compatible
conf_df <- as.data.frame(confusion$table)

colnames(conf_df) <- c("Reference", "Prediction", "Freq")

# Calculer pourcentages par vrai label (row-wise)
conf_df <- conf_df %>%
  group_by(Reference) %>%
  mutate(Percent = Freq / sum(Freq) * 100)

# Heatmap
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

# ----------------------------
# 5️⃣ Importance des variables
# ----------------------------
print(importance(modele_foret))
varImpPlot(modele_foret, type = 1)

importance(modele_foret, type = 1)

# ----------------------------
# 6️⃣ Application & Probabilités
# ----------------------------

# On applique le modèle sur l'ensemble du jeu de données nettoyé (ou sur de nouvelles données)
# 1. Prédire la CLASSE (ce que tu faisais déjà)
pred_class <- predict(modele_foret, newdata = test, type = "response")

# 2. Prédire les PROBABILITÉS (ce qui te manque)
# Cela renvoie une matrice avec une colonne par classe
pred_prob_matrix <- predict(modele_foret, newdata = test, type = "prob")


# 3. Extraire la probabilité de la classe gagnante (la "certitude")
# Pour chaque ligne, on prend la valeur maximale de la matrice de probabilités
max_prob <- apply(pred_prob_matrix, 1, max)

# 4. Tout assembler dans un nouveau dataframe
resultat_final <- test %>%
  mutate(
    Prediction = pred_class,       # La classe prédite
    Probabilite = max_prob         # Le % de certitude (entre 0 et 1)
  )

# --- Optionnel : Voir les résultats ---
head(resultat_final %>% select(label, Prediction, Probabilite))

# --- Optionnel : Visualiser la distribution de la confiance ---
library(ggplot2)
ggplot(resultat_final, aes(x = Probabilite, fill = Prediction)) +
  geom_histogram(bins = 30, color = "white", alpha = 0.8) +
  labs(
    title = "Distribution de la certitude du modèle",
    x = "Probabilité (Certitude)",
    y = "Nombre d'observations"
  ) +
  theme_minimal()



st_write(resultat_final, "TGH_RF_v2.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile

# write.csv(test_clean, "dataset_rf.csv", row.names = FALSE)







# 1. Variables numériques
data_numeric <- test_clean %>%
  select(-label) %>%
  select(where(is.numeric))

# 2. Corrélation Spearman
cor_matrix <- cor(data_numeric, method = "spearman", use = "complete.obs")

corrplot(cor_matrix,
         method = "color",
         type = "upper",
         order = "hclust",
         tl.cex = 0.7,
         addCoef.col = "black")

# 3. Distance
dist_matrix <- as.dist(1 - abs(cor_matrix))

# 4. Clustering
hc <- hclust(dist_matrix, method = "ward.D2")

# 5. Groupes avec seuil 0.2
groups <- cutree(hc, h = 0.2)

# 6. Voir les groupes de variables corrélées
clusters <- split(names(groups), groups)

print(clusters)


plot(hc, main = "Dendrogramme des variables")
abline(h = 0.20, col = "red", lwd = 2, lty = 2)













# ============================
# LIBRAIRIES
# ============================
library(dplyr)
library(ggplot2)
library(caret)
library(randomForest)
library(ggpubr)
# library(scales)

# ============================
# 1️⃣ PREPARATION DONNEES
# ============================
test_clean <- test %>% 
  st_drop_geometry() %>%
  drop_na(label) %>%
  select(-all_of(colonnes_a_exclure)) %>%
  drop_na()

test_clean$label <- factor(test_clean$label)

# Split
set.seed(123)
index <- createDataPartition(test_clean$label, p = 0.8, list = FALSE)

train <- test_clean[index, ]
testset <- test_clean[-index, ]

# ============================
# 2️⃣ RANDOM FOREST FINAL
# ============================
modele_foret <- randomForest(
  label ~ .,
  data = train,
  importance = TRUE,
  ntree = 500,
  mtry = floor(sqrt(ncol(train) - 1))
)

# ============================
# 3️⃣ MATRICE DE CONFUSION
# ============================
pred_test <- predict(modele_foret, newdata = testset)

confusion <- confusionMatrix(pred_test, testset$label)

# Dictionnaire de renommage
labels_map <- c(
  "rectiligne"        = "Straight",
  "rectiligne bars"   = "Straight with bars",
  "sinueux"           = "Sinuous",
  "sinueux ba"        = "Sinuous with bars",
  "meandre actif"     = "Active meandering",
  "meandre passif"    = "Passive meandering",
  "tresse"            = "Braided",
  "tresse vegetal"    = "Vegetated braided",
  "divagant"          = "Wandering",
  "anastomose"        = "Anastomosing",
  "retenue"           = "Reservoir"
)


conf_df <- as.data.frame(confusion$table)
colnames(conf_df) <- c("Reference", "Prediction", "Freq")

conf_df <- conf_df %>%
  mutate(
    Reference = recode(Reference, !!!labels_map),
    Prediction = recode(Prediction, !!!labels_map)
  )

conf_df <- conf_df %>%
  group_by(Reference) %>%
  mutate(Percent = Freq / sum(Freq))



# ============================
# CONFUSION MATRIX PLOT
# ============================
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

# ============================
# 4️⃣ IMPORTANCE AVEC INCERTITUDE (BOOTSTRAP)
# ============================
set.seed(123)
n_iter <- 30

all_importance <- list()

for(i in 1:n_iter){
  
  index <- createDataPartition(test_clean$label, p = 0.8, list = FALSE)
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
  "mean_AC" = "AC",
  "mean_WC" = "WC",
  "multi_chenaux_index" = "Multi-channel index",
  "iles_veget" = "Vegetated islands occurence",
  "step_AC_na" = "AC delta"
)

imp_summary <- imp_summary %>%
  mutate(
    Variable = recode(Variable, !!!var_map)
  )

# ============================
# VARIABLE IMPORTANCE PLOT
# ============================

plot_imp <- ggplot(imp_summary, aes(x = Mean, y = reorder(Variable, Mean))) +
  
  geom_point(size = 2.5, color = "black") +
  
  geom_errorbarh(
    aes(xmin = Mean - SD, xmax = Mean + SD),
    height = 0.15,
    color = "black",
    linewidth = 0.5
  ) +
  
  labs(
    x = "Mean Decrease in Gini",
    y = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_text(face = "bold"),  # ✅ ICI
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = ggplot2::margin(10, 10, 10, 10)
  )

plot_imp










# 
# rhonne <- resultat_final %>%
#   filter(axis == 2000804457,
#          ID_segment == 4) 
# 
# ana <- resultat_final %>%
#   filter(Prediction == "anastomose")
# 
# val_rhone <- rhonne$mean_AC
# 
# 
# ggplot(ana, aes(y = mean_AC)) +
#   geom_boxplot(fill = "lightblue") +
#   geom_point(aes(x = 1, y = val_rhone),
#              color = "red",
#              size = 4) +
#   labs(title = "Comparaison Rhone vs Anastomose",
#        x = "",
#        y = "Score") +
#   theme_minimal()
# 
# 
# ana <- resultat_final %>%
#   filter(Prediction == "retenue")
# 
# ggplot(ana, aes(x = retenue)) +
#   geom_histogram(bins = 30, fill = "lightblue", color = "white") +
#   theme_minimal()


# ============================================================
# 7️⃣ Méthode du Modèle Substitut (Arbre unique interprétable)
# ============================================================

# Installation des paquets nécessaires si tu ne les as pas
if(!require(rpart)) install.packages("rpart")
if(!require(rpart.plot)) install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

# 1. Préparation des données pour le substitut
# On repart des données d'entraînement (train)
data_surrogate <- train

# ⚠️ IMPORTANT : La cible devient la PRÉDICTION de la Random Forest
# On remplace le vrai label par ce que la forêt pense
data_surrogate$Target_Foret <- predict(modele_foret, newdata = train)

# On supprime le vrai label original pour ne pas confondre le modèle
data_surrogate$label <- NULL 

# 2. Entraînement de l'arbre unique (CART)
# On limite la profondeur (maxdepth) pour qu'il reste lisible (c'est le but !)
# cp (complexity parameter) aide aussi à élaguer les branches inutiles
arbre_substitut <- rpart(
  Target_Foret ~ ., 
  data = data_surrogate, 
  method = "class",       # Car c'est une classification
  control = rpart.control(maxdepth =9, cp = 0.005) 
)

# 3. Visualisation de l'arbre "moyen"
# C'est cet arbre que tu pourras montrer pour expliquer la logique globale
rpart.plot(
  arbre_substitut, 
  type = 2, 
  extra = 104,  # Affiche les pourcentages par classe
  under = TRUE, # Met le nom de la classe sous la boîte
  faclen = 0,   # Affiche les noms complets des variables
  main = "Arbre Substitut : La logique simplifiée de la Random Forest",
  box.palette = "BuGn"
)

# 4. Vérification de la "Fidélité"
# À quel point cet arbre unique respecte-t-il la forêt ?
# On compare les prédictions de l'arbre unique vs celles de la forêt
pred_substitut <- predict(arbre_substitut, type = "class")

fidélité <- confusionMatrix(pred_substitut, data_surrogate$Target_Foret)

cat("\n--- FIDÉLITÉ DU MODÈLE SUBSTITUT ---\n")
cat("Cet arbre unique reproduit les décisions de la forêt à : ", 
    round(fidélité$overall['Accuracy'] * 100, 2), "%\n")


# dev.off()


while (!is.null(dev.list())) dev.off()
dev.list()


pdf("arbre_substitut_total.pdf", width = 15, height = 12)
rpart.plot(arbre_substitut,
           type = 2,
           extra = 104,
           under = TRUE,
           faclen = 0,
           box.palette = "BuGn",
           main = "Arbre Substitut")
dev.off()
# browseURL(getwd())

while (!is.null(dev.list())) dev.off()
dev.list()










# ------------------------------------------------------------------
# 6️⃣ Application Avancée : Récupérer le Podium (1er, 2ème, 3ème)
# ------------------------------------------------------------------

# 1. Générer la matrice complète des probabilités (1 colonne par classe)
probs_matrix <- predict(modele_foret, newdata = test, type = "prob")

# 2. Fonction pour extraire le podium d'une ligne
get_podium <- function(row_probs) {
  # On trie les index du plus grand au plus petit
  ordered_idx <- order(row_probs, decreasing = TRUE)
  
  # On récupère les noms des classes et les valeurs
  # (On gère le cas où il y aurait moins de 3 classes au total)
  n_classes <- length(row_probs)
  
  c1 <- if(n_classes >= 1) names(row_probs)[ordered_idx[1]] else NA
  p1 <- if(n_classes >= 1) row_probs[ordered_idx[1]] else NA
  
  c2 <- if(n_classes >= 2) names(row_probs)[ordered_idx[2]] else NA
  p2 <- if(n_classes >= 2) row_probs[ordered_idx[2]] else NA
  
  c3 <- if(n_classes >= 3) names(row_probs)[ordered_idx[3]] else NA
  p3 <- if(n_classes >= 3) row_probs[ordered_idx[3]] else NA
  
  return(c(c1, p1, c2, p2, c3, p3))
}

# 3. Appliquer cette fonction à chaque ligne (c'est rapide)
# Le résultat est une matrice de caractères
podium_matrix <- t(apply(probs_matrix, 1, get_podium))

# On nomme les colonnes proprement
colnames(podium_matrix) <- c("Class_1", "Prob_1", "Class_2", "Prob_2", "Class_3", "Prob_3")

# 4. Convertir en Data Frame et remettre les types numériques
podium_df <- as.data.frame(podium_matrix, stringsAsFactors = FALSE)
podium_df$Prob_1 <- as.numeric(podium_df$Prob_1)
podium_df$Prob_2 <- as.numeric(podium_df$Prob_2)
podium_df$Prob_3 <- as.numeric(podium_df$Prob_3)

# 5. Fusionner avec tes données spatiales d'origine
resultat_detaille <- bind_cols(test, podium_df)

# --- Vérification ---
head(resultat_detaille %>% 
       select(Class_1, Prob_1, Class_2, Prob_2, Class_3, Prob_3), 10)

# 6. Export
st_write(resultat_detaille, "TGH_RF_Podium.gpkg", delete_layer = TRUE)














# 2️⃣ Construction de l'arbre
modele_arbre <- rpart(
  label ~ .,                # Prédire label en utilisant toutes les autres colonnes sélectionnées
  data = test_clean,
  method = "class",         # "class" car c'est une classification (pas des chiffres continus)
  cp = 0.005                # Paramètre de complexité (voir explication plus bas)
)

# 3️⃣ Visualisation (Le graphique que tu aimes)
# type = 4 : dessine tous les nœuds
# extra = 104 : affiche les probabilités pour chaque classe + le % d'observations
rpart.plot(
  modele_arbre, 
  type = 4, 
  extra = 104, 
  under = TRUE,    # Met le label sous la boîte pour plus de lisibilité
  cex = 0.7,       # Taille du texte (réduis si ça se chevauche)
  box.palette = "auto"
)


while (!is.null(dev.list())) dev.off()
dev.list()


pdf("arbre_simple.pdf", width = 15, height = 12)
rpart.plot(
  modele_arbre, 
  type = 4, 
  extra = 104, 
  under = TRUE,    # Met le label sous la boîte pour plus de lisibilité
  cex = 0.7,       # Taille du texte (réduis si ça se chevauche)
  box.palette = "auto"
)
dev.off()







# devtools::install_github('araastat/reprtree')

library(reprtree)

# ----------------------------
# 7️⃣ Extraction de l'Arbre Représentatif
# ----------------------------

# 1. Récupérer les prédictions individuelles de chaque arbre (500 colonnes)
pred_individuelles <- predict(modele_foret, newdata = test, predict.all = TRUE)$individual

# 2. Récupérer la prédiction globale de la forêt (le consensus)
pred_foret <- predict(modele_foret, newdata = test)

# 3. Calculer le taux de conformité de chaque arbre
# On regarde combien de fois chaque arbre est d'accord avec la décision finale
taux_accord <- apply(pred_individuelles, 2, function(col) {
  mean(col == pred_foret)
})

# 4. Trouver l'index de l'arbre "Medoid"
id_arbre_rep <- which.max(taux_accord)
cat("L'arbre le plus représentatif est le n°", id_arbre_rep, 
    "avec un taux d'accord de", round(max(taux_accord)*100, 2), "%\n")

# 5. Extraire les règles de cet arbre sous forme de tableau
arbre_data <- getTree(modele_foret, k = id_arbre_rep, labelVar = TRUE)
head(arbre_data)

# Visualisation simple
reprtree:::plot.getTree(modele_foret, k = id_arbre_rep)
# Alternative : Visualisation textuelle des 10 premières branches pour comprendre la logique
# Utile pour votre rapport écrit
print_tree_rules <- function(rf, k) {
  tree <- getTree(rf, k = k, labelVar = TRUE)
  return(tree)
}
rules <- print_tree_rules(modele_foret, id_arbre_rep)


#2. Fonction pour convertir un arbre de randomForest en objet visualisable
# (Cette étape est nécessaire car randomForest stocke les arbres de façon brute)
tree_to_party <- function(rf, k) {
  tree_dat <- getTree(rf, k = k, labelVar = TRUE)
  # Transformation du format pour partykit
  # Note : cette étape simplifiée permet de récupérer la structure
  return(tree_dat)
}
while (!is.null(dev.list())) dev.off()
dev.list()

# 3. Préparation du fichier PDF
pdf("Arbre_Representatif_Typo_Route.pdf", width = 20, height = 12) # Grand format pour la lisibilité

# On utilise reprtree pour le rendu graphique si installé, 
# sinon on dessine une version structurée
library(reprtree)
reprtree:::plot.getTree(modele_foret, k = id_arbre_rep)

dev.off() # Ferme et enregistre le PDF
