# ----------------------------
# 1. Chargement des données spatiales
# ----------------------------
TGH_ID <- st_read("TGH_ID_fr.gpkg")
# TGH    <- st_read("TGH.gpkg")

# 👉 Ajout d'un indice multi-chenaux depuis TGH
# TGH_ID <- TGH_ID %>%
#   left_join(
#     TGH %>% 
#       st_drop_geometry() %>% 
#       mutate(axis = as.character(axis)) %>% 
#       select(axis, ID_segment, multi_chenaux_index),
#     by = c("axis", "ID_segment")
#   ) 
  # mutate(
    # multi_chenaux_index = ifelse(multi_chenaux_index == 0, 1, multi_chenaux_index)
  # )




# label_visu <- TGH_ID %>%
#   select(axis, ID_segment) %>%
#   mutate(label = NA)
# st_write(label_visu, "label_visu.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile



label <- st_read("label_visu.gpkg") %>%
  # select(-label) %>%
  rename(label = label_final_finall)

label <- label %>%
  mutate(label = recode(label,
                        # "intermittent" = NA_character_,
                        "a voir tresse" = "tresse",
                        "a voir divagant" = "divagant",
                        "a voir meandre passif" = "meandre passif",
                        "a voir sinueux bars" = "sinueux bars",
                        "a voir rectiligne bars" = "rectiligne bars",
                        "a voir anastomose" = "anastomose",
                        "a voir meandre actif" = "meandre actif",
                        "a voir anabranche" = "anabranche",
                        "a voir voir anabranche" = "anabranche",
                        "a voir anabranche anastomose" = "anastomose",
                        
                        
  ))%>%
  filter(
    !grepl("^a voir|^avoir", label, ignore.case = TRUE)) %>%
  select(-label_v1, -option1)

table(label$label)






# 
# label <- st_read("label_visu.gpkg") %>%
#   select(-option1, -label_v1, -label_final) %>%
#   rename(label = label_final_herve)
# 
# # table(label$label)
# 
# label <- label %>%
#   mutate(label = recode(label,
#   "a voir meandre actif" = "meandre actif",
#   "a voir tresse" = "tresse",
#   "a voir divagant" = "divagant",
#   "a voir meandre passif" = "meandre passif",
#   "a voir sinueux bars" = "sinueux bars",
#   "a voir rectiligne bars" = "rectiligne bars",
#   "a voir anabranche" = "anabranche",
#   # "a voir anastomose" = "anastomose"
#   ))%>%
#   filter(
#     !grepl("^a voir|^avoir", label, ignore.case = TRUE),
#     !label %in% c(
#     "anabranche anastomose")
#   )
# table(label$label)


# ----------------------------
# 2. Nettoyage des labels (classes)
# ----------------------------
# label <- st_read("label_fr.gpkg") %>%
#   select(-label) %>%
#   rename(label = label_final_v2) %>%
#   
#   # 👉 suppression des classes ambiguës ou rares
#   filter(!label %in% c(
#     "diffus", "a voir", "tresse vegetal",
#     "alternate bars", "tresse intermittent"
#   ))
# 
# table(label$label)

# ----------------------------
# 3. Jointure + création de variables
# ----------------------------
test <- TGH_ID %>%
  
  # 👉 jointure des labels
  left_join(
    label %>% st_drop_geometry() %>%
      mutate(axis = as.character(axis)) %>%
      select(axis, ID_segment, label),
    by = c("axis", "ID_segment")
  ) %>%
  
  ungroup() %>%
  group_by(axis) %>%
  
  # 👉 création d'indicateurs dérivés
  mutate(
    step_WC = ifelse(is.na(Delta_WC), 0, Delta_WC),
    # step_WC = (lag(mean_WC) - mean_WC)/ (lag(mean_WC) + mean_WC),
    # step_WC = ifelse(is.na(step_WC), 0, step_WC),
    
    # indice eau robuste
    mean_idx_water = case_when(
      mean_AC == 0 & mean_WC == 0 ~ 1,
      mean_WC == 0 & mean_AC > 0 ~ 0,
      TRUE ~ mean_WC / mean_AC
    )
  ) %>%
  ungroup()
  

# test1 <- test %>%
#   filter(axis == "2000801955")


# st_write(test, "test25.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile)
# ----------------------------
# 4. Sélection des variables utiles
# ----------------------------

colonnes_a_exclure <- c(
  "ID_segment", "toponyme", "nb_DGO", "axis", "source",
  "sum_length", "length_meander", "drainage_area",
  "mean_meander_belt", "Delta_AC", "Lag_AC",
  "Delta_AC_relatif", "Lag_AC_relatif", "measure",
  "mean_angle_deg", "mean_idx_conf", "mean_VB",
  "mean_active_channel_pc", "mean_water_channel_pc",
  "mean_forest_pc", "mean_grassland_pc", "mean_crops_pc",
  "mean_diffuse_urban_pc", "mean_dense_urban_pc",
  "mean_infrastructures_pc", "mean_riparian_corridor_pc",
  "mean_semi_natural_pc", "mean_reversible_pc",
  "mean_disconnected_pc_corrige", "mean_built_environment_pc",
  "mean_natural_open_pc", "mean_gravel_bars_pc",
  "gid_region", "strahler", "nb_na", "mean_Slope_VB",
  "na_pct", "mean_elevation", "Sinuosity_meander_1",
  "longueur_data", "sum_conf_degree", "retenue",
  # "dif_lenght_meander",
  "dif_longueur",
  # "length_segment",
  "idx_conf_segment",
  "mean_idx_water",
  # "ID_DGO",
  # "multi_chenal", 
  "Delta_WC",
  "mean_Slope_talweg",
  "mean_AC"
  
)

# 👉 ajout info NA
test <- test %>%
  mutate(
    nb_na = TGH_ID$nb_na,
    na_pct = nb_na / nb_DGO * 100
  )

# 👉 dataset final pour modèle
test_clean <- test %>% 
  st_drop_geometry() %>%
  drop_na(label) %>%
  select(-all_of(colonnes_a_exclure)) %>%
  drop_na()

test_clean$label <- factor(test_clean$label)

table(test_clean$label)

# ----------------------------
# 5. Split apprentissage / validation
# ----------------------------
set.seed(123)

index <- createDataPartition(test_clean$label, p = 0.8, list = FALSE)

train   <- test_clean[index, ]
testset <- test_clean[-index, ]

# ----------------------------
# 6. Entraînement du modèle
# ----------------------------
modele_foret <- randomForest(
  label ~ .,
  data = train,
  importance = TRUE,
  ntree = 500,
  mtry = floor(sqrt(ncol(train) - 1))
)

# ----------------------------
# 7. Évaluation des performances
# ----------------------------
pred_test <- predict(modele_foret, newdata = testset)

confusion <- confusionMatrix(pred_test, testset$label)

print(confusion)

# 👉 métriques principales
mean(confusion$byClass[, "Recall"], na.rm = TRUE)
mean(confusion$byClass[, "Precision"], na.rm = TRUE)
mean(confusion$byClass[, "F1"], na.rm = TRUE)
confusion$overall["Accuracy"]
confusion$overall["Kappa"]
mean(confusion$byClass[, "Specificity"], na.rm = TRUE)

# ----------------------------
# 8. Heatmap de confusion
# ----------------------------
conf_df <- as.data.frame(confusion$table)
colnames(conf_df) <- c("Reference", "Prediction", "Freq")

conf_df <- conf_df %>%
  group_by(Reference) %>%
  mutate(Percent = Freq / sum(Freq) * 100)

ggplot(conf_df, aes(x = Reference, y = Prediction, fill = Percent)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 4) +
  scale_fill_gradient(low = "#f7fbff", high = "#08306b") +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  ) +
  labs(x = "Observed", y = "Predicted", fill = "%")

# ----------------------------
# 9. Importance des variables
# ----------------------------
windows()
varImpPlot(modele_foret, type = 1)
# ----------------------------
# 10. Prédictions finales
# ----------------------------

# classe prédite
pred_class <- predict(modele_foret, newdata = test, type = "response")

# probabilités
pred_prob_matrix <- predict(modele_foret, newdata = test, type = "prob")

# confiance (max prob)
max_prob <- apply(pred_prob_matrix, 1, max)

# assemblage final
resultat_final <- test %>%
  mutate(
    Prediction  = pred_class,
    Probabilite = max_prob
  )

# export
st_write(resultat_final, "TGH_testnew.gpkg", delete_layer = TRUE)
# str(resultat_final)
# ----------------------------
# 11. Corrélation entre variables
# ----------------------------
data_numeric <- test_clean %>%
  select(-label) %>%
  select(where(is.numeric))

cor_matrix <- cor(data_numeric, method = "spearman", use = "complete.obs")

corrplot(cor_matrix,
         method = "color",
         type = "upper",
         order = "hclust",
         tl.cex = 0.7)

# clustering des variables
dist_matrix <- as.dist(1 - abs(cor_matrix))
hc <- hclust(dist_matrix, method = "ward.D2")

plot(hc)
abline(h = 0.20, col = "red", lty = 2)

clusters <- split(names(cutree(hc, h = 0.2)),
                  cutree(hc, h = 0.2))

print(clusters)


# ----------------------------
# SURROGATE 1 : train → test
# ----------------------------
# TRAIN (80%)
surrogate_train <- train %>%
  mutate(
    rf_pred = predict(modele_foret, newdata = train)
  ) %>%
  select(-label)  # on veut juste apprendre à imiter le RF, pas les

# TEST (20%)
surrogate_test <- testset %>%
  mutate(
    rf_pred = predict(modele_foret, newdata = testset)
  )

# entraînement surrogate
surrogate_tree_1 <- rpart(
  rf_pred ~ .,  # 👉 on apprend le RF
  data = surrogate_train,
  method = "class",
  control = rpart.control(
    maxdepth = 10,     # 👉 arbre simple = interprétable
    minsplit = 30,
    cp = 0.001
  )
)

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

print(conf_fidelity_1)

cat("Surrogate 1 - Fidelity vs RF:",
    conf_fidelity_1$overall["Accuracy"], "\n")


# COMPARAISON SURROGATE vs label (fidélité réelle)
conf_real_1 <- confusionMatrix(
  surrogate_pred_test,
  surrogate_test$label
)

print(conf_real_1)

cat("Surrogate 1 - Accuracy vs label :",
    conf_real_1$overall["Accuracy"], "\n")


# # prédiction surrogate sur RF selon DF total
# surrogate_data_global <- resultat_final %>%
#   st_drop_geometry() %>%
#   select(Prediction, Sinuosity_meander_2, mean_idx_water, mean_WC,
#          mean_ACW_star, iles_veget, Delta_AC, multi_chenaux_index,
#          step_AC_na)  %>%
#   filter(!is.na(Prediction))
# surrogate_data_global$Prediction <- factor(surrogate_data_global$Prediction)
# 
# 
# surrogate_pred_test_1 <- predict(
#   surrogate_tree_1,
#   surrogate_data_global,
#   type = "class"
# )
# 
# conf_fidelity_1 <- confusionMatrix(
#   surrogate_pred_test_1,
#   surrogate_data_global$Prediction
# )
# 
# print(conf_fidelity_1)
# 
# cat("Surrogate 1 - Fidelity vs RF:",
#     conf_fidelity_1$overall["Accuracy"], "\n")



# ----------------------------
# surrogate model 2 : donnée global
# ----------------------------
surrogate_data_global <- resultat_final %>%
  st_drop_geometry() %>%
  select(Prediction, Sinuosity_meander_2, mean_idx_water, mean_WC,
         mean_ACW_star, iles_veget, Delta_AC, multi_chenaux_index,
         step_AC_na)  %>%
  filter(!is.na(Prediction))
surrogate_data_global$Prediction <- factor(surrogate_data_global$Prediction)

# ENTRAINEMENT SURROGATE GLOBAL
surrogate_tree_2 <- rpart(
  Prediction ~ .,
  data = surrogate_data_global,
  method = "class",
  control = rpart.control(
    maxdepth = 10,
    minsplit = 30,
    cp = 0.001
  )
)

# prédiction surrogate sur RF
surrogate_pred_test_2 <- predict(
  surrogate_tree_2,
  surrogate_data_global,
  type = "class"
)

conf_fidelity_2 <- confusionMatrix(
  surrogate_pred_test_2,
  surrogate_data_global$Prediction
)

print(conf_fidelity_2)

cat("Surrogate 2 - Fidelity vs RF:",
    conf_fidelity_2$overall["Accuracy"], "\n")






# ------------------------------------------------------------------
# Application Avancée : Récupérer le Podium (1er, 2ème, 3ème)
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












