vars <- TGH_ID %>%
  select(mean_AC, mean_VB, mean_idx_water, mean_Slope_talweg, 
         Sinuosity_meander, mean_ACW_star, mean_elevation,
         mean_idx_conf,mean_enveloppe,multi_chenal,
         iles_veget,Delta_AC_relatif) %>%
  st_drop_geometry()


vars_scaled <- scale(vars)

# 3️⃣ Calcul de la matrice de distances
d <- dist(vars_scaled, method = "euclidean")

# 4️⃣ Classification hiérarchique ascendante (méthode Ward par ex.)
hc <- hclust(d, method = "ward.D2")

# 5️⃣ Visualisation du dendrogramme
plot(hc, labels = FALSE, hang = -1, main = "Classification hiérarchique (CAH)")

# 6️⃣ Découper en k classes (par ex. 3 classes)
clusters <- cutree(hc, k = 8)

# 7️⃣ Ajouter le résultat au dataframe
TGH_ID$cluster <- as.factor(clusters)

st_write(TGH_ID, "TGH_ID_cluster.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile



#boxplot des variables par cluster
library(ggplot2)

# Préparation des données pour le boxplot
df_plot <- TGH_ID %>%
  st_drop_geometry() %>%
  select(cluster, mean_AC, mean_VB, mean_idx_water, mean_Slope_talweg, 
         Sinuosity_meander, mean_ACW_star)

# Mise en format long (tidy)
df_long <- df_plot %>%
  pivot_longer(cols = -cluster, names_to = "variable", values_to = "valeur")

# Boxplot ggplot
ggplot(df_long, aes(x = cluster, y = valeur, fill = cluster)) +
  geom_boxplot(outlier.shape = 16, outlier.alpha = 0.4) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "Distribution des métriques par cluster (CAH)",
       x = "Cluster", y = "Valeur") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"))
