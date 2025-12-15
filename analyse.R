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
  labs(title = "Distribution des métriques par cluster (CAH)",
       x = "Cluster", y = "Valeur") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # rotation verticale
        strip.text = element_text(face = "bold"))
















# --- Packages ---
library(plotly)

# --- Données ---
x <- seq(1, 5, length.out = 400)

# --- Fonctions d’appartenance (µ) ---

# Classe 1 : de 1 à 1.1
mu1 <- ifelse(x <= 1, 0,
              ifelse(x <= 1.0, (x - 1)/(1.0 - 1),
                     ifelse(x <= 1.1, (1.1 - x)/(1.1 - 1.0), 0)))

# Classe 2 : de 1.0 à 1.3
mu2 <- ifelse(x <= 1.0, 0,
              ifelse(x <= 1.1, (x - 1.0)/(1.1 - 1.0),
                     ifelse(x <= 1.3, (1.3 - x)/(1.3 - 1.1), 0)))

# Classe 3 : de 1.1 à 1.3 puis reste à 1 jusqu’à 5
mu3 <- ifelse(x <= 1.1, 0,
              ifelse(x <= 1.3, (x - 1.1)/(1.3 - 1.1), 1))

# --- Dataframe ---
df <- data.frame(x, mu1, mu2, mu3)

# --- Graphique Plotly ---
p <- plot_ly(df, x = ~x, y = ~mu1, type = 'scatter', mode = 'lines',
             name = 'Classe 1', line = list(color = 'blue', width = 3)) %>%
  add_trace(y = ~mu2, name = 'Classe 2', mode = 'lines',
            line = list(color = 'orange', width = 3)) %>%
  add_trace(y = ~mu3, name = 'Classe 3', mode = 'lines',
            line = list(color = 'red', width = 3)) %>%
  layout(title = "Fuzzy Membership Functions (x ∈ [1,5])",
         xaxis = list(title = "x"),
         yaxis = list(title = "Degré d'appartenance µ(x)", range = c(0,1.1)),
         hovermode = "x unified")

p







library(plotly)

# --- Fonctions fuzzy
fuzzy_tri <- function(x, a, b, c) {
  res <- pmax(pmin((x - a)/(b - a), (c - x)/(c - b)), 0)
  res[is.nan(res)] <- 0
  res
}

fuzzy_trap <- function(x, a, b, c, d) {
  # trapèze classique, gère aussi cas extrême (d == c)
  res <- ifelse(x <= a, 0,
                ifelse(x <= b, (x - a)/(b - a),
                       ifelse(x <= c, 1,
                              ifelse(x <= d, (d - x)/(d - c), 0))))
  res[is.nan(res)] <- 0
  res
}

# --- Données ---
x <- seq(0.75, 5, length.out = 400)

# --- Fonctions d'appartenance corrigées ---
mu1 <- fuzzy_trap(x, a = 0.75, b = 0.75, c = 1.0, d = 1.2)
mu2 <- fuzzy_tri(x, a = 1.05, b = 1.20, c = 1.40)
mu3 <- fuzzy_trap(x, a = 1.25, b = 1.40, c = 2, d = 2)  # corrigé ici

# --- DataFrame ---
df <- data.frame(x, mu1, mu2, mu3)

# --- Graphique interactif ---
plot_ly(df, x = ~x, y = ~mu1, type = 'scatter', mode = 'lines',
        name = 'Classe 1', line = list(width = 3, color = 'blue')) %>%
  add_trace(y = ~mu2, name = 'Classe 2', mode = 'lines',
            line = list(width = 3, color = 'orange')) %>%
  add_trace(y = ~mu3, name = 'Classe 3', mode = 'lines',
            line = list(width = 3, color = 'red')) %>%
  layout(title = "Fonctions d'appartenance floues",
         xaxis = list(title = "x", range = c(0.75,2)),
         yaxis = list(title = "µ(x)", range = c(0,1.05)),
         hovermode = "x unified")











# Générer un exemple de données
x <- seq(1, 2, by = 0.01)  # données de 1 à 2

# Définir des fonctions d'appartenance sigmoïdes
membership1 <- function(x) 1 / (1 + exp(50*(x-1.1)))   # transition classe 1 -> 2
membership2 <- function(x) {
  1 / (1 + exp(50*(x-1.3))) - 1 / (1 + exp(50*(x-1.1)))  # classe 2
}
membership3 <- function(x) 1 / (1 + exp(-50*(x-1.3)))   # classe 3

# Calculer les valeurs
mu1 <- membership1(x)
mu2 <- membership2(x)
mu3 <- membership3(x)

# Vérifier la somme (pas forcément 1 exactement, mais logique floue)
plot(x, mu1, type="l", col="red", ylim=c(0,1), ylab="Membership", xlab="x")
lines(x, mu2, col="blue")
lines(x, mu3, col="green")
legend("topright", legend=c("Classe 1","Classe 2","Classe 3"), col=c("red","blue","green"), lty=1)









# Fonction sigmoïde
sigmoid <- function(x, a, c) {
  1 / (1 + exp(-a * (x - c)))
}

# Fonctions d'appartenance
mu1 <- function(x) {
  1 - sigmoid(x, a = 50, c = 1.1)
}

mu2 <- function(x) {
  sigmoid(x, a = 50, c = 1.1) * (1 - sigmoid(x, a = 50, c = 1.3))
}

mu3 <- function(x) {
  sigmoid(x, a = 50, c = 1.3)
}

# Exemple d'utilisation
x <- seq(0.5, 2, by = 0.01)
data.frame(
  x = x,
  Classe1 = sapply(x, mu1),
  Classe2 = sapply(x, mu2),
  Classe3 = sapply(x, mu3)
)


plot(x, sapply(x, mu1), type = "l", col = "blue", ylim = c(0, 1),
     main = "Fonctions d'appartenance (sigmoïdes)", xlab = "x", ylab = "Degré d'appartenance")
lines(x, sapply(x, mu2), col = "red")
lines(x, sapply(x, mu3), col = "green")










sig_dec <- function(x, c, s) {
  1 / (1 + exp((x - c) / s))   # décroissante
}

sig_inc <- function(x, c, s) {
  1 / (1 + exp((c - x) / s))   # croissante
}

membership_3classes <- function(x, t1, t2, s) {
  
  # Appartenance brute
  mu_rect      <- sig_dec(x, t1, s)
  mu_meandre   <- sig_inc(x, t1, s) * sig_dec(x, t2, s)
  mu_sinueux   <- sig_inc(x, t2, s)
  
  # Normalisation optionnelle (somme = 1)
  denom <- mu_rect + mu_meandre + mu_sinueux
  mu_rect_n    <- mu_rect    / denom
  mu_meandre_n <- mu_meandre / denom
  mu_sinueux_n <- mu_sinueux / denom
  
  return(list(
    raw  = c(rectiligne = mu_rect, meandre = mu_meandre, sinueux = mu_sinueux),
    norm = c(rectiligne = mu_rect_n, meandre = mu_meandre_n, sinueux = mu_sinueux_n)
  ))
}


t1 <- 1.1
t2 <- 2.0
s  <- 0.2

x_values <- c(0.5, 1.0, 1.2, 1.6, 2.5)

for (x in x_values) {
  res <- membership_3classes(x, t1, t2, s)
  cat("\nx =", x, "\n")
  print(round(res$raw, 3))
  print(round(res$norm, 3))
}





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

















ggplot(label, aes(x = mean_idx_water, y = mean_ACW_star, color = label)) +
  geom_point() +
  scale_x_continuous(n.breaks = 10)   # augmente le nombre de graduations  geom_point()



ggplot(TGH_ID, aes(x = mean_amplitude, y = Sinuosity_meander)) +
  geom_point() +
  scale_x_continuous(n.breaks = 10)   # augmente le nombre de graduations  geom_point()








