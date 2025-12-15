
# histogramme du nb de sgments homogènes par classe
resultat_final %>%
  filter(!Prediction == "Pas de lit") %>%
  ggplot() +
  aes(x = Prediction) +
  geom_bar(aes(fill = Prediction)) +
  theme_minimal()


# histogramme du nb de sgments homogènes par classe
test1 <- resultat_final %>%
  group_by(Prediction) %>%
  summarize(long = sum(sum_length)/1000)

test1 %>%
  filter(Prediction != "Pas de lit") %>%
  ggplot(aes(x = Prediction, y = long, fill = Prediction)) +
  geom_col() +   # ou geom_bar(stat = "identity") si long contient déjà des valeurs
  theme_minimal()


# camenbert pour la longueur par classe
pie_data <- test1 %>%
  filter(Prediction != "Pas de lit") %>%
  group_by(Prediction) %>%
  summarise(total_long = sum(long, na.rm = TRUE)) %>%  # somme de long par catégorie
  ungroup() %>%
  mutate(percent = total_long / sum(total_long) * 100)  # pourcentage par rapport au total

ggplot(pie_data, aes(x = "", y = percent, fill = Prediction)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) 
