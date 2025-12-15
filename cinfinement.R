table(resultat_final$Prediction)

resultat_conf <- resultat_final %>%
  mutate(
    mean_enveloppe = mean_enveloppe/100
    confinement = case_when(
      Prediction %in% c("rectiligne", "rectiligne bars", "sinueux", "sinueux ba", 
                        "meandre passif", "meandre actif", "intermittent") & mean_enveloppe > 0.7 ~ "contraint",
      Prediction %in% c("rectiligne", "rectiligne bars", "sinueux", "sinueux ba", 
                        "meandre passif", "meandre actif", "intermittent") & mean_enveloppe > 0.4 & mean_enveloppe <= 0.7 ~ "partially contraint",
      Prediction %in% c("rectiligne", "rectiligne bars", "sinueux", "sinueux ba", 
                        "meandre passif", "meandre actif", "intermittent") & mean_enveloppe <= 0.4 ~ "non contraint",
      Prediction %in% c("tresse", "tresse vegetal","tresse intermittent", 
                        "divagant") & mean_idx_conf > 0.7 ~ "contraint",
      Prediction %in% c("tresse", "tresse vegetal","tresse intermittent", 
                        "divagant") & mean_idx_conf > 0.4 & mean_idx_conf <= 0.7 ~ "partially contraint",
      Prediction %in% c("tresse", "tresse vegetal","tresse intermittent", 
                        "divagant") & mean_idx_conf <= 0.4 ~ "non contraint",
      Prediction %in% c("anamostose") & mean_idx_conf > 0.7 ~ "contraint",
      Prediction %in% c("anamostose") & mean_idx_conf > 0.4 & mean_idx_conf <= 0.7 ~ "partially contraint",
      Prediction %in% c("anamostose") & mean_idx_conf <= 0.4 ~ "non contraint",
      TRUE ~ Prediction
    )
  )

resultat_conf <- resultat_conf %>%
  mutate(
    style_confinement = case_when(
      Prediction %in% c("rectiligne", "rectiligne bars") & confinement == "Contraint" ~ "confined",
      Prediction %in% c("rectiligne", "rectiligne bars") & confinement == "partially contraint" ~ "semi-confined",
      Prediction %in% c("rectiligne", "rectiligne bars") & confinement == "Non contraint" ~ "endigué",
      Prediction %in% c("sinueux", "sinueux ba") & confinement == "Contraint" ~ "confined",
      Prediction %in% c("sinueux", "sinueux ba") & confinement == "partially contraint" ~ "semi-confined",
      Prediction %in% c("sinueux", "sinueux ba") & confinement == "Non contraint" ~ "no confined",
      Prediction %in% c("meandre", "meandre bars") & confinement == "Contraint" ~ "incised meander",
      Prediction %in% c("meandre", "meandre bars") & confinement == "partially contraint" ~ "meander",
      Prediction %in% c("meandre", "meandre bars") & confinement == "Non contraint" ~ "free meander",
      Prediction %in% c("tresse", "tresse vegetal") & confinement == "Contraint" ~ "braided confined",
      Prediction %in% c("tresse", "tresse vegetal") & confinement == "partially contraint" ~ "braided",
      Prediction %in% c("tresse", "tresse vegetal") & confinement == "Non contraint" ~ "free braided",
      Prediction %in% c("divagant") & confinement == "Contraint" ~ "wandering confined",
      Prediction %in% c("divagant") & confinement == "partially contraint" ~ "wandering",
      Prediction %in% c("divagant") & confinement == "Non contraint" ~ "free wandering",
      TRUE ~ confinement
    )
  )

write_sf(resultat_conf, "TGH_confinement.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile
