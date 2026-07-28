resultat_final_filtre <- resultat_final %>%
  mutate(
    Prediction = if_else(
      Prediction == "tresse" & !gid_region %in% c(16, 11, 33, 26),
      NA_character_,
      Prediction
    )
  )

resultat_final_filtre <- resultat_final_filtre %>%
  mutate(
    Prediction_en = recode(Prediction,
                           "rectiligne"        = "Straight",
                           "rectiligne bars"   = "Straight with alternate bars",
                           "sinueux"           = "Sinuous",
                           "sinueux bars"      = "Sinuous with bars",
                           "meandre actif"     = "Active meandering",
                           "meandre passif"    = "Passive meandering",
                           "tresse"            = "Braided",
                           "ile eparses"       = "Sparse islands",
                           "divagant"          = "Wandering",
                           "anastomose"        = "Anastomosed",
                           "anabranche"        = "Anabranching",
                           "reservoir"         = "Reservoir",
                           "intermittent"      = "Intermittent",
    )
  )


cat(
  sum(resultat_final$longueur_data < 575, na.rm = TRUE),
  "segments ont une longueur < 500 m.\n"
)


resultat_final_filtre <- resultat_final_filtre %>%
  mutate(
    Prediction_en = if_else(longueur_data < 575, NA_character_, Prediction_en),
    Prediction    = if_else(longueur_data < 575, NA_character_, Prediction)
  )

st_write(resultat_final_filtre, "resultat_final_na.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile

resultat_final_filtre <- resultat_final_filtre %>%
  filter(!is.na(Prediction_en))


st_write(resultat_final_filtre, "resultat_final_filtre.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile


sum(resultat_final_filtre$longueur_data)
