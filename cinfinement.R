library(circlize)


# resultat_final <- st_read("TGH_RF_total.gpkg") %>%
#   left_join(
#     st_drop_geometry(TGH) %>% select(axis, ID_segment, sum_conf_degree),
#     by = c("axis", "ID_segment")) %>%
#   mutate(CD = sum_conf_degree/sum_length
#            )

resultat_final <- resultat_final %>%
  mutate(CD = sum_conf_degree / sum_length)

resultat_final <- resultat_final %>%
  mutate(
    Prediction_en = recode(Prediction,
                           "rectiligne"        = "Straight",
                           "rectiligne bars"   = "Alternate bars",
                           "sinueux"           = "Sinuous",
                           # "alternate bars"  = "Alternate bars",
                           "sinueux bars"      = "Sinuous with bars",
                           "meandre actif"     = "Active meandering",
                           "meandre passif"    = "Passive meandering",
                           "tresse"            = "Braided",
                           # "tresse vegetal"  = "Vegetated braided",
                           "divagant"          = "Wandering",
                           "anastomose"        = "Anastomosed",
                           "anabranche"        = "Anabranching",
                           "reservoir"           = "Reservoir",
                           "intermittent"      = "Intermittent",
    )
  )


resultat_conf <- resultat_final %>%
  mutate(
    # idx_conf_inverse2 = case_when(
    #   mean_AC == 0 & mean_VB > 0 ~ 0,
    #   TRUE ~ mean_VB / mean_AC
    # ),
    idx_conf_inverse = mean_VB / mean_AC,
    
    CD = ifelse(mean_AC < 4 & mean_Slope_talweg > 0.05, 1, CD), # pour eviter mettre en confiné les secteurs pentu sans ac
    
    type_plan = Prediction %in% c(
      "rectiligne", "rectiligne bars",
      "sinueux", "sinueux bars", "alternate bars",
      "meandre passif", "meandre actif" 
      # "retenue"
    ),
    
    type_multi = Prediction %in% c(
      "tresse", "tresse vegetal",
      "divagant", "anastomose", "anabranche"
    ),
    
    type_reservoir = Prediction %in% c("retenue","intermittent"),
    
    conf_simple = case_when(
      
      # ---------------- PLAN ----------------
      type_plan & (
        CD > 0.90 |
          (CD > 0.10 & CD <= 0.90 & idx_conf_inverse <= 1.5)
      ) ~ "confined",
      
      type_plan & (
        (CD > 0.10 & CD <= 0.90 & idx_conf_inverse > 1.5) |
          (CD <= 0.10 & idx_conf_inverse <= 5)
      ) ~ "partly confined",
      
      type_plan & (
        CD <= 0.10 & idx_conf_inverse > 5 |
          (CD = 0 & idx_conf_inverse > 5)
      ) ~ "unconfined",
      
      
      # ---------------- MULTI ----------------
      type_multi & (
        CD > 0.90 |
          (CD > 0.10 & CD <= 0.90 & idx_conf_inverse <= 1.5)
      ) ~ "confined",
      
      type_multi & (
        (CD > 0.10 & CD <= 0.90 & idx_conf_inverse > 1.5) |
          (CD <= 0.10 & idx_conf_inverse <= 2)
      ) ~ "partly confined",
      
      type_multi & (
        CD <= 0.10 & idx_conf_inverse > 2 |
          (CD == 0 & idx_conf_inverse > 2)
      ) ~ "unconfined",
      
      type_reservoir ~ "partly confined",
      
      TRUE ~ NA_character_
    ),
    
    conf_detaille = case_when(
      Prediction_en %in% c(
        "Straight",
        "Straight with bars",
        "Sinuous",
        "Sinuous with bars",
        "Alternate bars",
        "Passive meandering",
        "Active meandering",
        "Braided",
        "Wandering",
        "Anastomosed",
        "Anabranching"
        
      ) ~ paste(Prediction_en, conf_simple),
      
      Prediction_en == "Reservoir" ~ "Reservoir",
      Prediction_en == "Intermittent" ~ "Intermittent",
      
      TRUE ~ NA_character_
    )
  )

resultat_conf <- resultat_conf %>%
  mutate(
   order_figure = case_when(
      Prediction %in% c("meandre actif", "meandre passif") ~ 1,
      TRUE ~ 2
  ))

st_write(resultat_conf, "TGH_conf.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile
# st_write(resultat_conf, "TGH_terrain.shp", delete_layer = TRUE) # Export des données nettoyées en shapefile

# resultat_final <- st_read("TGH_RF_10.gpkg")
# resultat_conf <- st_read("TGH_RF_total_conf.gpkg")







table(resultat_final$Prediction)

resultat_conf <- resultat_final %>%
  mutate(
    mean_meander_belt = mean_meander_belt/100,
    confinement = case_when(
      Prediction %in% c("rectiligne", "rectiligne bars", "sinueux", "sinueux ba", 
                        "meandre passif", "meandre actif") & mean_meander_belt > 0.7 ~ "contraint",
      Prediction %in% c("rectiligne", "rectiligne bars", "sinueux", "sinueux ba", 
                        "meandre passif", "meandre actif") & mean_meander_belt > 0.4 & mean_meander_belt <= 0.7 ~ "partially contraint",
      Prediction %in% c("rectiligne", "rectiligne bars", "sinueux", "sinueux ba", 
                        "meandre passif", "meandre actif") & mean_meander_belt <= 0.4 ~ "non contraint",
      Prediction %in% c("tresse", "tresse vegetal", 
                        "divagant") & mean_idx_conf > 0.7 ~ "contraint",
      Prediction %in% c("tresse", "tresse vegetal", 
                        "divagant") & mean_idx_conf > 0.4 & mean_idx_conf <= 0.7 ~ "partially contraint",
      Prediction %in% c("tresse", "tresse vegetal", 
                        "divagant") & mean_idx_conf <= 0.4 ~ "non contraint",
      Prediction %in% c("anastomose") & mean_idx_conf > 0.7 ~ "contraint",
      Prediction %in% c("anastomose") & mean_idx_conf > 0.4 & mean_idx_conf <= 0.7 ~ "partially contraint",
      Prediction %in% c("anastomose") & mean_idx_conf <= 0.4 ~ "non contraint",
      TRUE ~ NA_character_
    )
  )

resultat_conf <- resultat_conf %>%
  mutate(
    style_confinement_simple = case_when(
      Prediction %in% c("rectiligne", "rectiligne bars") & confinement == "contraint" ~ "Straight confined",
      Prediction %in% c("rectiligne", "rectiligne bars") & confinement == "partially contraint" ~ "Straight semi-confined",
      Prediction %in% c("rectiligne", "rectiligne bars") & confinement == "non contraint" ~ "Straight no confined",
      Prediction %in% c("sinueux", "sinueux ba") & confinement == "contraint" ~ "sinuous confined",
      Prediction %in% c("sinueux", "sinueux ba") & confinement == "partially contraint" ~ "sinuous semi-confined",
      Prediction %in% c("sinueux", "sinueux ba") & confinement == "non contraint" ~ "sinuous no confined",
      Prediction %in% c("meandre passif", "meandre actif") & confinement == "contraint" ~ "incised meander",
      Prediction %in% c("meandre passif", "meandre actif") & confinement == "partially contraint" ~ "meander semi-confined",
      Prediction %in% c("meandre passif", "meandre actif") & confinement == "non contraint" ~ "free meander",
      Prediction %in% c("tresse", "tresse vegetal") & confinement == "contraint" ~ "braided confined",
      Prediction %in% c("tresse", "tresse vegetal") & confinement == "partially contraint" ~ "braided semi-confined",
      Prediction %in% c("tresse", "tresse vegetal") & confinement == "non contraint" ~ "free braided",
      Prediction %in% c("divagant") & confinement == "contraint" ~ "wandering confined",
      Prediction %in% c("divagant") & confinement == "partially contraint" ~ "wandering semi-confined",
      Prediction %in% c("divagant") & confinement == "non contraint" ~ "free wandering",
      Prediction %in% c("anastomose") & confinement == "contraint" ~ "anastomosed confined",
      Prediction %in% c("anastomose") & confinement == "partially contraint" ~ "anastomosed semi-confined",
      Prediction %in% c("anastomose") & confinement == "non contraint" ~ "free anastomosed",
      TRUE ~ confinement
    )
  )
  
  resultat_conf <- resultat_conf %>%
    mutate(
      style_confinement_2 = case_when(
        Prediction %in% c("rectiligne") & confinement == "contraint" ~ "rectiligne confined",
        Prediction %in% c("rectiligne") & confinement == "partially contraint" ~ "rectiligne semi-confined",
        Prediction %in% c("rectiligne") & confinement == "non contraint" ~ "rectiligne endigué",
        Prediction %in% c("rectiligne bars") & confinement == "contraint" ~ "rectiligne bars confined",
        Prediction %in% c("rectiligne bars") & confinement == "partially contraint" ~ "rectiligne bars semi-confined",
        Prediction %in% c("rectiligne bars") & confinement == "non contraint" ~ "rectiligne bars endigué",
        Prediction %in% c("sinueux") & confinement == "contraint" ~ "sinueux confined",
        Prediction %in% c("sinueux") & confinement == "partially contraint" ~ "sinueux semi-confined",
        Prediction %in% c("sinueux") & confinement == "non contraint" ~ "sinueux no confined",
        Prediction %in% c("sinueux ba") & confinement == "contraint" ~ "sinueux bars confined",
        Prediction %in% c("sinueux ba") & confinement == "partially contraint" ~ "sinueux bars semi-confined",
        Prediction %in% c("sinueux ba") & confinement == "non contraint" ~ "sinueux bars no confined",
        Prediction %in% c("meandre") & confinement == "contraint" ~ "incised meander",
        Prediction %in% c("meandre") & confinement == "partially contraint" ~ "meander semi-confined ",
        Prediction %in% c("meandre") & confinement == "non contraint" ~ "free meander",
        Prediction %in% c("meandre bars") & confinement == "contraint" ~ "incised meander bars",
        Prediction %in% c("meandre bars") & confinement == "partially contraint" ~ "meander bars semi-confined ",
        Prediction %in% c("meandre bars") & confinement == "non contraint" ~ "free meander bars",
        Prediction %in% c("divagant") & confinement == "contraint" ~ "Artificialised",
        Prediction %in% c("divagant") & confinement == "partially contraint" ~ "wandering semi-confined",
        Prediction %in% c("divagant") & confinement == "non contraint" ~ "free wandering",
        Prediction %in% c("tresse") & confinement == "contraint" ~ "braided confined",
        Prediction %in% c("tresse") & confinement == "partially contraint" ~ "braided semi-confined",
        Prediction %in% c("tresse") & confinement == "non contraint" ~ "free braided",
        Prediction %in% c("tresse vegetal") & confinement == "contraint" ~ "braided vegetal confined",
        Prediction %in% c("tresse vegetal") & confinement == "partially contraint" ~ "braided vegetal semi-confined",
        Prediction %in% c("tresse vegetal") & style_confinement_simple == "non contraint" ~ "free braided vegetal",
        TRUE ~ style_confinement_simple
      )
    )

  
  
write_sf(resultat_conf, "TGH_confinement.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile





























resultat_conf <- resultat_final %>%
  mutate(
    mean_enveloppe = mean_enveloppe/100,
    confinement = case_when(
      noeudfinal %in% c("rectiligne", "rectiligne bars", "sinueux", "sinueux ba", 
                        "meandre passif", "meandre actif", "intermittent") & mean_enveloppe > 0.7 ~ "contraint",
      noeudfinal %in% c("rectiligne", "rectiligne bars", "sinueux", "sinueux ba", 
                        "meandre passif", "meandre actif", "intermittent") & mean_enveloppe > 0.4 & mean_enveloppe <= 0.7 ~ "partially contraint",
      noeudfinal %in% c("rectiligne", "rectiligne bars", "sinueux", "sinueux ba", 
                        "meandre passif", "meandre actif", "intermittent") & mean_enveloppe <= 0.4 ~ "non contraint",
      noeudfinal %in% c("tresse", "tresse vegetal","tresse intermittent", 
                        "divagant") & mean_idx_conf > 0.7 ~ "contraint",
      noeudfinal %in% c("tresse", "tresse vegetal","tresse intermittent", 
                        "divagant") & mean_idx_conf > 0.4 & mean_idx_conf <= 0.7 ~ "partially contraint",
      noeudfinal %in% c("tresse", "tresse vegetal","tresse intermittent", 
                        "divagant") & mean_idx_conf <= 0.4 ~ "non contraint",
      noeudfinal %in% c("anamostose") & mean_idx_conf > 0.7 ~ "contraint",
      noeudfinal %in% c("anamostose") & mean_idx_conf > 0.4 & mean_idx_conf <= 0.7 ~ "partially contraint",
      noeudfinal %in% c("anamostose") & mean_idx_conf <= 0.4 ~ "non contraint",
      TRUE ~ noeudfinal
    )
  )

resultat_conf <- resultat_conf %>%
  mutate(
    style_confinement_simple = case_when(
      noeudfinal %in% c("rectiligne", "rectiligne bars") & confinement == "contraint" ~ "rectiligne confined",
      noeudfinal %in% c("rectiligne", "rectiligne bars") & confinement == "partially contraint" ~ "rectiligne semi-confined",
      noeudfinal %in% c("rectiligne", "rectiligne bars") & confinement == "non contraint" ~ "rectiligne endigué",
      noeudfinal %in% c("sinueux", "sinueux ba") & confinement == "contraint" ~ "sinueux confined",
      noeudfinal %in% c("sinueux", "sinueux ba") & confinement == "partially contraint" ~ "sinueux semi-confined",
      noeudfinal %in% c("sinueux", "sinueux ba") & confinement == "non contraint" ~ "sinueux no confined",
      noeudfinal %in% c("meandre passif", "meandre actif") & confinement == "contraint" ~ "incised meander",
      noeudfinal %in% c("meandre passif", "meandre actif") & confinement == "partially contraint" ~ "meander semi-confined",
      noeudfinal %in% c("meandre passif", "meandre actif") & confinement == "non contraint" ~ "free meander",
      noeudfinal %in% c("tresse", "tresse vegetal") & confinement == "contraint" ~ "braided confined",
      noeudfinal %in% c("tresse", "tresse vegetal") & confinement == "partially contraint" ~ "braided semi-confined",
      noeudfinal %in% c("tresse", "tresse vegetal") & confinement == "non contraint" ~ "free braided",
      noeudfinal %in% c("divagant") & confinement == "contraint" ~ "wandering confined",
      noeudfinal %in% c("divagant") & confinement == "partially contraint" ~ "wandering semi-confined",
      noeudfinal %in% c("divagant") & confinement == "non contraint" ~ "free wandering",
      noeudfinal %in% c("anamostose") & confinement == "contraint" ~ "anastomosed confined",
      noeudfinal %in% c("anamostose") & confinement == "partially contraint" ~ "anastomosed semi-confined",
      noeudfinal %in% c("anamostose") & confinement == "non contraint" ~ "free anastomosed",
      noeudfinal %in% c("intermittent") & confinement == "contraint" ~ "intermittent confined",
      noeudfinal %in% c("intermittent") & confinement == "partially contraint" ~ "intermittent semi-confined",
      noeudfinal %in% c("intermittent") & confinement == "non contraint" ~ "intermittent no confined",
      TRUE ~ confinement
    )
  )


resultat_conf <- resultat_conf %>%
  mutate(
    style_confinement = case_when(
      noeudfinal %in% c("rectiligne") & confinement == "contraint" ~ "rectiligne confine",
      noeudfinal %in% c("rectiligne") & confinement == "partially contraint" ~ "rectiligne semi-confine",
      noeudfinal %in% c("rectiligne") & confinement == "non contraint" ~ "rectiligne non confine",
      noeudfinal %in% c("rectiligne bars") & confinement == "contraint" ~ "rectiligne bars confine",
      noeudfinal %in% c("rectiligne bars") & confinement == "partially contraint" ~ "rectiligne bars semi-confine",
      noeudfinal %in% c("rectiligne bars") & confinement == "non contraint" ~ "rectiligne bars non confine",
      noeudfinal %in% c("sinueux") & confinement == "contraint" ~ "sinueux confine",
      noeudfinal %in% c("sinueux") & confinement == "partially contraint" ~ "sinueux semi-confine",
      noeudfinal %in% c("sinueux") & confinement == "non contraint" ~ "sinueux non confine",
      noeudfinal %in% c("sinueux ba") & confinement == "contraint" ~ "sinueux bars confine",
      noeudfinal %in% c("sinueux ba") & confinement == "partially contraint" ~ "sinueux bars semi-confine",
      noeudfinal %in% c("sinueux ba") & confinement == "non contraint" ~ "sinueux bars non confine",
      noeudfinal %in% c("meandre passif") & confinement == "contraint" ~ "meandre passif confine",
      noeudfinal %in% c("meandre passif") & confinement == "partially contraint" ~ "meandre passif semi-confine",
      noeudfinal %in% c("meandre passif") & confinement == "non contraint" ~ "meandre passif non confine",
      noeudfinal %in% c("meandre actif") & confinement == "contraint" ~ "meandre actif confine",
      noeudfinal %in% c("meandre actif") & confinement == "partially contraint" ~ "meandre actif semi-confine",
      noeudfinal %in% c("meandre actif") & confinement == "non contraint" ~ "meandre actif non confine",
      noeudfinal %in% c("tresse") & confinement == "contraint" ~ "tresse confine",
      noeudfinal %in% c("tresse") & confinement == "partially contraint" ~ "tresse semi-confine",
      noeudfinal %in% c("tresse") & confinement == "non contraint" ~ "tresse non confine",
      Prediction %in% c("tresse vegetal") & confinement == "contraint" ~ "tresse vegetal confined",
      noeudfinal %in% c("tresse vegetal") & confinement == "partially contraint" ~ "tresse vegetal semi-confined",
      noeudfinal %in% c("tresse vegetal") & style_confinement_simple == "non contraint" ~ "free tresse vegetal",
      noeudfinal %in% c("divagant") & confinement == "contraint" ~ "wandering confine",
      noeudfinal %in% c("divagant") & confinement == "partially contraint" ~ "wandering semi-confine",
      noeudfinal %in% c("divagant") & confinement == "non contraint" ~ "wandering non confine",
      noeudfinal %in% c("anamostose") & confinement == "contraint" ~ "anastomosed confine",
      noeudfinal %in% c("anamostose") & confinement == "partially contraint" ~ "anastomosed semi-confine",
      noeudfinal %in% c("anamostose") & confinement == "non contraint" ~ "anastomosed non confine",
      TRUE ~ style_confinement_simple
    )
  )


resultat_conf <- resultat_conf %>%
  mutate(
    style_confinement_2 = case_when(
      noeudfinal %in% c("rectiligne") & confinement == "contraint" ~ "rectiligne confined",
      noeudfinal %in% c("rectiligne") & confinement == "partially contraint" ~ "rectiligne semi-confined",
      noeudfinal %in% c("rectiligne") & confinement == "non contraint" ~ "rectiligne endigué",
      noeudfinal %in% c("rectiligne bars") & confinement == "contraint" ~ "rectiligne bars confined",
      noeudfinal %in% c("rectiligne bars") & confinement == "partially contraint" ~ "rectiligne bars semi-confined",
      noeudfinal %in% c("rectiligne bars") & confinement == "non contraint" ~ "rectiligne bars endigué",
      noeudfinal %in% c("sinueux") & confinement == "contraint" ~ "sinueux confined",
      noeudfinal %in% c("sinueux") & confinement == "partially contraint" ~ "sinueux semi-confined",
      noeudfinal %in% c("sinueux") & confinement == "non contraint" ~ "sinueux no confined",
      noeudfinal %in% c("sinueux ba") & confinement == "contraint" ~ "sinueux bars confined",
      noeudfinal %in% c("sinueux ba") & confinement == "partially contraint" ~ "sinueux bars semi-confined",
      noeudfinal %in% c("sinueux ba") & confinement == "non contraint" ~ "sinueux bars no confined",
      noeudfinal %in% c("meandre passif") & confinement == "contraint" ~ "incised meander",
      noeudfinal %in% c("meandre passif") & confinement == "partially contraint" ~ "meander semi-confined ",
      noeudfinal %in% c("meandre passif") & confinement == "non contraint" ~ "free meander",
      noeudfinal %in% c("meandre actif") & confinement == "contraint" ~ "incised meander bars",
      noeudfinal %in% c("meandre actif") & confinement == "partially contraint" ~ "meander bars semi-confined ",
      noeudfinal %in% c("meandre actif") & confinement == "non contraint" ~ "free meander bars",
      noeudfinal %in% c("tresse") & confinement == "contraint" ~ "braided confined",
      noeudfinal %in% c("tresse") & confinement == "partially contraint" ~ "braided semi-confined",
      noeudfinal %in% c("tresse") & confinement == "non contraint" ~ "free braided",
      Prediction %in% c("tresse vegetal") & confinement == "contraint" ~ "braided vegetal confined",
      noeudfinal %in% c("tresse vegetal") & confinement == "partially contraint" ~ "braided vegetal semi-confined",
      noeudfinal %in% c("tresse vegetal") & style_confinement_simple == "non contraint" ~ "free braided vegetal",
      TRUE ~ style_confinement_simple
    )
  )

write_sf(resultat_conf, "TGH_confinement.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile
