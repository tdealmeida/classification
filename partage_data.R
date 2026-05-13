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
st_write(Segment_homogene, "Segment_homogene.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile




resultat_conf <- st_read("TGH_conf.gpkg")
ALL_SUBDATA <- st_read("ALL_SUBDATA.gpkg") 


test <- data %>%
  mutate(
  axis = as.character(axis),
  measure = as.numeric(measure),
  ID_DGO = as.character(fid)
  ) %>%
  left_join(
    ALL_SUBDATA %>% 
      st_drop_geometry() %>% 
      mutate(
        axis = as.character(axis),
        # measure = as.numeric(measure)
        ID_DGO = as.character(ID_DGO)
      ) %>%
      select(axis, ID_DGO, ID_segment),
    by = c("axis", "ID_DGO")
  )
st_write(test, "TGH_dgo_scale.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile


# ta <- ALL_SUBDATA %>%
#   mutate(axis = as.character(axis)) %>%
#   left_join(
#     data %>%
#       st_drop_geometry() %>%
#       mutate(axis = as.character(axis)) %>%
#       select(axis, measure, fid),
#     by = c("axis", "measure")
#   )
names(resultat_conf)
style_dgo <- ALL_SUBDATA %>%
  left_join(
    resultat_conf %>%
      st_drop_geometry() %>%
      select(axis, ID_segment, conf_simple,Prediction, 
             Probabilite, Sinuosity_meander_2
             )%>%
      mutate(axis = as.character(axis)),
    by = c("axis", "ID_segment")
  ) %>%
  select(axis,ID_DGO,toponyme,measure_medial_axis, measure,strahler,
         angle_deg, conf_degree,ID_segment, Prediction, Probabilite,conf_simple,
         Sinuosity_meander_2,meander_belt,multi_index,ile_veget,
         ACW_star,drainage_area)%>%
  rename(conf_margin = conf_degree,
         sinuosite = Sinuosity_meander_2,
         angle_local = angle_deg,
         meander_belt_width = meander_belt,
         W_star = ACW_star,
         milti_channel_index = multi_index,
         ile_vegetalise = ile_veget) %>%
  mutate(
    # ID_DGO = fid,
    Prediction = recode(Prediction,
                           "rectiligne"        = "Rectiligne",
                           "rectiligne bars"   = "Bancs alternés",
                           "sinueux"           = "Sinueux",
                           # "alternate bars"  = "Alternate bars",
                           "sinueux bars"      = "Sinueux à bancs",
                           "meandre actif"     = "Méandre actif",
                           "meandre passif"    = "Méandre passif",
                           "tresse"            = "Tresse",
                           # "tresse vegetal"  = "Vegetated braided",
                           "divagant"          = "Divagant",
                           "anastomose"        = "Anastomosé",
                           "anabranche"        = "Anabranche",
                           "reservoir"         = "Reservoir",
                           "intermittent"      = "Intermittent"
    ),
    confinement = recode(conf_simple,
                        "unconfined"        = "Non Confiné",
                        "partly confined"   = "Partiellement confiné",
                        "confined"          = "Confiné"
    ),
  ) %>%
  select(-conf_simple)

st_write(style_dgo, "style_dgo.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile






ajd <- test %>%
  filter(!is.na(label))
st_write(ajd, "ajd.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile)






