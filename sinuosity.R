##############################################################
# SCRIPT : sinuosity.R
# OBJECTIF : calcul de la sinuosité par angle (technique Lise)
##############################################################

library(sf)
library(dplyr)
library(purrr)

#' Calculates angles between triplets of points in an sf object
#' @param points_sf 
#' @return
#' @export
#' @examples
calculate_angles_sf <- function(points_sf) {
  coords <- sf::st_coordinates(points_sf)
  if (nrow(coords) < 3) {
    stop("At least 3 points are necessary to calculate angles")
  }
  angles <- sapply(2:(nrow(coords) - 1), function(i) {
    p1 <- coords[i - 1, ]
    p2 <- coords[i, ]
    p3 <- coords[i + 1, ]
    v1 <- p1 - p2
    v2 <- p3 - p2
    dot_product <- sum(v1 * v2)
    norm_v1 <- sqrt(sum(v1^2))
    norm_v2 <- sqrt(sum(v2^2))
    angle <- acos(dot_product / (norm_v1 * norm_v2))
    return(angle)
  })
  
  # Crée un vecteur complet avec NA pour les premiers/derniers points
  angle_full <- rep(NA, nrow(coords))
  angle_full[2:(nrow(coords)-1)] <- angles
  
  angle_sf <- points_sf
  angle_sf$angle_rad <- abs(angle_full - pi) # écart à un angle plat
  angle_sf$angle_deg <- angle_sf$angle_rad * 180 / pi # conversion en degrés
  
  return(angle_sf)
}


# 1. Nettoyage de base
Data_sf <- Data %>%
  st_as_sf()

# 2. Extraction du point aval pour chaque ligne
Data_sf <- Data_sf %>%
  mutate(
    geom = st_sfc(map(geom, ~ {
      coords <- st_coordinates(.x)
      st_point(coords[nrow(coords), 1:2])
    }), crs = st_crs(.))
  ) %>%
  mutate(geom = st_zm(geom, drop = TRUE, what = "ZM")) %>%
  st_as_sf()

# 3. Reprojection
Data_sf_proj <- st_transform(Data_sf, 2154)

# 4. Calcul des angles pour chaque toponyme
Data_sin <- Data_sf_proj %>%
  group_by(axis) %>%
  # arrange(toponyme) %>%
  group_modify(~ {
    # .x = groupe par toponyme (un sf)
    if (nrow(.x) >= 3) {
      angles <- calculate_angles_sf(.x)
    } else {
      angles <- .x %>%
        mutate(angle_rad = NA_real_, angle_deg = NA_real_)
    }
    return(angles)
  }) %>%
  ungroup()







tes <- Data_sin %>%
  filter(axis == "2000794593") %>%
  # arrange(measure)%>%
  mutate(ID = row_number(),
         log = log(angle_deg+1)) 
  
values <- na.omit(tes$log)
log(length(values))   # Ajout de valeurs

cpt_result_pelt <- cpt.meanvar(
      values,
      method = "PELT",
      penalty = "Manual",
      pen.value = "log(n)",  # 4 pour avoir au moins 3 segments valides
      minseglen = 4  # 4 pour avoir au moins 3 segments valides
    )
    
    # Extraire les points de changement
cpt_pelt <- cpt_result_pelt@cpts
cpt_pelt <- cpt_pelt[-length(cpt_pelt)]  # retirer le dernier, c'est juste la fin
    
   
ggplot(tes, aes(x = ID, y = log)) +
  geom_line() +
  geom_vline(xintercept = cpt_pelt, color = "red", linetype = "dashed") +
  theme_minimal() 



# Identification du segment pour chaque DGO
segment <- rep(0, nrow(tes))
segment[cpt_pelt] <- 1
segment <- cumsum(segment) + 1

# Ajout des segments dans le tibble des DGO
tes$ID_segment <- segment

points <- tes %>%
  group_by(axis, ID_segment) %>%
  arrange(desc(measure_medial_axis)) %>%
  slice_head(n = 1) %>%
  st_as_sf() %>%
  mutate(
    geom = st_sfc(map(geom, ~ {
      coords <- st_coordinates(.x)
      st_point(coords[1, 1:2])
    }), crs = st_crs(.))
  ) %>%
  st_as_sf() %>%
  mutate(geom = st_zm(geom, drop = TRUE, what = "ZM")) %>%
  ungroup() %>%
  group_by(axis) %>%
  slice(-1) %>%   # <-- supprime le dernier point car sinon crée un point de rupture pour le premier segment amont
  ungroup() %>%
  st_transform(2154)


st_write(points, "testtt3.gpkg", delete_layer = TRUE) # Export des données nettoyées en shapefile













