library(sf)
library(dplyr)
library(qgisprocess)



axiss <- st_read("Data/Adour/networks_axis_clean_adour.gpkg") %>%
  ungroup() %>%
  select(toponyme, axis, geom) %>%
  st_set_geometry("geom") %>%            #  très important
  st_cast("LINESTRING", warn = FALSE) %>%
  st_as_sf()




inflection_list <- list()
axes_unique <- unique(axiss$axis)

for(i in seq_along(axes_unique)) {
  
  axis_value <- axes_unique[i]
  
  cat("AXIS :", axis_value, "\n")
  
  axis_group <- axiss %>% 
    filter(axis == axis_value) %>% 
    st_cast("LINESTRING", warn = FALSE)
  
  # --- écriture temporaire (comme QGIS fait en interne) ---
  in_file  <- tempfile(fileext = ".gpkg")
  out_file <- tempfile(fileext = ".shp")   # shapefile = pas de bug fid
  
  st_write(axis_group, in_file, "input", delete_dsn = TRUE, quiet = TRUE)
  
  # --- algorithme ---
  res <- qgis_run_algorithm(
    "fcw:inflectiondisaggregation",
    INPUT = paste0(in_file, "|layername=input"),
    SIMPLIFY = 10,
    MAX_DISTANCE = 200,
    MIN_AMPLITUDE = 10,
    MAX_ANGLE = 50,
    OUTPUT_LINES = out_file
  )
  
  # --- lecture résultat ---
  inflection_list[[i]] <- st_read(out_file, quiet = TRUE) %>% 
    mutate(axis = axis_value)
}

inflection_lines_all <- do.call(rbind, inflection_list)

inflection_lines_all <- inflection_lines_all %>%
  select(-fid)

st_write(inflection_lines_all, "Data/Adour/meanderbelt_axis_adour.gpkg", delete_layer = TRUE)
