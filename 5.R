2000788066 %in% rupture$axis
2000788066 %in% AC$axis
2000790852 %in% VB$axis
2000788066 %in% metrique$axis

t <- Data %>%
  filter(if_any(everything(), is.na))
sum(is.na(t$AC))


st_write(t, "t.gpkg", delete_layer = TRUE)
