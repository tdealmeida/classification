ggplot(TGH_ID,aes(x=mean_angle_deg,y=Sinuosity_meander))+
  geom_point()+
  theme_minimal()



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
