data(Arkansas)
x <- Arkansas$year
y <- Arkansas$sqrt.mayflies
model <- piecewise.linear(x,y, CI=FALSE)
plot(model)
print(model)
predict(model, 2001)



data('Arkansas')
x <- Arkansas$year
y <- Arkansas$sqrt.mayflies
df <- data.frame(
  year = Arkansas$year,
  mayflies = Arkansas$sqrt.mayflies
)

# Graphique avec ggplot
ggplot(df, aes(x = year, y = mayflies)) +
  geom_point(color = "steelblue", size = 2) +           # points
  geom_line(color = "steelblue", linewidth = 0.8) +     # ligne reliant les points
  theme_minimal(base_size = 14) +
  labs(
    x = "Année",
    y = "√(Abondance des éphémères)",
    title = "Évolution de l’abondance des éphémères (Arkansas River)"
  )# Calculate the SiZer map for the first derivative
SiZer.1 <- SiZer(x, y, h=c(.5,10), degree=1, derv=1, grid.length=21)
plot(SiZer.1)
plot(SiZer.1, ggplot2=TRUE)
# Calculate the SiZer map for the second derivative
SiZer.2 <- SiZer(x, y, h=c(.5,10), degree=2, derv=2, grid.length=21);
plot(SiZer.2,ggplot2=TRUE)
# By setting the grid.length larger, we get a more detailed SiZer
# map but it takes longer to compute.
#
# SiZer.3 <- SiZer(x, y, h=c(.5,10), grid.length=100, degree=1, derv=1)
# plot(SiZer.3)