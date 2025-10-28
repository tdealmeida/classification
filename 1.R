# Longueur de la série
n <- 120
x <- 1:n

# Générer une série avec 3 segments différents
y <- c(
  0.5*x[1:40] + rnorm(40, 0, 2),   # segment 1
  20 + 0.1*x[41:80] + rnorm(40, 0, 2), # segment 2
  15 + 0.3*x[81:120] + rnorm(40, 0, 2) # segment 3
)

data <- data.frame(x, y)
mod <- lm(y ~ x)




library(segmented)

# On donne une estimation initiale du breakpoint
seg_mod <- segmented(mod, seg.Z = ~x, psi = list(x=c(40, 80)))

# Résumé du modèle segmenté
summary(seg_mod)

# Extraire le breakpoint estimé
breakpoints <- seg_mod$psi[, "Est."]
print(paste("Breakpoints estimés à x =", paste(round(breakpoints, 2), collapse=", ")))

par(mar=c(4,4,2,2))  # bas, gauche, haut, droite
# Tracer la série et la ligne segmentée
plot(x, y, main="Détection de breakpoint avec segmented", xlab="Temps", ylab="Valeur")
plot(seg_mod, add=TRUE, col="red", lwd=2)







library(stepR)

n <- 100L
x <- seq(1 / n, 1, 1 / n)
mu <- stepfit(cost = 0, family = "gauss", value = c(0, 3, 0, -2, 0), param = NULL,
              leftEnd = x[c(1, 21, 26, 71, 81)],
              rightEnd = x[c(20, 25, 70, 80, 100)], x0 = 0,
              leftIndex = c(1, 21, 26, 71, 81),
              rightIndex = c(20, 25, 70, 80, 100))
sigma0 <- 0.5
epsilon <- rnorm(n, 0, sigma0)
y <- fitted(mu) + epsilon
plot(x, y, pch = 16, col = "grey30", ylim = c(-3, 4))
lines(mu, lwd = 3)


fit <- stepFit(y, x = x, alpha = 0.5, jumpint = TRUE, confband = TRUE)
plot(x, y, pch = 16, col = "grey30", ylim = c(-3, 4))
lines(mu, lwd = 3)
lines(fit, lwd = 3, col = "red", lty = "22")
# confidence intervals for the change-point locations
points(jumpint(fit), col = "red")
# confidence band
lines(confband(fit), lty = "22", col = "darkred", lwd = 2)





set.seed(123) # pour reproductibilité
n <- 200

# Série avec deux segments de moyenne différente
mu <- c(rep(0, 100), rep(5, 100))
y <- mu + rnorm(n, sd = 1)  # ajout de bruit normal

# Visualisation
plot(y, type = "l", main = "Série temporelle avec changement de moyenne", ylab = "Valeur", xlab = "Temps")

# -----------------------------
# 2. Détection des points de changement avec stepR
# -----------------------------
# Création de l'objet stepR
step_obj <- stepFit(y = y)  # order = 0 pour changements de niveau (mean)

# Résultat
print(step_obj)

# Ajouter les segments détectés sur le plot
lines(step_obj, col = "red", lwd = 2)











b <- c(sort(sample(1:99, 4)), 100)
p <- rep(runif(5), c(b[1], diff(b))) # success probabilities
# binomial observations, each with 10 trials
y <- rbinom(100, 10, p)
# find solution with 5 blocks
fit <- steppath(y, family = "binomial", param = 10)[[5]]
plot(y, ylim = c(0, 10))
lines(fit, col = "red")












# Installer le package si nécessaire
# install.packages("sizer")

library(SiZer)

# -----------------------------
# 1. Création d'une série temporelle artificielle
# -----------------------------
set.seed(123)
n <- 200

# Série avec trois segments de moyenne différente
y <- c(rnorm(70, mean = 0, sd = 1),
       rnorm(80, mean = 3, sd = 1),
       rnorm(50, mean = -2, sd = 1))

# Visualisation
plot(y, type = "l", main = "Série temporelle avec changements de moyenne",
     ylab = "Valeur", xlab = "Temps")

# -----------------------------
# 2. Détection des points de changement avec sizer
# -----------------------------
# sizer détecte automatiquement les points de changement
# Installer le package si nécessaire
# install.packages("SiZer")

library(SiZer)

# -----------------------------
# 1. Création d'une série temporelle artificielle
# -----------------------------
set.seed(123)
n <- 200

# Série avec trois segments de moyenne différente
y <- c(rnorm(70, mean = 0, sd = 1),
       rnorm(80, mean = 3, sd = 1),
       rnorm(50, mean = -2, sd = 1))

# -----------------------------
# 2. Utilisation de SiZer pour détecter les changements de tendance
# -----------------------------
# x est l'indice de temps
x <- 1:n

# Exécution de l'analyse SiZer
a <- SiZer(x, y)
# Installer le package si nécessaire
# install.packages("SiZer")

library(SiZer)

# -----------------------------
# 1. Création d'une série temporelle artificielle
# -----------------------------
set.seed(123)
n <- 200

# Série avec trois segments de moyenne différente
y <- c(rnorm(70, mean = 0, sd = 1),
       rnorm(80, mean = 3, sd = 1),
       rnorm(50, mean = -2, sd = 1))

# -----------------------------
# 2. Utilisation de SiZer pour détecter les changements de tendance
# -----------------------------
# x est l'indice de temps
x <- 1:n


data('Arkansas')
x <- Arkansas$year
y <- Arkansas$sqrt.mayflies

plot(x,y)



# Simulate
set.seed(42)  # I always use 42; no fiddling
df = data.frame(
  x = 1:100,
  y = c(rnorm(30, 2), rnorm(40, 0), rnorm(30, 1))
)

x = 1:100
y = c(rnorm(30, 2), rnorm(40, 0), rnorm(30, 1))


# Calculate the SiZer map for the first derivative
SiZer.1 <- SiZer(x, y, h=c(.5,10), degree=1, derv=1, grid.length=21)
plot(SiZer.1)
plot(SiZer.1, ggplot2=TRUE)

# Calculate the SiZer map for the second derivative
SiZer.2 <- SiZer(x, y, h=c(.5,10), degree=2, derv=2, grid.length=21);
plot(SiZer.2)






data(Arkansas)
x <- Arkansas$year
y <- Arkansas$sqrt.mayflies
model <- piecewise.linear(x,y, CI=TRUE)
plot(model)
print(model)
predict(model, 2001)



fit <- lm(y ~ x)


# Ajustement avec plusieurs breakpoints (initial guess)
seg_fit <- segmented(fit, seg.Z = ~x, psi = list(x=c(1950, 1970))) # psi = points initiaux
seg_fit <- segmented(fit, seg.Z = ~x, psi = c(1950, 1970))  # ajuster à l’intérieur de min(x) et max(x)

# Résultats
summary(seg_fit)
plot(seg_fit, add=TRUE, col="red")









# Simulate
set.seed(42)  # I always use 42; no fiddling
df = data.frame(
  x = 1:100,
  y = c(rnorm(30, 2), rnorm(40, 0), rnorm(30, 1))
)

# Plot it
plot(df)
abline(v = c(30, 70), col="red")

fit_lm = lm(y ~ 1 + x, data = df)  # intercept-only model
fit_segmented = segmented(fit_lm, seg.Z = ~x, npsi = 2)  # Two change points along x
summary(fit_segmented)

plot(fit_segmented)
points(df)
lines.segmented(fit_segmented)
points.segmented(fit_segmented)

fit_segmented_1 = segmented(fit_lm, seg.Z = ~x, npsi = 1)
BF = exp((BIC(fit_segmented) - BIC(fit_segmented_1))/2)  # From Wagenmakers (2007)
BF
