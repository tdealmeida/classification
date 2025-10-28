library(dplyr)
library(ggplot2)
library(reshape2)

# ==== Préparer les données ====
x <- Data %>%
  st_drop_geometry() %>%
  filter(toponyme == "le Drac") %>%
  select(active_channel_width, valley_bottom_width)  # deux variables

n <- nrow(x)

secciones <- data.frame(
  Ref = 1:n,
  Var1 = x$active_channel_width,
  Var2 = x$valley_bottom_width
)

# ==== Fonctions pour 2 variables ====
distancias <- function(x, v){
  n <- nrow(x)
  D <- matrix(0, n, n)
  for(i in 1:n) for(j in i:n){
    D[j,i] <- D[i,j] <- sum((x[i,] - x[j,])^2)^(v/2)
  }
  return(D)
}

delta <- function(x, reach, D){
  grupos <- length(unique(reach))
  n <- nrow(x)
  A <- matrix(0, n, grupos)
  for(i in 1:n) A[i, reach[i]] <- 1
  N <- diag(t(A) %*% A)
  P <- A %*% t(A)
  S <- rep(0, grupos)
  for(i in 1:n) for(j in i:n) S[reach[i]] <- S[reach[i]] + D[i,j]*P[i,j]
  Epsi <- S / choose(N, 2)
  delta <- sum(N*Epsi, na.rm=TRUE) / sum(N, na.rm=TRUE)
  return(delta)
}

pvalue.bootmielke <- function(x, reach, v, num.resamples, D){
  dd <- numeric(num.resamples)
  delta.actual <- dd[1] <- delta(x, reach, D)
  for(r in 2:num.resamples){
    dd[r] <- delta(x, sample(reach, length(reach), replace=F), D)
  }
  pvalue <- length(which(dd <= delta.actual)) / num.resamples
  return(list(delta=delta.actual, pvalue=pvalue))
}

# ==== Fonction récursive multivariée pour 2 variables ====
detecter_ruptures <- function(x, start=1, end=nrow(x), v=1, num.resamples=500, alpha=0.05, min_segment=3){
  # Si le segment est trop petit, on arrête
  if((end-start+1) < min_segment*2) return(NULL)  # au moins 2 segments possibles avec min_segment
  
  segment <- x[start:end, ]
  n <- nrow(segment)
  D <- distancias(segment, v)
  mdelta <- matrix(NA, nrow=n, ncol=2)
  
  # On teste seulement les points de rupture qui respectent min_segment
  for(k in min_segment:(n-min_segment)){
    reach <- rep(1,n)
    reach[k:n] <- 2
    mdelta[k,] <- c(delta(segment, reach, D), k)
  }
  
  mdelta.util <- mdelta[min_segment:(n-min_segment), , drop=FALSE]
  rdelta <- order(mdelta.util[,1])
  kmax <- mdelta.util[rdelta[1],2]
  
  reach <- rep(1,n); reach[kmax:n] <- 2
  pval <- pvalue.bootmielke(segment, reach, v, num.resamples, D)$pvalue
  
  if(pval < alpha){
    rupture_pos <- start + kmax -1
    gauche <- detecter_ruptures(x, start, rupture_pos, v, num.resamples, alpha, min_segment)
    droite <- detecter_ruptures(x, rupture_pos+1, end, v, num.resamples, alpha, min_segment)
    return(c(rupture_pos, gauche, droite))
  } else {
    return(NULL)
  }
}

# ==== Détection des ruptures ====
x_multiv <- secciones[,c("Var1","Var2")]
ruptures <- detecter_ruptures(x_multiv)
cat("Ruptures détectées aux positions :", ruptures, "\n")

# ==== Visualisation superposée ====
df_plot <- secciones
df_plot$Index <- 1:n

# Normalisation pour superposition
df_plot[,c("Var1","Var2")] <- scale(df_plot[,c("Var1","Var2")])

# Mettre en forme pour ggplot
df_melt <- melt(df_plot, id.vars="Index", measure.vars=c("Var1","Var2"))

ggplot(df_melt, aes(x=Index, y=value, color=variable)) +
  geom_line(size=1) +
  geom_vline(xintercept=ruptures, color="green", linetype="dashed", size=1) +
  labs(title="Variables superposées avec ruptures détectées (2 variables)",
       x="Index", y="Valeur normalisée") +
  theme_minimal() +
  scale_color_brewer(palette="Set1")

# ==== Comparaison avec les ruptures de PELT ====
# PELT
test <- rupture %>%
  filter(toponyme == "le Drac")
# MRPP
sort(ruptures)

# rand index pour comparer
rand_index_f <- function(bkps1, bkps2, N) { 
  bkps1 <- c(bkps1, N)
  bkps2 <- c(bkps2, N)
  
  sanity_check <- function(bkps1, bkps2) {
    if (length(bkps1) == 0 || length(bkps2) == 0) {
      stop("Both bkps1 and bkps2 must have at least one breakpoint.")
    }
    if (bkps1[length(bkps1)] != bkps2[length(bkps2)]) {
      stop("The last breakpoint of both bkps1 and bkps2 must be the same.")
    }
  }
  
  sanity_check(bkps1, bkps2)
  n_samples <- bkps1[length(bkps1)]
  bkps1_with_0 <- c(0, bkps1)
  bkps2_with_0 <- c(0, bkps2)
  n_bkps1 <- length(bkps1)
  n_bkps2 <- length(bkps2)
  
  disagreement <- 0
  beginj <- 0  # avoids unnecessary computations
  
  for (index_bkps1 in seq_len(n_bkps1)) {
    start1 <- bkps1_with_0[index_bkps1]
    end1 <- bkps1_with_0[index_bkps1 + 1]
    
    for (index_bkps2 in seq(beginj, n_bkps2)) {
      start2 <- bkps2_with_0[index_bkps2]
      end2 <- bkps2_with_0[index_bkps2 + 1]
      nij <- max(min(end1, end2) - max(start1, start2), 0)
      disagreement <- disagreement + nij * abs(end1 - end2)
      
      # we can skip the rest of the iteration, nij will be 0
      if (end1 < end2) {
        break
      } else {
        beginj <- index_bkps2 + 1
      }
    }
  }
  
  disagreement <- disagreement / (n_samples * (n_samples - 1) / 2)
  return(1.0 - disagreement)
}

ruptures1 <- sort(ruptures)
ruptures2 <- unlist(test$cp)
N <- nrow(x)
res <- rand_index_f(ruptures1,ruptures2, N)
cat("Rand Index entre les ruptures détectées et les ruptures réelles :", res, "\n")

