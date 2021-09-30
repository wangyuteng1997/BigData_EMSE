# TP2 - Probabilités Avancées - UP1
# Majeure science des donnes 2020-2021
# Solution

# ----------
# EXERCISE 1 
# ----------

# Partie 1 - mu = (1 2), sigma1 = sigma2 = 1 et rho = 0.8

# Question 1
# Méthode 1 : pas de boucle for sur le numéro i de simulation,
# mais directement par calcul matriciel
n <- 100 # taille de l'échantillon (les individus)

# Initialisation de la matrice des données simulées 
# n = 100 individus repérés par p = 2 variables en colonne
# --> c'est l'usage en Statistique 
X <- matrix(0, nrow= n, ncol=2) 

# Bruit blanc N(0,1) de taille n (composantes sur l'axe x1)
eps1 <- rnorm(n,mean=0,sd=1) 
X[,1] <- 1 + eps1
# Bruit blanc N(0,1) de taille n (composantes sur l'axe x2)
eps2 <- rnorm(n,mean=0,sd=1) 
X[,2] <- 2 + 0.8*eps1 + 0.6*eps2

# Visualisation de l'échantillon simulé  
# du vecteur X = (X1, X2) = nuage de n points
plot(X[,1], X[,2], asp=1, xlim=c(1-3,1+3), ylim=c(2-3,2+3),
     xlab="x1", ylab="x2")
abline(v=1, lty=2)
abline(h=2, lty=2)
abline(a=1, b=1, col="red")
abline(a=3, b=-1, col="red")
title("Simulation VG 2-dimensionnel (corrélation = 0.8)",
      cex.main=0.9)

# Méthode 2 : boucle for à éviter si n grand mais moins de 
# risque d'erreur et programme plus lisible
n <- 100
Xbis <- matrix(0, nrow= n, ncol=2) 
for (i in 1:n){
  eps1 <- rnorm(1) # par défaut mean=0, sd=1
  Xbis[i,1] <- 1 + eps1
  Xbis[i,2] <- 2 + 0.8*eps1 + 0.6*rnorm(1)
}

points(Xbis[,1], Xbis[,2], pch="+", col="blue")

# Méthode 3 : on peut bien sûr ne pas utiliser l'expression 
# analytique de la décomposition de Cholesky de la matrice de
# covariance donnée dans l'énoncé et l'obtenir numériquement
# avec la fonction chol() de R
Gamma <- matrix(0,nrow=2,ncol=2)
Gamma[1,1] <- 1
Gamma[2,2] <- 1
Gamma[1,2] <- 0.8
Gamma[2,1] <- 0.8
print(Gamma)

help(chol)
chol_res <- chol(Gamma)
print(chol_res)
# Attention : bien transposer puisque Gamma = S*t(S)
S <- t(chol_res) 
print(S) 

# Ensuite méthode 1 ou 2 ci-dessus en utilisant le fait que
# X = S*eps avec eps bruit blanc de dim 2

# Question 2 : décomposition spectrale de la matrice de 
# covariance du vecteur X, lien avec la  décomposition de
# Mahalanobis et l'Analyse en Composantes Principales (ACP)

# On utilise l'expression analytique de la décomposition
# spectrale 
lambda1 <- 1 + 0.8 # 1 + rho
lambda2 <- 1 - 0.8 # 1 - rho
# premier vecteur propre en colonne
U1 <- (1/sqrt(2))*matrix(c(1,1),n=2,ncol=1)
# second vecteur propre en colonne 
U2 <- (1/sqrt(2))*matrix(c(-1,1),n=2,ncol=1)

# (mu; U1, U2) définit une nouveau repère orthonormé (affine) 
# Nouvelles coordonnées qui correspondent aux deux axes 
# orthonormés de la question 1 : y = x + 1 et y = -x + 3

# Il faut d'abord centrer les données (mu1=1 et mu2=2)
Xc <- matrix(0, nrow= n, ncol=2) 
Xc[,1] <- X[,1] - 1
Xc[,2] <- X[,2] - 2

# Calcul des nouvelles coordonnées
# produit matriciel avec %*%, attention individus en ligne
C1 <- Xc%*%U1 
C2 <- Xc%*%U2

plot(C1, C2, xlab="u1", ylab="u2", asp=1)
abline(h=0,lty=2)
abline(v=0,lty=2)
title("Nouvelles variables (lambda1 = 1.8 et lambda2 = 0.2)",
      cex.main=0.9)

# Vérification empirique à partir des données simulées
# que les nouvelles variables sont indépendantes gaussiennes
# de variances lambda1 = 1.8 et lambda2 = 0.2 (respt)

# Variances estimées 
var1 <- var(C1)
print(var1) # A comparer avec lambda1 = 1.8 (var théorique)
var2 <- var(C2)
print(var2) # A comparer avec lambda2 = 0.2

# Normalité des composantes
qqnorm(C1, main="Q-Q Plot C1")
qqline(C1, probs = c(0.1, 0.9), col="red")

qqnorm(C2, main="Q-Q Plot C2")
qqline(C2, probs = c(0.1, 0.9), col="red")

# Indépendance, c'est le graphique déjà obtenu et qui 
# correspondrait à une ACP normée ou non
# puisque les variables initiales sont de variance 1 :
# axe 1 = axe de plus grande inertie ou variance projetée

# % de variance axe 1 : 90% = 100*1.8/(1.8 + 0.2) %
plot(C1, C2, xlab="C1 (90%)", ylab="C2 (10%)", asp=1) 
abline(h=0, lty=2)
abline(v=0, lty=2)
grid()
title("Nuage des individus décrits par les nouvelles variables",
      cex.main=0.9)

# Lien avec la décomposition de Mahalanobis
U <- cbind(U1,U2)
print(U)
# valeurs singulières = écart-types des nouvelles variables
sv <- c(sqrt(1.8), sqrt(0.2)) 
SIGMA <- diag(sv)
print(SIGMA)
# Vérification :
print(U%*%SIGMA%*%SIGMA%*%t(U))

# Simulation de X par la décomposition de Mahalanobis 
# <-> simulation des nouvelles coordonnées indépendantes
# A éviter si la dimension d est grande (ici d = 2)
n <- 200
# Attention aux dimensions
Xsimu <- U%*%SIGMA%*%matrix(rnorm(2*n), nrow=2, ncol=n) 
Xsimu <- t(Xsimu) # format standard d'un jeu de données 
# Ne pas oublier de décentrer
Xsimu[,1] <- Xsimu[,1] + 1  
Xsimu[,2] <- Xsimu[,2] + 2 
# Structure de données R de type data frame 
Xsimu <- data.frame(Xsimu) 

plot(Xsimu, asp=1); abline(v=1,lty=2); abline(h=2,lty=2)
pairs(Xsimu)
cor(Xsimu) # matrice de corrélation empirique

# Partie 2
# Below: Create a function to simulate a 2D Gaussian Vector X = (X1, X2)
# ------
# Inputs
# ------
#  mu   : a vector of size 2 giving the mean of X
#  rho  : a real number between [-1, 1] giving the correlation cor(X1,X2)
#  sig    : a vector of size 2 containing the standard deviation of X. Default is (1,1).
#  n    : an integer giving the sample size. Default is 1000.
#  plot : should we plot the result? Default is TRUE.
#  ...  : optional arguments (color, labels, etc.) to be passed to plot
# ------
# Output
# ------
# X     : A matrix of size nx2 containing a sample of size n from the Gaussian distribution of X


simu_VG <- function(mu, rho, sig = c(1,1), n = 1000, plot = TRUE, ...){
  
  # construction of the covariance matrix, such that :
  # cor(X1, X2) = rho, var(X1) = sig[1]^2, var(X2) = sig[2]^2
  
  Gamma <- matrix(0, nrow=2, ncol=2)
  Gamma[1,1] <- sig[1]*sig[1]
  Gamma[2,2] <- sig[2]*sig[2]
  Gamma[1,2] <- sig[1]*sig[2]*rho
  Gamma[2,1] <- Gamma[1,2] 
  
  # here simulation of a sample of size n drawn from N(mu, Gamma) 

  # -- MY CODE --
  
  X <- matrix(NA, n, ncol=2)   # the matrix of size nx2 containing the simulations
  
  # warning : to be computed out of the loop 
  chol_Gamma <- chol(Gamma)
  S <- t(chol_Gamma) # to be a lower matrix
  
  for (i in 1:n){
    eps <- rnorm(n=2, mean=0, sd=1) # a 2-dimensional Gaussian noise
    X[i, ] <- mu + S%*%eps   # a vector of length 2 (row or column, as you like)
  }
  
  # plot the results if argument 'plot' is equal to TRUE
  if (plot){
    par(mfrow = c(1,1))
    plot(X, asp=1, ...)  # asp = 1 --> same scale for the x and y axis
    abline(v = mu[1], h = mu[2])     
  }
    
  return(X)
}
    
# Run the function
X <- simu_VG(mu = c(1,2), rho = -0.8, sig=c(3,1), n = 1000)

# Simulations avec une même variance 
X <- simu_VG(mu = c(0,0), rho = 0, sig=c(1,1), n = 1000)
title(expression(paste('Même variance = 1 et ', rho, " = ", 0)))

X <- simu_VG(mu = c(0,0), rho = 0.87, sig=c(1,1), n = 1000)
title(expression(paste('Même variance = 1 et ', rho, " = ", .87)))

X <- simu_VG(mu = c(0,0), rho = -0.87, sig=c(1,1), n = 1000)
title(expression(paste('Même variance = 1 et ', rho, " = ", -0.87)))

X <- simu_VG(mu = c(0,0), rho = 0.87, sig=c(2,1), n = 1000)
title(expression(paste('Variances différentes = 1 et ', rho, " = ", .87)))

# Cas extrême |rho| ~= 1
X <- simu_VG(mu = c(0,0), rho = 0.999, sig=c(2,1), n = 1000)
title(expression(paste('variances différentes (2, 1) et ',
                       rho, " = ", .999)))

# ----------
# EXERCISE 2 
# ----------

# Question 1
# On reprend la simulation de l'exercice 1 avec n plus grand
n <- 500 # taille de l'échantillon 
X <- matrix(0, nrow= n, ncol=2) 

eps1 <- rnorm(n, mean=0, sd=1) 
X[,1] <- 1 + eps1
eps2 <- rnorm(n, mean=0, sd=1) 
X[,2] <- 2 + 0.8*eps1 + 0.6*eps2

# Visualisation 
plot(X[,1], X[,2], asp=1, xlim=c(1-4,1+4), ylim=c(2-4,2+4),
     xlab="x1", ylab="x2")
abline(v=1, lty=2)
abline(h=2, lty=2)
abline(a=1, b=1, col="red", lwd=2)
abline(a=3, b=-1, col="red", lwd=2)
abline(a=1.2, b=0.8, col="blue", lwd=2) # droite de régression théorique
title("Simulation VG 2-dimensionnel (corrélation = 0.8)", cex.main=0.9)

# Loi théorique
plot(1, 2, asp=1, xlim=c(1-4,1+4), ylim=c(2-4,2+4), xlab="x1", ylab="x2")
# points(X[,1], X[,2])
abline(v=1, lty=2)
abline(h=2, lty=2)
abline(a=1, b=1, col="black", lty=2)
abline(a=3, b=-1, col="black", lty=2)
abline(a=1.2, b=0.8, col="red", lwd=2) # droite de régression théorique

library(mixtools)  # pour la fonction ellipse()
mu <- c(1,2) # Moyenne
rho <- 0.8
sigma <- matrix(c(1, rho, rho, 1), nrow = 2) # Matrice de covariance
for (niv in 1:5){
  ellipse(mu, sigma, .5/niv)
}

# Loi conditionnelle théorique de X2 sachant X1= x1
x1 <- 2
muX2 <- 1.2 + 0.8*x1
varX2 <- 1 - 0.8^2
xx2 <- seq(-2, 6, 0.01)
xx1 <- x1 + dnorm(xx2, mean=muX2, sd=sqrt(varX2))
lines(xx1, xx2, col='red', lwd=2)
segments(x1, muX2, x1+dnorm(muX2,mean=muX2, sd=sqrt(varX2)), muX2,
         col="red", lwd=2)
abline(v=x1, lty=1)
title('Loi conditionnelle théorique de X2 | X1 = x1')

# Points simulés (X1, X2) qui vérifient x1-h <= X1 <= x1+h
x1 <- 2
h <- 0.1
# Indices correspondants
indices <- (X[,1] >= x1-h) & (X[,1] <= x1+h)
plot(X[,1], X[,2], asp=1, xlim=c(1-4,1+4), ylim=c(2-4,2+4),
     xlab="x1", ylab="x2")
abline(v=1, lty=2)
abline(h=2, lty=2)
abline(a=1, b=1, col="black", lty=2)
abline(a=3, b=-1, col="black", lty=2)
abline(a=1.2, b=0.8, col="red", lwd=2) # droite de régression théorique
points(X[indices, 1], X[indices, 2], col="blue")
abline(v = c(x1-h, x1+h))

# Etude empirique de la loi conditionnelle de X2
X2 <- X[indices, 2] # valeurs de X2 pour lesquelles x1-h <= X1 <= x1+h
muX2 <- mean(X2) 
muYsachantx1 <- 1.2 + 0.8*x1 # équation de la droite de régression
print(muX2)
print(muYsachantx1)

varX2 <- var(X2)
varYsachantx1 <- 1 - 0.8^2 # = (1 - rho^2)*Var(Y)
print(varX2)
print(varYsachantx1)

# Normalité de la loi conditionnelle ?
qqnorm(X2); qqline(X2, probs = c(0.1, 0.9), col="red")
# oui

# Visualisation
plot(X[,1], X[,2], asp=1, xlim=c(1-4,1+4), ylim=c(2-4,2+4),
     xlab="x1", ylab="x2")
grid()
abline(v=1, lty=2)
abline(h=2, lty=2)
abline(a=1, b=1, col="black", lty=2)
abline(a=3, b=-1, col="black", lty=2)
abline(a=1.2, b=0.8, col="red", lwd=2) # droite de régression théorique
title("Simulation VG 2-dimensionnel (corrélation = 0.8)", cex.main=0.9)
points(X[indices, 1], X[indices, 2], col="blue")
abline(v = c(x1-h, x1+h))
xx2 <- seq(-2, 6, 0.01)
xx1 <- x1 + dnorm(xx2, mean=muX2, sd=sqrt(varX2))
lines(xx1, xx2, col='red', lwd=2)
segments(x1, muX2, x1+dnorm(muX2,mean=muX2,sd=sqrt(varX2)),muX2,
         col="red", lwd=2)

# Loi conditionnelle pour une autre valeur de x1
x1 <- 0
h <- 0.1
indices <- (X[,1] >= x1-h) & (X[,1] <= x1+h)   
X2 <- X[indices, 2]
muX2 <- mean(X2)
varX2 <- var(X2)
points(X[indices, 1], X[indices, 2], col="blue")
abline(v = c(x1-h, x1+h))
xx2 <- seq(-2,6,0.01)
xx1 <- x1 + dnorm(xx2, mean=muX2, sd=sqrt(varX2))
lines(xx1,xx2,col='red',lwd=2)
segments(x1, muX2, x1+dnorm(muX2,mean=muX2,sd=sqrt(varX2)), muX2,
         col="red",lwd=2)

# Loi conditionnelle pour une dernière valeur de x1
x1 <- 1
h <- 0.1

indices <- (X[,1] >= x1-h) & (X[,1] <= x1+h)   
X2 <- X[indices, 2]
muX2 <- mean(X2) 
varX2 <- var(X2)
points(X[indices, 1], X[indices, 2], col="blue")
abline(v = c(x1-h, x1+h))
xx2 <- seq(-2, 6, 0.01)
xx1 <- x1 + dnorm(xx2, mean=muX2, sd=sqrt(varX2))
lines(xx1,xx2,col='red',lwd=2)
segments(x1, muX2, x1+dnorm(muX2,mean=muX2,sd=sqrt(varX2)), muX2,
         col="red",lwd=2)

# Question 2
n <- 10000 # taille de l'échantillon 
X <- matrix(0, nrow= n, ncol=2) 
eps1 <- rnorm(n, mean=0, sd=1) 
X[,1] <- 1 + eps1
eps2 <- rnorm(n, mean=0, sd=1) 
X[,2] <- 2 + 0.8*eps1 + 0.6*eps2

x1 <- seq(-1, 3, 0.5)
lgx1 <- length(x1)
mux1 <-rep(0, lgx1)
varx1 <- rep(0, lgx1)

for (k in 1:lgx1){
  indices <- (X[,1] >= x1[k]-h) & (X[,1] <= x1[k]+h)   
  X2 <- X[indices, 2]
  mux1[k] <- mean(X2)
  varx1[k] <- var(X2)
}

plot(x1, mux1, main="Moyenne estimée de X2 sachant X1 = x1", ylab='')
abline(a=1.2, b=0.8, col="red")

plot(x1, varx1, main="Variance estimée de X2 sachant X1 = x1",
     ylim=c(0,1), ylab='')
abline(h = 1-0.8*0.8, col="red")

# Conclusion
# La moyenne conditionnelle est donnée par la fonction de régression linéaire
# x1 --> E(X2|X1 = x1) = 1.2 + 0.8*x1
# La variance conditionnelle ne dépend pas de x1 et vaut
# (1 - rho^2)*Var(X2) = Var(eps) = "variance résiduelle"

# ----------
# EXERCISE 3 
# ----------

RENAULT_data <- read.csv("RENAULT_2019-09-24.txt",
                         header=T, sep="", dec=".")
RENAULT <- RENAULT_data$clot
plot(RENAULT, type='l', ylab = "Cours de clôture (en euros)", 
     xlab="Jours boursiers (du 24/09/2018 au 24/09/2019)")
grid()
title("Evolution de l'action RENAULT sur un an")

# Taux de hausse ou de baisse logarithmique
# Fonction R diff() pour la différence discrète log(S(t)) - log(S(t-1))
tx_RENAULT <- diff(log(RENAULT)) 
plot(100*tx_RENAULT, type='l',ylab="Taux en %",xlab="Temps")
abline(h=0, col="red")
grid()
title("Taux de rendement logarithmiques action RENAULT sur un an (en %)",cex.main=0.9)

# Questions 1 : analyse du couple d'actions (SOGE, BNP)
SOCIETEGENERALE_data <- read.csv("SOCIETEGENERALE_2019-09-24.txt", 
                                 header=T, sep="", dec=".")
SOGE <- SOCIETEGENERALE_data$clot
BNPPARIBAS_data <- read.csv("BNPPARIBAS_2019-09-24.txt", 
                            header=T, sep="", dec=".")
BNP <- BNPPARIBAS_data$clot

# Taux de hausse ou de baisse logarithmiques
tx_SOGE <- diff(log(SOGE))  
tx_BNP <- diff(log(BNP)) 

# mfrow paramètre graphique pour avoir plusieurs sous-graphes
par(mfrow=c(1,2)) 
plot(100*tx_SOGE, type='l', ylab="Taux en %",
     xlab="Temps", ylim=c(-7,6))
grid()
abline(h=0, col="red")
title("Taux SG (en %)", cex.main=0.9)
# Même échelle en y avec ylim pour comparer!
plot(100*tx_BNP, type='l', ylab="Taux en %",
     xlab="Temps", ylim=c(-7,6)) 
grid()
abline(h=0, col="red")
title("Taux BNP (en %)", cex.main=0.9)
par(mfrow=c(1,1)) 

# Analyse du vecteur bidimensionnel (taux SG, taux BNP)
# Vecteur Gaussien ?
plot(100*tx_SOGE, 100*tx_BNP,
     xlab="Taux SG en %", ylab="Taux BNP en %", asp=1)
grid()
abline(h=0, lty=2)
abline(v=0, lty=2)
title("Taux de hausse ou de baisse de la BNP contre ceux de la SOGE",cex.main=0.9)
rhoSG_BNP <- cor(tx_SOGE,tx_BNP)
covSG_BNP <- cov(tx_SOGE,tx_BNP)
GAM <- matrix(c(var(tx_SOGE),covSG_BNP,covSG_BNP,var(tx_BNP)),
              nrow=2)
res <- eigen(GAM)
print(res$vectors)
# signe moins comme choix de l'orientation de l'axe
U1 <- - res$vectors[,1] 
# idem : repère positif (rotation)
U2 <- - res$vectors[,2] 
MatSGBNP <- cbind(tx_SOGE,tx_BNP); class(MatSGBNP)
C1 <- MatSGBNP%*%U1
C2 <- MatSGBNP%*%U2
plot(C1, C2, asp=1, xlim=c(-0.08,0.08), ylim=c(-0.05,+0.05))
grid()
abline(h=0, lty=2)
abline(v=0, lty=2)
title("Changement de repère orthonormé (nouvelles composantes)",
      cex.main=0.9)
# Vérification variances = valeurs propres
res$values
var(C1);var(C2)
 
# Normalité des composantes ?
qqnorm(C1, main="Normal Q-Q Plot - C1")
grid()
qqline(C1, probs = c(0.1, 0.9), col="red")

qqnorm(C2, main="Normal Q-Q Plot - C2")
grid()
qqline(C2, probs = c(0.1, 0.9), col="red")

# Test sur les lois marginales du vecteur (taux SG, taux BNP)
qqnorm(tx_SOGE, main="Normal Q-Q Plot - taux SG")
grid()
qqline(tx_SOGE, probs = c(0.1, 0.9), col="red")

qqnorm(tx_BNP, main="Normal Q-Q Plot - taux BNP")
grid()
qqline(tx_BNP, probs = c(0.1, 0.9), col="red")

# Question 2
matplot(cbind(SOGE,BNP), type='l', ylab = "Cours de clôture (en euros)",
        xlab="Jours boursiers (du 24/09/2018 au 24/09/2019)")
grid()
title("Evolution des actions SG et BNP sur un an")
legend('topright', c("SG","BNP"), 
       lty=c(1,2), col=c("black","red"), cex=0.6)

# Question 3 - paires d'actions dans 5 secteurs différents

# Secteur bancaire
SOCIETEGENERALE_data <- read.csv("SOCIETEGENERALE_2019-09-24.txt",
                                 header=T, sep="", dec=".")
SOGE <- SOCIETEGENERALE_data$clot
BNPPARIBAS_data <- read.csv("BNPPARIBAS_2019-09-24.txt",
                            header=T, sep="", dec=".")
BNP <- BNPPARIBAS_data$clot

matplot(cbind(SOGE,BNP), type='l', ylab = "Cours de clôture (en euros)",
        xlab="Jours boursiers (du 24/09/2018 au 24/09/2019)")
grid()
title("Evolution des actions SG et BNP sur un an")
legend('topright', c("SG","BNP"), 
       lty=c(1,2), col=c("black","red"), cex=0.6)

# Secteur automobile
PEUGEOT_data <- read.csv("PEUGEOT_2019-09-24.txt",
                         header=T, sep="", dec=".")
PEUGEOT <- PEUGEOT_data$clot
RENAULT_data <- read.csv("RENAULT_2019-09-24.txt",
                         header=T, sep="", dec=".")
RENAULT <- RENAULT_data$clot

matplot(cbind(PEUGEOT,RENAULT), type='l', ylab = "Cours de clôture (en euros)",
        xlab="Jours boursiers (du 24/09/2018 au 24/09/2019)")
grid()
title("Evolution des actions PEUGEOT et RENAULT sur un an")
legend('topright', c("PEUGEOT","RENAULT"), 
       lty=c(1,2), col=c("black","red"), cex=0.6)

# Secteur du bâtiment
BOUYGUES_data <- read.csv("BOUYGUES_2019-09-24.txt", 
                          header=T, sep="", dec=".")
BOUYGUES <- BOUYGUES_data$clot
SAINTGOBAIN_data <- read.csv("SAINTGOBAIN_2019-09-24.txt", 
                             header=T, sep="", dec=".")
SAINTGOBAIN <- SAINTGOBAIN_data$clot
matplot(cbind(BOUYGUES,SAINTGOBAIN), type='l', ylab = "Cours de clôture (en euros)",
        xlab="Jours boursiers (du 24/09/2018 au 24/09/2019)")
title("Evolution des actions BOUYGUES et SAINTGOBAIN sur un an")
grid()
legend('topright', c("BOUYGUES","SAINTGOBAIN"), 
       lty=c(1,2), col=c("black","red"), cex=0.6)

# Secteur biens et services industriels
AIRBUS_data <- read.csv("AIRBUS_2019-09-24-1.txt", 
                        header=T, sep="", dec=".")
AIRBUS <- AIRBUS_data$clot
SAFRAN_data <- read.csv("SAFRAN_2019-09-24.txt", 
                        header=T, sep="", dec=".")
SAFRAN <- SAFRAN_data$clot
matplot(cbind(AIRBUS,SAFRAN), type='l', ylab = "Cours de clôture (en euros)",
        xlab="Jours boursiers (du 24/09/2018 au 24/09/2019)")
grid()
title("Evolution des actions AIRBUS et SAFRAN sur un an")
legend('bottomright', c("AIRBUS","SAFRAN"), 
       lty=c(1,2), col=c("black","red"), cex=0.6)

# Secteur des médias
PUBLICIS_data <- read.csv("PUBLICISGRP_2019-09-24.txt", 
                          header=T, sep="", dec=".")
PUBLICIS <- PUBLICIS_data$clot
VIVENDI_data <- read.csv("VIVENDI_2019-09-24.txt", 
                         header=T, sep="", dec=".")
VIVENDI <- VIVENDI_data$clot
matplot(cbind(PUBLICIS,VIVENDI), type='l', ylab = "Cours de clôture (en euros)",
        xlab="Jours boursiers (du 24/09/2018 au 24/09/2019)")
title("Evolution des actions PUBLICIS et VIVENDI sur un an")
legend(x=0,y=35, c("PUBLICIS","VIVENDI"), 
       lty=c(1,2), col=c("black","red"), cex=0.5)

data <- data.frame(cbind(SOGE,BNP,PEUGEOT,RENAULT,BOUYGUES,
                         SAINTGOBAIN,AIRBUS,SAFRAN,PUBLICIS,VIVENDI))
noms <- names(data)
print(noms)
matplot(data, type=c(rep('l',10)), lty=c(rep(1,10)),
        xlab='Temps', ylab='Cours (en euros)')
title("Evolution d'un panier de 10 actions du CAC40")

logdata <- log(data) # logarithme des cours
tauxdata <- apply(logdata,MARGIN=2,FUN='diff')

corrdata <- cor(tauxdata) # corrélations 2 à 2
View(corrdata)

matplot(cbind(PUBLICIS,SAFRAN,RENAULT,VIVENDI), 
        type='l', ylab = "Cours de clôture (en euros)", xlab="jours boursiers (du 24/09/2018 au 24/09/2019)")
title("Evolution de 4 actions sur un an")
legend("right",c("PUBLICIS","SAFRAN","RENAULT","VIVENDI"),
       lty=c(1,2,3,4), col=c(1,2,3,4), cex=0.5)

# Taux de rendement sur un an 
RENAULT[1];RENAULT[256]
100*(RENAULT[256]-RENAULT[1])/RENAULT[1]
SAFRAN[1];SAFRAN[256]
100*(SAFRAN[256]-SAFRAN[1])/SAFRAN[1]




