# Majeure Science des Données 2020-2021
# Script du TP n°1, Séries Temporelles 

# Partie 1 : étude d'un MA(1)
# Modèle X[t] = mu + Z[t] + theta*Z[t-1]
# Z[t] bruit blanc gaussien N(0,varZ)

# Question 1
mu <- 0	    # moyenne du processus X[t]
theta <- 1  # paramètre MA(1)
sigZ <- 1	  # écart-type du bruit Z[t]

# Simulation d'un MA(1) de taille n
n <- 200
x <- rep(0,n) # initialisation de la série x[t]

z0 <- sigZ*rnorm(1)     # simulation de Z[0]
z <- sigZ*rnorm(n)      # simulation de Z[1], ... , Z[n]

x[1] <- mu + z[1] + theta*z0
for (t in 2:n) {
	x[t] <- mu + z[t] + theta*z[t-1]
}

# Chronogramme de la série simulée
plot(x, type='o', xlab="Temps t", main = "MA(1) simulé")
abline(h=mu, col="red", lwd=2)
grid()

# Question 2

# ACF empirique
x.acf <- acf(x,20,main="ACF empirique",ylim=c(-1,1))
x.pacf<- pacf(x,20,main="PACF empirique",ylim=c(-1,1))

# Question 3

# On utilise ici la vision "MA(1) = moyenne mobile calculée sur un bruit",
# l'occasion de découvrir la fonction filter() de R
# Faire un help(filter)
n <- 200
theta <- 1
poids <- c(1,theta) # reverse time order

op <- par(mfrow=c(3,1))
sigZ <- 0.1
x <- filter(sigZ*rnorm(n+1), poids, sides=1, method="conv")
plot(x, type='o', xlab="", main=expression(paste(sigma[Z]," = 0.1")))
abline(h=mu, col="red", lwd=2)
sigZ <- 1
x <- filter(sigZ*rnorm(n+1), poids, sides=1, method="conv")
plot(x, type='o', xlab="", main=expression(paste(sigma[Z]," = 1")))
abline(h=mu, col="red", lwd=2)
sigZ <- 10
x <- filter(sigZ*rnorm(n+1), poids, sides=1, method="conv")
plot(x, type='o', xlab="Temps t", main=expression(paste(sigma[Z]," = 10")))
abline(h=mu, col="red", lwd=2)
par(op)
# On voit que l'écart-type sigZ du bruit Z est un facteur d'échelle!

# Question 4

n <- 200
sigZ <- 1
theta <- 10
poids <- c(1,theta)
x <- filter(sigZ*rnorm(n+1), poids, sides=1, method="conv")
plot(x, type='o', xlab="Temps t", main = "MA(1) simulé")
abline(h=mu, col="red", lwd=2)
grid()

# Attention à x[1] = NA (donnée manquante)
acf(x[2:(n+1)],20,main="ACF empirique",ylim=c(-1,1))

# ACF empirique = ACF empirique d'un bruit, ce qu s'explique par la fait que
# X[t] ~= theta*EPS[t-1] car theta grand, d'où aussi la variance importante!

# Question 5

n <- 200
sigZ <- 1
theta <- 1
mu <- 5
z0 <- sigZ*rnorm(1)     # simulation de Z[0]
z <- sigZ*rnorm(n)      # simulation de Z[1], ... , Z[n]

x <- rep(0,n)
x[1] <- mu + z[1] + theta*z0
for (t in 2:n) {
  x[t] <- mu + z[t] + theta*z[t-1]
}
plot(x, type='o', xlab="Temps t", main = "MA(1) simulé décentré", ylim=c(-max(abs(x)),max(abs(x))))
abline(h=mu, col="red", lwd=2)
abline(h=0,lty=2)
grid()

acf(x,20,main="Fonction d'autocorrélation empirique",ylim=c(-1,1))
# mu est la moyenne du processus = paramètre de translation

# Partie 2 

rm(list=ls())

unemptab <- read.table("unemp.txt")
unemp <- unemptab$V1

unemp <- ts(unemp, start = c(1961,1), freq = 12) # série initiale x(t)
plot(unemp, type='l', xlab="Année", ylab="nombre (en milliers)",
     main="Chômage des femmes de 16 à 19 ans aux USA - période 1961-1985",
     cex.main=0.8)
grid()

# Stationnarisation par différenciation simple y(t) = x(t) - x(t-1) et visualisation : 
diffunemp <- diff(unemp) # série y(t)
mu <- mean(diffunemp)
print(mu)
(unemp[300]-unemp[1])/299 # autre expression de mu

plot(diffunemp, type='o', main = "Variations mensuelles du nombre de chômeurs",
     xlab="Année", ylab = expression(y[t]),cex.main=1)
abline(h=mu, col="red", lwd=2)
grid()

# ACF empirique  
ro <- acf(as.vector(diffunemp), lag=20, ylim = c(-1,1), 
          main = expression(paste("ACF empirique de la série ", y[t])),
          xlab="Décalage h (mois)")

# Estimation du coefficient d'auto-corrélation d'ordre 1 de la série y
rho = ro$acf[2]; print(rho)

# Identification du MA(1) (méthode des moments) : Y(t) = mu + Z(t) + theta*Z(t-1)
theta <- (1 - sqrt(1 - 4*rho*rho) )/(2*rho) # choix de la racine en module <= 1
n <- length(diffunemp)
sigmaY <- sd(diffunemp) # estimation de l'écart-type sigmaY de la série Y
sigma <- sigmaY/sqrt(1+theta^2) # sigma = sigZ = écart-type du bruit Z  

# "Vraie" série de chômage en noir et simulations via le modèle MA(1) pour la série des variations
plot(unemp, type='l', col=1, xlab="Année", main = "Série initiale et simulations",
     ylab="nombre en milliers", ylim=c(0,1200))
grid()

n <- length(unemp) # simulations sur n = 300 mois = 25 années
Nsimu <- 3         # nombre de simulations = trajectoires du processus
Xsimu <- matrix(0,nrow=n,ncol=Nsimu) # simulations en colonnes
for (k in 1:Nsimu) {
  Xsim <- rep(0,n) # initialisation de la simulation n°k
  Xsim[1] <- unemp[1] # on démarre de la valeur du nombre de chômeurs de janvier 1961
  bruit <- sigma*rnorm(n)
  for (t in 2:n) Xsim[t] <-  Xsim[t-1] + mu + bruit[t] + theta*bruit[t-1] 
  Xsim <- ts(Xsim, start = c(1961,1), freq = 12)
  lines(Xsim,col=k+1)
  Xsimu[,k] <- Xsim
}

# Variance du processus X par méthode Monte-Carlo (moyenne spatiale)
# On effectue pour cela Nsimu simulations du processus X avec Nsimu grand
Nsimu <- 1000
Xsimu <- matrix(0,nrow=n,ncol=Nsimu)
for (k in 1:Nsimu) {
  Xsim <- rep(0,n)
  Xsim[1] <- unemp[1]
  bruit <- sigma*rnorm(n)
  for (t in 2:n) Xsim[t] <-  Xsim[t-1] + mu + bruit[t] + theta*bruit[t-1] 
  Xsimu[,k] <- Xsim
}

varX <- rep(0,n)
for (t in 1:n) { 
  varX[t] = var(Xsimu[t,])
}
# Graphique
plot(1:n,varX,type="l",xlab="Numéro d'observation (temps)",
     ylab='variance de X',lwd=2)
title('Variance estimée du processus X',cex.main=1)
time = 1:n
abline(lm(varX ~ time - 1),col="red",lwd=2)

plot(1:n,sqrt(varX),type="l",xlab="Numéro d'observation (temps)",
     ylab='écart-type de X',lwd=2)
title('Ecart-type estimé du processus X',cex.main=1)

# Question 4
ro <- acf(unemp, lag=20, ylim = c(-1,1), 
          main = "ACF empirique de la série de chômage", xlab="Décalage h (année)")


# Partie 3 - étude d'un AR(1) 
# Modèle X(t) = phi*X(t-1) + Z(t) 
# Z[t] bruit blanc gaussien N(0,varZ)

rm(list=ls())

phi <- 0.9		        
sigZ <- 1			      # écart-type sigma du bruit Z[t]  
sigX <- sigZ/sqrt(1-phi^2)

# Simulation AR(1) de taille n
n <- 200

x <- rep(0,n)
x0 <- rnorm(1,mean=0,sd=sigX)         # valeur initiale en date t = 0
x[1] <- phi*x0 + sigZ*rnorm(1)

x[1] <- rnorm(n=1,mean=0,sd=sigX)
for (t in 2:n) x[t] <- phi*x[t-1] + sigZ*rnorm(1)

# Chronogramme de la série simulée
plot(1:n,x, type='o', xlab="Temps t",
     main = "AR(1) simulé",ylab="x(t)",cex.main=0.8)
abline(v=0,lty=2)
abline(h=0, col="red", lwd=2)
grid()

# Visualisation des auto-corrélations d'un AR(1)
lag.plot(x,4)

acf(x,20,ylim=c(-1,1),main="ACF empirique d'un AR(1)",cex.main=1)
pacf(x,20,ylim=c(-1,1),main="PACF empirique d'un AR(1)",cex.main=1)





