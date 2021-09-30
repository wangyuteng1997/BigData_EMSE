# Script du TP n?2, S?ries Temporelles, Majeure Science des Donn?es 2020-21

##############################################################################
# PARTIE 1 : Etude d'un AR(2)
# Mod?le X(t) = mu + phi1*(X(t-1)-mu) + phi2*(X(t-2)-mu) + Z(t)
# Z[t] bruit blanc gaussien N(0,varZ)
# simulation d''un AR(2) par une phase initiale de stationnarisation
##############################################################################

# inverses des racines du polyn?me P(z) = 1 - phi1*z - phi2*Z^2

# cas de deux racines r?elles dans ]-1, 1[
r1 <- 0.9

theta <- 60
phi1 <- 2*r*cos(2*pi*theta/360) 
phi2 <- r*r			# param?tres AR(2) 

mu <- 0			        # moyenne du processus X[t]
sigZ <- 1	                # ?cart-type du bruit Z[t]

# simulation avec r?gime transitoire de taille ninit = 50
ninit <- 50
n <- 200
ntot <- ninit + n

xtot <- rep(0,ntot)
xtot[1] <- 0
xtot[2] <- 0

for (t in 3:ntot) xtot[t] <- phi1*xtot[t-1] + phi2*xtot[t-2] + sigZ*rnorm(1)

xtot <- mu + xtot             # d?centrage
xinit <- xtot[1:ninit]        # r?gime transitoire (initial)

xstat <- xtot[(ninit+1):ntot] # r?gime stationnaire --> AR(2) de taille n

# visualisation r?gime transient
plot(xtot, type='o', xlab="Temps t", main = "AR(2) simul? avec r?gime transitoire, col="grey")
lines((ninit+1):ntot, xstat, type='o')
abline(mu, 0, col="red", lwd=2)

# analyse graphique - chronogramme de la s?rie xstat  

plot(xstat,type='o',xlab='Temps t',main = "Simulation d'un AR(2)")
abline(mu,0,col='red')

# acf et pacf de la s?rie simul?e
op <- par(mfrow = c(1,2))
ro <- acf(xstat, lag=15, ylim = c(-1,1), main = "ACF empirique")
alpha <- pacf(xstat, lag=15, ylim = c(-1,1), main = "et PACF", xlim=c(0,15))
par(op)


###############################################################################
# PARTIE 2 : identification de mod?les
############################################################################### 

# On commence avec la premi?re s?rie de donn?es, fichier "serie1.Rdata"

rm(list=ls())           # clear all 
load("serie1.Rdata")
ls.str()

# chronogramme de la s?rie  
plot(serie, type='o', xlab="Temps t", ylab="", main = "data")
abline(h=0, col="red", lwd=2)


# acf et pacf de la s?rie  
op <- par(mfrow = c(1,2))
ro <- acf(serie, lag=15, ylim = c(-1,1), main = "ACF empirique")
alpha <- pacf(serie, lag=15, ylim = c(-1,1), main = "et PACF empirique", xlim=c(0,15))
par(op)

# seconde s?rie : fichier "serie2.Rdata"

...
