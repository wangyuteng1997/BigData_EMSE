# Majeure Science des Données 2020-2021
# Script du TP n°1, Séries Temporelles 
# A compléter

# Partie 1 : étude d'un MA(1) de moyenne mu
# Modèle X[t] = mu + Z[t] + theta*Z[t-1]
# Z[t] bruit blanc gaussien N(0,varZ)

mu <- 0     # moyenne du processus X[t]
theta <- -1   # paramètre MA(1)
sigZ <- 4   # écart-type du bruit Z[t]


# Simulation d'un MA(1) de taille n

n <- 1000
x <- rep(0,n) # initialisation de la série x[t]

z0 <- sigZ*rnorm(1)     # simulation de Z[0]
z <- sigZ*rnorm(n)      # simulation du bruit blanc Z[1], ... , Z[n]

x[1] <- mu + z[1] + theta*z0
for (t in 2:n) {
	x[t] <- mu + z[t] + theta*z[t-1]
}

# Chronogramme de la série simulée

plot(x, type='o', xlab="Temps t", main = "MA(1) simulé", cex.main=1)
abline(h=mu, col="red", lwd=2)

# ACF
ro <- acf(x, 20, main="Fonction d'autocorrélation empirique", ylim=c(-1,1))


# A vous de continuer...





