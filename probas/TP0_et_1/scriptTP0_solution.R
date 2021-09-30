# TP0 - Probabilités Avancées - UP1 
# Majeure Science des Données 2020-2021
# Solution R

# Question 1
# Simulation d'une loi normale N(0,1) (loi normale centrée-réduite ou standard)

mu <- 0     # moyenne
sigma <- 1  # écart-type = racine carrée de la variance
nb.tirages <- 100  # nombre de tirages aléatoires = taille échantillon simulé

# Utilisation du générateur de nombre normal rnorm : "r = random" et "norm = normal"

x.simu <- rnorm(n=nb.tirages, mean=mu, sd=sigma)  

# Visualisation des données simulées

plot(1:nb.tirages, x.simu, xlab="Numéro de simulation", ylab="Simulations",
     main="Simulations d'une loi normale")
abline(h=mu, lty=1, col="red", lwd=2)   # lty = line type (1 par défaut)
abline(h=1.96, lty=2, col="red", lwd=2) # lwd = line width (1 par défaut)
abline(h=-1.96, lty=2, col="red", lwd=2)

# Question 2
# Histogramme : on utilise breaks pour jouer sur le nombre de classes et l'option
# freq = FALSE pour le normaliser (densité de probabilité)

nb.tirages <- 10000
x.simu <- rnorm(n=nb.tirages, mean=mu, sd=sigma)
hist(x.simu, breaks=20, freq=FALSE, xlab="Valeurs simulées", ylab="Densité",
     main="",ylim=c(0,0.45))  
points(x.simu, rep(0,nb.tirages), pch='+', cex=0.5, col="blue") # cex = coeff. expansion
maxx = mu + 3.5*sigma
minx = mu - 3.5*sigma
x.grid <- seq(minx, maxx, 0.01)
fd <- dnorm(x.grid, mean=mu, sd=sigma) # dnorm pour densité normale : "d = density"
lines(x.grid, fd, col="red", lwd=2)
title("Histogramme et densité réelle")

# Question 3
# Quantiles théoriques d'une loi normale N(0, 1)

probas <- c(0.025, 0.25, 0.5, 0.75, 0.975)
q <- qnorm(probas, mean=0, sd=1) # quantiles théoriques : "q = quantile"
print(round(q,2))

# Vérification à l'aide de la fonction de répartition F définie par: 
# q -> p = P(X <= q) (cumulative distribution function ou cdf)

probas.q  <- pnorm(q, mean=0, sd=1) # cdf : "p = probability" 
print(probas.q)

# A connaître probas à +/- sigma | 2*sigma | 3*sigma
#                          68%   | 95 %    | 99.7%

probas <- 2*pnorm(c(1,2,3), mean=0, sd=1) - 1
print(round(probas,3))

# Question 4  
# Simulation d'un échantillon d'une loi normale quelconque N(mu, var)

mu <- 1      
sigma <- 3   
nb.tirages <- 100  
x.simu <- rnorm(n=nb.tirages, mean=mu, sd=sigma) # simulation

# Quartiles empiriques/expérimentaux calculés sur l'échantillon simulé
# et quantiles théoriques

q4.hat <- quantile(x.simu, probs=c(0.25,0.5,0.75))
print(round(q4.hat,2))
q4.theo <- qnorm(probas, mean=mu, sd=sigma)
q4.theo <- mu + sigma*qnorm(c(0.25,0.5,0.75), mean=0, sd=1) # variante
print(round(q4.theo,2))

# Question 5
# Quantiles expérimentaux contre quantiles théoriques de la loi normale N(0, 1) :
# quantiles-quantiles plot ou q-q plot 
# L'un des graphiques les plus utilisés en Statistique!

mu <- 1      
sigma <- 3   
nb.tirages <- 1000  
x.simu <- rnorm(n=nb.tirages, mean=mu, sd=sigma) # simulation

probas <- seq(from=0.05, to=0.95, by=0.05)
q.theo <- qnorm(probas, mean=0, sd=1)
q.hat <- quantile(x.simu, probas)
plot(q.theo, q.hat, xlab="Quantiles théoriques N(0,1)", ylab="Quantiles expérimentaux")
abline(a=mu, b=sigma, col="red", lwd=2)
grid()
title("Droite de Henry")

# Les fonctions R qui permettent de réaliser ce graphique, voir l'aide

qqnorm(x.simu); qqline(x.simu, col="red", lwd=2); grid()

# Question 6 : cas d'un échantillon simulé selon une loi exponentielle

t.simu <- rexp(n=nb.tirages, rate=1) # rate = paramètre lambda de la loi exponentielle
qqnorm(t.simu, main="Q-Q plot"); qqline(t.simu, col="red", lwd=2); grid()

# Etude de la loi normale bidimensionnelle associée au couple (X,Y)

# Question 7
# Cas X et Y indépendantes N(0,1) ou loi normale bidimensionnelle standard
 
nb.tirages <- 200
x.simu <- rnorm(n=nb.tirages, mean=0, sd=1)
y.simu <- rnorm(n=nb.tirages, mean=0, sd=1)

plot(x.simu, y.simu, type='p', pch='+', asp=1, xlab="x", ylab="y")
abline(h=0,lty=2)
abline(v=0,lty=2)

library(mixtools)  # pour la fonction ellipse()
mu <- c(0,0) # Moyenne du vecteur aléatoire (X,Y)
sigma <- matrix(c(1, 0, 0, 1),nrow = 2) # Matrice de covariance du vecteur (X,Y)

# courbes d'iso-probabilité du vecteur (X,Y)
for (niv in 1:5){
  ellipse(mu, sigma, alpha=.5/niv, col='red')
}

title("Courbes d'iso-probabilité (cas de l'indépendance)", cex.main=0.8)

# Questions 8 
sX <- 3 
sY <- 1
nb.tirages <- 200
x.simu <- sX*rnorm(n=nb.tirages, mean=0, sd=1)
y.simu <- sY*rnorm(n=nb.tirages, mean=0, sd=1)

plot(x.simu, y.simu, type='p', pch='+', asp=1, xlab="x", ylab="y")
abline(h=0,lty=2)
abline(v=0,lty=2)

mu <- c(0,0) # Moyenne du vecteur aléatoire (X,Y)
sigma <- matrix(c(sX^2, 0, 0, sY^2),nrow = 2) # Matrice de covariance du vecteur (X,Y)

# courbes d'iso-probabilité du vecteur (X,Y)
for (niv in 1:5){
  ellipse(mu, sigma, alpha=.5/niv, col='red')
}
title("Courbes d'iso-probabilité = ellipses", cex.main=0.8)

# Commentaires : les variables X et Y sont encore indépendantes
# La densité du couple est le produit des densités marginales

# Question 9
nb.tirages <- 200
u.simu <- rnorm(n=nb.tirages, mean=0, sd=1)
v.simu <- rnorm(n=nb.tirages, mean=0, sd=1)

sX <- 3 
sY <- 1
theta <- pi/3
x.simu <- sX*cos(theta)*u.simu - sY*sin(theta)*v.simu
y.simu <- sX*sin(theta)*u.simu + sY*cos(theta)*v.simu

plot(x.simu, y.simu, type='p', pch='+', asp=1, xlab="x", ylab="y")
abline(h=0,lty=2)
abline(v=0,lty=2)

mu <- c(0,0) # Moyenne du vecteur aléatoire (X,Y)
sigma <- matrix(0, nrow=2, ncol=2) 
sigma[1,1] <- sX^2*cos(theta)^2+sY^2*sin(theta)^2
sigma[2,2] <- sX^2*sin(theta)^2+sY^2*cos(theta)^2
sigma[1,2] <- (sX^2-sY^2)*cos(theta)*sin(theta)
sigma[2,1] <- sigma[1,2]

# courbes d'iso-probabilité du vecteur (X,Y)
for (niv in 1:5){
  ellipse(mu, sigma, alpha=.5/niv, col='red')
}
abline(a=0, b=tan(theta), col='blue')
abline(a=0, b=-1/tan(theta), col='blue')
title("Courbes d'iso-probabilité = ellipses", cex.main=0.8)

# Corrélation linéaire en fonction de theta
theta <- seq(0, pi/2, 0.01)
varX <- sX^2*cos(theta)^2+sY^2*sin(theta)^2
varY <- sX^2*sin(theta)^2+sY^2*cos(theta)^2
covXY <- (sX^2-sY^2)*cos(theta)*sin(theta)
rho <- covXY/sqrt(varX*varY)
plot(theta, rho, 'l', main='Corrélation')
grid()

# question 10
# Question 9
nb.tirages <- 200
u.simu <- rnorm(n=nb.tirages, mean=0, sd=1)
v.simu <- rnorm(n=nb.tirages, mean=0, sd=1)

a <- -1; b <- 1; c <- 0.5; d <- 1
x.simu <- a*u.simu + b*v.simu
y.simu <- c*u.simu + d*v.simu

plot(x.simu, y.simu, type='p', pch='+', asp=1, xlab="x", ylab="y")
abline(h=0,lty=1)
abline(v=0,lty=1)

mu <- c(0,0) # Moyenne du vecteur aléatoire (X,Y)
sigma <- matrix(0, nrow=2, ncol=2) 
sigma[1,1] <- a^2 + b^2
sigma[2,2] <- c^2 + d^2
sigma[1,2] <- a*c + b*d
sigma[2,1] <- sigma[1,2]

# courbes d'iso-probabilité du vecteur (X,Y)
for (niv in 1:5){
  ellipse(mu, sigma, alpha=.5/niv, col='red')
}
title("Courbes d'iso-probabilité et DMC", cex.main=0.8)
print(eigen(sigma))
Vp <- eigen(sigma)$vectors
abline(a=0, b=-Vp[1,2]/Vp[2,2], col='red', lty=2, lwd=2)
abline(a=0, b=-Vp[1,1]/Vp[2,1], col='red', lty=2, lwd=2)
abline(a=0, b = sigma[1,2]/sqrt(sigma[1,1]*sigma[2,2]),
       col="blue", lwd=2)


