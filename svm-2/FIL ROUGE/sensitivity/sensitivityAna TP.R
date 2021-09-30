#bismilah
#------------------------ Sensivity analysis-------------

#-------------------------Cas 2d------------------------- 

#Dans cette partie du projet on va utiliser les bibliothèques si-dessous

library(sensitivity)
library(DiceDesign)
library(DiceKriging)

# Data fil rouge

load("antennes_2d_train.Rdata")
design<-C
response<-S

# Modèle de krigeage

kmodel<-km(formula=~1, design, response, covtype="gauss",
           coef.trend = NULL, coef.cov = NULL, coef.var = NULL,
           nugget = NULL, nugget.estim=TRUE, noise.var=NULL, estim.method="MLE",
           penalty = NULL, optim.method = "BFGS", lower = NULL, upper = NULL, 
           parinit = NULL, multistart = 1, control = NULL, gr = TRUE, 
           iso=FALSE, scaling=FALSE, knots=NULL, kernel=NULL)


#Simulation des coordonées (X et Y) d'une antenne (cas 2d)

X1<-runif(1000,-6,5)
X2<-runif(1000,-6.5,8.7)

newdata<-cbind(X1,X2)

# Application du modele pour la prediction de newdata

S_tilde<-predict(kmodel, newdata, type="SK")

#Prototype des fonctions.

s_tilde<-function(newdata)
{
  S_tilde<-predict(kmodel, newdata, type="SK")
  result<-S_tilde$mean
  return(result)
}

# Lissage des fonctions de Sobol

z<-s_tilde(newdata)



mu0 <- mean(z)

par(mfrow = c(1,2))

for (i in 1:2){
  
  cat("Projection sur la variable") ; pause <- readLines(n =1)
  plot(z - mu0 ~ newdata[,i], xlab=paste("X", i, sep=""), ylab=expression(S(X)-mu[0]))
  
  cat("Esperance conditionnelle simulee") ; pause <- readLines(n =1)
  m.ss <- smooth.spline(newdata[,i], z-mu0)
  lines(m.ss, lwd=10, col="blue")
  
 
}


#print(var(m.ss$y))



cat("Indices de Sobol") ; pause <- readLines(n = 1)





design<-C
response<-S
type<-"SK"

SA.Surface <- fast99(model =s_tilde, 
                     factors = 2, 
                     n = 100,
                     q = c("qunif","qunif"), 
                     q.arg = list(
                       list(min = -10, max = 10),list(min = -10, max = 10)))

plot(SA.Surface); box()
print(SA.Surface)


# Morris

m <- morris(model = s_tilde, factors = 2, r = 4, 
            design = list(type = "oat", 
                          levels = 5, 
                          grid.jump = 1), 
            binf =c(-6,-6.5), bsup = c(5,8.7))

print(m)
plot(m)

# al hamdolilah


#-------------------------Cas 6d-------------------------


load("antennes_6d_train.Rdata")
design<-C
response<-S

# Modèle de krigeage

kmodel<-km(formula=~1, design, response, covtype="gauss",
           coef.trend = NULL, coef.cov = NULL, coef.var = NULL,
           nugget = NULL, nugget.estim=TRUE, noise.var=NULL, estim.method="MLE",
           penalty = NULL, optim.method = "BFGS", lower = NULL, upper = NULL, 
           parinit = NULL, multistart = 1, control = NULL, gr = TRUE, 
           iso=FALSE, scaling=FALSE, knots=NULL, kernel=NULL)


#Simulation des coordonées (X et Y) d'une antenne (cas 2d)

X1<-runif(1000,-6,5)
X3<-runif(1000,-6,5)
X5<-runif(1000,-6,5)

X2<-runif(1000,-6.5,8.7)
X4<-runif(1000,-6.5,8.7)
X6<-runif(1000,-6.5,8.7)

newdata<-cbind(X1,X2,X3,X4,X5,X6)

# Application du modele pour la prediction de newdata

S_tilde<-predict(kmodel, newdata, type="SK")

#Prototype des fonctions.

s_tilde<-function(newdata)
{
  S_tilde<-predict(kmodel, newdata, type="SK")
  result<-S_tilde$mean
  return(result)
}

# Lissage des fonctions de Sobol

z<-s_tilde(newdata)



mu0 <- mean(z)

par(mfrow = c(2,3))

for (i in 1:6){
  
  #cat("Projection sur la variable") ; pause <- readLines(n =1)
  plot(z - mu0 ~ newdata[,i], xlab=paste("X", i, sep=""), ylab=expression(S(X)-mu[0]))
  
  cat("Esperance conditionnelle simulee") ; pause <- readLines(n =1)
  m.ss <- smooth.spline(newdata[,i], z-mu0)
  lines(m.ss, lwd=10, col="blue")
  
  
}

cat("Indices de Sobol") ; pause <- readLines(n = 1)


design<-C
response<-S
type<-"SK"

par(mfrow = c(1,1))
SA.Surface <- fast99(model =s_tilde, 
                     factors = 6, 
                     n = 100,
                     q = c("qunif","qunif","qunif","qunif","qunif","qunif"), 
                     q.arg = list(
                       list(min = -6, max = 6),
                       list(min = -7, max = 9),
                       list(min = -6, max = 6),
                       list(min = -7, max = 9),
                       list(min = -6, max = 6),
                       list(min = -7, max = 9)))

plot(SA.Surface); box()
print(SA.Surface)


# Morris

m <- morris(model = s_tilde, factors = 6, r = 4, 
            design = list(type = "oat", 
                          levels = 5, 
                          grid.jump = 1), 
            binf =c(-6,-7,-6,-7,-6,-7), bsup = c(6,9,6,9,6,9))

print(m)
plot(m, xlim = c(4, 31),ylim = c(5,30))

# al hamdolilah
