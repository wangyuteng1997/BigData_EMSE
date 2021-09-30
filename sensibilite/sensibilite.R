#############partie 1#################################
library(sensitivity)
library(DiceDesign)
library(DiceKriging)
# on pourra changer les valeurs de a et b 
# dans le TP, a = 1 et b = 1

#  fonction qui peut �tre int�ressant
#question 1
#faites varier b12 et b11 et X.
b12 = 10
b11 = 0
n <- 100
X <- data.frame(x1 = runif(n, min = -0.5, max = 0.5), 
                x2 = runif(n, min = -0.5, max = 0.5), 
                x3 = runif(n, min = -0.5, max = 0.5))



poly2.fun <- function(X, b12 = 10 , b11 = 0){
  X[,1] - 2*X[,2] + b12*X[,1]*X[,2] + b11*X[,1]^2
}

g.fun <- function (X, b12 = 10 , b11 = 0){
  abs(X[,1])
}
#question 2 Méthode de Morris :
m <- morris(model = poly2.fun, factors = 3, r = 4,
            design = list(type = "oat",
                          levels = 5,
                          grid.jump = 3),
            binf = -0.5, bsup = 0.5,
            b12 = b12, b11 = b11)
#on visualise le même résultat que pour le cours avec les valeurs de b par défault correspondante
#b on fait de même pour la fonction g
n <- morris(model = g.fun, factors = 2, r = 4,
            design = list(type = "oat",
                          levels = 5,
                          grid.jump = 3),
            binf = -0.5, bsup = 0.5,
            b12 = b12, b11 = b11)

print(m)
print(n)
plot(m, xlim = c(0, 5))
plot(n,xlim = c(0, 5))
#si on utilise b12 = 1, b11 = 1
#on peux voir que dans la méthode de Morris, X1 est la variable qui a le plus d'influence, suivie de près par X2 alors que dans la
#les indices de Sobol permettent de dire que X2 est plus influente, suivie par X1
#si on utilise b12 = 10, b11 = 0
#Les résultats sont ils cohérents avec la méthode de Morris 

#question 3 Indices de Sobol
par(mfrow = c(1, 1))
msobol <- fast99(model = poly2.fun, factors = 3, n = 100,
                 q = "qunif", q.arg = list(min=-0.5, max=0.5)) 
plot(msobol)

par(mfrow = c(1, 1))
msobol <- fast99(model = g.fun, factors = 3, n = 100,
                 q = "qunif", q.arg = list(min=-0.5, max=0.5)) 
plot(msobol)


#question 4
#on peux voir que dans la méthode de Morris, X1 est la variable qui a le plus d'influence, suivie de près par X2 alors que dans la
#les indices de Sobol permettent de dire que X2 est plus influente, suivie par X1
#si on utilise b12 = 10, b11 = 0
#Les résultats sont ils cohérents avec la méthode de Morris
#La méthode de Morris permet de faire plusieurs OAT différentes, et les chemins et les points de départ sont aléatoires.
#Mais l'indice de Sobol est toujours calculé de la même manière, et il n'y a pas de hasard dans cette méthode.


#############partie 2#################################
#Q1
n<-1000
X <- data.frame(x1 = runif(n, min = -pi, max = pi), 
                x2 = runif(n, min = -pi, max = pi), 
                x3 = runif(n, min = -pi, max = pi))

g <- ishigami.fun

par(mfrow = c(1, 1))
msobol <- fast99(model = g, factors = 3, n = 1000,
                 q = "qunif", q.arg = list(min=-pi, max=pi)) 
plot(msobol)

#Q2
msobol_100 <- fast99(model = g, factors = 3, n = 100,
                     q = "qunif", q.arg = list(min=-pi, max=pi)) 

plot(msobol_100)

msobol_50 <- fast99(model = g, factors = 3, n = 50,
                    q = "qunif", q.arg = list(min=-pi, max=pi)) 

plot(msobol_50)

msobol_20 <- fast99(model = g, factors = 3, n = 20,
                    q = "qunif", q.arg = list(min=-pi, max=pi)) 

plot(msobol_20)

#L'évaluation des effets principaux et des interactions n'est pas assez précise pour n = 100,
#et elle ne peut pas être évaluée lorsque n = 50 et n = 20

#############partie 3#################################
##partie 3 Domaine de définition de x1 et x2 : [-5, 5]
#Rayon de l'antenne : 1.5 (diamètre = 3)
#Territoire rectangulaire (de largeur 2 et de hauteur 10)

#question a
doeBANDE <- read.csv("doeBANDE.csv",header = TRUE, sep = ";")
colmeans <- colMeans(doeBANDE)

#µ0 = E(S)
#µ1(X1) = E(S|X1) - µ0
#µ2(X2) = E(S|X2) - µ0
#µ12(X1,X2) = E(S|X1,X2) - µ1(X1) - µ2(X2) - µ0

#question b
design <- doeBANDE[,1:2]
response <- doeBANDE[,3]
kmodel<-km(formula=~1, design, response, covtype="gauss",
           coef.trend = NULL, coef.cov = NULL, coef.var = NULL,
           nugget = NULL, nugget.estim=TRUE, noise.var=NULL, estim.method="MLE",
           penalty = NULL, optim.method = "BFGS", lower = NULL, upper = NULL, 
           parinit = NULL, multistart = 1, control = NULL, gr = TRUE, 
           iso=FALSE, scaling=FALSE, knots=NULL, kernel=NULL)
#question c

xnew <- data.frame(x1=c(3,2,1),x2=c(2,3,4))

kriging.mean <- function(Xnew,kmodel ){
  pre_simple <- predict(kmodel, xnew,"SK")
  m <- pre_simple$mean
  return(m)
}
kriging.mean(xnew,kmodel)
#question d 
µ0 <- colmeans[3]
#µ1 <-  E(doeBANDE[3]|doeBANDE[1])- µ0
#µ2 <- E(doeBANDE[3]|doeBANDE[2]) - µ0
#µ12 <- E(doeBANDE[3]|doeBANDE[2],doeBANDE[1]) - E(doeBANDE[1]) - E(doeBANDE[1]) - µ0

#indices de sobol
par(mfrow = c(1, 1))
msobol <- fast99(model = kriging.mean, factors = 2, n = 1000,
                 q = "qunif", q.arg = list(min = -5, max = 5))                                        
plot(msobol)
#question 2.a
doeBANDE4D <- read.csv("doeBANDE4D.csv",header = TRUE, sep = ";")
colmeans4D <- colMeans(doeBANDE4D)

#Il s'agit de la décomposition de Sobol de S.
#Ceci est similaire à la décomposition de la première partie de la partie 3. 
#Traitez simplement X1, X2 comme un ensemble, X3, X4 comme un ensemble et effectuez une décomposition binaire.

#question 2.b

#S1,2 est le même que le résultat obtenu à partir de deux dimensions.
#S1,2 et S3,4 sont indépendants l'un de l'autre.

#question 2.c

µ0_4D <- colmeans4D[5]
#µ1_4D <- E(doeBANDE4D[5]|doeBANDE4D[1]) - µ0_4D
#µ2_4D <- E(doeBANDE4D[5]|doeBANDE4D[2]) - µ0_4D
#µ3_4D <- E(doeBANDE4D[5]|doeBANDE4D[3]) - µ0_4D
#µ4_4D <- E(doeBANDE4D[5]|doeBANDE4D[4]) - µ0_4D


#indices de sobol
design <- doeBANDE4D[,1:4]
response <- doeBANDE4D[,5]
kmodel<-km(formula=~1, design, response, covtype="gauss",
           coef.trend = NULL, coef.cov = NULL, coef.var = NULL,
           nugget = NULL, nugget.estim=TRUE, noise.var=NULL, estim.method="MLE",
           penalty = NULL, optim.method = "BFGS", lower = NULL, upper = NULL, 
           parinit = NULL, multistart = 1, control = NULL, gr = TRUE, 
           iso=FALSE, scaling=FALSE, knots=NULL, kernel=NULL)

xnew <- data.frame(x1=c(3,2,1),x2=c(2,3,4),x3=c(-1,-4,3),x4=c(1,-2,-3))

kriging.mean <- function(Xnew,kmodel ){
  pre_simple <- predict(kmodel, xnew,"SK")
  m <- pre_simple$mean
  return(m)}

par(mfrow = c(1, 1))
msobol <- fast99(model = kriging.mean, factors = 4, n = 1000,
                 q = "qunif", q.arg = list(min = -5, max = 5))  

msobol <- fast99(model = kriging.mean, factors = 4, n = 1000,
                 q = c("qunif","qunif","qunif","qunif"), q.arg = list(list(min = -5, max = 5),list(min = -5, max = 5),list(min = -5, max = 5),list(min = -5, max = 5)) ) 

plot(msobol)
#question 3




