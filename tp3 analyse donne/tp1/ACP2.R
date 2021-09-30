################
#Nuage isotrope#
################
nb.tirages <- 2000
X <- matrix(0,nb.tirages,3)
x <- rnorm(n=nb.tirages, mean=0, sd=1)
y <- rnorm(n=nb.tirages, mean=0, sd=1)
z <- rnorm(n=nb.tirages, mean=0, sd=1)
X[,1] <- x
X[,2] <- y
X[,3] <- z
library(rgl)
open3d()
plot3d(X[,1], X[,2], X[,3], col = rainbow(nb.tirages))

###############################################################
#X<-scale(X, center = TRUE, scale = FALSE) #donnee centree#
P <- scale(X, center=T,scale=T) #donnee normal  
Q <- scale(X)#
###############################################################
#  la matrice de covariance ou de corrélation
MatCov<-cov(X, y = X, use = "everything", method = c("pearson", "kendall", "spearman"))
r<-eigen(MatCov)
VecteursPropres<-r$vectors
ValeursPropres<-as.vector(r$values)
Correlation <-  cor(X)


#  la cascade des valeurs propres
    ## projection = x u
    ## Poids de chaque valeur propre
barplot(ValeursPropres/sum(ValeursPropres),main="Poids de chaque valeur propre dans le spectre",
        xlab="Valeur Propre i",
        ylab="Valeur Propre i/Somme des valeurs propres")

    ##Taux de inertie explique
TauxInertie=NULL
TauxInertie[1]<-ValeursPropres[1]/sum(ValeursPropres)
for (i in 2:3){
  TauxInertie[i]<-TauxInertie[i-1]+ValeursPropres[i]/sum(ValeursPropres)
}
plot(0:3,c(0,TauxInertie[1:3]),xlim=c(0,3.5),ylim=c(0,1),
     type="b",main="Taux d'inertie explique selon le nombre de valeurs propres retenues",
     xlab="Nombre de valeurs propres retenues",
     ylab="Taux d'inertie explique")



# la qualité de la projection des individus
donnees <- X%*%VecteursPropres
donnees2 <- X%*%VecteursPropres[,1:2]
Q<-(rowSums(donnees2[,1:2]^2)/rowSums(donnees[,1:3]^2))


####################
#Nuage non isotrope#
####################
nb.tirages <- 1000
X1 <- matrix(0,nb.tirages,3)
x1 <- sort(rnorm(nb.tirages))
y1 <- rnorm(nb.tirages)
z1 <- rnorm(nb.tirages) + atan2(x, y)
X1[,1] <- x1
X1[,2] <- y1
X1[,3] <- z1
library(rgl)
open3d()
plot3d(X1[,1], X1[,2], X1[,3], col = rainbow(nb.tirages))


#  la matrice de covariance ou de corrélation
MatCov<-cov(X1, y = X1, use = "everything", method = c("pearson", "kendall", "spearman"))
r<-eigen(MatCov)
VecteursPropres<-r$vectors
ValeursPropres<-as.vector(r$values)
Correlation <-  cor(X1)


#  la cascade des valeurs propres
   ## projection = x u
   ## Poids de chaque valeur propre
barplot(ValeursPropres/sum(ValeursPropres),main="Poids de chaque valeur propre dans le spectre",
        xlab="Valeur Propre i",
        ylab="Valeur Propre i/Somme des valeurs propres")

   ##Taux de inertie explique
TauxInertie=NULL
TauxInertie[1]<-ValeursPropres[1]/sum(ValeursPropres)
for (i in 2:3){
  TauxInertie[i]<-TauxInertie[i-1]+ValeursPropres[i]/sum(ValeursPropres)
}
plot(0:3,c(0,TauxInertie[1:3]),xlim=c(0,3.5),ylim=c(0,1),
     type="b",main="Taux d'inertie explique selon le nombre de valeurs propres retenues",
     xlab="Nombre de valeurs propres retenues",
     ylab="Taux d'inertie explique")



# la qualité de la projection des individus
donnees <- X1%*%VecteursPropres
donnees2 <- X1%*%VecteursPropres[,1:2]
Q<-(rowSums(donnees2[,1:2]^2)/rowSums(donnees[,1:3]^2))


####################
#Points  extrémaux #
####################

# rajouter certains points extrémaux pour une ou deux variables

nb.tirages <- 2000
X2 <- matrix(0,nb.tirages,3)
x2 <- rnorm(nb.tirages,mean =2,sd = 2 )
y2 <- rnorm(nb.tirages,mean =4,sd = 5)
z2 <- rnorm(nb.tirages,mean =2,sd = 3)
X2[,1] <- x2
X2[,2] <- y2
X2[,3] <- z2
library(rgl)
open3d()
plot3d(X2[,1], X2[,2], X2[,3])
###############################################################
X2<-scale(X2, center = TRUE, scale = FALSE) #donnee centree#
#X2<-scale(X2) #donnee normal                                  #
###############################################################

library(rgl)
open3d()
plot3d(X2[,1], X2[,2], X2[,3])

# Prenez l'intervalle de confiance de (0,01, 99,9)
boxplot(X2[,1])
print(length(x2))
#trouver les analies dans 1st dimension
q1<-quantile(X2[,1], 0.001) 
q99<-quantile(X2[,1], 0.999) 
lowbound <- which(x2<q1)
upbound <- which(x2>q99)

#Suppression des anomalies dans trois dimensions
X2 <- X2[-lowbound,]
X2 <- X2[-upbound,]
open3d()
plot3d(X2[,1], X2[,2], X2[,3])













