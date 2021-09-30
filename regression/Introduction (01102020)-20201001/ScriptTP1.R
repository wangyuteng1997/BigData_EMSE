# Régression linéaire sous R
# Majeure Data Science 2020-2021

# Chargement des données de pluie/rendement, cf. support sur Campus "Introduction à la Régression"
data1 <- read.table("data1.txt", header=TRUE)

is.data.frame(data1)
names(data1)
print(data1)  
summary(data1)

# Régression linéaire simple de la variable "rendement" sur la variable "pluie" avec la fonction lm()
pluie <- data1$pluie               # le $ permet d'extraire ici la variable nommée pluie
rend <- data1$rendement
# rend ~ pluie est une formule :rend est la variable à expliquer et pluie la variable explicative
data1.reg <- lm(rend ~ pluie)      
print(data1.reg)

# Représentation graphique
plot(pluie, rend, xlab="hauteur précipitations cumulées (m)", ylab="rendement (sans unité)", lwd=2)
abline(data1.reg, lwd=2, lty=1, col="red")
grid()
title(main=list("Régression linéaire : rendement de blé contre quantité de pluie",cex=1))


# Information supplémentaire avec summary
data1.reg.s <- summary(data1.reg)
print(data1.reg.s)
#可以提供最小值、最大值、四分位数和数值型变量的均值

# Table d'analyse de la variance (ANOVA)
#方差分析 
anova(data1.reg)

# Intervalles de confiance sur les paramètres
confint(data1.reg)
confint(data1.reg,level=0.9)

# Ellipse de confiance
library(car) # pour tracer des ellipses
confidenceEllipse(data1.reg)
#置信椭圆

# Matrice de var-covar des paramètres estimés
VarBeta.hat <- vcov(data1.reg)
print(VarBeta.hat)

# Intervalles de confiance pour la réponse espérée 
plot(pluie, rend, xlab="hauteur précipitations cumulées (m)", ylab="rendement (sans unité)", lwd=2, pch=3)
abline(data1.reg, lwd=2, lty=1, col="blue")

newdata <- data.frame(pluie=seq(min(pluie),max(pluie),by=0.01)) # attention à bien spécifier pluie = ...
print(pluie)
print(newdata)
int.conf <- predict(data1.reg, newdata, interval="confidence") 
lines(newdata[,1], int.conf[,2], col="red", lty=2, lwd=2)
lines(newdata[,1], int.conf[,3], col="red", lty=2, lwd=2)

# Intervalle de prédiction pour la réponsehel
int.pred <- predict(data1.reg, newdata, interval="prediction") 
lines(newdata[,1], int.pred[,2], col="black", lty=2)
lines(newdata[,1], int.pred[,3], col="black", lty=2)
grid()
title(main=list("Intervalle de confiance pour la réponse espérée et intervalle de prédiction",cex=0.8))

# Analyse des résidus
res1 <- residuals(data1.reg)
n <- length(res1)
plot(1:n, res1, ylim=c(-0.2,0.15), xlab="numéro d'observation i = 1, ..., n ", ylab="résidus estimés")
abline(h=0, lty=2)
grid()
title(main=list("Tracé séquentiel des résidus estimés",cex=1))

plot(data1.reg$fitted.values, res1, ylim=c(-0.2,0.15), xlab="réponse estimée", ylab="résidus estimés")
abline(h=0,lty=2)
grid()
title(main=list("Tracé des résidus estimés en fonction de la réponse estimée", cex=1))

plot(pluie, res1, ylim=c(-0.2,0.15), xlab="hauteur précipitations cumulées (m)", ylab="résidus estimés")
abline(h=0, lty=2)
grid()
title(main=list("Tracé des résidus estimés en fonction de la variable pluie",cex=1))

# Vers le modèle linéaire multiple y ~ x + x^2 
pluie2 <- pluie*pluie # nouveau prédicteur = carré du prédicteur "pluie"
data1.reg2 <- lm(rend ~ pluie + pluie2, data=data1)
print(data1.reg2)
summary(data1.reg2)

plot(pluie, rend, xlab="hauteur précipitations cumulées (m)", ylab="rendement (sans unité)", 
     lwd=2, xlim=c(0,0.5), ylim=c(0,0.9))
lines(pluie, data1.reg2$fitted.values, col="blue", lwd=2)
grid() 
title(main=list("Régression linéaire avec terme quadratique", cex=1))

res2 <- residuals(data1.reg2)
plot(data1.reg2$fitted.values, res2, xlim=c(0,1), ylim=c(-0.15,0.1), xlab="réponse estimée",
     ylab="résidus estimés")
abline(h=0, lty=2)
grid()
title(main=list("Tracé des résidus estimés en fonction de la réponse estimée",cex=1))

# Vérification de la dépendance des résidus : à compléter

...


# Résidus standardisés
plot(rstandard(data1.reg2), xlab="numéro d'observation i", ylab="Résidus", ylim=c(-3,3), lwd=2)
grid()
abline(h=0, lty=2)


# Résidus studentisés
points(rstudent(data1.reg2), col="red", pch=3)
abline(h=qt(p=0.025,df=data1.reg2$df.residual), col="red", lwd=2, lty=2)
abline(h=qt(p=0.975,df=data1.reg2$df.residual), col="red", lwd=2, lty=2)
grid()
title('Résidus standardisés et studentisés')

# Partie II 

rm(list=ls.str()) # pour nettoyer l'environnement de travail!

data2 <- read.table("data2.txt", header=TRUE)
names(data2)
nb <- data2$nb
dist <- data2$dist
temps <- data2$temps

pairs(data2) # pour visualiser les dépendances 2 à 2

# Régression de la variable d'intérêt temp sur les autres variables : à vous de jouer...






