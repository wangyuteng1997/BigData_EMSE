# Majeure Science des Données 2020-2021
# Cours de Régression - Exemple 2    

# Modèle de Régression sur données réelles
# Approvisionnement d'un parc de distributeurs de boissons

data2 <- read.table("data2.txt", header=TRUE)
names(data2)
n <- dim(data2)[1]
temps <- data2$temps
nb <- data2$nb
dist <- data2$dist

# Modèle de Régression Linéaire Multiple
mod <- lm(temps ~ nb + dist)     
mod.s <- summary(mod)
print(mod.s)

# Analyse de la variance
anova(lm(temps~1),mod)
anova(mod)

# Analyse des résidus standardisés
par(mfrow=c(2,2))
plot(1:n,rstandard(mod),xlab="Numéro observation",
     ylab="",ylim=c(-4,4),lwd=2, cex.lab=0.8)
grid()
abline(h=c(0,2,-2),lty=2, col="red")
plot(mod$fitted.values,rstandard(mod),xlab="Réponse estimée (mn)",
     ylab="",ylim=c(-4,4),lwd=2, cex.lab=0.8)
grid()
abline(h=c(0,2,-2),lty=2, col="red")
plot(nb,rstandard(mod),xlab="Nbre de caisses",
     ylab="",ylim=c(-4,4),lwd=2, cex.lab=0.8)
grid()
abline(h=c(0,2,-2),lty=2, col="red")
plot(dist,rstandard(mod),xlab="Distance (m)",
     ylab="",ylim=c(-4,4),lwd=2, cex.lab=0.8)
grid()
abline(h=c(0,2,-2),lty=2, col="red")
par(mfrow=c(1,1))
title('Résidus standardisés', cex.main=0.8)

# Résidus studentisés en fonction de la réponse prédite
plot(mod$fitted.values,rstandard(mod),xlab="Réponse estimée (mn)",
     ylab="",ylim=c(-5,5),lwd=2, cex.lab=0.8)
grid()
points(mod$fitted.values,rstudent(mod),col="red",pch=3)
abline(h=qt(p=0.025,df=mod$df.residual-1),col="red",lwd=2,lty=2)
abline(h=qt(p=0.975,df=mod$df.residual-1),col="red",lwd=2,lty=2)
title(main=list("Résidus standardisés et studentisés",cex=0.8))

# Tracé séquentiel des résidus studentisés 
plot(1:n,rstandard(mod),xlab="Numéro d'observation",
     ylab="",ylim=c(-3,6),lwd=2, cex.lab=0.8)
grid()
points(1:n,rstudent(mod),col="red",pch=3)
abline(h=qt(p=0.025,df=mod$df.residual-1),col="red",lwd=2,lty=2)
abline(h=qt(p=0.975,df=mod$df.residual-1),col="red",lwd=2,lty=2)
title(main=list("Résidus standardisés et studentisés",cex=0.8))

# Leviers
lev <- hatvalues(mod)
print(lev)
plot(lev,xlab="Numéro d'observation",ylab="",main="Leviers",type="h",
     ylim=c(0,1),cex.main=0.8,cex.lab=0.8)
points(lev)
grid()
p <- ncol(data2) - 1
abline(h=2*(p+1)/n,col="red",lty=2)
index.leviers <- which(lev>2*(p+1)/n)
print(index.leviers)

# Distance de Cook
dist.Cook <- cooks.distance(mod)
plot(dist.Cook,xlab="Numéro d'observation",ylab="",main="Distance de Cook",
     type="h",cex.main=0.8,cex.lab=0.8)
points(dist.Cook)
grid()

# Calcul direct de la distance de Cook
dist2.Cook <- (lev/(1-lev))*rstandard(mod)^2/(p+1)
points(dist2.Cook,col="red")

# Suppression de l'observation n°9
print(n)
data.lo9 <- data2[-9,]
temps <- data.lo9$temps
nb <- data.lo9$nb
dist <- data.lo9$dist
n <- length(temps)
print(n)

# Modèle de Régression Linéaire Multiple
mod <- lm(temps ~ nb + dist)     
mod.s <- summary(mod)
print(mod.s)

# Analyse des résidus studentisés
par(mfrow=c(2,2))
plot(1:n,rstudent(mod),xlab="Numéro observation",
     ylab="",ylim=c(-4,4),lwd=2, cex.lab=0.8)
grid()
abline(h=qt(p=0.025,df=mod$df.residual-1),col="red",lwd=2,lty=2)
abline(h=qt(p=0.975,df=mod$df.residual-1),col="red",lwd=2,lty=2)
plot(mod$fitted.values,rstudent(mod),xlab="Réponse estimée (mn)",
     ylab="",ylim=c(-4,4),lwd=2, cex.lab=0.8)
grid()
abline(h=qt(p=0.025,df=mod$df.residual-1),col="red",lwd=2,lty=2)
abline(h=qt(p=0.975,df=mod$df.residual-1),col="red",lwd=2,lty=2)
plot(nb,rstudent(mod),xlab="Nbre de caisses",
     ylab="",ylim=c(-4,4),lwd=2, cex.lab=0.8)
grid()
abline(h=qt(p=0.025,df=mod$df.residual-1),col="red",lwd=2,lty=2)
abline(h=qt(p=0.975,df=mod$df.residual-1),col="red",lwd=2,lty=2)
plot(dist,rstudent(mod),xlab="Distance (m)",
     ylab="",ylim=c(-4,4),lwd=2, cex.lab=0.8)
grid()
abline(h=qt(p=0.025,df=mod$df.residual-1),col="red",lwd=2,lty=2)
abline(h=qt(p=0.975,df=mod$df.residual-1),col="red",lwd=2,lty=2)
par(mfrow=c(1,1))
title('Résidus studentisés', cex.main=0.8)

# Résidus en fonction de la réponse prédite
plot(mod$fitted.values,rstandard(mod),xlab="Réponse estimée (mn)",
     ylab="",ylim=c(-5,5),lwd=2, cex.lab=0.8)
grid()
points(mod$fitted.values,rstudent(mod),col="red",pch=3)
abline(h=qt(p=0.025,df=mod$df.residual-1),col="red",lwd=2,lty=2)
abline(h=qt(p=0.975,df=mod$df.residual-1),col="red",lwd=2,lty=2)
title(main=list("Résidus standardisés et studentisés",cex=0.8))

# Tracé séquentiel des résidus  
plot(1:n,rstandard(mod),xlab="Numéro d'observation",
     ylab="",ylim=c(-3,6),lwd=2, cex.lab=0.8)
grid()
points(1:n,rstudent(mod),col="red",pch=3)
abline(h=qt(p=0.025,df=mod$df.residual-1),col="red",lwd=2,lty=2)
abline(h=qt(p=0.975,df=mod$df.residual-1),col="red",lwd=2,lty=2)
title(main=list("Résidus standardisés et studentisés",cex=0.8))

# Leviers
lev <- hatvalues(mod)
print(lev)
plot(lev,xlab="Numéro d'observation",ylab="",main="Leviers",type="h",
     ylim=c(0,1),cex.main=0.8,cex.lab=0.8)
points(lev)
grid()
p <- ncol(data2) - 1
abline(h=2*(p+1)/n,col="red",lty=2)
index.leviers <- which(lev>2*(p+1)/n)
print(index.leviers)

# Distance de Cook
dist.Cook <- cooks.distance(mod)
plot(dist.Cook,xlab="Numéro d'observation",ylab="",main="Distance de Cook",
     type="h",cex.main=0.8,cex.lab=0.8)
points(dist.Cook)
grid()

# Analyse des résidus sous R
plot(mod)
