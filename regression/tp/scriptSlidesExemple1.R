# Majeure Science des Données 2020-2021
# Cours de Régression - Exemple 1    

# Modèle de Régression sur données réelles
# Données de pluie

data1 <- read.table("data1.txt", header=TRUE)
pluie <- data1$pluie               
rend <- data1$rendement

# Modèle 1 : Régression Linéaire Simple (RLS)  
mod1 <- lm(rend ~ pluie)     
mod1.s <- summary(mod1)
print(mod1.s)

plot(pluie,rend,xlab="hauteur précipitations cumulées (m)",ylab="rendement (sans unité)",lwd=2)
abline(mod1,lwd=2,lty=1,col="red")
title(main=list("Régression linéaire : rendement de blé contre quantité de pluie",cex=1))

# Modèle 2 : modèle avec terme quadratique
pluie2 <- pluie*pluie               # on définit le second prédicteur
mod2 <- lm(rend ~ pluie + pluie2)   # modèle 2 : RLM avec p = 2 prédicteurs    
mod2.s <- summary(mod2)
print(mod2.s)
print(mod1.s) # pour comparer
x.new <- seq(0,max(pluie)*1.1,0.01)
y.fit <- mod2$coef[1]+mod2$coef[2]*x.new+mod2$coef[3]*x.new*x.new
lines(x.new,y.fit,col="blue",lwd=2)

# Modèle 3 : régression polynomiale de degré 3
pluie3 <- pluie^3               
mod3 <- lm(rend ~ pluie + pluie2 + pluie3)       
mod3.s <- summary(mod3)
print(mod3.s)
x.new <- seq(0,max(pluie)*1.1,0.01)
y.fit <- mod3$coef[1]+mod3$coef[2]*x.new+mod3$coef[3]*x.new*x.new+mod3$coef[4]*x.new^3
lines(x.new,y.fit,col="green",lwd=2)

# Analyse de la variance
anova(mod1);print(mod1.s) # tests équivalents de l'hypothèse H0 : beta1 = 0
anova(mod2);print(mod2.s) # tests équivalents de l'hypothèse H0 : beta2 = 0
anova(mod1,mod2)          # test modèles emboîtés
anova(lm(rend~1),mod2)
anova(mod3);print(mod3.s)
cor(data.frame(pluie,pluie2,pluie3))

# Vers l'ANOVA

beta0 <- 1  # H0 : beta0 = 0 est fausse! 
beta1 <- 1  # H0 : beta1 = 0 est fausse!
beta2 <- 1  # H0 : beta2 = 0 fausse!

sigma <- 0.5  # écart-type résiduel 

x <- seq(0,1,0.1) # prédicteur contrôlé!
xsq <- x^2
cor(x,xsq)
n <- length(x)
# xopt <- rep(c(0,1),length.out=n)
# x <- xopt

residus <- rnorm(n,mean=0,sd=sigma)
y <- beta0 + beta1*x + beta2*xsq + residus

dataxy.reg <- lm(y ~ x + xsq)       

# Représentation graphique

plot(x,y,xlab="x",ylab="y",lwd=2,ylim=1.1*c(min(y),max(y)))
xx=seq(0,1,0.01)
lines(x,dataxy.reg$fitted.values,col="red",lwd=2)
lines(xx,beta0 + beta1*xx + beta2*xx^2,col='blue',lwd=2)
legend("topleft",legend=c("Fitted","True"),lty=1,lwd=2,
       col=c("red","blue"),cex=0.6)
title(main=list("Régression linéaire multiple sur données simulées",cex=1))

dataxy.reg.s <- summary(dataxy.reg)
print(dataxy.reg.s)
anova(dataxy.reg)

# Tracé des deux prédicteurs
plot(xx,xx,col='red',type='l')
lines(xx,xx^2,col='blue')

# Prédiction avec un modèle de Régression Linéaire
# exemple 1 du support

# Modèle n°1
data1 <- read.table("data1.txt", header=TRUE)
pluie <- data1$pluie               
rend <- data1$rendement

mod1 <- lm(rend ~ pluie)      

# Représentation graphique
plot(pluie,rend,xlab="hauteur précipitations cumulées (m)",ylab="rendement (sans unité)",lwd=2)
abline(mod1,lwd=2,lty=1,col="red")

# intervalle de confiance pour la réponse espérée
newdata <- data.frame(pluie=seq(min(pluie),max(pluie),by=0.01)) # attention é bien spécifier pluie = ...
int.conf <- predict(mod1,newdata,interval="confidence") 
lines(newdata[,1],int.conf[,2],col="red",lty=2,lwd=2)
lines(newdata[,1],int.conf[,3],col="red",lty=2,lwd=2)

# Intervalle de prédiction pour la réponse
int.pred <- predict(mod1,newdata,interval="prediction") 
lines(newdata[,1],int.pred[,2],col="black",lty=2)
lines(newdata[,1],int.pred[,3],col="black",lty=2)
title(main=list("Intervalle de confiance pour la réponse espérée et intervalle de prédiction",cex=0.8))

# modèle n°2
plot(pluie,rend,xlab="hauteur précipitations cumulées (m)",ylab="rendement (sans unité)",lwd=2)
pluie2 <- pluie*pluie              
mod2 <- lm(rend ~ pluie + pluie2)      

x.new <- seq(0,max(pluie)*1.1,0.01)
y.fit <- mod2$coef[1]+mod2$coef[2]*x.new+mod2$coef[3]*x.new*x.new
lines(x.new,y.fit,col="red",lwd=2)

# intervalle de confiance pour la réponse espérée
pluie.new <- seq(min(pluie),max(pluie),by=0.01)
newdata <- data.frame(pluie=pluie.new, pluie2=pluie.new^2) 
int.conf <- predict(mod2,newdata,interval="confidence") 
lines(newdata[,1],int.conf[,2],col="red",lty=2,lwd=2)
lines(newdata[,1],int.conf[,3],col="red",lty=2,lwd=2)

# Intervalle de prédiction pour la réponse
int.pred <- predict(mod2,newdata,interval="prediction") 
lines(newdata[,1],int.pred[,2],col="black",lty=2)
lines(newdata[,1],int.pred[,3],col="black",lty=2)
title(main=list("Intervalle de confiance pour la réponse espérée et intervalle de prédiction",cex=0.8))

# Analyse des résidus
res1 <- residuals(mod1)
n <- length(res1);print(n)
plot(1:n,res1,ylim=c(-0.2,0.15),xlab="numéro d'observation i = 1, ..., n ",ylab="résidus estimés")
abline(h=0,lty=2)
title(main=list("Tracé séquentiel des résidus estimés",cex=0.8))

#plot(mod1$fitted.values,res1,ylim=c(-0.2,0.15),xlab="réponse estimée",ylab="résidus estimés")
plot(pluie,res1,ylim=c(-0.2,0.15),xlab="Hauteur de pluie (m)",ylab="résidus estimés")
abline(h=0,lty=2)
grid()
title(main=list("Tracé des résidus estimés en fonction du prédicteur",cex=0.8))

# Résidus standardisés
plot(mod1$fitted.values,rstandard(mod1),xlab="réponse estimée",ylab="Résidus",ylim=c(-3,3),lwd=2)
abline(h=c(0,2,-2),lty=2)
# Résidus studentisés
points(mod1$fitted.values,rstudent(mod1),col="red",pch=3)
abline(h=qt(p=0.025,df=mod1$df.residual-1),col="red",lwd=2,lty=2)
abline(h=qt(p=0.975,df=mod1$df.residual-1),col="red",lwd=2,lty=2)
title(main=list("Résidus standardisés et studentisés",cex=0.8))

# Normalité des résidus
qqnorm(res1);qqline(res1,probs=c(0.1,0.9),col="red")

# Vers la régression linéaire multiple y ~ x + x^2 
 

res2 <- residuals(mod2)
plot(1:n,res2,ylim=c(-0.2,0.15),xlab="numéro d'observation i = 1, ..., n ",ylab="résidus estimés")
abline(h=0,lty=2)
title(main=list("Tracé séquentiel des résidus estimés",cex=0.8))

plot(mod2$fitted.values,res2,ylim=c(-0.2,0.15),xlab="réponse estimée",ylab="résidus estimés")
abline(h=0,lty=2)
title(main=list("Tracé des résidus estimés en fonction de la réponse estimée",cex=0.8))

# Résidus standardisés
plot(mod2$fitted.values,rstandard(mod2),xlab="réponse estimée",ylab="Résidus",ylim=c(-3,3),lwd=2)
abline(h=c(0,2,-2),lty=2)

# Résidus studentisés
points(mod2$fitted.values,rstudent(mod2),col="red",pch=3)
abline(h=qt(p=0.025,df=mod1$df.residual-1),col="red",lwd=2,lty=2)
abline(h=qt(p=0.975,df=mod1$df.residual-1),col="red",lwd=2,lty=2)
title(main=list("Résidus standardisés et studentisés",cex=0.8))

# Normalité des résidus
qqnorm(res2);qqline(res2,probs=c(0.1,0.9),col="red")

