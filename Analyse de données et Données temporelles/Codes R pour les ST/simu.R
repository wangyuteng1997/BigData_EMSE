# Prévision par des modèles ARMA
# Simulation de données

# MA(1)
n <- 100 # on apprend avec n données
hrz=5 # horizon h de prévision
z <- rnorm(n+hrz+1) # bruit

plot(z,type='l',main="Bruit")
abline(h=0);grid()
acf(z,lag=20,ylim=c(-1,1))
pacf(z,lag=20,ylim=c(-1,1))

# bruit différencié = MA(1) avec le paramètre theta=1
x <- diff(z)
plot(x,type='l',main="Bruit")
abline(h=0);grid()
acf(x,lag=20,ylim=c(-1,1))
pacf(x,lag=20,ylim=c(-1,1))

# fit MA(1)
z <- rnorm(n+hrz+1)
x <- diff(z)
mod <- arima(x,order=c(0,0,1),include.mean=F)
print(mod)

pred5 <- predict(mod,n.ahead=hrz)
plot((n-10):(n+hrz),c(x[(n-10):n],pred5$pred),
     ylab="",xlab='Temps',type='b',ylim=c(-3,3))
points((n+1):(n+hrz),pred5$pred,type='b',col='red')
points((n+1):(n+hrz),x[(n+1):(n+hrz)])
lines((n+1):(n+hrz),pred5$pred+2*pred5$se,lty=2,col='red')
lines((n+1):(n+hrz),pred5$pred-2*pred5$se,lty=2,col='red')
abline(v=n,lty=2)
abline(h=0,col='red')
title("Prévision MA(1)")

# AR(1)
n <- 300
hrz <- 5

sig <- 1
phi <- 0.95
sig.x <- sig/sqrt(1-phi*phi)
x <- rep(0,n+hrz)
x[1] <- rnorm(1,mean=0,sd=sig.x) 
for (t in 2:(n+hrz)) {
  x[t] = phi*x[t-1]+sig*rnorm(1)
}

# plot(x,type='l',main="AR(1)")
# abline(h=0);grid()
# acf(x,lag=20,ylim=c(-1,1))
# pacf(x,lag=20,ylim=c(-1,1))

# fit AR(1)
mod <- arima(x[1:n],order=c(1,0,0),include.mean = F)
print(mod)

pred5 <- predict(mod,n.ahead=hrz)
plot((n-10):(n+hrz),c(x[(n-10):n],pred5$pred),
     ylab="",xlab='Temps',type='b',ylim=c(-3*sig.x,3*sig.x))
points((n+1):(n+hrz),pred5$pred,col='red')
points((n+1):(n+hrz),x[(n+1):(n+hrz)])
lines((n+1):(n+hrz),pred5$pred+2*pred5$se,lty=2,col='red')
lines((n+1):(n+hrz),pred5$pred-2*pred5$se,lty=2,col='red')
abline(v=n,lty=2)
abline(h=0,col='red')
title("Prévision AR(1)")

# Modèle ARMAX
# Tendance m linéaire + résidu u = AR(1)
n <- 300
hrz <- 20

beta0 <- 10  
beta1 <- 0.05  
temps <- 1:(n+hrz)

sig <- 3
phi <- 0.7
sig.u <- sig/sqrt(1-phi*phi)
u <- rep(0,n+hrz)
u[1] <- rnorm(1,mean=0,sd=sig.u) 
for (t in 2:(n+hrz)) {
  u[t] = phi*u[t-1]+sig*rnorm(1)
}

x <- beta0 +beta1*temps + u
plot(x,type='l',main="AR(1) avec dérive linéaire")
abline(h=0);grid()

# fit avec u ~ AR(1)
mod <- arima(x[1:n],order=c(1,0,0),xreg = temps[1:n],
             include.mean = T)
print(mod)

pred5 <- predict(mod,n.ahead=hrz,newxreg = (n+1):(n+hrz))

lim.y <- c(min(x[(n-100):n]),max(pred5$pred+2*pred5$se))
plot((n-100):(n+hrz),c(x[(n-100):n],pred5$pred),
     ylab="",xlab='Temps',type='b',ylim=lim.y)
points((n+1):(n+hrz),pred5$pred,type='b',col='red')
points((n+1):(n+hrz),x[(n+1):(n+hrz)])
lines((n+1):(n+hrz),pred5$pred+2*pred5$se,lty=2,col='red')
lines((n+1):(n+hrz),pred5$pred-2*pred5$se,lty=2,col='red')
abline(v=n,lty=2)
lines((n-100):(n+hrz),mod$coef[2]+mod$coef[3]*temps[(n-100):(n+hrz)],
       col="blue")
title("Prévision AR(1) avec trend",cex.main=0.8)

pred5 <- predict(mod3,n.ahead=hrz)
derive <- rls$coeff[1]+rls$coeff[2]*((n+1):(n+hrz))
pred5 <- derive + pred5$pred
points((n+1):(n+hrz),pred5,col='blue')
