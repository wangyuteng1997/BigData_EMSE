
#Plans d'experience
library("DiceDesign")
library("maximin")
library("hetGP")

Y<-load('antennes_2d_train.Rdata')

plot(C[,1],C[,2])

grid(nx = 20, ny =20, col = "lightgray",
     lwd = par("lwd"), equilogs = TRUE)

Y<-load('antennes_2d_test.Rdata')

plot(C[,1],C[,2])

grid(nx = 20, ny =20, col = "lightgray",
     lwd = par("lwd"), equilogs = TRUE)

X<-load('antennes_6d_train.Rdata')
for (i in 1:6){
   for(j in 1:6){
     plot(C[,i],C[,j])
           grid(nx = 20, ny =20, col = "lightgray",
                lwd = par("lwd"), equilogs = TRUE)    
   }
 } 


load('antennes_6d_test.Rdata')
for (i in 1:6){
        for(j in 1:6){
                plot(C[,i],C[,j])
                grid(nx = 20, ny =20, col = "lightgray",
                     lwd = par("lwd"), equilogs = TRUE) 
        }
} 


#training Data:Uniform and full factorial designs

d <- 2; N <- 20
par(mfrow = c(1, 2))
X1<-matrix(runif(N * d), N)
plot(matrix(runif(N * d), N), xlab = expression(x[1]), ylab = expression(x[2]), 
     main = "Uniform design", pch = 20, asp = 1, xlim = c(0,1), ylim = c(0,1))
Y1<-expand.grid(seq(0,1,,sqrt(N)), seq(0,1,,sqrt(N)))
plot(expand.grid(seq(0,1,,sqrt(N)), seq(0,1,,sqrt(N))), xlab = expression(x[1]), 
     ylab = expression(x[2]), main = "Full factorial design", pch = 20, asp = 1)


discrepancyCriteria(X1,type='C2')
discrepancyCriteria(Y1,type='C2')
maximin(N, d, T=10*N, Xorig=X1, Xinit=NULL, verb=FALSE, plot=FALSE, boundary=FALSE)
library('randtoolbox')
rss <- rss2d(design=X1, lower=c(0,0), upper=c(1,1), 
             type="l", col="red")
rss <- rss2d(design=Y1, lower=c(0,0), upper=c(1,1), 
             type="l", col="red")




d <- 6; N <- 300
par(mfrow = c(1, 2))
X2<-matrix(runif(N * d), N)
plot(matrix(runif(N * d), N), xlab = expression(x[1]), ylab = expression(x[2]), 
     main = "Uniform design", pch = 20, asp = 1, xlim = c(0,1), ylim = c(0,1))


Y2<-expand.grid(seq(0,1,,sqrt(N)), seq(0,1,,sqrt(N)))
plot(expand.grid(seq(0,1,,sqrt(N)), seq(0,1,,sqrt(N))), xlab = expression(x[1]), 
     ylab = expression(x[2]), main = "Full factorial design", pch = 20, asp = 1)


discrepancyCriteria(X2,type='C2')
discrepancyCriteria(Y2,type='C2')
maximin(N, d, T=10*N, Xorig=X2, Xinit=NULL, verb=FALSE, plot=FALSE, boundary=FALSE)

library('randtoolbox')
rss <- rss2d(design=X2, lower=c(0,0), upper=c(1,1), 
             type="l", col="red")
rss <- rss2d(design=Y2, lower=c(0,0), upper=c(1,1), 
             type="l", col="red")

##################"



library(lhs)
par(mfrow = c(1, 1))
a=matrix(nrow=20,ncol=40)
maxx=rep(0,20)
for( i in 1:20){
X<-randomLHS(20,2)
plot(X, pch = 20, asp = 1, xlim = c(0, 1), ylim = c(0, 1), xlab = "x1", ylab = "x2")
abline(h = seq(0,1,0.1), col = "gray", lty = 3)
abline(v = seq(0,1,0.1), col = "gray", lty = 3)
Maxmin<-maximin(20, 2, 100, Xorig=X, Xinit=NULL, verb=FALSE, plot=FALSE, boundary=FALSE)
a[,(2*i-1):(2*i)]<-Maxmin$Xf[21:40,]
maxx[i]<-max(Maxmin$mi)
}
Max<-max(maxx)
j=which.max(maxx)
modelepropose1<-a[,(2*j-1):(2*j)]
plot(maxx)









library(lhs)
dsc<-rep(0,20)
par(mfrow = c(1, 1))
A=matrix(nrow=20,ncol=40)
for (i in 1:20){
Y<-randomLHS(20,2)
A[,(2*i-1):(2*i)]<-Y
plot(Y, pch = 20, asp = 1, xlim = c(0, 1), ylim = c(0, 1), xlab = "x1", ylab = "x2")
abline(h = seq(0,1,0.1), col = "gray", lty = 3)
abline(v = seq(0,1,0.1), col = "gray", lty = 3)
dsc[i]<-discrepancyCriteria(Y,type='C2')$DisC2
}
j=which.min(dsc)
modelepropose2<-A[,(2*j-1):(2*j)]
plot(dsc)






load("antennes_2d_train.Rdata")
library(lhs)
par(mfrow = c(1, 1))
X<-randomLHS(20,2)
plot(X, pch = 20, asp = 1, xlim = c(0, 1), ylim = c(0, 1), xlab = "x1", ylab = "x2")
abline(h = seq(0,1,0.1), col = "gray", lty = 3)
abline(v = seq(0,1,0.1), col = "gray", lty = 3)
Z<-S
model <- mleHomGP(X, Z, lower = rep(0.1, 2), upper = rep(1, 2))
IMSPE_optim(model, h = 2, Xcand = X, control = list(tol_dist =
                                                               1e-06, tol_diff = 1e-06, multi.start = 20, maxit = 100), Wijs = NULL,
            seed = NULL, ncores = 1)


