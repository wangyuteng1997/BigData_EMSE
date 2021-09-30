# TP2 - Probabilités Avancées - UP1
# Majeure science des donnes 2020-2021
# Script à compléter

# ----------
# EXERCISE 1 
# ----------

# Partie 1 - mu = (1 2), sigma1 = sigma2 = 1 et rho = 0.8

# Question 1
n <- 1000# taille de l'échantillon  

# Initialisation de la matrice des données simulées de taille nx2
X <- matrix(0, nrow= n,ncol=2) 

eigenvec = eig$vectors
eigenvalues = eig$values
# Simulation
eps1 <- rnorm(n) # bruit blanc N(0,1) de taille n (composantes sur l'axe x1)
X[,1] <- 1+eps1
eps2 <- rnorm(n)   # bruit blanc N(0,1) de taille n (composantes sur l'axe x2)
X[,2] <- 2+0.8*eps1+0.6*eps2
eps=cbind(eps1,eps2)
# Visualisation
plot(X[,1],X[,2],xlim = c(-2,5),ylim = c(-2,5))
abline(1,1,col = 'red')
abline(3,-1,col = 'red')
####################################################################################
cov = matrix(c(1,0.8,0.8,1), nrow = 2)
eig = eigen(cov)
a <- c(1,2,3,2,2)
print(a==2)
index = which(a==2)
print(index)
b = c(1,2,3,4,5,6)
print(b>=2)
print(b<=5)
index <- which(b>=2 & b<5)
print(index)

alpha <- pi/3
rotmat <- matrix(0,nrow = 2,ncol = 2)#协方差矩阵
rotmat[1,1] <- cos(alpha)
rotmat[2,2] <- -sin(alpha)
rotmat[1,2] <- sin(alpha)
rotmat[2,1] <- cos(alpha)
x1 <- rnorm(1000)
x2 = 3*rnorm(1000)
X <- cbind(x1,x2)

plot(X[,1],X[,2],ylim = c(-10,10),xlim = c(-10,10))

X_rot <- t(rotmat%*%t(X))
points(X_rot[,1],X_rot[,2],col ='red')


############################partie 2




# Question 2


# Partie 2
# Below: Create a function to simulate a 2D Gaussian Vector X=(X1, X2)
# ------
# Inputs
# ------
#  mu   : a vector of size 2 giving the mean of X
#  rho  : a real number between [-1, 1] giving the correlation cor(X1,X2)
#  sig    : a vector of size 2 containing the standard deviation of X. Default is (1,1).
#  n    : an integer giving the sample size. Default is 1000.
#  plot : should we plot the result? Default is TRUE.
#  ...  : optional arguments (color, labels, etc.) to be passed to plot
# ------
# Output
# ------
# X     : A matrix of size nx2 containing a sample of size n from the Gaussian distribution of X
mu <- c(1,2)
rho <- 0.8

simu_VG <- function(mu, rho, sig = c(1,1), n = 1000, plot = TRUE, ...){
  
  # construct the covariance matrix, such that :
  # cor(X1, X2) = rho, var(X1) = sig[1]^2, var(X2) = sig[2]^2
  rotmat <- matrix(0,nrow = 2,ncol = 2)#协方差矩阵
  rotmat[1,1] <- cos(alpha)
  rotmat[2,2] <- -sin(alpha)
  rotmat[1,2] <- sin(alpha)
  rotmat[2,1] <- cos(alpha)

  # here simulate a sample of size n drawn from N(mu, Gamma) 
  u.simu <- rnorm(n=n, mean=0, sd=1)
  v.simu <- rnorm(n=n, mean=0, sd=1)
  X1=u.simu+1
  X2=2+0.8*u.simu+0.6*v.simu
  # -- MY CODE --
  
  X <- matrix(NA, n, 2)   # the matrix of size nx2 containing the simulations
  ... # something to be computed, out of the loop
  for (i in 1:n){
    ...             # something to be computed, inside the loop
    X[i, ] <- ...   # a vector of length 2 (row or column, as you like)
  }
  
  # plot the results if argument 'plot' is equal to TRUE
  if (plot){
    par(mfrow = c(1,1))
    plot(X, asp=1, ...)  # asp = 1 --> same scale for the x and y axis
    abline(v = mu[1], h = mu[2])     
  }
    
  return(X)
}
    
# Run the function
X <- simu_VG(mu = c(1,2), rho = 0.8)


# ----------
# EXERCISE 2 
# ----------

# Consider the points such that x1-h <= X1 <= x1+h
x1 <- 1
h <- 0.1

indices <- ...   # the indices of X that fulfill the condition

X2 <- X[indices, 2]
points(X[indices, 1], X[indices, 2], col="blue")
abline(v = c(x1-h, x1+h))

# A compléter
...

# ----------
# EXERCISE 3 
# ----------

RENAULT_data <- read.csv("RENAULT_2019-09-24.txt", header=T, sep="", dec=".")
# 可以读取csv和txt格式文件 header=T表示从第二行开始读取数据，header=F表示从第一行开始读取数据
#sep是指定分隔符 dec是小数点的表示，默认就是一个点
RENAULT <- RENAULT_data$clot
#读取clot的数据tx_RENAULT <- diff(log(RENAULT)) # diff pour la différence discrète log(S(t)) - log(S(t-1))
plot(100*tx_RENAULT, type='l', ylab="taux en %", xlab="Temps")
abline(h=0, col="red")
title("Taux de rendement logarithmiques action RENAULT sur un an (en %)", cex.main=0.9)

plot(RENAULT, type='l', ylab = "cours de clôture (en euros)", 
     xlab="jours boursiers (du 24/09/2018 au 24/09/2019)")
grid()
#网格函数help
title("Evolution de l'action RENAULT sur un an")

# Taux de hausse ou de baisse logarithmique


# A compléter
...