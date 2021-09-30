# TP0 - UP1 Probabilités Avancées
# Majeure Science des Données 2020-2021

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

nb.tirages <- 2000
x.simu <- rnorm(n=nb.tirages, mean=mu, sd=sigma)
hist(x.simu, breaks=20, freq=FALSE, xlab="Valeurs simulées", ylab="Densité",
     main="",ylim=c(0,0.7))
points(x.simu, rep(0,nb.tirages), pch='+', cex=0.5, col="blue") # cex = coeff. expansion
maxx = mu + 3.5*sigma
minx = mu - 3.5*sigma
x.grid <- seq(minx, maxx, 0.01)
fd <- dnorm(x.grid, mean=mu, sd=sigma) # dnorm pour densité normale : "d = density"
lines(x.grid, fd, col="red", lwd=2)
title("Histogramme et densité réelle")

# 绘制出n个点的rnorm分布（类似正太，n越大越是正态）
# 绘制n个正太分布的点在x轴上投影分布的情况
# 绘制正太分布密度函数的直线（这是理想情况）
# rnorm（需要点的个数n）生成正态分布点的坐标 dnorm(需要点的坐标）生成理想的正太密度分布

# Question 3
# Quantiles théoriques d'une loi normale N(0, 1)

probas <- c(0.025, 0.25, 0.5, 0.75, 0.975)
q <- qnorm(probas, mean=0, sd=1) # quantiles théoriques : "q = quantile"
print(round(q,2))#取两位小数
#求出c（）中的概率所对应的x坐标 ----求出q

# Vérification à l'aide de la fonction de répartition F définie par:
# q -> p = P(X <= q) (cumulative distribution function ou cdf)

probas.q  <- pnorm(q, mean=0, sd=1) # cdf : "p = probability"
print(probas.q)
#根据求出的坐标再求出概率（probas.q）


# Question 4
# Simulation d'un échantillon d'une loi normale quelconque N(mu, var)

mu <- 1
sigma <- 3
nb.tirages <- 100
x.simu <- rnorm(n=nb.tirages, mean=mu, sd=sigma) # simulation

# Quartiles empiriques/expérimentaux calculés sur l'échantillon simulé
# et quantiles théoriques

q4.hat <- quantile(x.simu, probas=c(0.25,0.5,0.75))
#求出x.simu中的（0.25.0.5.0.75）百分比的位置
print(round(q4.hat,2))

q4.theo <- qnorm(probas, mean=mu, sd=sigma)
q4.theo <- mu + sigma*qnorm(probas, mean=0, sd=1) # variante
print(round(q4.theo,2))
#通过qnorm函数找出对应的（0.25.0.5.0.75）概率的位置
  

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
print(q.theo)
#求出理想正太分布的对应概率的x坐标 A
q.hat <- quantile(x.simu, probas)
print(q.hat)
#求出生成假设正太分布的对应概率的x坐标 B  rnorm是生成n个正态分布的点，但是我们验证是否是正态分布

plot(q.theo, q.hat, xlab="Quantiles théoriques N(0,1)", ylab="Quantiles expérimentaux")
abline(a=mu, b=sigma, col="red", lwd=2)
grid()
title("Droite de Henry")
#以A,B作为x y轴如果斜率为1 说明这个生成的分布就是正太分布
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
#加载这个包
mu <- c(0,0) # Moyenne du vecteur aléatoire (X,Y)
#相当于中心点
sigma <- matrix(c(1, 0, 0, 1),nrow = 2) # Matrice de covariance du vecteur (X,Y)
#协方差矩阵
#协方差矩阵的对角线元素为X1与X2轴的方差 反对角线就是两个分布协方差
#[cov(x,x),cov(x,y)]其中cov(x,x)和cov(y,y)是x和y的方差
#[cov(y,x),cov(y,y)]

#X=au+bv     协方差矩阵为[a^2+b^2,ac+bd]
#Y=cu+dv                 [ac+bd,b^2+d^2]

# https://blog.csdn.net/weixin_37895339/article/details/80351541

# courbes d'iso-probabilité du vecteur (X,Y)
for (niv in 1:5){
  ellipse(mu, sigma, alpha=.5/2, col='red')
  #绘制一个椭圆  alpha相当于概率的圈，第一个园只有百分之多少的点在其中（相当于多少的一个执行区间）
  #园增加半径添加的点就会增加
}

title("Courbes d'iso-probabilité (cas de l'indépendance)", cex.main=0.8)

# Questions 8 à 10 : à vous de jouer en complétant ce script...

#question 9
nb.tirages <- 200
u.simu <- rnorm(n=nb.tirages, mean=0, sd=1)
v.simu <- rnorm(n=nb.tirages, mean=0, sd=1)

sX <- 3 
sY <- 1
theta <- pi/3
y.simu <- -sX*cos(theta)*u.simu - sY*sin(theta)*v.simu
y.simu <- -sX*sin(theta)*u.simu + sY*cos(theta)*v.simu

plot(x.simu,y.simu,type='p',pch='+',asp=1,xlab = "x",ylab = "y")
abline(h=0,lty=2)
abline(v=0,lty=2)

mu <- c(0.0)
sigma <- matrix(0,nrow = 2,ncol = 2)#协方差矩阵
sigma[1,1] <- sX^2*cos(theta)^2+ sY^2*sin(theta)^2
sigma[2,2] <- sX^2*sin(theta)^2+ sY^2*cos(theta)^2
sigma[1,2] <- (sX^2-sY^2)*cos(theta)*sin(theta)
sigma[2,1] <- sigma[1,2]

for (niv in 1:5){
  ellipse(mu, sigma, alpha=.5/2, col='red')
  #绘制一个椭圆  alpha相当于概率的圈，第一个园只有百分之多少的点在其中（相当于多少的一个执行区间）
  #园增加半径添加的点就会增加
}
title("Courbes d'iso-probabilité (cas de l'indépendance)", cex.main=0.8)

theta <- seq(0,pi/2,0.01)
varX <- sX^2*cos(theta)^2+ sY^2*sin(theta)^2
varY <- sX^2*sin(theta)^2+ sY^2*cos(theta)^2
covXY <- (sX^2-sY^2)*cos(theta)*sin(theta)
rho <- covXY/sqrt(varX,varY)
plot(theta,rho,'1',main = "correlation")

