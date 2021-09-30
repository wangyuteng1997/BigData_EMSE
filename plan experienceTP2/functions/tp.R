library(DiceDesign)
library(geometry)
library(pracma)
# WANG Yuteng , ZHAO Wenxu , XU Liwei
#1
lhs=lhsDesign(10,2)
X=lhs$design
plot(lhs$design[,1],lhs$design[,2])


help(discrepSA_LHS)

#2 maximinSA_LHS
dimension <- 2
n <- 10
maximinSA_LHS
Xopt <- maximinSA_LHS(X, T0=10, c=0.99, it=2000)
plot(Xopt$design)
plot(Xopt$critValues, type="l")
plot(Xopt$tempValues, type="l")

# 3 discrepSA_LHS
Xopt1 <- discrepSA_LHS(X, T0=10, c=0.99, it=2000, criterion="C2")
plot(Xopt1$design)
plot(Xopt1$critValues, type="l")


#4 mindist
mindist(lhs$design)
mindist(Xopt$design)
mindist(Xopt1$design)

#5
source('Minimax.R')
Minimax(lhs$design)
Minimax(Xopt$design)
Minimax(Xopt1$design)

#5
help(discrepancyCriteria)
discrepancyCriteria(lhs$design)
discrepancyCriteria(Xopt$design)
discrepancyCriteria(Xopt1$design)

#6

lhs1=lhsDesign(50,5,seed=1234)
X=lhs1$design

  #it	The number of iterations
it <- 1000 # 2000 , 3000 .......
Xopt <- maximinSA_LHS(X, T0=10, c=0.99, it=it)
Xopt1 <- discrepSA_LHS(X, T0=10, c=0.99, it=it, criterion="C2")


#question 2
# Créer une function
lhs <- function (n, dimension, randomized = TRUE, seed = NULL) 
{
  if (is.null(seed)) {
    seed <- as.numeric(Sys.time())
  }
  set.seed(seed)
  if (randomized) 
    ran = matrix(runif(n * dimension), nrow = n, ncol = dimension)
  else ran = matrix(0.5, nrow = n, ncol = dimension)
  x = matrix(0, nrow = n, ncol = dimension)
  for (i in 1:dimension) {
    idx = sample(1:n)
    P = (idx - ran[, i])/n
    x[, i] <- P
  }
  return(list(n = n, dimension = dimension, design = x, randomized = randomized, 
              seed = seed))}

# visualiser un plan
plan <- lhs(10,2)
plot(plan$design[,1],plan$design[,2])

#question 3
library(vipor)
help(vanDerCorput)
suit_test <- vanDerCorput(10,2)
# 低差异序列 
# 参考链接 ： https://zhuanlan.zhihu.com/p/20197323

volQt <- function(Qt){
      result <- 1
  for (t in Qt) {
    result <- t*result
    }
 return(result)
 }


#la discrépance
# le plan est entre [0,1] ici on prends Q(t) entre [0,0.4]
# et nous trouvons le plus faible discrepance entre n egale ß10000 et 100000

n <- seq(from=10000, to=100000, by=1)
area <- 0.4
i <- 0
for (n in n) {
  suite <- vanDerCorput(n)
  seq <- which(suite <area)
  Qt <- suite[seq]
  
  #On définit la discrépance comme suit 
  Discrepancy[i] <- length(Qt)/length(suite)-area
  i <- i+1
  return(Discrepancy)
}

min(Discrepancy)




