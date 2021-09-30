
fun_PCA <- function(X, reserve_ratio=0.95){
  # X: matrix of data
  # reserve_ratio: the ratio of reserved information
  # RETURN: Matrix after PCA process
  
  MatrixCov <- cov(X)
  # print(MatrixCov)
  len_col <- ncol(X)
  eig <- eigen(MatrixCov)
  eigValues <- eig$values
  eigVectors <- eig$vectors
  print(eigValues)
  
  eigValueCum_percent <- cumsum(eigValues) / sum(eigValues)  
  
  plot(eigValues, col='grey')
  par(new=TRUE)
  plot(eigValueCum_percent, ylim=c(0,1), type='s',
       xaxt = "n", yaxt = "n",
       main='Eigenvalues & Cumsum Percentage')
  axis(side = 4)
  abline(h=reserve_ratio, col = "red", lty=3)
  mtext("Eigenvalues", side = 2)
  mtext("Percentage(*100%)", side = 4)
  
  for (i in 1:len_col) {
    if(eigValueCum_percent[i] >= reserve_ratio){
      k <- i
      break
    }
  }
  transMatrix <- cbind(eigVectors[,1:k])
  Matrix_afterPCA <- X%*%transMatrix
  
  return(Matrix_afterPCA)
}

fun_Quality <- function(X, X_PCA){
  Q <- rowSums(X_PCA^2) / rowSums(X^2)
  return(Q)
}

nb.tirages <- 2000
a <- rnorm(nb.tirages)
b <- rnorm(nb.tirages)
c <- rnorm(nb.tirages)

x <- sort(rnorm(nb.tirages))
y <- rnorm(nb.tirages)
z <- rnorm(nb.tirages) + atan2(x, y)


# espace de variable
X_Rp_iso <- cbind(a,b,c) 
pcaResult_Rp_iso <- fun_PCA(X_Rp_iso,reserve_ratio = 0.6)
# print(pcaResult_Rp_non_iso)
Q_Rp_iso <- fun_Quality(X_Rp_iso,pcaResult_Rp_iso)
# print(Q_Rp_iso)
print(sum(Q_Rp_iso))


X_Rp_non_iso <- cbind(x,y,z)
pcaResult_Rp_non_iso <- fun_PCA(X_Rp_non_iso,reserve_ratio = 0.9)
# print(pcaResult_Rp_iso)
Q_Rp_non_iso <- fun_Quality(X_Rp_non_iso,pcaResult_Rp_non_iso)
# print(Q_Rp_non_iso)
print(sum(Q_Rp_non_iso))


# espace d'individu
X_Rn_iso <- rbind(a,b,c) 
pcaResult_Rn_iso <- fun_PCA(X_Rn_iso)
print(pcaResult_Rn_iso)
Q_Rn_iso <- fun_Quality(X_Rn_iso, pcaResult_Rn_iso)
print(Q_Rn_iso)
print(sum(Q_Rn_iso))


X_Rn_non_iso <- rbind(x,y,z)
pcaResult_Rn_non_iso <- fun_PCA(X_Rn_non_iso)
print(pcaResult_Rn_non_iso)
Q_Rn_non_iso <- fun_Quality(X_Rn_non_iso,pcaResult_Rn_non_iso)
print(Q_Rn_non_iso)
print(sum(Q_Rn_non_iso))



####
library(rgl)
open3d()
plot3d(X_Rp_non_iso, col=topo.colors(3))
