#question 1 Visualiser la matrice  de dimension (n,p)
data1<-read.table('data_PDE20.txt',header=TRUE)
plot(data1)
data1 <- data1[,-1]
data <- as.matrix(data1)

a <- colMeans(data1)

class(a)
# dans le cas de centree 
  #d<-scale(data1, center = TRUE, scale = FALSE) #donnee centree

  d<-scale(data1) #donnee normal
  
#question 4  valeur propre et vecteur propre
MatCov<-cov(d, y = d, use = "everything", method = c("pearson", "kendall", "spearman"))
r<-eigen(MatCov)
VecteursPropres<-r$vectors
ValeursPropres<-as.vector(r$values)

#Omi = x u
donnees <- d%*%VecteursPropres

# question 5 l’inertie projetée totale
Q<-(rowSums(donnees[,1:8]^2))
plot(Q,main = "", xlab = "Individus")
IPT <- sum(Q)/26
print(IPT)

# question 6

boxplot(donnees,main="Visualisation des donnee projete", xlab="Axes de projections")
barplot(ValeursPropres/sum(ValeursPropres),main="Poids de chaque valeur propre dans le spectre",
        xlab="Valeur Propre i",
        ylab="Valeur Propre i/Somme des valeurs propres")

TauxInertie=NULL
TauxInertie[1]<-ValeursPropres[1]/sum(ValeursPropres)
for (i in 2:8){
  TauxInertie[i]<-TauxInertie[i-1]+ValeursPropres[i]/sum(ValeursPropres)
}
plot(0:8,c(0,TauxInertie[1:8]),xlim=c(0,8),ylim=c(0,1),
     type="b",main="Taux d'inertie explique selon le nombre de valeurs propres retenues",
     xlab="Nombre de valeurs propres retenues",
     ylab="Taux d'inertie explique")

# question 7
#on dermine que le k=3(k<p)

donnees2 <- d%*%VecteursPropres[,1:3]
print(donnees2)

#question 8
#on dermine que k=5
donnees3 <- d%*%VecteursPropres[,1:5]
Q<-(rowSums(donnees3[,1:5]^2)/rowSums(donnees[,1:8]^2))
print(Q)

#question 9
# Fournir la contribution de ces 8 individus aux 5 premiers axes factoriels
Y=matrix(data = NA,nrow = 26,ncol = 8)
for (j in 1:5){#5 premiers axes factoriels
  Y[,j]<-(1/26)*(donnees3[,j]^2)/(ValeursPropres[j])
}
# 1 contribution
plot(1:26,Y[1:26,1],xlab="Individus",ylab="k=1 Contribution de l'individu sur l'axe principal",
     main="Contribution des individus sur l'axe principal")
# 2 contribution
plot(1:26,Y[1:26,2],xlab="Individus",ylab="k=2 Contribution de l'individu sur l'axe principal",
     main="Contribution des individus sur l'axe principal")
# 3 contribution
plot(1:26,Y[1:26,3],xlab="Individus",ylab="k=3 Contribution de l'individu sur l'axe principal",
     main="Contribution des individus sur l'axe principal")
# 4 contribution
plot(1:26,Y[1:26,4],xlab="Individus",ylab="k=4 Contribution de l'individu sur l'axe principal",
     main="Contribution des individus sur l'axe principal")
# 5 contribution
plot(1:26,Y[1:26,5],xlab="Individus",ylab="k=5 Contribution de l'individu sur l'axe principal",
     main="Contribution des individus sur l'axe principal")


#question 10
#Calculer   la corrélation entre chacune des p variables avec les p composante
R=matrix(data = NA,nrow = 8,ncol = 8)
  # dans le cas de normal
 # for (i in 1:8){
 #   R[i,]<-sqrt(ValeursPropres[i])*VecteursPropres[,i]
 # }

  # dans le cas de centree
for (i in 1:8){
    R[i,]<-sqrt(ValeursPropres[i]/MatCov[i,i])*VecteursPropres[,i]
  }

plot(R[1,],R[2,],xlab="Composantes c",ylab="Variables initiales j",
     main="Correlation lineaire entre une composante c et une variable j")



