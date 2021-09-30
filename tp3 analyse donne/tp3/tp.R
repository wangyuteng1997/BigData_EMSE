library(FactoMineR)
#question  4
temps <- data<-read.csv('temps de travaille.csv')
qualite <- data<-read.csv('qualite de la vie.csv')

temps <- temps[,-1]
qualite <- qualite[,-1]
row.names(temps) <- c("fonction0","fonction1","fonction2","fonction3","fonction4","fonction5","fonction6","fonction7")
names(temps) <- c("sex0","sex1","sex2")
row.names(qualite) <- c("fonction0","fonction1","fonction2","fonction3","fonction4","fonction5","fonction6","fonction7")
names(qualite) <- c("sex0","sex1","sex2")

res.temps<- CA(temps)
res.qualite<- CA(qualite)

#question  5
fonction_qualite <- data<-read.csv('fonction_qualite.csv')
sex_qualite <- data<-read.csv('sex_qualite.csv')
sex_temps <- data<-read.csv('sex_temps.csv')

fonction_qualite <- fonction_qualite[,-1]
sex_qualite <- sex_qualite[,-1]
sex_temps <- sex_temps[,-1]

row.names(fonction_qualite) <- c("qualite_99","qualite_16","qualite_4","qualite_2")
row.names(sex_temps) <- c("sex1","sex2")
row.names(sex_qualite) <- c("qualite_99","qualite_16","qualite_4","qualite_2")


res.fonction_qualite<- CA(fonction_qualite)
res.sex_qualite<- CA(sex_qualite)
res.sex_temps<- CA(sex_temps)

