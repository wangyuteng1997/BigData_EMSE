# Majeure SD 2020-2021
# Séries Temporelles - TP mini-challenge

data <- read.table('Data_app.txt',header=T)

data.ts <- ts(data,frequency = 12,start=c(1980,1))

# Série de consommation d'électricité 
kwh <- data.ts[,"kwh"] 
plot(kwh,type='l',main="Consommation électricité",cex.main=0.8)
grid()

# Séries htdd (heating degree day / chauffage) et cldd (cooling degree day / climatisation)
htdd <- data.ts[,"htdd"]
plot(htdd,type='l',main="Série HTDD",cex.main=0.8)
grid()
cldd <- data.ts[,"cldd"]
plot(cldd,type='l',main="Série CLDD",cex.main=0.8)
grid()

# Visualisation conjointe des 3 séries
op <- par(mfrow=c(3,1),mar=c(2,5,2,4))
plot(kwh,type='l',main="",xlab="")
grid()
title("Données d'apprentissage")
plot(htdd,type='l',xlab="")
grid()
plot(cldd,type='l',xlab="")
grid()
par(op)


