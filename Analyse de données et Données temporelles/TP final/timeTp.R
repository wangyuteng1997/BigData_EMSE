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

plot.ts(kwh)
kwh.log=log(kwh)
kwh.log.diff1 <- diff(kwh.log, lag=1)
plot.ts(kwh.log.diff1)
acf(kwh.log.diff1,lag.max=30)
acf(kwh.log.diff1,lag.max=30,plot=FALSE)
pacf(kwh.log.diff1,lag.max=30)
pacf(kwh.log.diff1,lag.max=30,plot=FALSE)

library(forecast)
library(tseries)
library(fUnitRoots)
auto.arima(kwh.log,trace=T)  # ARIMA(1,0,1)(2,1,0)[12]

airarima1 <- arima(kwh.log,order=c(0,0,4),seasonal=list(order=c(0,1,0),period=12),method="ML")

print(airarima1)
prevision <- predict(airarima1, n.ahead=12, prediction.interval=T)
print(prevision)

print(exp(prevision$pred))

arimaforecast <- forecast(airarima1, h=12)
print(arimaforecast)

arimaforecast

zwx <- 1:12 
zwx[1] <- 7.669971
zwx[2] <-        7.591122
zwx[3] <-7.553135
zwx[4] <-7.565150
zwx[5] <-7.549666
zwx[6] <-7.610852
zwx[7] <-7.865953
zwx[8] <-7.799000
zwx[9] <-7.565952
zwx[10] <-7.513850
zwx[11] <-7.693138
zwx[12] <-7.723261
zwx.pre=exp(zwx)
print(zwx.pre)   
