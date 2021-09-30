# Majeure Science des Données 2020-21


# ********************************************************************************************************
# On étudie la série de trafic aérien international "airline" (série Airpassengers de R)
# C'est l'exemple 1 du support de cours : voir pages 4 et 5
# ********************************************************************************************************

# Chargement des données brutes et mise sous la forme d'une série chronologique avec ts

airlinetab <- read.table("airline.dat")
airline <- airlinetab$V1
airline <- ts(airline, start = c(1949,1), freq = 12)
print(airline)
# Chronogramme et légendes...

plot(airline, type='o', xlab="Temps",
ylab="Passagers (en milliers)", main="Trafic aérien international de janv. 1949 à déc. 1960")


# Méthodologie de Box&Jenkins, première transformation simple par
# passage au logarithme et visualisation de l'effet obtenu

logair <- log(airline)
op <- par(mfrow = c(2,1))
plot(airline,type='o', main = "Série x initiale")
plot(logair, type='o', main = "log(x)")
par(op)

# On élimine la tendance (linéaire) par différenciation simple

difflogair <- diff(logair)
plot(difflogair, type='o', main = "Série log(x) différenciée",
xlab="Temps", ylab = expression(paste("(",I-B,")",log(x[t])) ))
abline(h=0, col="red", lwd=2)

# puis différenciation saisonnière (comparer la figure obtenue avec celle du support page 13)
# pour éliminer la composante périodique de période s = 12 mois

diff2logair <- diff(difflogair, lag=12)
op <- par(cex.lab = 0.8)
plot(diff2logair, type='o', main = "Différenciation simple et différenciation saisonniére",
xlab="Temps", ylab = expression(paste("(",I-B^12,")","(",I-B,")",log(x[t]))))
par(op)
abline(h=0, col="red", lwd=2)

# ********************************************************************************************************
# On déroule la méthodologie de Box et Jenkins en partant de la série qui vient
# d'étre obtenue par 2 différenciations successives (série diff2logair). On l'analyse comme une
# série stationnaire à l'aide des ACF et PACF. On vérifie alors que le modèle SARIMA semble bien adapté.
# On estime ensuite ce modèle, on vérifie que les coefficients sont bien ceux du support et on valide 
# graphiquement. Enfin, on utilise le modèle pour faire de la prévision à un an et on visualise la qualité
# de prévision à un an par une technique de back-testing (voir support page 45, analyse post-sample)...
# ********************************************************************************************************

# on rebaptise la série obtenue (sans Tendance et DésaiSonnalisée = dts) 

airdts <- diff2logair

# ACF et PACF de airdts (comparer avec le support page 38)

op <- par(mfrow = c(1,2))
airdts <- as.vector(airdts)
ro <- acf(airdts , lag=25, ylim = c(-1,1), main = expression("ACF série stationnarisée"), xlab="Lag (en mois)",
lwd=2)
alpha <- pacf(airdts , lag=25, ylim = c(-1,1), main = expression("PACF"), xlab="Lag (en mois)", lwd=2)
par(op)

# Ajustement d'un modèle SARIMA(0,1,1)(0,1,1) avec saisonnalité s = 12 et comparaison des coefficients obtenus
# avec ceux du support (voir page 38 pour l'expression complète du modèle et les valeurs des coeff)
# fonction "arima" de R

install.packages("forecast")
library(forecast)
print(auto.arima(logair))

modele <- arima(logair, order = c(0,1,1), seasonal = list(order=c(0,1,1), period = 12))
print(modele)

# Validation du modèle
# fonction "tsdiag" de R

tsdiag(modele)

# Extraction des "résidus" du modéle (processus de bruit sous-jacent) et "normal qqplot"

residus <- modele$residuals
qqnorm(residus, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE)
qqline(residus,col="red")

# Back-testing du modèle SARIMA : on enlève les 12 dernières valeurs que l'on cherche ensuite
# à prévoir. On compare alors avec les valeurs réelles de la série!
# On ré-estime le modèle sur les seuls données d'apprentissage (pour ne pas tricher!)

nair <- length(airline)
airfit <- airline[1:(nair - 12)]	# on enlève les 12 dernières valeurs

logairfit <- ts(log(airfit), start = c(1949,1), freq = 12)

# Ajustement d'un modèle SARIMA(0,1,1)(0,1,1) avec s = 12 sur les données d'apprentissage logairfit

modele <- arima(logairfit, order = c(0,1,1), seasonal = list(order=c(0,1,1), period = 12))
print(modele)

tsdiag(modele)

residus <- modele$residuals
qqnorm(residus, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE)
qqline(residus,col="red")

# prévision avec horizon h = 12

logair <- log(airline)

plot(logair, type='o', xlab='Temps', ylab='Log Nbre Passagers',
     main ="Prévision SARIMA et valeurs réelles", ylim=c(4.5,6.8))

prevision <- predict(modele, n.ahead=12, prediction.interval=T)

lines( ts(prevision$pred, start=c(1960,1), freq=12), type='o', col='red', lwd=2 )
lines( ts(prevision$pred + 2*prevision$se, start=c(1960,1), freq=12),  col='blue', lwd=2 )
lines( ts(prevision$pred - 2*prevision$se, start=c(1960,1), freq=12),  col='blue', lwd=2 )

plot( airline , type='o', xlab='Année', ylab='Nombre (en milliers) de passagers',
      main ="Prévision SARIMA du trafic aérien et valeurs réelles", ylim=c(100,700))

prevision <- predict(modele, n.ahead=12, prediction.interval=T)
lines( ts( exp(prevision$pred),start=c(1960,1), freq=12), type='o', col='red', lwd=2 )
lines( ts( exp(prevision$pred + 2*prevision$se), start=c(1960,1), freq=12),  col='blue', lwd=2 )
lines( ts( exp(prevision$pred - 2*prevision$se), start=c(1960,1), freq=12),  col='blue', lwd=2 )

