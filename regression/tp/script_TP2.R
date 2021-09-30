# Majeure Science des Données 2020-2021
# TP mini-challenge - cours de Régression 

data <- read.table('Data_app.txt',header=TRUE)
names(data)
View(data)

# variable à prédire
ventes <- data$ventes 

# moyenne et écart-type des ventes observées
ventes.mean <- mean(ventes)
print(ventes.mean)
ventes.sd <- sd(ventes)
print(ventes.sd)

# Modèle de Régression Linéaire vide : p = 0 --> pas de prédicteur utilisé
# Ainsi, on prédit par la moyenne des ventes observées

##########################################
data <- read.table('Data_app.txt',header=TRUE)
temperature <- data$temperature
temperature2 <- temperature*temperature
jour <- data$jour
jour2 <- jour*jour
ventes <- data$Ventes

model <- lm(ventes~temperature+jour,data = data)
summary(model)
plot(model) # 4 plotwe can see the condition of the data 
###########################################

data <- read.table('Data_app.txt',header=TRUE)
temperature <- data$temperature
temperature2 <- temperature*temperature
jour <- data$jour
jour2 <- jour*jour
jour3 <- jour^3
ventes <- data$Ventes

temperature2 <- temperature*temperature
model2 <- lm(ventes~temperature+temperature2+jour,data = data)
summary(model2)
par(mfrow=c(2,2))
plot(model2) # 4 plotwe can see the condition of the data 


##################################################
data <- read.table('Data_app.txt',header=TRUE)
temperature <- data$temperature
temperature2 <- temperature*temperature
jour <- data$jour
jour2 <- jour*jour
jour3 <- jour^3
ventes <- data$Ventes

model3 <- lm(ventes~temperature+jour+jour2,data = data)
summary(model3)
par(mfrow=c(2,2))
plot(model3) # 4 plotwe can see the condition of the data
plot(rstudent(model3))
imnormal <- which(rstudent(model3)>2)

#去除异常点
data <- data[-imnormal,]
temperature <- data$temperature
temperature2 <- temperature*temperature
jour <- data$jour
jour2 <- jour*jour
jour3 <- jour^3
ventes <- data$Ventes

model3 <- lm(ventes~temperature+jour+jour2,data = data)
summary(model3)
par(mfrow=c(2,2))
plot(model3) # 4 plotwe can see the condition of the data

###########################################
data <- read.table('Data_app.txt',header=TRUE)
temperature <- data$temperature
temperature2 <- temperature*temperature
jour <- data$jour
jour2 <- jour*jour
jour3 <- jour^3
ventes <- data$Ventes

model4 <- lm(ventes~temperature+jour+jour2+jour3,data = data)
summary(model4)
par(mfrow=c(2,2))
plot(model4) # 4 plotwe can see the condition of the data
plot(rstudent(model4))
imnormal <- which(rstudent(model4)>2)

#去除异常点
data <- data[-imnormal,]
temperature <- data$temperature
temperature2 <- temperature*temperature
jour <- data$jour
jour2 <- jour*jour
jour3 <- jour^3
ventes <- data$Ventes

model4 <- lm(ventes~temperature+jour+jour2+jour3,data = data)
summary(model4)
par(mfrow=c(2,2))
plot(model4) # 4 plotwe can see the condition of the data

##########################################################################
data <- read.table('Data_app.txt',header=TRUE)
temperature <- data$temperature
temperature2 <- temperature*temperature
jour <- data$jour
jour2 <- jour*jour
jour3 <- jour^3
ventes <- data$Ventes

model5 <- lm(ventes~temperature+temperature2+jour+jour2+jour3,data = data)
summary(model5)
par(mfrow=c(2,2))
plot(model5) # 4 plotwe can see the condition of the data
plot(residuals(model5))
plot(rstudent(model5))
plot(residuals(model5))
imnormal <- which(rstudent(model5)>2)

#去除异常点
data <- data[-imnormal,]
temperature <- data$temperature
temperature2 <- temperature*temperature
jour <- data$jour
jour2 <- jour*jour
jour3 <- jour^3
ventes <- data$Ventes

model5 <- lm(ventes~temperature+temperature2+jour+jour2+jour3,data = data)
summary(model5)
par(mfrow=c(2,2))
plot(model5) # 4 plotwe can see the condition of the data
plot(residuals(model5))
plot(rstudent(model5))
plot(residuals(model5))




#predict
temperature <- 13.5
tem <- data.frame(temperature=13.5,temperature2=13.5^2,jour=2,jour2=2^2,jour3=2^3)
tem2 <- data.frame(temperature2=13.5^2)
jour <- data.frame(jour=2)
jour2 <- data.frame(jour2=2^2)
jour3 <- data.frame(jour3=2^3)
use <- c(tem,tem2,jour,jour2,jour3)
a <- predict(model5,use,interval="prediction",level=0.95)


wmodel <- lm(ventes~temperature+jour,data = data)
model
summary(model)
plot(model)

# On prédit par la moyenne des ventes = prédicteur constant
# ne dépend pas des prédicteurs "température" et 'type de jour'
# construction du fichier des prévisions pour les données test

data.test <- read.table('Data_test.txt', header=TRUE)

m <- dim(data.test)[1]
prediction1 <- matrix(0,nrow=m,ncol=1)


for (k in 1:m) {
  temperture <- data.test[k,1]
  jour <- data.test[k,2]
  df <- data.frame(temperature=temperture,temperature2=temperture^2,jour=jour,jour2=jour^2,jour3=jour^3)
  predict_value <- predict(model5,df,interval="prediction",level=0.95)
  prediction1[k] = round(predict_value[1],0)
}

# fichier des prédictions associées aux données "test"
write.table(prediction1,file="WANG_Yuteng_pred1.txt",row.names=F)

# fichier complet avec les prédicteurs
prediction2 <- data.frame(temperature=data.test$temperature,jour=data.test$jour,prediction=prediction1)
write.table(prediction2,file="WANG_Yuteng_pred2.txt",row.names=F)

