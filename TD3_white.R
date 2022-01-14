library(ggplot2)
library(car) # Pour les VIF
library(leaps) # Pour la sélection automatique de modèles
library(lmtest) # Pour le test de Breusch-Pagan

white_wine <- read.csv("wineQualityWhites.csv", header=TRUE, sep=",", dec=".")


reg_multi <- lm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+
                  free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol,data=white_wine)
summary(reg_multi)

reg_multi <- lm(quality~fixed.acidity+volatile.acidity+residual.sugar+chlorides+
                  free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol,data=white_wine)
summary(reg_multi)

reg_multi <- lm(quality~fixed.acidity+volatile.acidity+residual.sugar+
                  free.sulfur.dioxide+density+pH+sulphates+alcohol,data=white_wine)
summary(reg_multi)

# Question 7

alpha <- 0.05

n <- dim(white_wine)[1]
p <- 4

analyses <- data.frame(obs=1:n)

analyses$levier <- hat(model.matrix(reg_multi))
seuil_levier <- 2*p/n

ggplot(data=analyses,aes(x=obs,y=levier))+
  geom_bar(stat="identity",fill="steelblue")+
  geom_hline(yintercept=seuil_levier,col="red")+
  theme_minimal()+
  xlab("Observation")+
  ylab("Leviers")+
  scale_x_continuous(breaks=seq(0,n,by=5))

idl <- abs(analyses$levier)>seuil_levier
analyses$levier[idl]

analyses$rstudent <- rstudent(reg_multi)
seuil_rstudent <- qt(1-alpha/2,n-p-1)

ggplot(data=analyses,aes(x=obs,y=rstudent))+
  geom_bar(stat="identity",fill="steelblue")+
  geom_hline(yintercept=-seuil_rstudent,col="red")+
  geom_hline(yintercept=seuil_rstudent,col="red")+
  theme_minimal()+
  xlab("Observation")+
  ylab("Résidus studentisés")+
  scale_x_continuous(breaks=seq(0,n,by=5))

ids <- abs(analyses$rstudent)>seuil_rstudent
analyses$rstudent[ids]

influence <- influence.measures(reg_multi)
names(influence)
colnames(influence$infmat)
analyses$dcook <- influence$infmat[,"cook.d"]
seuil_dcook <- 4/(n-p)

ggplot(data=analyses,aes(x=obs,y=dcook))+
  geom_bar(stat="identity",fill="steelblue")+
  geom_hline(yintercept=seuil_dcook,col="red")+
  theme_minimal()+
  xlab("Observation")+
  ylab("Distance de Cook")+
  scale_x_continuous(breaks=seq(0,n,by=5))

idc <- analyses$dcook>seuil_dcook
analyses$dcook[idc]

# Question 8-1

vif(reg_multi)

# Question 9

reg_null <- lm(quality~1,data=white_wine)
reg_tot <- lm(quality~fixed.acidity+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+alcohol,data=white_wine)

reg_backward <- step(reg_tot,direction="backward")

reg_forward <- step(reg_null,scope=formula(reg_tot),direction="forward")

reg_backward <- regsubsets(quality~fixed.acidity+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+alcohol,data=white_wine,method="backward")
summary(reg_backward)
plot(reg_backward)
plot(reg_backward,scale="bic")
plot(reg_backward,scale="Cp")

reg_forward <- regsubsets(quality~fixed.acidity+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+alcohol,data=white_wine,method="forward")
summary(reg_forward)
plot(reg_forward)
plot(reg_forward,scale="bic")
plot(reg_forward,scale="Cp")

# Question 10

select <- sample(1:n,round(n*0.8))
white_wine_apprent <- white_wine[select,]
white_wine_test <- white_wine[-select,]
reg_multi_apprent <- lm(quality~fixed.acidity+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+alcohol,data=white_wine_apprent)
summary(reg_multi_apprent)
white_wine_test$quality_prev <- predict(reg_multi_apprent,white_wine_test)
white_wine_test$err_prev <- white_wine_test$quality-white_wine_test$quality_prev

rmse <- sqrt(mean((white_wine_test$err_prev)^2))
round(rmse,digits=2)

mape <- mean(abs(1-white_wine_test$quality_prev/white_wine_test$quality))*100
round(mape,digits=2)

# Question 11

select <- sample(1:n,round(n*0.2))
white_wine_apprent <- white_wine[-select,]
white_wine_test <- white_wine[select,]
reg_multi_apprent <- lm(quality~fixed.acidity+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+alcohol,data=white_wine_apprent)
summary(reg_multi_apprent)
white_wine_test$quality_prev <- predict(reg_multi_apprent,white_wine_test)
white_wine_test$err_prev <- white_wine_test$quality-white_wine_test$quality_prev

rmse <- sqrt(mean((white_wine_test$err_prev)^2))
round(rmse,digits=2)

mape <- mean(abs(1-white_wine_test$quality_prev/white_wine_test$quality))*100
round(mape,digits=2)

# Question 12

nbloc <- 5
bloc <- sample(rep(1:nbloc,length.out=n))

cv <- data.frame(Bloc=numeric(),RMSE=numeric(),MAPE=numeric())

for(i in 1:nbloc){
  cv[i,"Bloc"] <- i
  
  white_wine_apprent <- white_wine[bloc!=i,]
  white_wine_test <- white_wine[bloc==i,]
  
  reg_apprent <- lm(quality~fixed.acidity+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+alcohol,data=white_wine_apprent)
  
  quality_prev <- predict(reg_apprent,white_wine_test)
  
  cv[i,"RMSE"] <- sqrt(mean((white_wine_test$quality-quality_prev)^2))
  cv[i,"MAPE"] <- mean(abs(1-quality_prev/white_wine_test$quality))*100
}

print(paste("RMSE CV :",round(mean(cv$RMSE),digits=2)))
print(paste("MAPE CV :",round(mean(cv$MAPE),digits=2)))

ggplot()+
  geom_bar(data=cv,aes(x=Bloc,y=RMSE),stat="identity",fill="steelblue")+
  geom_hline(aes(yintercept=mean(cv$RMSE),colour="RMSE CV"),lwd=1)+
  labs(x="Bloc",y="RMSE",title="Validation croisée")+
  scale_colour_manual("",values="red")

ggplot()+
  geom_bar(data=cv,aes(x=Bloc,y=MAPE),stat="identity",fill="steelblue")+
  geom_hline(aes(yintercept=mean(cv$MAPE),colour="MAPE CV"),lwd=1)+
  labs(x="Bloc",y="MAPE (%)",title="Validation croisée")+
  scale_colour_manual("",values="red")

# ON ENLEVE LE TROUDUCU QUI SKEW LA DATA

skewers <- idl|ids
skewers
white_wine<-white_wine[!(skewers),]

reg_multi <- lm(quality~fixed.acidity+volatile.acidity+residual.sugar+
                  free.sulfur.dioxide+density+pH+sulphates+alcohol,data=white_wine)
summary(reg_multi)

# Question 7

alpha <- 0.05

n <- dim(white_wine)[1]
p <- 4

analyses <- data.frame(obs=1:n)

analyses$levier <- hat(model.matrix(reg_multi))
seuil_levier <- 2*p/n

ggplot(data=analyses,aes(x=obs,y=levier))+
  geom_bar(stat="identity",fill="steelblue")+
  geom_hline(yintercept=seuil_levier,col="red")+
  theme_minimal()+
  xlab("Observation")+
  ylab("Leviers")+
  scale_x_continuous(breaks=seq(0,n,by=5))

idl <- abs(analyses$levier)>seuil_levier
analyses$levier[idl]

analyses$rstudent <- rstudent(reg_multi)
seuil_rstudent <- qt(1-alpha/2,n-p-1)

ggplot(data=analyses,aes(x=obs,y=rstudent))+
  geom_bar(stat="identity",fill="steelblue")+
  geom_hline(yintercept=-seuil_rstudent,col="red")+
  geom_hline(yintercept=seuil_rstudent,col="red")+
  theme_minimal()+
  xlab("Observation")+
  ylab("Résidus studentisés")+
  scale_x_continuous(breaks=seq(0,n,by=5))

ids <- abs(analyses$rstudent)>seuil_rstudent
analyses$rstudent[ids]

influence <- influence.measures(reg_multi)
names(influence)
colnames(influence$infmat)
analyses$dcook <- influence$infmat[,"cook.d"]
seuil_dcook <- 4/(n-p)

ggplot(data=analyses,aes(x=obs,y=dcook))+
  geom_bar(stat="identity",fill="steelblue")+
  geom_hline(yintercept=seuil_dcook,col="red")+
  theme_minimal()+
  xlab("Observation")+
  ylab("Distance de Cook")+
  scale_x_continuous(breaks=seq(0,n,by=5))

idc <- analyses$dcook>seuil_dcook
analyses$dcook[idc]

# Question 8

vif(reg_multi)

# Question 9

reg_null <- lm(quality~1,data=white_wine)
reg_tot <- lm(quality~fixed.acidity+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+alcohol,data=white_wine)

reg_backward <- step(reg_tot,direction="backward")

reg_forward <- step(reg_null,scope=formula(reg_tot),direction="forward")

reg_backward <- regsubsets(quality~fixed.acidity+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+alcohol,data=white_wine,method="backward")
summary(reg_backward)
plot(reg_backward)
plot(reg_backward,scale="bic")
plot(reg_backward,scale="Cp")

reg_forward <- regsubsets(quality~fixed.acidity+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+alcohol,data=white_wine,method="forward")
summary(reg_forward)
plot(reg_forward)
plot(reg_forward,scale="bic")
plot(reg_forward,scale="Cp")

# Question 10

select <- sample(1:n,round(n*0.8))
white_wine_apprent <- white_wine[select,]
white_wine_test <- white_wine[-select,]
reg_multi_apprent <- lm(quality~fixed.acidity+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+alcohol,data=white_wine_apprent)
summary(reg_multi_apprent)
white_wine_test$quality_prev <- predict(reg_multi_apprent,white_wine_test)
white_wine_test$err_prev <- white_wine_test$quality-white_wine_test$quality_prev

rmse <- sqrt(mean((white_wine_test$err_prev)^2))
round(rmse,digits=2)

mape <- mean(abs(1-white_wine_test$quality_prev/white_wine_test$quality))*100
round(mape,digits=2)

# Question 11

select <- sample(1:n,round(n*0.2))
white_wine_apprent <- white_wine[-select,]
white_wine_test <- white_wine[select,]
reg_multi_apprent <- lm(quality~fixed.acidity+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+alcohol,data=white_wine_apprent)
summary(reg_multi_apprent)
white_wine_test$quality_prev <- predict(reg_multi_apprent,white_wine_test)
white_wine_test$err_prev <- white_wine_test$quality-white_wine_test$quality_prev

rmse <- sqrt(mean((white_wine_test$err_prev)^2))
round(rmse,digits=2)

mape <- mean(abs(1-white_wine_test$quality_prev/white_wine_test$quality))*100
round(mape,digits=2)

# Question 12

nbloc <- 5
bloc <- sample(rep(1:nbloc,length.out=n))

cv <- data.frame(Bloc=numeric(),RMSE=numeric(),MAPE=numeric())

for(i in 1:nbloc){
  cv[i,"Bloc"] <- i
  
  white_wine_apprent <- white_wine[bloc!=i,]
  white_wine_test <- white_wine[bloc==i,]
  
  reg_apprent <- lm(quality~fixed.acidity+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+pH+sulphates+alcohol,data=white_wine_apprent)
  
  quality_prev <- predict(reg_apprent,white_wine_test)
  
  cv[i,"RMSE"] <- sqrt(mean((white_wine_test$quality-quality_prev)^2))
  cv[i,"MAPE"] <- mean(abs(1-quality_prev/white_wine_test$quality))*100
}

print(paste("RMSE CV :",round(mean(cv$RMSE),digits=2)))
print(paste("MAPE CV :",round(mean(cv$MAPE),digits=2)))

ggplot()+
  geom_bar(data=cv,aes(x=Bloc,y=RMSE),stat="identity",fill="steelblue")+
  geom_hline(aes(yintercept=mean(cv$RMSE),colour="RMSE CV"),lwd=1)+
  labs(x="Bloc",y="RMSE",title="Validation croisée")+
  scale_colour_manual("",values="red")

ggplot()+
  geom_bar(data=cv,aes(x=Bloc,y=MAPE),stat="identity",fill="steelblue")+
  geom_hline(aes(yintercept=mean(cv$MAPE),colour="MAPE CV"),lwd=1)+
  labs(x="Bloc",y="MAPE (%)",title="Validation croisée")+
  scale_colour_manual("",values="red")

