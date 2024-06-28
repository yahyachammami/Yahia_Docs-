#require(caschrono)
#require(fSeries)
library(tseries)
#library(caschrono)
library(forecast);
library(fpp);
library(ggplot2);
#library(xlsx);
# Chargement de séries temporelles
data(popfr)
#par(mfrow=c(2,1))
plot.ts(popfr,xlab='année',ylab='population')
plot.ts(uspop,xlab='année',ylab='population')

par(mfrow=c(2,1))
data(m30)
plot.ts(m30,xlab="année",ylab="nombre de morts",las=1)
polygon(c(1995,2005,2005,1995),c(340,340,910,910),lty=2)
deb=c(1995,1); fin=c(2004,12) # zoom
plot.ts(window(m30,start=deb,end=fin),xlab="année",ylab="nombre de morts")
plot.ts(log(m30))

#Importation de série temporelle 
Amtrak_data <- read.csv("C:/Users/farou/Desktop/Amtrak data.csv")
plot(Amtrak_data$Ridership)
#Transformation en série temporelle
ridership.ts <- ts(Amtrak_data$Ridership, start = c(1991,1),end=c(2003,12), freq = 12)

#Représentation graphique
plot(ridership.ts, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300), bty = "l")

#Zoom

ridership.ts.zoom <- window(ridership.ts, start = c(1997, 1), end = c(2000, 12))
plot(ridership.ts.zoom, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300), bty = "l")

# Estimation de la tendance 
ridership.lm <- tslm(ridership.ts ~ trend + I(trend^2)) # package forecast
summary(ridership.lm);

#Superposition de figures
par(mfrow = c(2, 1))
plot(ridership.ts, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300), bty = "l")
lines(ridership.lm$fitted, lwd = 2,color='red')
plot(ridership.lm$residuals)

#Estimation du modèle avec tendance et saisonnalité mensuelle
train.lm.trend.season <- tslm(ridership.ts ~ trend + I(trend^2 )+season)
summary(train.lm.trend.season)
par(mfrow=c(2,1))
acf(train.lm.trend.season$residuals)
pacf(train.lm.trend.season$residuals)
#Représentation graphique.

plot(ridership.ts)
lines(train.lm.trend.season$fitted,col=2)
plot(train.lm.trend.season$residuals)


# C'est le niveau basique 
# La phase de prévision sera abordée dans le module techniques de prévision (Période 4)



######################## Visualisation de la saisonnalité ###################

# 1er graphique :

ggseasonplot(ridership.ts, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: antidiabetic drug sales")
## ggseasonplot prend en considération la fréquence de la série 
#entrée comme paramètre avec le type ts de la série.
##

# 2ème graphique : 

ggseasonplot(ridership.ts, polar=TRUE) +
  ylab("$ million") +
  ggtitle("Polar seasonal plot: antidiabetic drug sales")

# 3ème graphique :

ggsubseriesplot(ridership.ts) +
  ylab("$ million") +
  ggtitle("Seasonal subseries plot: antidiabetic drug sales")

# 4ème graphique :

monthplot(ridership.ts)

#### Autrement :

plot(S,X,xlab="saisonnalité")



ridership.ts <- ts(Amtrak_data$Ridership, start = c(1991,1), end = c(2003, 12), freq = 12)
plot(ridership.ts, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300), bty = "l")

ridership.ts.zoom <- window(ridership.ts, start = c(1997, 1), end = c(2000, 12))
plot(ridership.ts.zoom, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300), bty = "l")

library(forecast)
ridership.lm <- tslm(ridership.ts ~ trend + I(trend^2)) 
summary(ridership.lm);

par(mfrow = c(2, 1))
plot(ridership.ts, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300), bty = "l")
lines(ridership.lm$fitted, lwd = 2)
plot(ridership.lm$residuals)

#Estimation du modèle avec tendance et saisonnalité mensuelle
train.lm.trend.season <- tslm(ridership.ts ~ trend + I(trend^2 )+season)
summary(train.lm.trend.season)

#Représentation graphique

plot(ridership.ts)
lines(train.lm.trend.season$fitted,col=2)
plot(train.lm.trend.season$residuals)

# Apprentissage 
nValid <- 12
nTrain <- length(ridership.ts) - nValid
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid))
# Apprentissage
train.lm.trend.season <- tslm(train.ts ~ trend + I(trend^2 )+season)
summary(train.lm.trend.season)
# Validation
ridership.lm.pred <- forecast(train.lm.trend.season, h = nValid, level = 0)
ridership.lm.pred
# Représentation graphique des résultats

plot(ridership.lm.pred, ylim = c(1300, 2600), ylab = "Ridership", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1)))
lines(ridership.lm$fitted, lwd = 2)
lines(valid.ts)
# Quantification de la performance du modèle
names(ridership.lm.pred)
accuracy(ridership.lm.pred$mean, valid.ts)

hist(ridership.lm.pred$residuals, ylab = "Frequency", xlab = "Forecast Error", bty = "l", main = "")

### Faire une fonction avec h (horizon de prévision) 
# et K (nombre des itérations)  variables

valid=function(periode,nbr){
  rmesmape=matrix(ncol = 2, nrow = nbr)
  colnames(rmesmape)=c("rmse","mape")
  for(i in seq_len(nbr)){
    nvalid=periode
    ntrain= length(ridership.ts)-nvalid*i
    train.ts=window(ridership.ts,start=c(1991,1),end=c(1991,ntrain))#train
    valid.ts=window(ridership.ts,start=c(1991,ntrain+1),end=c(1991,ntrain+nvalid))
    train.lm.train.season= tslm(train.ts~ trend+I(trend^2)+season)
    ridership.lm.pred=forecast(train.lm.train.season,h=nvalid,level=0)
    rmesmape[i,1]=accuracy(ridership.lm.pred,valid.ts)[2,2]
    rmesmape[i,2]=accuracy(ridership.lm.pred,valid.ts)[2,5]
    
    
  }
  
  
  return(rmesmape)
}

colonnes <- c(1, 2)  # Vous pouvez ajuster les indices des colonnes selon vos besoins

print(valid(1,12))
# Calculer la moyenne des valeurs pour les colonnes sélectionnées
moyenne_colonnes <- apply(valid(1,12)[, colonnes], 2, mean)
moyenne_colonnes


















#travail demandé : Reprendre la démarche pour la série AirPassengers (library(tseries))
plot(AirPassengers)
plot(don_log)
don_log=log(AirPassengers)
plot(don_log)
don_log.lm <- tslm(don_log ~ trend + season) 
nValid <- 12
nTrain <- length(don_log) - nValid-36
train.ts <- window(don_log, start = c(1952, 1), end = c(1952, nTrain))
valid.ts <- window(don_log, start = c(1952, nTrain + 1), end = c(1952, nTrain + nValid))

don_log.lm.trend.season <- tslm(train.ts ~ trend+season)
summary(don_log.lm.trend.season)
plot(don_log.lm.trend.season$residuals)
# Validation
don_log.lm.pred <- forecast(don_log.lm.trend.season, h = nValid, level = 0)
don_log.lm.pred

plot(don_log.lm.pred, ylim = c(4, 7), ylab = "AirPassengers", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1952,1960.25), main = "", flty = 2)
axis(1, at = seq(1952, 1960, 1), labels = format(seq(1952, 1960, 1)))
#lines(don_log.lm$fitted, lwd = 2)
lines(valid.ts)
# Quantification de la performance du modèle
names(don_log.lm.pred)
accuracy(exp(don_log.lm.pred$mean), exp(valid.ts))

Autoroute=read.table("http://freakonometrics.free.fr/autoroute.csv",header=TRUE,sep=";")
Autoroute


a7=Autoroute$a007
plot(ts(a7))

# exemples de séries saisonnières

aa=scan(system.file("/import/champagne_2001.txt",package="caschrono"))
champa=aa/1000; #expéditions en milliers de bouteilles
ytr=cbind(champa,log(champa))
colnames(ytr)=c("champagne","log(champagne)")
ytr.ts=ts(ytr,start=c(2001,1),frequency=12)
plot.ts(ytr.ts,xlab='temps',main='',oma.multi=c(4.5,3,.2,0),
          mar.multi=c(0,4,0,.5),las=0)

plot(LakeHuron)
LakeHuron
summary(mod1)
temps=time(LakeHuron)
mod1=lm(LakeHuron~temps)
mod1
summary(mod1)
resi.mco=residuals(mod1)
ychap=fitted(mod1)
v12=c(var(LakeHuron),var(resi.mco))

plot.ts(LakeHuron,las=1,ylab="niveau",xlab="année")
abline(coef=coef(mod1))
s=c(12,48,59,80);
segments(temps[s],ychap[s],temps[s],LakeHuron[s],lty=3,lwd=1)
y.gra=1.0009*par()$usr[3]
text(temps[1],y.gra,labels="obs.",cex=.8)
text(temps[s],rep(y.gra,length(s)),labels=s,cex=.8)


plot(as.vector(temps),resi.mco,type='l',xlab="année",ylab='résidu')
abline(h=0)
zero=rep(0,length(s))
segments(temps[s],zero,temps[s],resi.mco[s],lty=3,lwd=1)
y.gra=0.9*par()$usr[3]
text(temps[1],y.gra,labels="obs.",cex=.8)
text(temps[s],rep(y.gra,length(s)),labels=s,cex=.8)

n=length(resi.mco)
plot(resi.mco[-n],resi.mco[-1],xlab="résidu en t-1",asp=1,ylab='résidu en t')



# données tunisiennes
don_tues_mens=read.xlsx('C:/Users/Farou/OneDrive/Bureau/mensuelle_tues.xlsx',1)
don_blesses_mens=read.xlsx('C:/Users/Farou/OneDrive/Bureau/mensuelle_acc.xlsx',1)
nb_tues_ts=ts(don_tues_mens[1:240,2],start=c(2000,1),frequency=12)
nb_blesses_ts=ts(don_blesses_mens[1:240,2],start=c(2000,1),frequency=12)
nb_tues_ts
plot(nb_tues_ts)
monthplot(nb_tues_ts)
seasonplot(nb_tues_ts)



