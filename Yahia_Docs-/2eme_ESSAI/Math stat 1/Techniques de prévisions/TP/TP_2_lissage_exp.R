library(forecast)
library(tseries)

library(datasets)
plot(ts(Nile))
X=as.numeric(Nile)
recette=c(1293,1209,1205,1273,1220,1290,1243,1203,1390,1360,1353,1343,1364,1330,1377,1332)
# Lissage exponentiel simple : Yt=lambda Xt + (1-lambda)Yt-1
Lissage_simple=function(a,X){
    T=length(X)
    L=rep(NA,T)
    L[1]=X[1]
    for(t in 2:T){L[t]=a*X[t]+(1-a)*L[t-1]}
    return(L)
   }
plot(X,type="b",cex=.6)
plot(ts(X))
lines(Lissage_simple(.2,X),col="red")
lines(Lissage_simple(0.8,X),col="blue")
lines(Lissage_simple(0.1,X),col="green")

#Minimisation de l'erreur de prévision à 1 pas:
#Il est alors possible de voir le poids comme un 
#paramètre et on va alors chercher le poids optimal. 
#La stratégie classique est de minimiser l'erreur
#de prédiction commise à un horizon de 1

V=function(a,X){
  
    T=length(X)
    L=erreur=rep(NA,T)
    erreur[1]=0
    L[1]=X[1]
    for(t in 2:T){
      L[t]=a*X[t]+(1-a)*L[t-1]
      erreur[t]=X[t]-L[t-1] }
   return(sum(erreur^2))
   }

# Application :

#A=seq(0,1,by=.02)
#plot(A)
#Ax=Vectorize(V)(A)
#plot(A,Ax,ylim=c(min(Ax),min(Ax)*1.05))
optimize(V,c(0.01,0.9),recette)$minimum


# On notera 
#que c'est ce que suggère la fonction de R

hw=HoltWinters(X,beta=FALSE,gamma=FALSE,l.start=X[1])
hw
# Lissage exponentielle double :

Lissage_double=function(a,b,X){
    T=length(X)
    L=B=rep(NA,T)
    L[1]=X[1]; B[1]=0
    for(t in 2:T){
      L[t]=a*X[t]+(1-a)*(L[t-1]+B[t-1])
      B[t]=b*(L[t]-L[t-1])+(1-b)*B[t-1] }
   return(L)
}

#Série simulée (série mensuelle)
t=seq(1:120);
epsilon=rnorm(120);
X_sim = 0.5*t+3*cos(t*pi/6);
plot(ts(X_sim))

X_sim_bruite=X_sim+epsilon;
plot(ts(X_sim_bruite))

# Appliquer les fonctions de liss simple et double
ser_liss=Lissage_simple(0.2,X_sim_bruite)
plot(ts(X_sim_bruite))
lines(ser_liss,col="red")
#optimisation
X=X_sim_bruite;

hw=HoltWinters(X_sim_bruite,beta=FALSE,gamma=FALSE,l.start=X[1])
hw
ser_liss=ets(X_sim_bruite,model="ANN")
#optimize(V,c(0,.9))$minimum
#plot(ts(X))
#lines(Lissage_simple(0.5,X),col="red")
#lines(Lissage_simple(0.8,X),col="green")
ser_liss

# Fonction ets
ser_liss_simple <- ets(X_sim_bruite,model="ANN")
summary(ser_liss_ets)

forc_liss_simple=forecast.ets(ser_liss_simple,h=36)
plot(forc_liss_simple)
# lissage exponentiel double (comp additives)
ser_liss_double <- ets(X_sim_bruite,model="AAN")
summary(ser_liss_double)
forc_liss_double=forecast.ets(ser_liss_double,h=36)
plot(forc_liss_double)

# Holt Winter Filter (comp sais additive)
# s?rie simul?e

ser_liss_Hw <- ets(X_sim_bruite,model="AAA")
# ne marche pas !!! pourquoi ??
ser_liss_Hw <- ets(ts(X_sim_bruite,frequency=12),model="AAA")
summary(ser_liss_Hw)
forc_liss_Hw=forecast.ets(ser_liss_Hw,h=36)
plot(forc_liss_Hw)
# ets(X_sim_bruite,model="AAA",ic="aic")
#Application sur la série a10 ???
library(fpp)
plot(a10)
a10_liss_exp=ets(a10)
a10_liss_exp


# optimisation HW
serie=0.5*(1:100)+rnorm(100,0,1)+3*cos(pi/6*(1:100))
serie=ts(serie,start=c(1,1),end=c(9,4),frequency = 12)
LES=HoltWinters(serie)

LES
plot(serie,xlim=c(1,11),ylim=c(0,70))
p<-predict(LES,n.ahead=20)
lines(p,col=2)
# Comparaison de modèles de prévision sur la série AirPassengers
data_train=AirPassengers[1:length(AirPassengers)-24]


