library(stats)
library(maxLik)
library(plm)
library(truncnorm)
#x <- runif(1000,50,120)
#epsilon=rnorm(1000,0,1)
#u=rexp(1000,0.5)

#ub<-rtruncnorm(1000,1.5)
## log likelihood function.
## Note: 'param' is a 2-vector c(mu, sd)
#llf <- function(param) {
 # alpha<- param[1]
  #beta <- param[2]
  #llValue <- alpha+beta*log(dunif(x,50,120))+dnorm(epsilon, mean=0, sd=1.5)+dexp(u,0.5)  ####A CHANGER!!!!!!!
  #sum(llValue)
#}
## Estimate it with mu=0, sd=1 as start values
#ml <- maxLik(llf, start = c(alpha=0.1, beta=0.1) )
#print(summary(ml))
#ml$estimate
#1 




#2
#A
matrixQ<-data.frame()
Q<-c()
for (i in 1:50) {
  Q<-runif(n=20,min=50,max=120)
  matrixQ<-rbind(matrixQ,Q)
}
colnames(matrixQ)<-c(1:20)
matrixQ<-as.matrix(matrixQ)



matrixE<-data.frame()
E<-c()
for (i in 1:50) {
  E<-rnorm(n=20,mean=0,sd=1.5)
  matrixE<-rbind(matrixE,E)
}
colnames(matrixE)<-c(1:20)
matrixE<-as.matrix(matrixE)



matrixU<-data.frame()
matrixUb<-data.frame()
ub<-rtruncnorm(20,1.5)
U<-rexp(20,rate=1.5)
for (i in 1:50) {
  matrixU<-rbind(matrixU,U)
  matrixUb<-rbind(matrixUb,ub)
  
}
colnames(matrixU)<-c(1:20)
matrixU<-as.matrix(matrixU)
colnames(matrixUb)<-c(1:20)
matrixUb<-as.matrix(matrixUb)


MatrixC<-0.1+0.8*log(matrixQ)+matrixE+matrixU
MatrixCb<-0.1+0.8*log(matrixQ)+matrixE+matrixUb




##3
indexi<-c()
indext<-c()
logc<-c()
logq<-c()
logcb<-c()
for (i in 1:20){
  for(t in 1:50){
    indexi<-append(indexi, i)
    indext<-append(indext,t)
    logc<-append(logc,MatrixC[t,i])
    logcb<-append(logcb,MatrixCb[t,i])
    logq<-append(logq,log(matrixQ[t,i]))
  }
}
panel=data.frame(indexi,indext,logc,logq,logcb)



fixed<-plm(logc~logq,data=panel,index=c("indexi","indext"),model="random")

summary(fixed)


plot(density(panel$logc))


fixedb<-plm(logcb~logq,data=panel,index=c("indexi","indext"),model="random")

alphab<-append(alphab,fixedb$coefficients[1])
betab<-append(betab,fixedb$coefficients[2])
summary(fixedb)
plot(density(panel$logcb))
set.seed(NULL)
alpha<-c()
beta<-c()
alphab<-c()
betab<-c()
for (simulation in 1:1000){
 
  matrixQ<-data.frame()
  Q<-c()
  for (i in 1:50) {
    Q<-runif(n=20,min=50,max=120)
    matrixQ<-rbind(matrixQ,Q)
  }
  colnames(matrixQ)<-c(1:20)
  matrixQ<-as.matrix(matrixQ)
  
  
  
  matrixE<-data.frame()
  E<-c()
  for (i in 1:50) {
    E<-rnorm(n=20,mean=0,sd=1.5)
    matrixE<-rbind(matrixE,E)
  }
  colnames(matrixE)<-c(1:20)
  matrixE<-as.matrix(matrixE)
  
  
  
  matrixU<-data.frame()
  matrixUb<-data.frame()
  ub<-rtruncnorm(20,1.5)
  U<-rexp(20,rate=1.5)
  for (i in 1:50) {
    matrixU<-rbind(matrixU,U)
    matrixUb<-rbind(matrixUb,ub)
    
  }
  colnames(matrixU)<-c(1:20)
  matrixU<-as.matrix(matrixU)
  colnames(matrixUb)<-c(1:20)
  matrixUb<-as.matrix(matrixUb)
  
  
  MatrixC<-0.1+0.8*log(matrixQ)+matrixE+matrixU
  MatrixCb<-0.1+0.8*log(matrixQ)+matrixE+matrixUb
  
  indexi<-c()
  indext<-c()
  logc<-c()
  logq<-c()
  logcb<-c()
  for (i in 1:20){
    for(t in 1:50){
      indexi<-append(indexi, i)
      indext<-append(indext,t)
      logc<-append(logc,MatrixC[t,i])
      logcb<-append(logcb,MatrixCb[t,i])
      logq<-append(logq,log(matrixQ[t,i]))
    }
  }
  panel=data.frame(indexi,indext,logc,logq,logcb)
  fixed<-plm(logc~logq,data=panel,index=c("indexi","indext"),model="random")
  alpha<-append(alpha,fixed$coefficients[1])
  beta<-append(beta,fixed$coefficients[2])
  fixedb<-plm(logcb~logq,data=panel,index=c("indexi","indext"),model="random")
  alphab<-append(alphab,fixedb$coefficients[1])
  betab<-append(betab,fixedb$coefficients[2])
}


simulation=data.frame(alpha,beta,alphab,betab)

library(plotly)
fig <- plot_ly(y = ~alpha, type = "box")
fig <- fig %>% add_trace(y = ~beta)

fig
library(plotly)

density1 <- density(alpha)
density2<-density(alphab)
fig <- plot_ly(x = ~density1$x, y = ~density1$y, type = 'scatter', mode = 'lines', name = 'alpha 1 er configuration', fill = 'tozeroy')
fig <- fig %>% add_trace(x = ~density2$x, y = ~density2$y, name = 'alpha 2eme configuration', fill = 'tozeroy')
fig <- fig %>% layout(xaxis = list(title = 'Alpha'),
                      yaxis = list(title = 'Density'))

fig


