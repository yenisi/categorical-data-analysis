rm(list=ls())
setwd("C:/Users/Lab4_51/Documents")
data=na.omit(read.csv(file="literacy.csv"));data
attach(data)

data$literacy=replace(data$literacy, which(data$literacy==2),0)
data$gender=replace(data$gender, which(data$gender ==2),0)
data
table(gender,literacy)

#ii
y=data$literacy;y
x=data$age;x
m1=glm(y~x,binomial(link="logit"))
beta0=1.91727
beta=-0.04242
pi.hat= exp(beta0+beta*x)/(1+exp(beta0+beta*x));pi.hat
deviance(m1)

#vi 
length(unique(pi.hat))
a=unique(pi.hat)
length(pi.hat)
yhat=0
yhata=0
for( i in 1:18716)
{
for ( j in 1:85)
{
yhat[i][j]=ifelse(pi.hat[i]>a[j],1,0)

}
yhata[j]=yhat[i][j]
A=as.matrix(table(yhata[j],data$literacy))
TPR[j]=A[2,2]/(A[2,2]+A[1,2]) 
FPR[j]=A[2,1]/(A[2,1]+A[1,1]) 
}
TPR=12044/12815;TPR
FPR=4859/5901;FPR
TPR*(1-FPR)

#iii
y=data$literacy;y
x=data$age;x
m2=glm(y~x,binomial(link="probit"))
Beta0=1.1636
Beta=-0.0256
pi_hat= pnorm(Beta0+Beta*x)
?pnorm
deviance(m2)

#vi

yhat2=0
for( i in 1:18716)
{
yhat2[i]=ifelse(pi.hat[i]>0.5,1,0)
}
yhat2
table(yhat2,data$literacy)

par(mfrow=c(1,2))
{
plot(sort(unique(x)),unique(pi.hat),type='l')
plot(unique(x),unique(pi_hat),type='l')
}

?approx

#
Data = data.frame(age,pi.hat,pi_hat);Data
D1 = Data[order(Data$age, decreasing = F),]
par(mfrow = c(1,2))
plot(D1$age,D1$pi.hat,type='l',lwd = 2,main = "Logistic Regression", xlab = "Age", ylab = "pi")
plot(D1$X,D1$pipi_hat,type='l',lwd = 2, main = "Probit Regression", xlab = "Age", ylab = "pi")

Confuse = function(Data1,p)
{
  Data1$Y_hat = ifelse(Data1$pi.hat>=p,1,0)
  T = table(Data1$Y_hat,Data1$literacy, dnn=c("Predicted","Response"))
  #print(T)
  return (c(T[4]/(T[3]+T[4]),T[2]/(T[1]+T[2]))) #returns c(TFR,FPR)
}


un = round(unique(D1$pi.hat),digits = 2);un
n = length(un)
TFR = rep(0,n)
FPR = rep(0,n)
for(i in 1:n)
{
A = Confuse(D1,un[i])
TFR[i] = A[1]
FPR[i] = A[2];

}
TFR
FPR
Criteria = TFR[-n]*(1-FPR[-n]);Criteria
pos = which(Criteria == max(Criteria));pos
par(mfrow = c(1,1))
Pi_Hat = un[pos];Pi_Hat #Pi_Hat used for max value of TPR*(1-FPR)
plot(un[-n],Criteria,type = 'l',xlab = "Pi Hat",ylab = "TPR*(1-FPR)",main = "Operating Curve")
Pi_Hat = un[pos];Pi_Hat
Data$Y_hat = ifelse(Data$piL>=Pi_Hat,1,0)
View(Data)

plot(FPR,TFR,type = 'l',main = "ROC Curve")


Confuse(Data,0.71)[1]*(1-Confuse(Data,0.71)[2])




