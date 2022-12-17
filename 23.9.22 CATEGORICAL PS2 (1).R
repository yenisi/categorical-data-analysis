rm(list=ls())
setwd("C:/Users/Lab4_39/Desktop")
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
#iii
y=data$literacy;y
x=data$age;x
m2=glm(y~x,binomial(link="probit"))
Beta0=1.1636
Beta=-0.0256
pi_hat= pnorm(Beta0+Beta*x)
?pnorm
deviance(m2)

par(mfrow=c(1,2))
{
plot(sort(unique(x)),unique(pi.hat),type='l')
plot(unique(x),unique(pi_hat),type='l')
}


#
Data = data.frame(X,piL,piP);Data
D1 = Data[order(Data$X, decreasing = F),]
par(mfrow = c(1,2))
plot(D1$X,D1$piL,type='l',lwd = 2,main = "Logistic Regression", xlab = "Age", ylab = "pi")
plot(D1$X,D1$piP,type='l',lwd = 2, main = "Probit Regression", xlab = "Age", ylab = "pi")
?plot


