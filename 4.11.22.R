rm(list=ls())
setwd("C:/Users/YENISI/OneDrive/Documents/R/Categorical Data Analysis")
data=read.csv("lbw_data.csv");data
attach(data)
Y=data$low;Y
X1=data$age;X1

#i
M1 = glm(Y~age+as.factor(race)+smoke,family=binomial(link="logit"));M1

#ii
piL = exp(M1$coefficients[1]+M1$coefficients[2]*age+M1$coefficients[3]*race+M1$coefficients[4]*smoke)/(1+exp(M1$coefficients[1]+M1$coefficients[2]*age+M1$coefficients[3]*race+M1$coefficients[4]*smoke))
d1=data[data$smoke==1 & data$race==3,];d1
X1=unique(d1$age);X1
predicted=piL[c(50,57,65,68,72,73,86,131,156,160,184,187)];predicted
plot(d1$age,predicted,type='l')

#iii
data2=cbind(piL,data)
data0=data2[smoke==0,];data0
data1=data2[smoke==1,];data1
mean0=mean(data0$piL);mean0
mean1=mean(data1$piL);mean1
dispersion.0=sqrt(var(data0$piL));dispersion.0
dispersion.1=sqrt(var(data1$piL));dispersion.1
 
#iv & #v
cutpoint=c(mean(piL),median(piL));cutpoint
w=array(0,2)
TPR=array(0,2)
FPR=array(0,2)
for(i in 1:2)
{
Y.hat=ifelse(piL>cutpoint[i],1,0)
t=as.vector(table(low,Y.hat))
TPR[i]=t[4]/(t[4]+t[2]);TPR[i]
FPR[i]=t[3]/(t[3]+t[1]);FPR[i]
w[i]=TPR[i]*(1-FPR[i])
}
Y1hat=ifelse(piL>cutpoint[1],1,0)
Y2hat=ifelse(piL>cutpoint[2],1,0)
table(low,Y1hat)
table(low,Y2hat)
w
1-TPR
FPR
max(w)
