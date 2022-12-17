rm(list=ls())
setwd("C:/Users/YENISI/OneDrive/Documents/R/Categorical Data Analysis")
data=read.csv("lbw_data.csv")
attach(data)
Y=data$low;Y
data$race=as.factor(race)
#i
M1 = glm(Y~age+race+smoke,data=data,family=binomial(link="logit"));M1
p=predict.glm(M1,type='response')
data=cbind(data,p)
#ii
#piL = exp(M1$coefficients[1]+M1$coefficients[2]*age+M1$coefficients[3]*race+M1$coefficients[4]*smoke)/(1+exp(M1$coefficients[1]+M1$coefficients[2]*age+M1$coefficients[3]*race+M1$coefficients[4]*smoke));piL
#p=predict.glm(g,type='response')
d1=data[data$smoke==1 & data$race==3,];d1
d1=d1[!duplicated(d1$age),];d1
d1=d1[order(d1$age),];d1
plot(d1$age,d1$p,type='l')

#iii
#data2=cbind(piL,data)
data0=data[smoke==0,];data0
data1=data[smoke==1,];data1
mean0=mean(data0$p);mean0
mean1=mean(data1$p);mean1
dispersion.0=sqrt(var(data0$p));dispersion.0
dispersion.1=sqrt(var(data1$p));dispersion.1
 
#iv & #v
cutpoint=c(mean(piL),median(piL));cutpoint
w=array(0,2)
TPR=array(0,2)
FPR=array(0,2)
for(i in 1:2)
{
Y.hat=ifelse(piL>cutpoint[i],1,0)
print(table(low,Y.hat))
t=as.vector(table(low,Y.hat))
TPR[i]=t[4]/(t[4]+t[2]);TPR[i]
FPR[i]=t[3]/(t[3]+t[1]);FPR[i]
w[i]=TPR[i]*(1-FPR[i])
}
w
1-TPR
FPR
max(w)
