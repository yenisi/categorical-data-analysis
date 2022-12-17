rm(list=ls())
set.seed(987654321)
n=c(20,50,100,500,1000)
R=1000
beta.hat=array(0,R)
se.beta.hat=array(0,R)
se.beta=array(0,R)
emp_bias=array(0,R)
emp_mse=array(0,R)
LCI=array(0,R)
UCI=array(0,R)
emp_coverage=array(0,R)
Beta=array(0,length(n))
se=array(0,length(n))
bias=array(0,length(n))
mse=array(0,length(n))
Uci=array(0,length(n))
Lci=array(0,length(n))
coverage=array(0,length(n))
for(j in 1:length(n))
{
for(i in 1:R)
{
x=runif(n[j],0,1);x
x.bar=mean(x);x.bar
alpha=-2
beta=1.5
mu=alpha+beta*x;mu
Y=rnorm(n[j],mu,2);Y
y.bar=mean(Y);y.bar
beta.hat[i]=(sum((x-x.bar)*(Y-y.bar)))/sum((x-x.bar)^2);beta.hat[i]
#alpha.hat=y.bar-(beta.hat*x.bar);alpha.hat
det=(n[j]*sum(x^2)/16)-((sum(x)^2)/16);det
Fisher=matrix(c(sum(x^2)/4,-sum(x)/4,-sum(x)/4,n[j]/4),nrow=2)/det;Fisher
se.beta.hat[i]=sqrt(Fisher[2,2]);se.beta.hat
se.beta[i]=sqrt(var(beta.hat))
emp_bias[i]=(beta.hat[i]-1.5);emp_bias[i]
emp_mse[i]=(beta.hat[i]-1.5)^2;emp_mse[i]
tau=qnorm(0.975);tau
LCI[i]=beta.hat[i]-tau*se.beta.hat[i];LCI
UCI[i]=beta.hat[i]+tau*se.beta.hat[i];UCI
emp_coverage[i]=(ifelse(LCI[i]<1.5 & 1.5<UCI[i],1,0))
}
Beta[j]=mean(beta.hat)
se[j]=mean(se.beta.hat)
bias[j]=mean(emp_bias)
mse[j]=mean(emp_mse)
Uci[j]=mean(UCI)
Lci[j]=mean(LCI)
coverage[j]=mean(emp_coverage)
}
level=c(0.048, 0.053, 0.041, 0.059, 0.047)
power=c(0.165, 0.245, 0.411, 0.944, 1.000)
A=matrix(c(n,Beta,se,bias,mse,Lci,Uci,coverage,level,power),ncol=10,nrow=5);A
colnames(A) <- c("n","Beta","SE","bias","MSE","Confidence","Interval","coverage","Level","Power")
A     
---------------------------------------------------------------------------------------------------
Power

rm(list=ls())
set.seed(987654321)
n=c(20,50,100,500,1000)
R=1000
beta.hat=array(0,R)
se.beta.hat=array(0,R)

test.stat=array(0,R)

power=array(0,length(n))
for(j in 1:length(n))
{
for(i in 1:R)
{
x=runif(n[j],0,1);x
x.bar=mean(x);x.bar
alpha=-2
beta=1
mu=alpha+beta*x;mu
Y=rnorm(n[j],mu,2);Y
y.bar=mean(Y);y.bar
beta.hat[i]=(sum((x-x.bar)*(Y-y.bar)))/sum((x-x.bar)^2);beta.hat[i]
#alpha.hat=y.bar-(beta.hat*x.bar);alpha.hat
det=(n[j]*sum(x^2)/16)-((sum(x)^2)/16);det
Fisher=matrix(c(sum(x^2)/4,-sum(x)/4,-sum(x)/4,n[j]/4),nrow=2)/det;Fisher
se.beta.hat[i]=sqrt(Fisher[2,2]);se.beta.hat

test.stat[i]=beta.hat[i]/se.beta.hat[i]

tau1=qnorm(0.95);tau1
}
power[j]=sum(ifelse(test.stat>tau1,1,0))/R
}

power
---------------------------------------------------------------------------------------------------
#Level
rm(list=ls())
set.seed(987654321)
n=c(20,50,100,500,1000)
R=1000
beta.hat=array(0,R)
se.beta.hat=array(0,R)

test.stat=array(0,R)

level=array(0,length(n))
for(j in 1:length(n))
{
for(i in 1:R)
{
x=runif(n[j],0,1);x
x.bar=mean(x);x.bar
alpha=-2
beta=0
mu=alpha+beta*x;mu
Y=rnorm(n[j],mu,2);Y
y.bar=mean(Y);y.bar
beta.hat[i]=(sum((x-x.bar)*(Y-y.bar)))/sum((x-x.bar)^2);beta.hat[i]
#alpha.hat=y.bar-(beta.hat*x.bar);alpha.hat
det=(n[j]*sum(x^2)/16)-((sum(x)^2)/16);det
Fisher=matrix(c(sum(x^2)/4,-sum(x)/4,-sum(x)/4,n[j]/4),nrow=2)/det;Fisher
se.beta.hat[i]=sqrt(Fisher[2,2]);se.beta.hat

test.stat[i]=beta.hat[i]/se.beta.hat[i]

tau1=qnorm(0.95);tau1
}
level[j]=sum(ifelse(test.stat>tau1,1,0))/R
}

level


