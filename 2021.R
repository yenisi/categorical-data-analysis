rm(list=ls())
#3
set.seed=987654321
n=100
x=rbinom(n,1,0.5);x
y=rbinom(n,1,0.4);y
table(x,y)

#if Z=1
x1=rbinom(n,1,0.6);x1
y1=rbinom(n,1,0.7);y1

#if Z=0
x2=rbinom(n,1,0.05);x2
y2=rbinom(n,1,0.1);y2

#2
rm(list=ls())
set.seed(seed=987654321)
n=100
x=rnorm(n,0,1)
eta_x=-1.5+0.8*x;eta_x
p=pnorm(eta_x) #mu=p
Y=rbinom(n,1,p)
g=glm(Y~x, family=binomial(link="probit"));g

#Way1
#p=predict(g,type="response");p
#df=data.frame(x,p);df
#df=df[order(df$p),];df
#plot(df$x,df$p, type='l', col='red')


#Way2
beta0=g$coefficient[1]
beta=g$coefficient[2]
pi_hat=pnorm(beta*x+beta0)
df1=data.frame(x,pi_hat,Y)
df1=df1[order(df1$pi_hat),]
plot(cbind(df$x,df1$x),cbind(df$p,df1$pi_hat), type='l', col='red')

thrs=0.1
Y_hat=c(rep(1, sum(df1$pi_hat >thrs )),rep(0, sum(df1$pi_hat < thrs)))
t=table(Y_hat , df1$Y);t

misclassification=function(M)
{
 M[2,1]/sum(M[2,1]+M[1,1]) +M[1,2]/sum(M[2,2]+M[1,2])
}
#probability of misclassification
misclassification(t)

thrs2 = median(p); thrs2
Y_hat1 = c(rep(1, sum(p > thrs2)), rep(0, sum(p < thrs2)))
t1 = table(Y_hat1, Y); t1
misclassification(t1)

#1

rm(list=ls())
set.seed(seed=987654321)
R=1000
n=100
beta0 = -2.5
beta = 1.2
x = runif(n, 0, 1)
eta_x = beta0 + beta*x
p = 1/(1 + exp(-eta_x))
beta0.hat=array(0,R)
beta.hat=array(0,R)
LCI=array(0,R)
UCI=array(0,R)
emp_coverage=array(0,R)
for(i in 1:R)
{
Y=rbinom(n,1,p)
g=glm(Y~x, family=binomial(link="logit"))
beta.hat[i]=g$coefficient[2]
beta0.hat[i]=g$coefficient[1]

tau=qnorm(0.975);tau
}
#MLE
MLbeta=mean(beta.hat);MLbeta
MLbeta0=mean(beta0.hat);MLbeta0
#simulated SE
sim.se.beta.hat=sqrt(var(beta.hat));sim.se.beta.hat
sim.se.beta0.hat=sd(beta0.hat);sim.se.beta0.hat 
fisher=matrix( c(sum(p*(1-p)), sum(p*(1-p)*x), sum(p*(1-p)*x),sum(p*(1-p)*x^2)),ncol=2)
inv.fisher=solve(fisher)
#Actual Se
se.beta.hat=sqrt(inv.fisher[2,2]) #actualSE
se.beta0.hat=sqrt(inv.fisher[1,1]) #actualSE

lcl = beta.hat - qnorm(0.975)*sqrt(inv.fisher[2,2])
ucl = beta.hat + qnorm(0.975)*sqrt(inv.fisher[2,2])
coverage = mean(ifelse( beta> lcl & beta < ucl,1,0)); coverage







