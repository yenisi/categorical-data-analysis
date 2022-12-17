rm(list=ls())
set.seed(987654321)
n=100
x=rbinom(n,1,0.5);x
y=rbinom(n,1,0.4);y
lowF=sum(x*y);lowF

highM=sum(ifelse(x-y==0,1,0))-lowF;highM
highF=sum(ifelse(x-y==-1,1,0));highF
lowM=sum(ifelse(x-y==1,1,0));lowM
contingency.table=matrix(c(highM,highF,lowM,lowF),nrow=2,ncol=2,byrow=T);contingency.table
table(x,y)
oddsratio=(25/33)/(19/23);oddsratio

#Z=1
rm(list=ls())
set.seed(987654321)
n=100
x=rbinom(n,1,0.6)
y=rbinom(n,1,0.7)
conditional_Z1=table(x,y)
oddsratioZ1=(12*36)/(18*36);oddsratioZ1
#Z=0
x1=rbinom(n,1,0.05)
y1=rbinom(n,1,0.1)
conditional_Z0=table(x1,y1)

marginal=conditional_Z0+conditional_Z1;marginal




