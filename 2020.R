rm(list = ls())

df = read.csv("C:/Users/YENISI/Downloads/2020.csv");df
attach(df)
df=na.omit(df)
# changing the variabel types :-
df$GENDER[df$GENDER == 2] = 0
df$LBXHBC[df$LBXHBC == 2] = 0
df$RACE = as.factor(df$RACE)
g=glm(LBXHBC~LBXTC+as.factor(RACE)+GENDER,data=df,family=binomial(link="logit"));g
deviance(g)

data=cbind(df, 'phat'=predict.glm(g,type='response'));data
head(data)
prob_plot = function(gen, race)
{
   df1 = df[df$GENDER == gen &
               df$RACE == race,]
   df1 = df1[!duplicated(df1$LBXTC),]
   df1 = df1[order(df1$LBXTC),]
   
   par(mar = c(6,10,6,8))
   plot(df1$LBXTC, df1$p_hat, type = 'l',
        xlab = 'LBXTC (mg/dL)',
        ylab = 'Predicted probabilities',
        col = 'red', las = 1, lwd = 2,
        main = paste('Gender:', 
                     ifelse(gen == 1, 'Male', 'Female'),'|',
                     'Race:', race), mgp = c(3.5,0.5,0),
        font.main = 7, font.lab = 7, font.axis = 2,
        cex.main = 2)
}
prob_plot(1,2)

#2
rm(list=ls())
set.seed(seed=987654321)
n=100
R=1000
beta.hat=array(0,R)
beta0.hat=array(0,R)
for(i in 1:R)
{
x=runif(n,-2,2)
eta_x=-1.5+0.8*x
p=pnorm(eta_x)
y=rbinom(n,1,p)
g=glm(y~x, family=binomial(link="logit"))
beta.hat[i]=g$coefficient[2]
beta0.hat[i]=g$coefficient[1]
}
mean(beta.hat)
mean(beta0.hat)

#3
rm(list=ls())
set.seed(seed=987654321)
n=100
R=1000
beta.hat=array(0,R)
beta0.hat=array(0,R)
for(i in 1:R)
{
x=runif(n,-2,2)
eta_x=-1.5+0.8*x+2.2*x^2
p=pnorm(eta_x)
y=rbinom(n,1,p)
g=glm(y~x, family=binomial(link="probit"))
beta.hat[i]=g$coefficient[2]
beta0.hat[i]=g$coefficient[1]
}
mean(beta.hat)
mean(beta0.hat)













