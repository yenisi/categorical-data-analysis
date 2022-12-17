rm(list = ls())

df = read.csv("E:/lbw_data.csv")
df = read.csv("C:/Users/LAB2 33/Downloads/lbw_data.csv")
View(df)

head(df, 2)

df$race = as.factor(df$race)
#df$smoke = as.factor(df$smoke)


# (i) :---
# fitting glm :-
g = glm(low ~ age + race + smoke, data = df,
        family = binomial(link = 'logit'))
g
summary(g)
deviance(g)
p = predict.glm(g, type = 'response')
df = cbind(df, p)



# (ii) :---
df1 = df[df$race == 3 & df$smoke == 1,]
View(df1)
df1 = df1[!duplicated(df1$age),]
df1 = df1[order(df1$age),]
plot(df1$age, df1$p, type = 'l')



# (iii) :---
p1 = df[df$smoke == 0,]$p
p2 = df[df$smoke == 1,]$p

mean(p1)
mean(p2)

sd(p1)
sd(p2)

Comment : janina.

# (iv) :---
## case1 : (cut point = mean)
p1_cut = mean(p); p1_cut

Y1_pred = ifelse(df$p >= p1_cut, 1, 0)

table(Y1_pred, 'Y_obs' = df$low)
61/130 + 16/59	# probability of miss classification

## case2 : (cut point = median)
p2_cut = median(p); p2_cut

Y2_pred = ifelse(df$p >= p2_cut, 1, 0)

table(Y2_pred, 'Y_obs' = df$low)
59/130 + 23/59

# probability pf miss classification is higher if we consider
# median as our cut point.


# modifying the data frame for the race factor :-
View(df)
df$z1 = ifelse(df$race == 1, 1, 0)
df$z2 = ifelse(df$race == 2, 1, 0)
head(df,2)

g1 = glm(low ~ age + smoke + z1 + z2, 
         data = df, family = binomial(link = 'logit'))
summary(g1)















