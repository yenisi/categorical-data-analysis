n = 100
x = rnorm(n,2,1)
y = rpois(n,1)
b0 = cov(x,y)/var(x);b0
b1 = mean(y) - b0*mean(x); b1

eta = b0+b1*x
mu = exp(eta)
z = eta + (y-mu)/mu

g_prime_mu = 1/mu
a_phi = 1
b_prime2_theta = mu
w = (a_phi*b_prime2_theta*(g_prime_mu^2))^(-1)



#===================================================
# Option 1 :
#glm(y~x, family = Binomial(link = 'probit'))



#problem 2:

rm(list = ls())
n = 100
R = 1000

b0 = -2
b = 1.2
x = runif(n)
eta = b0+b*x
Y = exp(eta)/(1+exp(eta)); Y


glm(Y~x,family = binomial(link = 'logit'))





rm(list = ls())
a = read.csv("D:/literacy_data_csv.csv")
a = read.csv("E:/literacy_data_csv.csv")
View(a)
is.data.frame(a)
summary(df)
df = na.omit(a)	#omitting NAs
View(df)

df[df == 2] = 0	# replacing 2 by 0
# do for specified columns 
#or,
df[df$literacy == 2 & df$gender == 2] = 0

is.null(df)	# checking for nulls

head(df)
nrow(df)

nrow(df)
nrow(a)	# earlier number of participants

# (i)
colnames(df) = c("Y","z","age")
attach(df)
table(Y,z)


g1 = glm(Y ~ age, data = df, family = binomial(link = 'logit')); g1
g2 = glm(Y ~ age, data = df, family = binomial(link = 'probit')); g2


b01 = g1$coefficients[1]; b01
b1 = -0.04242

b02 = 1.1636
b2 = -0.0256


mu1_hat = exp(b01+b1*age)/(1 + exp(b01+b1*age)); mu1_hat
mu2_hat = pnorm(b02+b2*age)

#or,

pi1 = predict(g1, type = 'response')
pi2 = predict(g2, type = 'response')



Dev1 = deviance(g1); Dev1
Dev2 = deviance(g2); Dev2


chisq1 = sum((Y-pi1)/sqrt(pi1*(1-pi1)))^2; chisq1
chisq2 = sum((Y-pi2)/sqrt(pi2*(1-pi2)))^2; chisq2



par(mar = c(6,10,6,6))
matplot(age, cbind(mu1_hat, mu2_hat), pch = 20,
	main = 'Regression plots for two models',
	xlab = 'Age', ylab = 'Pi_hat',
	las = 1, col = c(2,3), font.main = 7,
	font.lab = 7, font.axis = 2, cex.main = 2.2,
	cex.lab = 1.3)
legend(70,0.9, c("Logit link", "Probit link"),
	col = c(2,3), box.lty = 0, lty = 1,
	x.intersp = 0.1, lwd = 2, text.font = 2)


# taking distinct x values :
x = unique(age); x
pi1 = unique(mu1_hat)
pi2 = unique(mu2_hat)

d = data.frame(x,pi1,pi2)
d = d[order(x),]
head(d)

par(mfrow = c(1,2))
par(mar = c(3,5,3,3))
plot(d$x, d$pi1, type = 'l',
	main = 'Logit link',
	xlab = 'Age', ylab = 'Pi_hat',
	las = 1, col = 2, font.main = 7,
	font.lab = 7, font.axis = 2, cex.main = 2.2,
	cex.lab = 1.3, lwd = 2)
plot(d$x, d$pi2, type = 'l',
	main = 'Probit link',
	xlab = 'Age', ylab = 'Pi_hat',
	las = 1, col = 3, font.main = 7,
	font.lab = 7, font.axis = 2, cex.main = 2.2,
	cex.lab = 1.3, lwd = 2)






x = rnorm(100,5,1)
y = x + rnorm(100,0,0.2)
plot(x,y)

b = cov(x,y)/var(x); b
a = mean(y) - b*mean(x); a

f = function(p)
{
	c(sum(y-p[1]-p[2]*x), 
	   sum((y-p[1]-p[2]*x)*x))
}

xstart = c(a,b)
nleqslv::nleqslv(xstart, f)$x


#===========================================================
# Q2.

f = function(n, R)
{
   # parameters to generate sample : actually unknown
   beta0 = -2
   beta = 1.2
   
   # Necessary variables defining :-
   beta_hat = 0
   se_beta_ana = 0
   
   # function to return estimate of beta :-
   score_solve = function(x_val, y_val)
   {
      score_func = function(p)
      {
         c(sum(y_val - 1/(1 + exp(-(p[1] + p[2]*x_val)))),
           sum((y_val - 1/(1 + exp(-(p[1] + p[2]*x_val))))*x_val))
      }
      
      # starters :-
      b0 = cov(x_val, y_val)
      b1 = mean(y_val) - b0*mean(x_val)
      
      xstart = c(b0,b1)
      coefs = nleqslv::nleqslv(xstart, score_func)$x
      return(coefs[2])
   }
   
   
   for(i in 1:R)
   {
      # Generation of the data : (xi,Yi)
      x = runif(n,0,1)
      p_i = 1/(1 + exp(-(beta0 + beta*x)))
      Y = rbinom(n, 1, p_i)
      
      # Score equations :-
      # sum(yi - pi) = 0
      # sum(yi - pi)xi = 0
      
      beta_hat[i] = score_solve(x,Y)
      
      # Fisher information matrix :
      I11 = sum(p_i*(1-p_i))
      I12 = sum(p_i*(1-p_i)*x)
      I22 = sum(p_i*(1-p_i)*x^2)
      se_beta_ana[i] = sqrt(solve(matrix(c(I11,I12,I12,I22),
                                    byrow = T, ncol = 2))[2,2])
   }
   
   
   # Necessary calculations :-
   
   # MLE of beta :
   beta_hat_MLE = mean(beta_hat)
   
   # Standard error of beta :
   se_beta_sim = sd(beta_hat)*sqrt((R-1)/R)
   
   # Simulated bias :
   bias = mean(beta_hat - beta)
   
   # MSE of beta :
   MSE_beta = mean((beta_hat - beta)^2)
   
   # Coverage & CI :
   lcl = beta_hat - se_beta_ana*qnorm(0.975)
   ucl = beta_hat + se_beta_ana*qnorm(0.975)
   emp_cov = length(beta[beta > lcl & beta < ucl])/R
   CI = paste(round(mean(lcl),3),",",round(mean(ucl),3))
   
   return(c(round(c(beta_hat_MLE, bias,
            MSE_beta, se_beta_sim, mean(se_beta_ana)),5),
            CI, emp_cov))
}



n = c(100,500,1000)
R = 1000
M = matrix(ncol = 7, nrow = length(n))

set.seed(0)
for(i in 1:length(n))
   M[i,] = f(n[i], R)
M = as.data.frame(cbind(n,M))

colnames(M) = c("n", "MLE(beta)",
                "bias", "MSE","Simulated SE",
                "Analytical SE",
                "  CI of beta  ", "Emp coverage")
rownames(M) = paste(1:length(n))

M


#===============================================================================
rm(list = ls())

# Loading the data 
d = read.csv("E:/literacy_data_csv.csv")
View(d)
sum(is.null(d))
sum(is.na(d))
d = na.omit(d)


# summary of age 
summary(d$age)

# Re-coding the necessary columns :
sum(d$age == 2)   # no individual is having age 2
d[d == 2] = 0

table(d$literacy)
table(d$gender)
table(d$literacy, d$gender)

# Extracting & renaming necessary columns
df = data.frame('age' = d$age, 'Y' = d$literacy)
table(df$age)




View(df)
head(df, 5)
tail(df, 5)

# plotting the raw data :
par(mar = c(6,10,6,8))
plot(df$age, df$l_status, 
     main = "Plot of Literacy status",
     xlab = 'Age', ylab = 'Literacy status',
     pch = 20, col = 'red', las = 1)


# Fitting GLM :
g = glm(Y ~ age, data = df,
        family = binomial(link = 'probit'))
summary(g)
deviance(g)


b0 = g$coefficients[1]; b0
b = g$coefficients[2]; b

# predicted probabilities :
pi_hat = predict.glm(g, type = 'response'); pi_hat
length(unique(pi_hat))



# plot of the probabilities
x = sort(unique(df$age)); x
p = exp(b0+b*x)/(1+exp(b0+b*x))
p = pnorm(b0 + b*x)


plot(x,p, type = 'l',
     main = 'Regression plot',
     xlab = 'Age', ylab = expression(paste(pi)),
     las = 1, col = 'red', font.main = 7,
     font.lab = 7, font.axis = 2, cex.main = 2.2,
     cex.lab = 1.3, lwd = 2)


# Classification :---

# some necessary changes and oservations :-
{
df = cbind(df, pi_hat)
nrow(df)
df$pi_hat = round(df$pi_hat, 2)
p_unique = unique(df$pi_hat)
View(df)
length(p_unique)
}




# function to determine the optimum cut point :-
# (corrected form)

opt_func = function(d)   # d = input dataframe, containing Y, x, pi_hat.
{
  d = d[order(d$pi_hat, decreasing = T),]  # ordering the dataframe wrt estimeted probabilities
  p_hat_unq = unique(d$pi_hat)	# storing the unique values of pi_hats
  n = length(p_hat_unq)
  tpr_fpr = matrix(ncol = 3, nrow = n)  # matrix to store TPR, FPR, TPR(1-FPR)
  
  confmat = function(Y_predicted, Y_obs)	# function for confusion matrix
  {
    t = table(Y_predicted, Y_obs)
    if(nrow(t) == 1)
      t = rbind(c(0,0), t)
     
    tpr = t[2,2]/(t[1,2] + t[2,2])
    fpr = t[2,1]/(t[1,1] + t[2,1])
    return(c(tpr, fpr, tpr*(1-fpr)))
  }

  
  for(i in 1:n)
  {
    thrs = p_hat_unq[i]	# storing the threshold
    Y_pred = ifelse(d$pi_hat >= thrs, 1, 0)
    
    # TPR and FPR :-
    tpr_fpr[i,] = confmat(Y_pred, d$Y)
  }
  
  d1 = cbind(p_hat_unq, tpr_fpr)
  colnames(d1) = c('pi_hat', 'TPR', 'FPR', 'TPR(1-FPR)')
  opt_row = d1[which.max(d1[,4]),]
  pi_hat_opt = as.numeric(opt_row[1])
  tpfpr = opt_row[4]
  
  return(list('df' = d1, 'pi_hat' = pi_hat_opt,
		'TPR(1-FPR)' = tpfpr))
}

df1 = opt_func(df)

# optimum cut point :-
paste('The optimum cut point is given as :', df1$pi_hat)

# optimum confusion matrix :-
Y_pred_opt = ifelse(df$pi_hat >= df1$pi_hat, 1, 0)
table(Y_pred_opt, 'Y_obs' = df$Y)

# ROC curve :-
fpr = (df1$df)[,'FPR']
tpr = (df1$df)[,'TPR']

par(mar = c(6,10,6,8))
plot(fpr, tpr, type = 'l', col = 'red',
	main = 'ROC curve', xlab = 'FPR', ylab = 'TPR',
	font.main = 7, font.lab = 7, font.axis = 2,
	cex.main = 2.2, cex.lab = 1.2, lwd = 2, las = 1,
	mgp = c(3.5,0.5,0))

#====================================================================================
#====================================================================================
#====================================================================================


# The following we have to write in exam :-
# from the different confusion matrices we have obtained the TPR and FPR values and 
# it is maximum when pi = 0.71
# hence we finally classify the data based on the cut point 0.71, using this cut point,
# the table confusion matrix is given below.


attach(df)
Y_pred = ifelse(pi_hat > 0.5, 1, 0)
t = table(Y_pred, Y); t
detach(df)

# TPR and FPR :-
TPR = t[2,2]/(t[1,2] + t[2,2]); TPR
FPR = t[2,1]/(t[1,1] + t[2,1]); FPR
TPR*(1-FPR)


# TPR and FPR are coming same in both logit and probit,
# since, the two model are giving almost same fit.

#============================================================
# Trial in own dataset for classification :-

rm(list = ls())
set.seed(1)

n = 100
beta0 = 2
beta = 1.2

x = rnorm(n, 2, 4)
p_i = 1/(1 + exp(-(beta0 + beta*x)))
Y = rbinom(n, 1, p_i)
df = data.frame(Y,x)
head(df,2)
# data generation complete.

# plot of the given data :-
par(mar = c(6,10,6,8))
plot(x,Y, pch = 20, col = 'red',
     main = 'Plot of the given data',
     xlab = "values of x", ylab = 'Binary response',
     cex.main = 2, cex.lab = 1.2, las = 1, 
     font.main = 7, font.lab = 7, font.axis = 2,
     mgp = c(3,0.5,0))


# fitting model :
gl = glm(Y~x, family = binomial(link = 'logit')); gl
summary(gl)
deviance(gl)

# obtaining pi_i_hats :-
pi_hat = predict.glm(gl, type = 'response')
df = cbind(df, pi_hat)


# plotting the fitted with observes data :-
ft = function(x) (1/(1 + exp(-(gl$coefficients[1] + gl$coefficients[2]*x))))

par(mar = c(6,10,6,8))
plot(x,Y, pch = 20, col = 'red',
     main = 'Fitted with observed',
     xlab = "values of x", ylab = 'Binary response',
     cex.main = 2, cex.lab = 1.2, las = 1, 
     font.main = 7, font.lab = 7, font.axis = 2,
     mgp = c(3,0.5,0))
curve(ft, add = T, lwd = 2, col = 'blue')


# classification :

f = function(d)
{
   n = nrow(d)
   t_opt = matrix(ncol = 3, nrow = n)
   colnames(t_opt) = c('TPR','FPR','TPR(1-FPR)')
   
   A = matrix(rep(1, n^2), ncol = n)
   A[lower.tri(A)] = 0
   
   # reordering the dataframe according to pi_hats
   d = d[order(d$pi_hat, decreasing = T),]
   
   # function for confusion matrix
   confmat = function(t)
   {
      if(nrow(t) == 1)
         t = rbind(c(0,0), t)
      
      tpr = t[2,2]/(t[1,2] + t[2,2])
      fpr = t[2,1]/(t[1,1] + t[2,1])
      return(c(tpr, fpr, tpr*(1-fpr)))
   }
   
   # creating confusion matrix
   for(i in 1:n)
   {
      t_opt[i,] = confmat(table(A[,i], d$l_status))
   }
   
   d = cbind(d, as.data.frame(t_opt))
   pi_opt = d[which.max(d$`TPR(1-FPR)`),]$pi_hat
   Y_pred = c(rep(1, sum(d$pi_hat > pi_opt)),
              rep(0, sum(d$pi_hat <= pi_opt)))
 
    
   return(cbind(d, 'Predicted Y' = Y_pred))
}



df1 = f(df)  # final data frame


# ROC curve
par(mar = c(6,10,6,8))
plot(df1$FPR, df1$TPR, type = 'l',
     col = 'red', lwd = 2,
     main = 'ROC curve', xlab = "FPR", ylab = 'TPR',
     font.main = 7, font.lab = 7, font.axis = 2,
     cex.main = 2.3, cex.lab = 1.2, las = 1,
     mgp = c(3,0.5,0))
