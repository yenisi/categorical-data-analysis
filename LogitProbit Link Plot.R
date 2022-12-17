p=seq(0,1,0.0001)
plot(p, log(p/(1-p)),type='l')
plot(p, qnorm(p), type="l")