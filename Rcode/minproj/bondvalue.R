## ---- bondvalue

#calculate the price of zero coupon bonds with Vasicek
#with jump models

#financial parameters (all adjusted for risk neutral measure)
T = 10
theta = 0.04
cr = 0.015
kappa = 0.6
sigma = 0.03
lambda = 2
sigmaj = 0.01
muj = 0

#time interval set to be 0.01 to make 100 intervals per year
h=0.01
M = 2000
num = round(T/h)
vallist = rep(0,M)
plotX = matrix(nrow = 20, ncol = num+1)
for (j in 1:M) {
  X = rep(0,num+1)
  X[1] = cr
  for (i in 1:num) {
    if (runif(1)<lambda*h) {jump = rnorm(1,muj,sigmaj)}
    else jump = 0
    X[i+1] = rnorm(1,X[i]+kappa*(theta-X[i])*h,sigma*sqrt(h))+jump
    if (j<=20) plotX[j,] = X
  }
  vallist[j] = exp(-sum(X)*h)
}
price = mean(vallist)
se = sd(vallist)/sqrt(M)
cat('The estimated zero-coupon bond price is ',price,'\n')
cat('The standard error is ',se,'\n')