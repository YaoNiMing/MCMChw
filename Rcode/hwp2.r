## ---- hwp2

#Estimate E[(sigma(2pi)^0.5\lambda)(X^2+2X+2+sin(X+1))^(-|Y|^3-2)exp(Y^2/2sigma^2+X)]
#where X~Exp(lambda) and Y~normal(0,sigma^2) and are independent
sigma = 1
lambda = 1
fn = function(x,y) 
  {sigma/lambda*sqrt(2*pi)*(x^2+2*x+2+sin(x+1))^(-2-(abs(y))^3)*exp(y^2/2/sigma^2+x)}

#number of cases
M=10^6

xlist = rexp(M,1/lambda)
ylist = rnorm(M,0,sigma)
funclist = fn(xlist,ylist)
funcmean = mean(funclist)
funcse = sd(funclist)/sqrt(M)
cat('Using Monte Carlo Integration methods with \n')
cat('M = ',M,', sigma = ',sigma,', lambda = ',lambda,'\n')
cat('estimate = ',funcmean,'\n')
cat('standard error = ',funcse,'\n')

