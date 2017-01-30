## ---- hwp1

#Estimate E((Y+Z)/(1+|Z|)) where Y~Exp(3) and Z~normal(0,1) and are independent
fn = function(y,z) {(y+z)/(1+abs(z))}

#number of cases
M=10^6

ylist = rexp(M,3)
zlist = rnorm(M)
funclist = fn(ylist,zlist)
funcmean = mean(funclist)
funcse = sd(funclist)/sqrt(M)
cat('Estimate E((Y+Z)/(1+|Z|)) where Y~Exp(3) and Z~Normal(0,1)\n')
cat('M = ',M,'\n')
cat('estimate = ',funcmean,'\n')
cat('standard error = ',funcse,'\n')

