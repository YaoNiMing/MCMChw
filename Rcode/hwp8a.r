## ---- hwp8a

#Estimate E_pi(X+X^2) where pi=cg using RWM algorithm where
# g(x) = e^(-|x|/10)*(1+cos(x)sin(x^3))
# with increments following a N(0,sigma^2) distribution

fn = function(x) {x+x^2}
g = function(x) {exp(-abs(x)/10)*(1+cos(x)*sin(x^3))}

sigma = 53
M = 1e+6
B = 1e+5

#initialization
fnlist = numeric(M+B)
xlist = numeric(M+B)
x = rnorm(1,0,sigma)
acc = 0

for (i in 1:(M+B)) {
  eps = rnorm(1,0,sigma)
  
  if (runif(1)<=g(x+eps)/g(x)) {
    if (i>B)
      acc=acc+1
    x = x+eps
  }
  xlist[i] = x
  fnlist[i] = fn(x)
}

funcmean = mean(fnlist[(B+1):(M+B)])
funciidse = sd(fnlist[(B+1):(M+B)])/sqrt(M)
acf_k = acf(fnlist[(B+1):(M+B)],lag.max = 1000,plot = FALSE)$acf
varfact = 2*sum(acf_k[1:min(which(acf_k<0.05))])-1
funcse = funciidse*sqrt(varfact)
accrate = acc/M

cat('B = ',B,', M = ',M,'\n')
cat('Number of samples accepted = ',acc,', acceptance rate = ',accrate,'\n')
cat('Estimate = ',funcmean,'\n')
cat('i.i.d. standard error = ',funciidse,'\n')
cat('varfact = ',varfact,'\n')
cat('Standard error = ',funcse,'\n')
