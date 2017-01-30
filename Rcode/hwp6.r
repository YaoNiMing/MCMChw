## ---- hwp6

#Estimate E_pi((X1-X2)/(1+X3+X4X5)) where pi=cg using RWM algorithm 
# with increments following a N(0,sigma^2*I) distribution

A = 7; B = 6; C = 6; D = 3

fn = function(x1,x2,x3,x4,x5) {(x1-x2)/(1+x3+x4*x5)}
g = function(x1,x2,x3,x4,x5) {
  if (x1<=0 | x1>=2 | x2<=0 | x2>=2 |x3<=0 | x3>=2 |
      x4<=0 | x4>=2 | x5<=0 | x5>=2)
    0
  else 
    (x1+A+2)^(x2+3)*(1+cos((B+3)*x3))*(exp((12-C)*x4))*abs(x4-3*x5)^(D+2)
}

sigma = 0.2
M = 1e+6
B = 1e+5

#initialization
fnlist = numeric(M+B)
x1 = runif(1,0,2)
x2 = runif(1,0,2)
x3 = runif(1,0,2)
x4 = runif(1,0,2)
x5 = runif(1,0,2)
acc = 0

for (i in 1:(M+B)) {
  eps = rnorm(5,0,sigma)
  
  if (runif(1)<=g(x1+eps[1],x2+eps[2],x3+eps[3],x4+eps[4],x5+eps[5])/
        g(x1,x2,x3,x4,x5)) {
    if (i>B)
      acc=acc+1
    x1=x1+eps[1]
    x2=x2+eps[2]
    x3=x3+eps[3]
    x4=x4+eps[4]
    x5=x5+eps[5]
  }
  fnlist[i] = fn(x1,x2,x3,x4,x5)
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
