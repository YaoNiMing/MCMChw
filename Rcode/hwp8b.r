## ---- hwp8b

#Estimate E_pi(X+X^2) where pi=cg using Metropolis-Hasting algorithm where
# g(x) = e^(-|x|/10)*(1+cos(x)sin(x^3))
# with proposal N(X_(n-1)+1/2sigma^2g'(X_(n-1)),sigma^2)

sigma = 0.1
M =1e5
B = 2e4

fn = function(x) {x+x^2}
g = function(x) {exp(-abs(x)/10)*(1+cos(x)*sin(x^3))}
gp = function(x) {
  if (x>=0)
    -exp(-x/10)/10*(1+cos(x)*sin(x^3))+exp(-x/10)*(3*x^2*cos(x)*cos(x^3)-sin(x)*sin(x^3))
  else
    exp(x/10)/10*(1+cos(x)*sin(x^3))+exp(x/10)*(3*x^2*cos(x)*cos(x^3)-sin(x)*sin(x^3))
}
qq = function(x,y) {
  exp(-(y-x-sigma^2*gp(x)/g(x)/2)^2/sigma^2/2)
}

#initialization
fnlist = numeric(M+B)
xlist = numeric(M+B)
x = rnorm(1,0,sigma)
acc = 0

for (i in 1:(M+B)) {
  
  m = 1/2*sigma^2*gp(x)/g(x)
  y = rnorm(1,x+m,sigma)
  
  if (runif(1)<=g(y)*qq(y,x)/g(x)/qq(x,y)) {
    if (i>B)
      acc=acc+1
    x = y
  }
  xlist[i] = x
  fnlist[i] = fn(x)
}

funcmean = mean(fnlist[(B+1):(M+B)])
funciidse = sd(fnlist[(B+1):(M+B)])/sqrt(M)
acf_k = acf(fnlist[(B+1):(M+B)],lag.max = 1000,plot = FALSE)$acf
varfact = 2*sum(acf_k)-1
funcse = funciidse*sqrt(varfact)
accrate = acc/M

cat('B = ',B,', M = ',M,'\n')
cat('Number of samples accepted = ',acc,', acceptance rate = ',accrate,'\n')
cat('Estimate = ',funcmean,'\n')
cat('i.i.d. standard error = ',funciidse,'\n')
cat('varfact = ',varfact,'\n')
cat('Standard error = ',funcse,'\n')
