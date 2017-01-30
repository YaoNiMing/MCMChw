
## ---- hwp5optim

goptim = function(x) {
  x1=x[1]
  x2=x[2]
  x3=x[3]
  x4=x[4]
  x5=x[5]
  -(x1+A+2)^(x2+3)*(1+cos((B+3)*x3))*(exp((12-C)*x4))*abs(x4-3*x5)^(D+2)
}
optim(rep(1,5),goptim,NULL,method="L-BFGS-B",lower=rep(0,5),upper=rep(2,5))

## ---- hwp5

#Estimate E_pi((X1-X2)/(1+X3+X4X5)) where pi=cg using rejection sampling techniques 
# with density f = uniform(0,2)^5

A = 7; B = 6; C = 6; D = 3

fn = function(x1,x2,x3,x4,x5) {(x1-x2)/(1+x3+x4*x5)}
g = function(x1,x2,x3,x4,x5) 
{(x1+A+2)^(x2+3)*(1+cos((B+3)*x3))*(exp((12-C)*x4))*abs(x4-3*x5)^(D+2)}
K = 2e+15

M = 1e+7

#sample from f
x1 = runif(M,0,2)
x2 = runif(M,0,2)
x3 = runif(M,0,2)
x4 = runif(M,0,2)
x5 = runif(M,0,2)

#generate r.v. for acceptance
u = runif(M)
acc = (u<=32*g(x1,x2,x3,x4,x5)/K)

fnlist = fn(x1[acc],x2[acc],x3[acc],x4[acc],x5[acc])
funcmean = mean(fnlist)
funcse = sd(funclist)/sqrt(sum(acc))
accrate = sum(acc)/M

cat('M = ',M,'\n')
cat('Number of samples accepted = ',sum(acc),', acceptance rate = ',accrate,'\n')
cat('Estimate = ',funcmean,'\n')
cat('Standard error = ',funcse,'\n')
