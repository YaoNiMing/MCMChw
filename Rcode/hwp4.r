## ---- hwp4

#Estimate E_pi((X1-X2)/(1+X3+X4X5)) where pi=cg using importance sampling techniques 
# with density f = uniform(0,2)^5

A = 7; B = 6; C = 6; D = 3

fn = function(x1,x2,x3,x4,x5) {(x1-x2)/(1+x3+x4*x5)}
g = function(x1,x2,x3,x4,x5) 
  {(x1+A+2)^(x2+3)*(1+cos((B+3)*x3))*(exp((12-C)*x4))*abs(x4-3*x5)^(D+2)}

#number of runs
R=10
#number of cases
M=10^6

estlist=numeric(R)

cat('M = ',M,'\n')
for (i in 1:R) {
  #sample from unif(0,2)^5
  x1 = runif(M,0,2)
  x2 = runif(M,0,2)
  x3 = runif(M,0,2)
  x4 = runif(M,0,2)
  x5 = runif(M,0,2)
  
  fnlist = fn(x1,x2,x3,x4,x5)
  glist = g(x1,x2,x3,x4,x5)
  estlist[i] = sum(fnlist*glist)/sum(glist)
  cat('run = ',i,', estimate = ',estlist[i],'\n')
}

estmean = mean(estlist)
estse = sd(estlist)

cat('mean estimate = ',funcmean,'\n')
cat('estimate standard error = ',funcse,'\n')

