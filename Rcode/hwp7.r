## ---- hwp7

#Estimate E_pi(x) where pi=5x^-6 independence sampler algorithm
# with proposal distribution q(x) = r x^{-r-1}

h = function(x) {x}
g = function(x) {5*x^(-6)}
q = function(x,r) {r*x^(-r-1)}

runs = 10

M = 1e+5
B = 1e+4

funcmean = numeric(runs)
funciidse = numeric(runs)
varfact = numeric(runs)
funcse = numeric(runs)
accrate = numeric(runs)

cat('B = ',B,', M = ',M,'\n')
for (i in 1:runs) {
  acc = 0
  fnlist = numeric(M+B)
  x = (1-runif(1))^(-1/r)
  for (j in 1:(B+M)) {
    y = (1-runif(1))^(-1/r)
    if (runif(1)<=g(y)*q(x,r)/g(x)/q(y,r)) {
      acc = acc+1
      x = y
    }
    fnlist[j] = h(x)
  }
  
  funcmean[i] = mean(fnlist[(B+1):(M+B)])
  funciidse[i] = sd(fnlist[(B+1):(M+B)])/sqrt(M)
  acf_k = acf(fnlist[(B+1):(M+B)],lag.max = 1000,plot = FALSE)$acf
  varfact[i] = 2*sum(acf_k)-1
  funcse[i] = funciidse[i]*sqrt(varfact[i])
  accrate[i] = acc/M
  
  cat('Number of samples accepted = ',acc,', acceptance rate = ',accrate[i],'\n')
  cat('Estimate = ',funcmean[i],'\n')
  cat('i.i.d. standard error = ',funciidse[i],'\n')
  cat('varfact = ',varfact[i],'\n')
  cat('Standard error = ',funcse[i],'\n')
}  
meanse <- data.frame(funcmean,funciidse,varfact,funcse,accrate)
estse = sd(funcmean)
cat('Standard error calculated from multiple runs = ',estse,'\n')
