## ---- hwp9c

a=100
K=6
J=5

B=1e+5
M=5e+5
sigma = 20

Ydye = t( matrix(
  c(1545, 1440, 1440, 1520, 1580,
    1540, 1555, 1490, 1560, 1495,
    1595, 1550, 1605, 1510, 1560,
    1445, 1440, 1595, 1465, 1545,
    1595, 1630, 1515, 1635, 1625,
    1520, 1455, 1450, 1480, 1445), nrow=5) )

logg = function(m,V,W,theta,Ydye) {
  g = -a/V + log(max(c(V,1e-5)))*(-a-1) + (-a/W)
  g = g + log(max(c(W,1e-5)))*(-a-1) - (m-a)^2/a/2
  g = g + log(max(c(V,1e-5)))*(-K/2) + log(max(c(W,1e-5)))*(-K*J/2)
  g = g - sum((theta-m)^2)/V/2 - sum((Ydye-theta)^2)/W/2
  return (g)
}

m = rnorm(1,a,sigma)
V = var(c(Ydye))
W = var(c(Ydye))
theta = rnorm(K,mean(Ydye),sqrt(V))
acc = 0
fnlist = numeric(B+M)

for (i in 1:(B+M)) {
  eps_m=0
  eps_V=0
  eps_W=0
  eps_theta = rep(0,K)
  ci = i %% (3+K)
  if (ci==0)
    m = rnorm(1,1+sum(theta)/V/(1/a+K/V),(1/a + K/V)^(-0.5))
  else if (ci==1)
    V = 1/rgamma(1,a+K/2,a+sum((theta-m)^2)/2)
  else if (ci==2)
    W = 1/rgamma(1,a+K*J/2, a+sum((Ydye-theta)^2)/2)
  else 
    theta[ci-2] = rnorm(1,(m/V+sum(Ydye[ci-2,])/W)/(1/V+J/W),
                        (1/V+J/W)^(-0.5))
  
  fnlist[i] = W/V
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
