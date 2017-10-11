### calculate mean-variance principle
##   get monthly return of some selection of assets first, from 2000 to 2010

library(tseries)

## now try 6 assets from S&P 500
a_n = c('GS','MCD','ABT','IBM','GE','NLY')

a_price = get.hist.quote(instrument=a_n[1],start='2000-1-1',
 end='2011-1-5',compression='m',quote='AdjClose')
asset_ret = 1200*(a_price/lag(a_price,-1)-1)

for (i in (2:length(a_n))) {
 a_price = get.hist.quote(instrument=a_n[i],start='2000-1-1',
 end='2011-1-5',compression='m',quote='AdjClose')
 asset_ret = cbind(asset_ret,as.numeric(1200*(a_price/lag(a_price,-1)-1)))
}
dimnames(asset_ret)[[2]] = a_n
summary(asset_ret)
cor(asset_ret)

## boxplot
x11(10,7);boxplot(as.matrix(asset_ret))

mu = colMeans(asset_ret);V = var(asset_ret);s_v = sqrt(diag(V))
V_inv = solve(V)

one = rep(1,length(mu))
B = t(mu) %*% V_inv %*% mu; A = t(mu) %*% V_inv %*% one
C = t(one) %*% V_inv %*% one; D = B*C - A*A
g = (as.numeric(B)*V_inv%*%one-as.numeric(A)*V_inv%*%mu)/as.numeric(D)
h = (as.numeric(C)*V_inv%*%mu-as.numeric(A)*V_inv%*%one)/as.numeric(D)
m = seq(-20,40,0.1); s_m = C*((m-A/C)^2)/D + 1/C; s_m = sqrt(s_m)

## calculating vertex
sink('mean_variance.txt')
cat('Mean and Standard Deviation\n')
cbind(mu,s_v)
cat('Variance\n')
V
cat('Vertex of mean variance frontier is ',sqrt(1/C),A/C,'\n')
mm = c(-5,0,5,10,A/C,20,30)
wm = g + mm[1]*h;
for (i_m in (2:length(mm))) {
  wm = cbind(wm,g+mm[i]*h)
}
tmp = rbind(t(mm),wm)
cat('Weights for selected returns\n')
tmp
sink()

#### Plotting
x11(10,7)
plot(s_v,mu,xlim=c(6,150),ylim=c(-20,40),ylab='mu',xlab='sigma')
title('Mean Variance Frontier')
text(s_v,mu,a_n)
lines(s_m,m,col='blue',lwd=2)
abline(A/C,sqrt(D/C),lty=2);abline(A/C,-sqrt(D/C),lty=2);

### delete one of the asset and plot the new frontier with the old one
## delete NLY, the highest return, position is 6
mu1 = mu[-6]; V1 = V[-6,-6]; V1_inv = solve(V1)
one1 = rep(1,length(mu1))
B1 = t(mu1) %*% V1_inv %*% mu1; A1 = t(mu1) %*% V1_inv %*% one1
C1 = t(one1) %*% V1_inv %*% one1; D1 = B1*C1 - A1*A1
s_m1 = C1*((m-A1/C1)^2)/D1 + 1/C1; s_m1 = sqrt(s_m1)

x11(10,7)
plot(s_v,mu,xlim=c(20,150),ylim=c(-20,40),ylab='mu',xlab='sigma')
title('Mean Variance Frontier with/out NLY')
text(s_v,mu,a_n)
lines(s_m,m,col='blue',lwd=2); lines(s_m1,m,col='red',lwd=2,lty=2);

## delete GE, the loweest return, position is 5
mu1 = mu[-5]; V1 = V[-5,-5]; V1_inv = solve(V1)
one1 = rep(1,length(mu1))
B1 = t(mu1) %*% V1_inv %*% mu1; A1 = t(mu1) %*% V1_inv %*% one1
C1 = t(one1) %*% V1_inv %*% one1; D1 = B1*C1 - A1*A1
s_m1 = C1*((m-A1/C1)^2)/D1 + 1/C1; s_m1 = sqrt(s_m1)

x11(10,7)
plot(s_v,mu,xlim=c(20,150),ylim=c(-20,40),ylab='mu',xlab='sigma')
title('Mean Variance Frontier with/out GE')
text(s_v,mu,a_n)
lines(s_m,m,col='blue',lwd=2); lines(s_m1,m,col='red',lwd=2,lty=2);

## delete ABT, the loweest standard deviation, position is 3
mu1 = mu[-3]; V1 = V[-3,-3]; V1_inv = solve(V1)
one1 = rep(1,length(mu1))
B1 = t(mu1) %*% V1_inv %*% mu1; A1 = t(mu1) %*% V1_inv %*% one1
C1 = t(one1) %*% V1_inv %*% one1; D1 = B1*C1 - A1*A1
s_m1 = C1*((m-A1/C1)^2)/D1 + 1/C1; s_m1 = sqrt(s_m1)

x11(10,7)
plot(s_v,mu,xlim=c(20,150),ylim=c(-20,40),ylab='mu',xlab='sigma')
title('Mean Variance Frontier with/out ABT')
text(s_v,mu,a_n)
lines(s_m,m,col='blue',lwd=2); lines(s_m1,m,col='red',lwd=2,lty=2);

## delete GS, the highest standard deviation, position is 1
mu1 = mu[-1]; V1 = V[-1,-1]; V1_inv = solve(V1)
one1 = rep(1,length(mu1))
B1 = t(mu1) %*% V1_inv %*% mu1; A1 = t(mu1) %*% V1_inv %*% one1
C1 = t(one1) %*% V1_inv %*% one1; D1 = B1*C1 - A1*A1
s_m1 = C1*((m-A1/C1)^2)/D1 + 1/C1; s_m1 = sqrt(s_m1)

x11(10,7)
plot(s_v,mu,xlim=c(20,150),ylim=c(-20,40),ylab='mu',xlab='sigma')
title('Mean Variance Frontier with/out GS')
text(s_v,mu,a_n)
lines(s_m,m,col='blue',lwd=2); lines(s_m1,m,col='red',lwd=2,lty=2);


