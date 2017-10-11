### calculate mean-variance principle with risk free assets
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

mu = colMeans(asset_ret);V = var(asset_ret);s_v = sqrt(diag(V))
V_inv = solve(V)

one = rep(1,length(mu))
B = t(mu) %*% V_inv %*% mu; A = t(mu) %*% V_inv %*% one
C = t(one) %*% V_inv %*% one; D = B*C - A*A
rho = 5;  ### try 5% risky-free rate
H = C*rho*rho - 2*A*rho + B
g = (as.numeric(B)*V_inv%*%one-as.numeric(A)*V_inv%*%mu)/as.numeric(D)
h = (as.numeric(C)*V_inv%*%mu-as.numeric(A)*V_inv%*%one)/as.numeric(D)
m = seq(-20,40,0.1); s_m = C*((m-A/C)^2)/D + 1/C; s_m = sqrt(s_m)

## calculating vertex
sink('mean_variance_free.txt')
cat('Mean Variance Frontier with risk free asset of rate',rho,'\n')
cat('Mean and Standard Deviation\n')
cbind(mu,s_v)
cat('Variance\n')
V
cat('Vertex of mean variance frontier is ',sqrt(1/C),A/C,'\n')
t_m = A/C-D/(C*C*(rho-A/C)); t_s = sqrt(H)/(C*abs(rho-A/C))
cat('Tangent portfolio: mean of',t_m,
 'Standard deviation of',t_s,'\n')
wm = g + as.numeric(t_m)*h;
cat('Weights of tagent portfolio is\n')
wm
sink()

#### Plotting
x11(10,7)
plot(s_v,mu,xlim=c(6,150),ylim=c(-5,40),ylab='mu',xlab='sigma')
title('Mean Variance Frontier with a Risk Free Asset of 5%')
text(s_v,mu,a_n)
lines(s_m,m,col='blue',lwd=2)
abline(rho,sqrt(H),lty=2)
abline(h=rho,lty=2)
points(t_s,t_m)


