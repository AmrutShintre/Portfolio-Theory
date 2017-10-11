## illustration of fPortfolio
##  using Pictet Swiss Pension Fund included in the package

library(fPortfolio)

data = LPP2005.RET[,1:6]
summary(data)
head(data)

boxplot(as.matrix(data))

# get tangency portfolio
tg=tangencyPortfolio(data)
summary(tg)  # print out using default summary method
## tg is a S4 object, so get portfolio part
s_tg = tg@portfolio
s_tg
## get weights
getWeights(s_tg)
tg_m = getTargetReturn(s_tg)[1]
tg_s = getTargetRisk(s_tg)[2]
cat('Target Portfolio with mean of',tg_m,'sd of',tg_s,'\n')


## construct frontier with long only and plot
p1 = portfolioFrontier(data)
p1

# Plot
p0 = p1
frontierPlot(p0,pch=19,risk='Sigma',xlim=c(0,0.008),ylim=c(0,9e-4)); 
grid();abline(h=0,col='grey');abline(v=0,col='grey')
## add on -
minvariancePoints(p0,pch=19,col='red')
tangencyPoints(p0,pch=19,col='blue')
tangencyLines(p0,col='blue',lwd=2)
equalWeightsPoints(p0,pch=15,col='grey')
singleAssetPoints(p0,pch=19,cex=1.5,col=topo.colors(6))
twoAssetsLines(p0,lty=3,col='grey')

weightsPlot(p0)

Spec = portfolioSpec()
setRiskFreeRate(Spec) = 0.02/250;
p2 = portfolioFrontier(data,Spec)
p0 = p2

## construct frontier with short and plot
setRiskFreeRate(Spec) = 0;  # reset risk free rate to 0
setSolver(Spec)='solveRshortExact'
p3 = portfolioFrontier(data,Spec,constraints='Short')
p0 = p3  # recyle the previous codes to get the plot

## Plot two frontiers together
x11(10,7)
frontierPlot(p1,pch=19,risk='Sigma'); 
grid();abline(h=0,col='grey');abline(v=0,col='grey')
frontierPlot(p3,pch=19,col=c('blue','light blue'),add=T); 

