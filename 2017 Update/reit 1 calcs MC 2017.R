# 
# calcs and graphs for the reit 1.lyx file
#
## @knitr setup
# initial setup and data retrieval
#
require (zoo, quietly=TRUE)
require (boot, quietly=TRUE)
require (car, quietly=TRUE)
source('../../basic financial.r')
load("reitdata 2017.rdata")
#
## @knitr marketcap
# 
# market cap of REIT index copmared to NCREIF
#
eqreitmarketcap=c(5.5,8.8,11.1,26.1,38.8,49.9,
                  78.3,127.8,126.9,118.2,134.4,
                  147.1,151.3,204.8,275.3,301,
                  400,289,176,248,359,407,544,608)
eqreitmarketcap=zooreg(eqreitmarketcap,start=1990)
ncreifmarketcap=zoo(c(38,49,95,145,329),
                    c(1990,1995,2000,2004,2013))
plot(eqreitmarketcap,col='blue',lwd=2,
     xlab="",ylab="$ Billions")
lines(ncreifmarketcap,col='red',lwd=2)
legend('topleft',
       legend=c("NAREIT Index","NCREIF NPI"),
       col=c('blue','red'),lwd=2)
#
## @knitr rolling
#
# rolling returns
#
mu.ncreif.lev=(2*(mu.ncreif-(.5*.25*(zoneyr+.0175))))
ncreif.10yr=4*rollapply(mu.ncreif,
                             40,mean,align='right')
ncreif.10yr.lev=4*rollapply(mu.ncreif.lev,
                            40,mean,align='right')
reit.10yr=12*rollapply(mu.reit,
                            120,mean,align='right')
ncreif.10yr.geo=-1+exp(ncreif.10yr)
ncreif.10yr.lev.geo=-1+exp(ncreif.10yr.lev)
reit.10yr.geo=-1+exp(reit.10yr)
par(mfrow=c(2,1))
plot(100*reit.10yr.geo,col='blue',
     xlab="",ylab="percent",lwd=2,
     main="10 year rolling return",
     ylim=c(4,18))
lines(100*ncreif.10yr.geo,col='red',lwd=2)
lines(100*ncreif.10yr.lev.geo,col='green',lwd=2)
legend("topleft",legend=c("NAREIT","NCREIF","NCREIF Leveraged"),
       col=c("blue","red","green"),lwd=2,cex=.6)
ncreif.1yr=4*rollapply(mu.ncreif,
                            4,mean,align='right')
ncreif.1yr.geo=-1+exp(ncreif.1yr)
ncreif.1yr.lev=4*rollapply(mu.ncreif.lev,
                                4,mean,align='right')
ncreif.1yr.lev.geo=-1+exp(ncreif.1yr.lev)
reit.1yr=12*rollapply(mu.reit,
                           12,mean,align='right')
reit.1yr.geo=-1+exp(reit.1yr)
plot(100*reit.1yr.geo,col='blue',lwd=2,
     main="1 year rolling return",
     xlab="",ylab="percent")
lines(100*ncreif.1yr.geo,col='red',lwd=2)
lines(100*ncreif.1yr.lev.geo,col='green',lwd=2)
legend("topleft",legend=c("NAREIT","NCREIF","NCREIF Leveraged"),
       col=c("blue","red","green"),lwd=2,cex=.6)
#
## @knitr diff
# 
# plot difference between rolling returns
# and show regression line
#
#par(mfrow=c(2,1))
diff.10yr=reit.10yr.geo-ncreif.10yr.lev.geo
plot(100*diff.10yr,col='purple',lwd=2,
     xlab="",ylab='percent',
     main="10 year rolling returns\nNAREIT minus NCREIF Levered")
     
abline(lm(100*diff.10yr~time(diff.10yr)),lty='dashed',col='purple')
abline(h=0)
diff.1yr=reit.1yr.geo-ncreif.1yr.lev.geo
plot(100*diff.1yr,col='purple',lwd=2,
     xlab='',ylab='percent',
     main="1 year rolling returns\nNAREIT minus NCREIF Levered")
abline(lm(100*diff.1yr~time(diff.1yr)),lty='dashed',col='purple')
abline(h=0)
#
# plot confidence ellipses of returns and standard deviations
#
## @knitr ellipse
cormatdaily=cor(merge(mu.spxt.daily,mu.reit.daily),
                use="pairwise.complete.obs")
mu.reit.m=zooreg(coredata(mu.reit)[-(1:57)],
                 start=as.yearmon(time(mu.reit[58])),
                 freq=12)
mu.spxt.m=zooreg(coredata(mu.spxt)[-(1:57)],
                 start=as.yearmon(time(mu.spxt[58])),
                 freq=12)
mu.ncreif.m1=mu.ncreif[time(mu.ncreif)>=as.Date(time(mu.reit.m)[1])]
mun=coredata(mu.ncreif.m1)/3
ncreifmat=rbind(mun,mun,mun)
mu.ncreif.m=as.vector(ncreifmat)
mu.ncreif.m=zooreg(mu.ncreif.m,start=time(mu.reit.m)[1],freq=12)
mureit=coredata(mu.reit.m)[-(1:3)]
mu.reit.m=mu.reit.m[time(mu.reit.m)<=time(mu.ncreif.m)[length(mu.ncreif.m)]]
mu.spxt.m=mu.spxt.m[time(mu.spxt.m)<=time(mu.ncreif.m)[length(mu.ncreif.m)]]
muncreif=coredata(mu.ncreif.m)[-(1:2)]
mu.rr=data.frame(mureit,muncreif)
# bootstrap returns and standard deviations
br=function(r,d) {
  c(12*mean((r[d])),sqrt(12)*sd((r[d])))
}
set.seed(1532)
colors=c("blue","dark green","light blue","green")
par(mfrow=c(1,1))
reit.b10=boot::boot(coredata(tail(mu.reit.m,120)),br,R=999)
ncreif.b10=boot::boot(coredata(tail(mu.ncreif.m,120)),br,R=999)
reit.b20=boot::boot(coredata(tail(mu.reit.m,240)),br,R=999)
ncreif.b20=boot::boot(coredata(tail(mu.ncreif.m,240)),br,R=999)
# plot 95% confidence ellipse of expected return and standard deviation
nr=(nrow(mu.rr)-119):nrow(mu.rr)
muhat.vals10 = -1+exp(12*colMeans(mu.rr[nr,]))
sigmahat.vals10 = sqrt(12)*apply(mu.rr[nr,],2,sd)
nr=(nrow(mu.rr)-239):nrow(mu.rr)
muhat.vals20 = -1+exp(12*colMeans(mu.rr[nr,]))
sigmahat.vals20 = sqrt(12)*apply(mu.rr[nr,],2,sd)
muhat.vals=c(muhat.vals10,muhat.vals20)
sigmahat.vals=c(sigmahat.vals10,sigmahat.vals20)
se.muhat = c(sd(reit.b10$t[,1]),sd(ncreif.b10$t[,1]),
             sd(reit.b20$t[,1]),sd(ncreif.b20$t[,1]))
se.sigmahat = c(sd(reit.b10$t[,2]),sd(ncreif.b10$t[,2]),
                sd(reit.b20$t[,2]),sd(ncreif.b20$t[,2]))
mu.lower = muhat.vals - 2.2*se.muhat
mu.upper = muhat.vals + 2.2*se.muhat
sigma.lower = sigmahat.vals - 2.2*se.sigmahat
sigma.upper = sigmahat.vals + 2.2*se.sigmahat
plot(sigmahat.vals, muhat.vals, xlim=c(min(sigma.lower), max(sigma.upper)), 
     ylim=c(min(mu.lower),max(mu.upper)), xlab="standard deviation",ylab="expected return",
     main="95% confidence ellipses\nannual returns and standard deviations")
mm=2*2*matrix(c(1,0,0,1),nrow=2,ncol=2)
for (i in 1:length(muhat.vals)) {
  car::ellipse(c(sigmahat.vals[i],muhat.vals[i]),
               mm, c(se.sigmahat[i],se.muhat[i]),col=colors[i])
}
names=c("Reit 10 yr","Lev NCREIF 10 yr","Reit 20 yr","Lev NCREIF 20 yr")
legend("topleft",names,col=colors,lwd=2,cex=.7)
#
# plot the autocorrelation charts
#
## @knitr autocor
par(mfrow=c(3,1))
acf(coredata(mu.ncreif.lev),main="Lev. NCREIF (quarterly)")
acf(coredata(mu.reit),main="NAREIT (monthly)")
acf(coredata(mu.spxt),main="S&P 500 (monthly)")
#
# calculate and plot the desmoothed returns
#
## @knitr desmooth
par(mfrow=c(2,1))
order=2
mun=coredata(mu.ncreif.m1)
ncf.ar=ar(mun,aic=FALSE,order.max=order,method="ols")
ncf.mod=rep(NA,order)
for (i in (1+order):length(mun)) ncf.mod[i]=(mun[i+-(order:1)]%*%ncf.ar$ar)
ncf.desm=(mun-ncf.mod)/(1-sum(ncf.ar$ar))
ncf.desm=zoo(ncf.desm,time(mu.ncreif.m1))
plot(exp(cumsum(mu.ncreif.m1[-(1:order)])),col='blue',lwd=2,
     main="NCREIF Lev compared to Desmoothed NCREIF Lev",
     xlab='',ylab="Growth of a $",
     ylim=c(0,6))
gd.desm=exp(cumsum(ncf.desm[-(1:order)]))
lines(gd.desm,col='light blue',lwd=2)
legend('topleft',
       legend=c("NCREIF Lev","NCREIF Lev AR(2)"),
       col=c("blue","light blue"),lwd=2)
acf(coredata(ncf.desm)[-(1:order)],main="NCREIF Lev Desmoothed")
# uncomment the next three lines to see drawdown charts
#require(fBasics)
#drawdownPlot(as.timeSeries(coredata(ncf.desm)))
#drawdownPlot(as.timeSeries(coredata(mu.reit.m)))
ndesm=coredata(ncf.desm)/3
ndesmat=rbind(ndesm,ndesm,ndesm)
mu.ncd.m=as.vector(ndesmat)
mu.ncd.m=zooreg(mu.ncd.m,start=time(mu.reit.m)[1],freq=12)
#
# calculate and print tables of returns, standard deviations and correlation matrix
#
## @knitr coremat
require(xtable,quietly=TRUE)

mu.agg1=mu.agg[as.yearmon(time(mu.agg), format = "%m/%d/%Y")>=time(mu.reit.m)[1]]
mu.agg1=mu.agg1[as.yearmon(time(mu.agg1),format = "%m/%d/%Y")<=tail(time(mu.reit.m),1)]
mu.agg.m=zooreg(coredata(mu.agg1),start=time(mu.reit.m)[1],freq=12)
mumat=merge(mu.reit.m,mu.ncreif.m,mu.ncd.m,mu.spxt.m,mu.agg.m)
corematmonthly=cor(mumat,
                   use='pairwise.complete.obs')
corematm=corematmonthly
corematmonthly=round(100*corematmonthly,1)
names=c("REIT","Lev. NCREIF","Lev NCREIF AR(2)","SP500","LehmAGG")
colnames(mumat)=names
rownames(corematmonthly)=names
colnames(corematmonthly)=names
muvec=apply(mumat,2,mean,na.rm=TRUE)
muvec=round(100*(-1+exp(12*muvec)),1)
sdvec=apply(mumat,2,sd,na.rm=TRUE)
sdvec=round(100*sdvec*sqrt(12),1)
assumed=c(8,8,8,9,5)
assumedsd=c(20,3,15,15,4)
musdmat=cbind(assumed,muvec,assumedsd,sdvec)
rownames(musdmat)=names
colnames(musdmat)=c("Assumed Rtrn","Calculated Rtrn","Assumed SD", "Calculated SD")
musdmat.x=xtable(as.data.frame(musdmat),caption="Returns and Standard Deviation")
print(musdmat.x,scalebox=.8)
coremat.x=xtable(as.data.frame(corematmonthly),caption="Correlation Matrix")
print(coremat.x,scalebox=.8)
#
# calculate and graph the first group of efficient portfolios of real estate
# stocks and bonds
#
## @knitr efficient
#
source('portfolio_noshorts.r.txt', encoding='UTF-8')
par(mfrow=c(3,1))
omitlist=list(c(-2,-3),c(-1,-3),c(-1,-2))
for (i in 1:length(omitlist)) {
  omit=omitlist[[i]]
  mu.rr=mumat[,omit]#merge(mu.ncd.m,mu.spxt.m,mu.agg.m)
  hasna=apply(is.na(mu.rr),1,any)
  mu.rr=mu.rr[!hasna,]
  cov.mat=var(mu.rr)
  #muhat.vals=apply(mu.rr,2,mean)
  #muhat.vals=assumed[omit]
  muhat.vals=log(1+assumed*.01)/12
  names(muhat.vals)=colnames(mumat)
  tr=(log(1.08))/12
  eff.port=efficient.portfolio(muhat.vals[omit],cov.mat,tr,shorts=FALSE)
  #summary(eff.port)
  colors=c("red","green","blue","purple","yellow")
  plot(eff.port,col=colors[omit])
}
#
# calculate and the second set of efficient portfolios
#
## @knitr efficient2
par(mfrow=c(3,1))
omitlist=list(c(-2,-3),c(-1,-3),c(-1,-2))
corematm[4,3]=.5
corematm[3,4]=.5
assumedsd1=.01*assumedsd/sqrt(12)
covmatadj=corematm*outer(assumedsd1,assumedsd1)
for (i in 1:length(omitlist)) {
  omit=omitlist[[i]]
  mu.rr=mumat[,omit]#merge(mu.ncd.m,mu.spxt.m,mu.agg.m)
  hasna=apply(is.na(mu.rr),1,any)
  mu.rr=mu.rr[!hasna,]
  #cov.mat=var(mu.rr)
  #muhat.vals=apply(mu.rr,2,mean)
  #muhat.vals=assumed[omit]
  cov.mat=covmatadj[omit,omit]
  muhat.vals=log(1+assumed*.01)/12
  names(muhat.vals)=colnames(mumat)
  tr=(log(1.08))/12
  eff.port=efficient.portfolio(muhat.vals[omit],cov.mat,tr,shorts=FALSE)
  #summary(eff.port)
  colors=c("red","green","blue","purple","yellow")
  plot(eff.port,col=colors[omit])
}




# some other charts and analysis that I didn't end up using in the presentation
# attempts a different approach to portfolio construction by smoothing the public 
# assets instead of desmoothing the private ones
## @knitr smoothedgraph
gdreit=exp(cumsum(mu.reit.m))
gdreit.roll=rollapply(gdreit,18,mean,align='right')
plot(gdreit,col='blue')
lines(gdreit.roll,col='green')
gdreit.2000=gdreit[time(gdreit)>2004]
gdreit.2000=gdreit.2000/coredata(gdreit.2000[1])
gdreit.roll.2000=gdreit.roll[time(gdreit.roll)>2004]
gdreit.roll.2000=gdreit.roll.2000/coredata(gdreit.roll.2000[1])
ncreif.2000=exp(cumsum(mu.ncreif.m[time(mu.ncreif.m)>2004]))
ncreif.2000=ncreif.2000/coredata(ncreif.2000[1])
par(mfrow=c(1,1))
plot(gdreit.2000,col='blue')
lines(gdreit.roll.2000,col='red')
lines(ncreif.2000,col='green')
## @knitr smoothcor
smooth.reit=diff(log(gdreit.roll))
rets=merge(smooth.reit,mu.ncreif.m,mu.spxt.m)
corematsmooth=cor(rets,use='pairwise.complete.obs')
cori=vector()
lags=2:30
for (i in 1:length(lags)) {
  gdreit.rolli=rollapply(gdreit,lags[i],mean,align='right')
  smooth.reiti=diff(log(gdreit.rolli))
  rets=merge(smooth.reiti,mu.ncreif.m)
  coremati=cor(rets,use='pairwise.complete.obs')
  cori[i]=coremati[2,1]
}
plot(lags,cori)

