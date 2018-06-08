## @knitr setup
#
# reit question 2
# active management
#
load("reitbbgdata.rdata")
#
#
## @knitr indexplot
require(zoo, quietly=TRUE)
startdate=as.Date('1995-1-1')
usind1=mu.nareit[time(mu.nareit)>=startdate]
#usind1=mu.djusreit[time(mu.djusreit)>=startdate]
usind=zoo(coredata(usind1),(time(usind1)))
glblind1=mu.djglblre[time(mu.djglblre)>=startdate]
glblind=zoo(coredata(glblind1),(time(glblind1)))
xusind1=mu.djxusre[time(mu.djxusre)>=startdate]
xusind=zoo(coredata(xusind1),(time(xusind1)))
plot(exp(cumsum(usind)),type='l',col='blue',
     main='REIT and RE Securities Indices',
     xlab='',ylab='Growth of a $')
lines(exp(cumsum(glblind)),type='l',col='red')
lines(exp(cumsum(xusind)),type='l',col='green')
legend('topleft',legend=c("US","Global","International"),
       lwd=2,col=c('blue','red','green'))
#

## @knitr performance
#
require(zoo, quietly=TRUE)
require(lubridate, quietly=TRUE)
tick.reg=read.csv('reit mf tickers.csv',stringsAsFactors=FALSE)
mfreg=tickers$Region
mfname=sub(" US Equity","",tick)

tfun=function(x,bench) {
  mat=merge(x,bench)
  goodrow=which((!is.na(mat[,1]))&(!is.na(mat[,2])))
  mat=mat[goodrow,]
  port=coredata(mat[,1])
  ben=coredata(mat[,2])
  if(length(port)!=length(ben)) stop()
  ans=NULL
  ans$statistic=NA
  ans$p.value=NA
  if(length(port)>1) ans=t.test(port,ben)
  alpha=-1+exp(12*(mean(port)-mean(ben)))
  ir=alpha/(sd(port-ben)*12/sqrt(12))
  ansvec=c(length(port),
           100*alpha,
           ir,
           ans$statistic,
           100*ans$p.value)
  names(ansvec)=c("n","excess.return","info.ratio","t value","p value")
  return(ansvec)
}
resultmat=matrix(0,nrow=0,ncol=5)
for (i in 1:length(mfret)) {
  bench=0
  #if (i==xusbenchind) next (i)
  if (mfreg[i]=="OTHER") next(i)
  if (mfreg[i]=="US") bench=usind
  if (mfreg[i]=="INTL") bench=xusind
  if (mfreg[i]=="GLBL") bench=glblind
  #timport=as.yearmon(time(mfret[[i]]))
  #timport=timport+months(1)-day(timport)
  portret=mfret[[i]]
  resultmat=rbind(resultmat,tfun(portret,bench))
}
srt=sort(resultmat[,3],decreasing=TRUE,index.return=TRUE)
otherind=which(mfreg=="OTHER")
resultdf=data.frame(resultmat,ticker=mfname[-otherind],
                    region=mfreg[-otherind])
sorted.result=resultdf[srt$ix,]
shortmfreg=mfreg[-otherind]
rowplot=which(shortmfreg=="US")
xlim=range(resultmat[,3],na.rm=TRUE)
ylim=range(resultmat[,2],na.rm=TRUE)
plot(resultmat[rowplot,3],resultmat[rowplot,2],
     xlab="Annual Information Ratio",ylab="Annual Excess Return",
     col='blue',pch=20,xlim=xlim,ylim=ylim,
     main='Equity Reit Mutual Fund Performance')
rowplot=which(shortmfreg=="INTL")
points(resultmat[rowplot,3],resultmat[rowplot,2],
       col='green',pch=20)
rowplot=which(shortmfreg=="GLBL")
points(resultmat[rowplot,3],resultmat[rowplot,2],
       col='red',pch=20)
abline(h=0,v=0,lty='dotted')
legend('topleft',legend=c("US","Global","International"),
       lwd=2,col=c('blue','red','green'))
#
## @knitr significance
#
plot(resultmat[,4],resultmat[,5],col='blue',pch=20,
     xlab="t value",ylab="p value (percent)",
     main="Statistical Significance of Performance")

#
## @knitr persistence
#
require(zoo, quietly=TRUE)
nbench=length(usind)
benchlast5=usind[(nbench-59):nbench]
benchprior5=usind[(nbench-119):(nbench-60)]
last5tim=time(benchlast5)
prior5tim=time(benchprior5)
irl5=vector()
irp5=vector()
irname=vector()
indused=vector()
regused=vector()
for (i in 1:length(mfret)) {
  if (i==benchind) next(i)
  port=mfret[[i]]
  if (length(port)<120) next(i)
  portl5=coredata(port[last5tim])
  portp5=coredata(port[prior5tim])
  if (mfreg[i]=="OTHER") next(i)
  if (mfreg[i]=="US") bench=usind
  if (mfreg[i]=="INTL") bench=xusind
  if (mfreg[i]=="GLBL") bench=glblind
  benchdatal5=coredata(bench[last5tim])
  benchdatap5=coredata(bench[prior5tim])
  if (length(portl5)!=length(benchdatal5)) next(i)
  if (length(portp5)!=length(benchdatap5)) next(i)
  indused=c(indused,i)
  regused=c(regused,mfreg[i])
  alphal5=-1+exp(12*(mean(portl5)-mean(benchdatal5)))
  portirl5=alphal5/(sd(portl5-benchdatal5)*12/sqrt(12))
  alphap5=-1+exp(12*(mean(portp5)-mean(benchdatap5)))
  portirp5=alphap5/(sd(portp5-benchdatap5)*12/sqrt(12))
  irl5=c(irl5,portirl5)
  irp5=c(irp5,portirp5)
  irname=c(irname,mfname[i])
}
plotind=which(regused=="US")
plot(irp5[plotind],irl5[plotind],pch=20,col='blue',
     xlab="Prior 5 year Information Ratio",
     ylab="Last 5 year Information Ratio",
     main="Equity Reit Mutual Fund Performance Persistence")
plotind=which(regused=="INTL")
points(irp5[plotind],irl5[plotind],pch=20,col='red')
plotind=which(regused=="GLBL")
points(irp5[plotind],irl5[plotind],pch=20,col='green')
abline(a=0,b=1,lty='dashed')
regdata=lm(irl5~irp5)
abline(regdata,lty='dashed',col='red')
legend('bottomright',legend=c("X=Y","regression line"),
       lty='dashed',col=c('black','red'))
legend('topleft',legend=c("US","Global","International"),
       lwd=2,col=c('blue','red','green'))
irdf=data.frame(irl5,irp5,(irl5+irp5)/2,irname,regused)
colnames(irdf)=c("IR.last.5","IR.prior.5","mean.IR","ticker","region")
srtdf2=sort(irdf[,3],decreasing=TRUE,index.return=TRUE)
sorted.irdf=irdf[srtdf2$ix,]
## @knitr end

