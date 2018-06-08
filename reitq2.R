## @knitr setup
#
# reit question 2
# active management
#
load("reitmfdata.rdata")
xusbenchind=which(mfname=="VGRNX")
xusbench=mfret[[xusbenchind]]
xusbench=zoo(coredata(xusbench),as.yearmon(time(xusbench)))
#
#
## @knitr indexplot
startdate=as.Date('1995-1-1')
usind1=mu.djusreit[time(mu.djusreit)>=startdate]
usind=zoo(coredata(usind1),as.yearmon(time(usind1)))
glblind1=mu.djglblre[time(mu.djglblre)>=startdate]
glblind=zoo(coredata(glblind1),as.yearmon(time(glblind1)))
xusind1=mu.djxusre[time(mu.djxusre)>=startdate]
xusind=zoo(coredata(xusind1),as.yearmon(time(xusind1)))
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
mfreg=vector()
for (i in 1:length(mfname)) {
  nameind=grep(mfname[i],tick.reg$Ticker)
  mfreg=c(mfreg,tick.reg$Region[nameind])
}

tfun=function(x,bench) {
  if(length(x)<=length(bench)) {
    tim=time(x)
  } else {
    tim=time(bench)
  }
  port=coredata(x[tim])
  ben=coredata(bench[tim])
  if(length(port)!=length(ben)) stop()
  ans=t.test(port,ben)
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
  if (i==xusbenchind) next (i)
  if (mfreg[i]=="OTHER") next(i)
  if (mfreg[i]=="US") bench=usind
  if (mfreg[i]=="INTL") bench=xusbench
  if (mfreg[i]=="GLBL") bench=glblind
  timport=as.yearmon(time(mfret[[i]]))
  #timport=timport+months(1)-day(timport)
  portret=zoo(coredata(mfret[[i]]),timport)
  resultmat=rbind(resultmat,tfun(portret,bench))
}
srt=sort(resultmat[,3],decreasing=TRUE,index.return=TRUE)
otherind=c(xusbenchind,which(mfreg=="OTHER"))
resultdf=data.frame(resultmat,ticker=mfname[-otherind],
                    region=mfreg[-otherind])
sorted.result=resultdf[srt$ix,]
shortmfreg=mfreg[-otherind]
rowplot=which(shortmfreg=="US")
xlim=range(resultmat[,3])
ylim=range(resultmat[,2])
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
nbench=length(bench)
benchlast5=bench[(nbench-59):nbench]
benchprior5=bench[(nbench-119):(nbench-60)]
last5tim=time(benchlast5)
prior5tim=time(benchprior5)
benchdatal5=coredata(benchlast5)
benchdatap5=coredata(benchprior5)
irl5=vector()
irp5=vector()
irname=vector()
for (i in 1:length(mfret)) {
  if (i==benchind) next(i)
  port=mfret[[i]]
  if (length(port)<120) next(i)
  portl5=coredata(port[last5tim])
  portp5=coredata(port[prior5tim])
  alphal5=-1+exp(12*(mean(portl5)-mean(benchdatal5)))
  portirl5=alphal5/(sd(portl5-benchdatal5)*12/sqrt(12))
  alphap5=-1+exp(12*(mean(portp5)-mean(benchdatap5)))
  portirp5=alphap5/(sd(portp5-benchdatap5)*12/sqrt(12))
  irl5=c(irl5,portirl5)
  irp5=c(irp5,portirp5)
  irname=c(irname,mfname[i])
}
plot(irp5,irl5,pch=20,col='blue',
     xlab="Prior 5 year Information Ratio",
     ylab="Last 5 year Information Ratio",
     main="Equity Reit Mutual Fund Performance Persistence")
abline(a=0,b=1,lty='dashed')
regdata=lm(irl5~irp5)
abline(regdata,lty='dashed',col='red')
legend('topleft',legend=c("X=Y","regression line"),lty='dashed',col=c('black','red'))
irdf=data.frame(irl5,irp5,(irl5+irp5)/2,irname)
colnames(irdf)=c("IR.last.5","IR.prior.5","mean.IR","ticker")
srtdf2=sort(irdf[,3],decreasing=TRUE,index.return=TRUE)
sorted.irdf=irdf[srtdf2$ix,]
## @knitr end

