## get reit mutual fund data
#
require (Rbbg)
require (zoo)
require (lubridate)
require (XLConnect)
require(quantmod)
setwd("P:/IMD/Karl/R projects/reit r")
load("reitbbgdata.rdata")

tickers=read.csv('reit mf tickers.csv',stringsAsFactors=FALSE)
tick=tickers$Ticker
tick=sub(" US","",tick)
mfret=list()
mfname=vector()
first=as.Date("1994-1-1")
last=Sys.Date()-1
ticker.vec=tick
benchnames=tickers$Name
bench.lst=list()
for (i in 1:length(ticker.vec)) {
  # bench.lst[[i]] = get.hist.quote.fix(instrument=ticker.vec[i], start=first-5,
  #                               end=last+5, quote="AdjClose", 
  #                               provider="yahoo", 
  #                               retclass="zoo")
  i=i+1
  benchhold=getSymbols(ticker.vec[i],src='yahoo',from=first,to=last,auto.assign=FALSE)
  bench.lst[[i]]=as.zoo(benchhold[,6])
}

#add missing days to benchmark
#fill NAs with neighboring values
days365=zooreg(rep(0,1+last-first),start=first,end=last)
for (i in 1:length(ticker.vec)){
  b=merge(days365,bench.lst[[i]])
  b=b[,2]
  b=na.approx(b,na.rm=FALSE)
  bench.lst[[i]]=b
}
save.image("reitmfdata.rdata")
