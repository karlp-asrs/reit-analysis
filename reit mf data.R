## get reit mutual fund data
#
require (Rbbg)
require (zoo)
require (lubridate)
require (XLConnect)
setwd("P:/IMD/Karl/R projects/reits")
# first get Bloomberg tickers
# RETRANAT Index -- average cap rate U.S. national
# FNERTR Index -- nareit total return
# SPXT Index -- S&P Total Return Index
# CPI YOY Index -- CPI All urban consumers
# USGG5YR Index -- 5 yr treasury yield
# NPPITR Index -- NCREIF total return
conn=blpConnect()
#get daily REIT and SPX for last 5 years
# reit.daily=bdh(conn,"FNERTR Index","PX_LAST",Sys.Date()-5*366)
# spxt.daily=bdh(conn,"SPXT Index","PX_LAST",Sys.Date()-5*366)
# zreit.daily=zoo(reit.daily$PX_LAST,as.Date(reit.daily$date))
# zspxt.daily=zoo(spxt.daily$PX_LAST,as.Date(spxt.daily$date))
# mu.reit.daily=diff(log(zreit.daily))
# mu.spxt.daily=diff(log(zspxt.daily))
#get monthly on all for last 25 years
caprates=bdh(conn,"RETRANAT Index","PX_LAST",Sys.Date()-25*366,
             option_names="periodicitySelection",option_value="MONTHLY")
zcaprates=.01*zoo(caprates$PX_LAST,as.Date(caprates$date))
cpi=bdh(conn,"CPI YOY Index","PX_LAST",Sys.Date()-25*366,
        option_names="periodicitySelection",option_value="MONTHLY")
zcpi=.01*zoo(cpi$PX_LAST,as.Date(cpi$date))
ncreif=bdh(conn,"NPPITR Index","PX_LAST",Sys.Date()-25*366,
           option_names="periodicitySelection",option_value="QUARTERLY")
zncreif=.01*zoo(ncreif$PX_LAST,as.Date(ncreif$date))
mu.ncreif=log(1+zncreif)
fiveyr=bdh(conn,"USGG5YR Index","PX_LAST",Sys.Date()-25*366,
           option_names="periodicitySelection",option_value="MONTHLY")
zfiveyr=.01*zoo(fiveyr$PX_LAST,as.Date(fiveyr$date))
oneyr=bdh(conn,"US0012M Index","PX_LAST",Sys.Date()-25*366,
          option_names="periodicitySelection",option_value="MONTHLY")
zoneyr=.01*zoo(oneyr$PX_LAST,as.Date(oneyr$date))
reit=bdh(conn,"FNERTR Index","PX_LAST",Sys.Date()-25*366,
         option_names="periodicitySelection",option_value="MONTHLY")
zreit=zoo(reit$PX_LAST,as.Date(reit$date))
mu.reit=diff(log(zreit))
spxt=bdh(conn,"SPXT Index","PX_LAST",Sys.Date()-25*366,
         option_names="periodicitySelection",option_value="MONTHLY")
zspxt=zoo(spxt$PX_LAST,as.Date(spxt$date))
mu.spxt=diff(log(zspxt))
# #get lehmann agg from excel file
# data=loadWorkbook("Performance Measurement SP.xlsm")
# aggval=readWorksheet(data,sheet="Index_Returns",region="W9:W472",header=FALSE)
# aggdate=readWorksheet(data,sheet="Index_Returns",region="A9:A472",header=FALSE)
# zagg=zoo(aggval$Col1,aggdate$Col1)
# mu.agg=log(1+.01*zagg)
#don't forget to disconnect
blpDisconnect(conn)
# use yahoo finance to get reit mutual fund monthly adj close
require(tseries)
tick="VGSIX"
# test=(getSymbols(tick,from=Sys.Date()-11*366,to=Sys.Date(),auto.assign=FALSE))
# test2=get.hist.quote(tick,start=as.Date("1994-1-1"),end=Sys.Date(),
#                      quote="AdjClose",provider="yahoo",retclass='zoo',compression='m')
tickers=read.csv('reit mf tickers.csv',stringsAsFactors=FALSE)
tick=tickers$Ticker
tick=sub(" US","",tick)
mfret=list()
mfname=vector()
i=0
#for (i in 1:length(tick)) {
i=1+i
temp=zoo(0,Sys.Date())
paste(i,'of',length(tick),tick[i])
temp=get.hist.quote.fix(tick[i],start=as.Date("1994-1-1"),end=Sys.Date(),
                      quote="AdjClose",provider="yahoo",retclass='zoo',compression='m')
  if (length(temp)>12) {
    mfname=c(mfname,tick[i])
    coretemp=coredata(temp)[-1]
    coretemp=coretemp/coretemp[1]
    coretemp=diff(log(coretemp))
    timetemp=time(temp)
    timetemp=timetemp[c(-1,-length(temp))]
    mfret=c(mfret,list(zoo(coretemp,timetemp)))
    
  }
#}
save.image("reitmfdata.rdata")
