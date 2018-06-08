#
# capture and prep data from tickers
#
require (Rblpapi)
require (zoo)
require (lubridate)
require(xts)
setwd("P:/IMD/Karl/R projects/reits/2017 Update")
# first get Bloomberg tickers
# RETRANAT Index -- average cap rate U.S. national
# FNERTR Index -- nareit total return
# SPXT Index -- S&P Total Return Index
# CPI YOY Index -- CPI All urban consumers
# USGG5YR Index -- 5 yr treasury yield
# NPPITR Index -- NCREIF total return
conn=blpConnect()
#get daily REIT and SPX for last 5 years
reit.daily=bdh("FNERTR Index","PX_LAST",start.date = Sys.Date()-5*366)
spxt.daily=bdh("SPXT Index","PX_LAST",start.date = Sys.Date()-5*366)
zreit.daily=zoo(reit.daily$PX_LAST,as.Date(reit.daily$date))
zspxt.daily=zoo(spxt.daily$PX_LAST,as.Date(spxt.daily$date))
mu.reit.daily=diff(log(zreit.daily))
mu.spxt.daily=diff(log(zspxt.daily))
#get monthly on all for last 25 years
caprates=bdh("RETRANAT Index","PX_LAST",start.date = Sys.Date()-25*366)
zcaprates=.01*zoo(caprates$PX_LAST,as.Date(caprates$date))
cpi=bdh("CPI YOY Index","PX_LAST",start.date = Sys.Date()-25*366)
zcpi=.01*zoo(cpi$PX_LAST,as.Date(cpi$date))
ncreif=bdh("NPPITR Index","PX_LAST",start.date = Sys.Date()-25*366)
zncreif=.01*zoo(ncreif$PX_LAST,as.Date(ncreif$date))
mu.ncreif=log(1+zncreif)
fiveyr=bdh("USGG5YR Index","PX_LAST",start.date = Sys.Date()-25*366)
zfiveyr=.01*xts(fiveyr$PX_LAST,as.Date(fiveyr$date))
month.end <- endpoints(zfiveyr, on = "months")
monthly <- period.apply(zfiveyr, INDEX = month.end, FUN = last)
zfiveyr <- zoo(coredata(monthly), time(monthly))
oneyr=bdh("US0012M Index","PX_LAST",start.date = Sys.Date()-25*366)
zoneyr=.01*zoo(oneyr$PX_LAST,as.Date(oneyr$date))
month.end <- endpoints(zoneyr, on = "months")
monthly <- period.apply(zoneyr, INDEX = month.end, FUN = last)
zoneyr <- zoo(coredata(monthly), time(monthly))
reit=bdh("FNERTR Index","PX_LAST",start.date = Sys.Date()-25*366)
reit.x = xts(reit$PX_LAST, as.Date(reit$date))
month.end <- endpoints(reit.x, on = "months")
monthly <- period.apply(reit.x, INDEX = month.end, FUN = last)
zreit=zoo(coredata(monthly),time(monthly))
mu.reit=diff(log(zreit))
spxt=bdh("SPXT Index","PX_LAST",start.date = Sys.Date()-25*366)
spx.x = xts(spxt$PX_LAST, as.Date(spxt$date))
month.end <- endpoints(spx.x, on = "months")
monthly <- period.apply(spx.x, INDEX = month.end, FUN = last)
zspx=zoo(coredata(monthly),time(monthly))
mu.spxt=diff(log(zspx))
#get lehmann agg from excel file
ssbt.agg = XSLD
data=read.csv("P:/IMD/Karl/R projects/Public Performance/BenchmarkData/Index_Returns.csv", 
              stringsAsFactors = FALSE)
zagg=zoo(data$XSLD,data$Date)
mu.agg=log(1+.01*zagg)
#don't forget to disconnect
blpDisconnect(conn)
save.image("reitdata 2017.rdata")
